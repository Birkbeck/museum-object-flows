import pandas as pd
from sentence_transformers import SentenceTransformer, util
import torch
from transformers import (
    AutoTokenizer,
    AutoModelForCausalLM,
    AutoModelForSeq2SeqLM,
    pipeline,
    set_seed,
)

CLASSIFICATIONS_DIR = "classifications"
CLUSTERS_FILE = f"{CLASSIFICATIONS_DIR}/labels-notes-clustered-with-llama.csv"

CACHED_EMBEDDINGS = {}

LABELLING_MODEL_NAME = "google/flan-t5-base"
PIPELINE_TYPE = "text2text-generation"
LABELLING_MODEL = pipeline(
    PIPELINE_TYPE,
    model=AutoModelForSeq2SeqLM.from_pretrained(
        LABELLING_MODEL_NAME,
        device_map="auto",
        dtype=(torch.float16 if torch.backends.mps.is_available() else torch.float32),
    ),
    tokenizer=AutoTokenizer.from_pretrained(LABELLING_MODEL_NAME),
    max_new_tokens=8,
)

SENTENCE_MODEL = SentenceTransformer("all-mpnet-base-v2")

ROLES = {
    "noRole": "",
    "short": "You create short taxonomy labels.",
}

TASKS = {
    "noInstruction": "",
    "instruction": "Return ONLY a concise and general 2-4 word category (no punctuation) that is a hypernym of the given categories",
    "no repeats": "Return ONLY a concise and general 2-4 word category (no punctuation) that is a hypernym of the given categories. Do not repeat from the list of labels.",
}


def name_clusters(
    df: pd.DataFrame, cluster_id_column: str, label_column: str, role: str, task: str
) -> pd.DataFrame:
    cluster_to_name = {}
    for cluster_id, subset in df.groupby(cluster_id_column, dropna=False, sort=True):
        unique_labels = sorted(set(subset[label_column].dropna()))
        cluster_to_name[cluster_id] = _label_labels(unique_labels, role, task)
    out = df.copy()
    out[f"{cluster_id_column}_name"] = out[cluster_id_column].map(cluster_to_name)
    return out


def _label_labels(labels: list, role: str, task: str) -> str:
    prompt = (
        role + "\n" + task + "\n\n"
        "Sub-categories: " + ", ".join(labels) + "\nCategory:"
    )
    response = LABELLING_MODEL(
        prompt,
        do_sample=False,
        num_beams=1,
        # do_sample=True,
        # temperature=0.7,
        # top_p=0.9,
        # repetition_penalty=1.1,
        max_new_tokens=10,
    )[0]["generated_text"]
    if PIPELINE_TYPE == "text-generation":
        response = response[len(prompt) :]
    response = response.strip().lower()
    print(prompt)
    print("-> ", response)
    print()
    return response


def get_cached_embedding(text):
    try:
        return CACHED_EMBEDDINGS[text]
    except KeyError:
        CACHED_EMBEDDINGS[text] = SENTENCE_MODEL.encode(text, convert_to_tensor=True)
        return CACHED_EMBEDDINGS[text]


def calculate_label_similarity(cluster_label, sub_label):
    """Returns cosine similarity of candidate and human labels"""
    if cluster_label == "":
        return 0.0
    similarity = util.cos_sim(
        get_cached_embedding(cluster_label), get_cached_embedding(sub_label)
    )
    return similarity.item()


clusters = pd.read_csv(CLUSTERS_FILE)
role = ROLES["short"]
task = TASKS["no repeats"]
clusters = name_clusters(clusters, "core_cluster", "label", role, task)

clusters = pd.concat(
    [
        name_clusters(subset, "sub_cluster", "label", role, task)
        for _, subset in clusters.groupby("core_cluster")
    ]
)

clusters["core_label_similarity"] = clusters.apply(
    lambda row: calculate_label_similarity(row["core_cluster_name"], row["label"]),
    axis=1,
)
clusters["sub_label_similarity"] = clusters.apply(
    lambda row: calculate_label_similarity(row["sub_cluster_name"], row["label"]),
    axis=1,
)

for cluster_id in clusters["core_cluster"].unique():
    clusters["sub_label_similarity"] = clusters.apply(
        lambda row: calculate_label_similarity(row["sub_cluster_name"], row["label"]),
        axis=1,
    )

core_evals = clusters.groupby("core_cluster")["core_label_similarity"].agg(
    coherence=lambda x: x.mean(),
    coverage=lambda x: x.min(),
)
sub_evals = clusters.groupby(["core_cluster", "sub_cluster"])[
    "core_label_similarity"
].agg(
    coherence=lambda x: x.mean(),
    coverage=lambda x: x.min(),
)
overall_evals = pd.concat([core_evals, sub_evals])

overall_coherence = overall_evals["coherence"].mean()
overall_coverage = overall_evals["coverage"].mean()
print(overall_coherence)
print(overall_coverage)
