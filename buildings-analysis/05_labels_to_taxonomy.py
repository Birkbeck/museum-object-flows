import re

import numpy as np
import pandas as pd
import requests
from requests.adapters import HTTPAdapter
from sentence_transformers import SentenceTransformer
from sklearn.cluster import KMeans, MiniBatchKMeans
from sklearn.metrics import silhouette_score
from sklearn.preprocessing import normalize
import torch
from transformers import (
    AutoTokenizer,
    AutoModelForCausalLM,
    AutoModelForSeq2SeqLM,
    pipeline,
    set_seed,
)
from urllib3.util.retry import Retry

GET_CONTEXT_FROM_WIKIPEDIA = False
GET_CONTEXT_FROM_GENERATIVE_LLM = True

CLASSIFICATIONS_DIR = "scratch/classifications"

NOTES_WITH_LABELS_FILE = f"{CLASSIFICATIONS_DIR}/llama3-mixed-longer-5shots-t0.5.csv"
NOTES_LABELS_EMBEDDINGS_FILE = (
    # "classifications/labels-notes-and-embeddings-wiki.parquet"
    # "classifications/labels-notes-and-embeddings-flan-t5-base.parquet"
    f"{CLASSIFICATIONS_DIR}/labels-notes-and-embeddings-llama.parquet"
)
NOTES_LABELS_CLUSTERED_FILE = (
    f"{CLASSIFICATIONS_DIR}/labels-notes-clustered-with-llama.csv"
)
BUILDING_USE_HIERARCHY_FILE = f"{CLASSIFICATIONS_DIR}/building-use-types-with-llama.csv"

# SENTENCE_MODEL = SentenceTransformer("all-mpnet-base-v2")
# SENTENCE_MODEL = SentenceTransformer("BAAI/bge-large-en-v1.5")
SENTENCE_MODEL = SentenceTransformer("BAAI/bge-small-en-v1.5")

# CLUSTER_LABEL_MODEL_NAME = "google/flan-t5-base"
# CLUSTER_LABEL_MODEL = pipeline(
#    "text2text-generation",
#    model=AutoModelForSeq2SeqLM.from_pretrained(
#        CLUSTER_LABEL_MODEL_NAME,
#        device_map="auto",
#        dtype=(torch.float16 if torch.backends.mps.is_available() else torch.float32),
#    ),
#    tokenizer=AutoTokenizer.from_pretrained(CLUSTER_LABEL_MODEL_NAME),
#    max_new_tokens=8,
# )
TEXT_GENERATION_MODEL_NAME = "/scratch/users/k2480370/llama3.1-8B"
TEXT_GENERATION_MODEL = pipeline(
    "text-generation",
    model=AutoModelForCausalLM.from_pretrained(
        TEXT_GENERATION_MODEL_NAME,
        trust_remote_code=True,
    ),
    tokenizer=AutoTokenizer.from_pretrained(
        TEXT_GENERATION_MODEL_NAME,
        trust_remote_code=True,
    ),
    device=0,
    pad_token_id=0,
)
CLUSTER_LABEL_MODEL = TEXT_GENERATION_MODEL

WIKIPEDIA_SESSION = requests.Session()
WIKIPEDIA_SESSION.headers.update(
    {"User-Agent": "BuildingUseTaxonomy/0.1 (george.wright@bbk.ac.uk)"}
)
WIKIPEDIA_RETRIES = Retry(
    total=5,
    backoff_factor=0.5,  # 0.5s, 1s, 2s, ...
    status_forcelist=[429, 500, 502, 503, 504],
    allowed_methods=["GET"],
    raise_on_status=False,
)
WIKIPEDIA_SESSION.mount("https://", HTTPAdapter(max_retries=WIKIPEDIA_RETRIES))


def get_labels_and_texts_with_embeddings():
    try:
        labelled_texts = pd.read_parquet(NOTES_LABELS_EMBEDDINGS_FILE)
        for c in [
            "label_embedding",
            "note_embedding",
            "label_note_concatenation_embedding",
            "label_note_embedding_sum",
        ]:
            labelled_texts[c] = labelled_texts[c].apply(
                lambda v: np.array(v, dtype=np.float32)
            )
        return labelled_texts
    except FileNotFoundError:
        pass
    labelled_texts = (
        pd.read_csv(NOTES_WITH_LABELS_FILE)
        .rename(columns={"building_notes": "note", "predicted_change_in_use": "label"})
        .loc[:, ["name", "note", "label"]]
    )
    labelled_texts = labelled_texts[
        labelled_texts["label"].notna()
        & (labelled_texts["label"].str.strip() != "")
        & labelled_texts["note"].notna()
        & (labelled_texts["note"].str.strip() != "")
    ]
    labelled_texts["label"] = labelled_texts["label"].map(
        lambda x: x.replace("cafã©", "cafe").replace("cafã", "cafe")
    )
    labelled_texts["label"] = labelled_texts["label"].astype(str).str.split(r"\s*;\s*")
    labelled_texts = labelled_texts.explode("label", ignore_index=True)
    labelled_texts["label"] = labelled_texts["label"].map(_normalize_label)
    labelled_texts = labelled_texts[labelled_texts["label"] != ""].reset_index(
        drop=True
    )
    labelled_texts["contextualized_label"] = labelled_texts.apply(
        lambda row: _contextualize_label(
            row["label"],
            row["note"],
            GET_CONTEXT_FROM_WIKIPEDIA,
            GET_CONTEXT_FROM_GENERATIVE_LLM,
        ),
        axis=1,
    )
    labelled_texts["label_and_note"] = labelled_texts.apply(
        lambda row: f"{row['label']} {row['note']}", axis=1
    )
    unique_labels = list_unique(labelled_texts, "contextualized_label")
    label_embeddings = SENTENCE_MODEL.encode(unique_labels, normalize_embeddings=True)
    label_to_embedding = dict(zip(unique_labels, [e for e in label_embeddings]))
    labelled_texts["label_embedding"] = labelled_texts["contextualized_label"].map(
        label_to_embedding
    )
    labelled_texts["note_embedding"] = list(
        SENTENCE_MODEL.encode(
            labelled_texts["note"].tolist(),
            normalize_embeddings=True,
        )
    )
    labelled_texts["label_note_concatenation_embedding"] = list(
        SENTENCE_MODEL.encode(
            labelled_texts["label_and_note"].tolist(),
            normalize_embeddings=True,
        )
    )
    labelled_texts["label_note_embedding_sum"] = [
        _l2_normalize(
            labelled_texts["label_embedding"].iat[i]
            + labelled_texts["note_embedding"].iat[i]
        )
        for i in range(len(labelled_texts))
    ]
    # save embeddings to parquet
    for col in [
        "label_embedding",
        "note_embedding",
        "label_note_concatenation_embedding",
        "label_note_embedding_sum",
    ]:
        labelled_texts[col] = labelled_texts[col].apply(
            lambda x: x.tolist() if isinstance(x, np.ndarray) else x
        )
    labelled_texts.to_parquet(NOTES_LABELS_EMBEDDINGS_FILE, index=False)
    return labelled_texts


def _normalize_label(s: str) -> str:
    if not isinstance(s, str):
        return ""
    s = s.replace("?", " ")
    s = s.replace("\n", " ")
    s = re.sub(r"\s+", " ", s).strip()
    return s


def _contextualize_label(
    s: str,
    note: str,
    add_wikipedia_text: bool = True,
    add_generative_llm_text: bool = False,
) -> str:
    if not isinstance(s, str):
        return ""
    contextualized_label = f"The new use of the building is {s}."
    if add_wikipedia_text:
        print(f"Getting Wikipedia context for label: {s}")
        wiki_context = get_wiki_context_for_label(s)
        contextualized_label += f" {wiki_context}"
    if add_generative_llm_text:
        print(f"Getting LLM context for label: {s}")
        llm_context = get_llm_context_for_label(s, note)
        contextualized_label += f" {llm_context}"
    return contextualized_label


def wiki_search(label, n=5, lang="en"):
    params = {
        "action": "query",
        "list": "search",
        "srsearch": label,
        "srlimit": n,
        "format": "json",
        "utf8": 1,
    }
    r = WIKIPEDIA_SESSION.get(
        f"https://{lang}.wikipedia.org/w/api.php", params=params, timeout=10
    )
    r.raise_for_status()  # will retry on 429/5xx due to adapter
    data = r.json()
    return [hit["title"] for hit in data.get("query", {}).get("search", [])]


def wiki_intro(title, lang="en"):
    # REST summary endpoint (also needs UA)
    r = WIKIPEDIA_SESSION.get(
        f"https://{lang}.wikipedia.org/api/rest_v1/page/summary/{requests.utils.quote(title)}",
        headers={"accept": "application/json"},
        timeout=10,
    )
    if r.status_code != 200:
        return ""
    data = r.json()
    if data.get("type") == "disambiguation":
        return ""
    txt = (data.get("extract") or "").strip()
    return re.sub(r"\s+", " ", txt)


def get_wiki_context_for_label(label, top_k=5, minimum_relevance=0.7, lang="en"):
    titles = wiki_search(label, n=top_k, lang=lang)
    # remove titles which are proper nouns with same name as label
    titles = [t for t in titles if len(label.split()) == 1 or t != label.title()]
    if not titles:
        return ""
    label_embedding = SENTENCE_MODEL.encode([label], normalize_embeddings=True)
    candidate_texts = [wiki_intro(t, lang=lang) for t in titles]
    pairs = [(t, c) for t, c in zip(titles, candidate_texts)]
    if not pairs:
        return ""
    candidate_embeddings = SENTENCE_MODEL.encode(
        [c for _, c in pairs], normalize_embeddings=True
    )
    similarity_scores = (candidate_embeddings @ label_embedding.T).ravel()
    order = np.argsort(-similarity_scores)
    total_relevance = 0
    context_texts = []
    for i in order[:top_k]:
        context_texts.append(pairs[i][1])
        total_relevance += similarity_scores[i]
        if total_relevance >= minimum_relevance:
            break
    snippet = " ".join(context_texts)
    return snippet


def get_llm_context_for_label(label, note):
    prompt = (
        "The following text has been summarised with a building use type label."
        "Provide a concise definition of the building use type label in this context.\n\n"
        f"Text: {note}\n\n"
        f"Building use type: {label}\n\n"
        "Definition:"
    )
    response = TEXT_GENERATION_MODEL(
        prompt,
        num_return_sequences=1,
        max_new_tokens=100,
        temperature=0.7,
    )[0]["generated_text"][len(prompt) :].strip()
    return response


def _l2_normalize(v):
    v = np.asarray(v, dtype=np.float32)
    v = np.nan_to_num(v, nan=0.0, posinf=0.0, neginf=0.0)
    n = np.linalg.norm(v)
    return (v / n).astype(np.float32) if n > 0 else v


def list_unique(df, column):
    vals = df[column].dropna().astype(str)
    return sorted(set(v for v in vals if v))


def kmeans_on_embedding_column(
    df: pd.DataFrame,
    embeddings_column: str,
    min_k: int,
    max_k: int,
    minibatch: bool = False,  # use MiniBatchKMeans for large N
    random_state: int = 42,
):
    work = df.copy()
    embeddings = normalize(
        np.vstack(
            work[embeddings_column]
            .apply(lambda v: np.asarray(v, dtype=np.float32))
            .to_numpy()
        ),
        norm="l2",
    )
    highest_score = 0
    best_clusters = None
    for k in range(min_k, max_k + 1):
        estimator = (
            MiniBatchKMeans(
                n_clusters=k, batch_size=2048, n_init=10, random_state=random_state
            )
            if minibatch
            else KMeans(n_clusters=k, n_init=10, random_state=random_state)
        )
        clusters = estimator.fit_predict(embeddings)
        score = (
            silhouette_score(embeddings, clusters)
            if k > 1 and len(np.unique(clusters)) > 1
            else np.nan
        )
        if score > highest_score:
            highest_score = score
            best_clusters = clusters
    df[f"{embeddings_column}_cluster"] = best_clusters
    return df, highest_score


def name_clusters(
    df: pd.DataFrame, cluster_id_column: str, label_column: str
) -> pd.DataFrame:
    cluster_to_name = {}
    for cluster_id, subset in df.groupby(cluster_id_column, dropna=False, sort=True):
        unique_labels = sorted(set(subset[label_column].dropna()))
        cluster_to_name[cluster_id] = _label_labels(unique_labels)
    out = df.copy()
    out[f"{cluster_id_column}_name"] = out[cluster_id_column].map(cluster_to_name)
    return out


def _label_labels(labels: list) -> str:
    prompt = (
        "You create short taxonomy labels. "
        "Return ONLY a concise and general 2–4 word category (no punctuation) that all of these sub-categories belong to.\n\n"
        "Sub-categories: " + ", ".join(labels) + "\nCategory:"
    )
    # response = CLUSTER_LABEL_MODEL(prompt, do_sample=False, num_beams=1)[0][
    #    "generated_text"
    # ]
    # return response.strip().lower()
    response = (
        TEXT_GENERATION_MODEL(
            prompt,
            num_return_sequences=1,
            max_new_tokens=10,
            temperature=0.7,
        )[0]["generated_text"][len(prompt) :]
        .strip()
        .lower()
    )
    return response


if __name__ == "__main__":
    labelled_texts_with_embeddings = get_labels_and_texts_with_embeddings()

    labels_with_clusters, score = kmeans_on_embedding_column(
        labelled_texts_with_embeddings,
        "label_embedding",
        min_k=10,
        max_k=20,
    )

    sub_cluster_data_frames = []
    for cluster_id, subset in labels_with_clusters.groupby("label_embedding_cluster"):
        subset["label_embedding_specific"] = subset["label_embedding"]
        print(cluster_id, len(subset))
        labels_with_sub_clusters, score = kmeans_on_embedding_column(
            subset,
            "label_embedding_specific",
            min_k=min(2, len(subset)),
            max_k=min(10, len(subset) - 1),
        )
        labels_with_named_sub_clusters = name_clusters(
            labels_with_sub_clusters, "label_embedding_specific_cluster", "label"
        )
        sub_cluster_data_frames.append(labels_with_named_sub_clusters)

    labels_with_clusters_and_named_sub_clusters = pd.concat(
        sub_cluster_data_frames, ignore_index=True
    )
    labels_with_named_clusters_and_sub_clusters = name_clusters(
        labels_with_clusters_and_named_sub_clusters,
        "label_embedding_cluster",
        "label_embedding_specific_cluster_name",
    )

    final_classifications = labels_with_named_clusters_and_sub_clusters[
        [
            "name",
            "note",
            "label",
            "contextualized_label",
            "label_embedding_cluster",
            "label_embedding_cluster_name",
            "label_embedding_specific_cluster",
            "label_embedding_specific_cluster_name",
        ]
    ]
    final_classifications["core_use_type"] = final_classifications.apply(
        lambda row: f"{row['label_embedding_cluster_name']} ({row['label_embedding_cluster']})",
        axis=1,
    )
    final_classifications["use_type"] = final_classifications.apply(
        lambda row: f"{row['label_embedding_specific_cluster_name']} ({row['label_embedding_specific_cluster']})",
        axis=1,
    )
    final_classifications.sort_values(
        by=["label_embedding_cluster", "label_embedding_specific_cluster"]
    )[
        ["name", "note", "core_use_type", "use_type", "label", "contextualized_label"]
    ].to_csv(
        NOTES_LABELS_CLUSTERED_FILE, index=False
    )

    core_use_types = (
        final_classifications[["core_use_type"]]
        .drop_duplicates()
        .rename(columns={"core_use_type": "type_name"})
    )
    core_use_types["sub_type_of"] = ""
    core_use_types["is_core_category"] = True

    specific_use_types = (
        final_classifications[["core_use_type", "use_type"]]
        .drop_duplicates()
        .assign(use_type=lambda d: d["use_type"].astype("string").str.strip())
        .loc[
            lambda d: d["use_type"].notna()
            & d["use_type"].ne("")
            & d["use_type"].ne(d["core_use_type"])
        ]
        .query("use_type != ''")
        .query("use_type != 'nan (None)'")
        .rename(columns={"use_type": "type_name", "core_use_type": "sub_type_of"})
    )
    specific_use_types["is_core_category"] = False

    use_hierarchy = pd.concat([core_use_types, specific_use_types], ignore_index=True)
    use_hierarchy.to_csv(BUILDING_USE_HIERARCHY_FILE, index=False)

    print(len(set(final_classifications["label"].dropna())))
