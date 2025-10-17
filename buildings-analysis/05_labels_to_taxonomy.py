import re

import numpy as np
import pandas as pd
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

NOTES_WITH_LABELS_FILE = "classifications/llama3-mixed-longer-5shots-t0.5.csv"
NOTES_LABELS_EMBEDDINGS_FILE = "classifications/labels-notes-and-embeddings.parquet"

# SENTENCE_MODEL = SentenceTransformer("all-mpnet-base-v2")
SENTENCE_MODEL = SentenceTransformer("BAAI/bge-large-en-v1.5")

CLUSTER_LABEL_MODEL_NAME = "google/flan-t5-base"
CLUSTER_LABEL_MODEL = pipeline(
    "text2text-generation",
    model=AutoModelForSeq2SeqLM.from_pretrained(
        CLUSTER_LABEL_MODEL_NAME,
        device_map="auto",
        torch_dtype=(
            torch.float16 if torch.backends.mps.is_available() else torch.float32
        ),
    ),
    tokenizer=AutoTokenizer.from_pretrained(CLUSTER_LABEL_MODEL_NAME),
    max_new_tokens=8,
)


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
    labelled_texts["label_and_note"] = labelled_texts.apply(
        lambda row: f"{row['label']} {row['note']}", axis=1
    )
    unique_labels = list_unique(labelled_texts, "label")
    label_embeddings = SENTENCE_MODEL.encode(
        [_contextualize_label(l) for l in unique_labels], normalize_embeddings=True
    )
    label_to_embedding = dict(zip(unique_labels, [e for e in label_embeddings]))
    labelled_texts["label_embedding"] = labelled_texts["label"].map(label_to_embedding)
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


def _contextualize_label(s: str) -> str:
    if not isinstance(s, str):
        return ""
    return f"The new use of the building is {s}."


def _l2_normalize(v):
    v = np.asarray(v, dtype=np.float32)
    v = np.nan_to_num(v, nan=0.0, posinf=0.0, neginf=0.0)
    n = np.linalg.norm(v)
    return (v / n).astype(np.float32) if n > 0 else v


def list_unique(df, column):
    vals = df[column].dropna().astype(str).map(_normalize_label)
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
    print(embeddings)
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
        print(k, score)
        if score > highest_score:
            highest_score = score
            best_clusters = clusters
    df[f"{embeddings_column}_cluster"] = best_clusters
    return df, highest_score


def name_clusters(df: pd.DataFrame, cluster_id_column: str):
    cluster_to_name = {}
    for cluster_id, subset in df.groupby(cluster_id_column, dropna=False, sort=True):
        unique_labels = sorted(set(subset["label"].dropna()))
        cluster_to_name[cluster_id] = _label_labels(unique_labels)
    out = df.copy()
    out["cluster_name"] = out[cluster_id_column].map(cluster_to_name)
    return out


def _label_labels(labels: list) -> str:
    prompt = (
        "You create short taxonomy labels. "
        "Return ONLY a concise and general 2–4 word category (no punctuation) that all of these sub-categories belong to.\n\n"
        "Sub-categories: " + ", ".join(labels) + "\nCategory:"
    )
    response = CLUSTER_LABEL_MODEL(prompt, do_sample=False, num_beams=1)[0][
        "generated_text"
    ]
    return response.strip()


if __name__ == "__main__":
    labelled_texts_with_embeddings = get_labels_and_texts_with_embeddings()

    with_clusters, score = kmeans_on_embedding_column(
        labelled_texts_with_embeddings,
        "label_embedding",
        min_k=10,
        max_k=20,
    )
    with_named_clusters = name_clusters(with_clusters, "label_embedding_cluster")

    for cluster_id, subset in with_named_clusters.groupby(
        "label_embedding_cluster", sort=True
    ):
        unique_labels = sorted(set(subset["label"].dropna()))
        print(
            f"Cluster {cluster_id} {subset.iloc[0]['cluster_name']} ({len(subset)} rows):"
        )
        for lbl in unique_labels:
            print(f"  - {lbl}")
        print()
