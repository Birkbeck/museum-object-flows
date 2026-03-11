import os
from itertools import combinations

import numpy as np
import pandas as pd
from sentence_transformers import SentenceTransformer

TAXONOMIES_DIR = "../data/unlabelled-taxonomies"

# Load models once
SMALL_MODEL = SentenceTransformer("BAAI/bge-small-en-v1.5")
LARGE_MODEL = SentenceTransformer("BAAI/bge-large-en-v1.5")


def infer_encoder_from_filename(filename: str) -> str:
    name = filename.lower()
    if "bge-small-en-v1.5" in name:
        return "bge-small"
    if "bge-large-en-v1.5" in name:
        return "bge-large"
    raise ValueError(f"Could not infer encoder from filename: {filename}")


def get_model_for_encoder(encoder_key: str) -> SentenceTransformer:
    if encoder_key == "bge-small":
        return SMALL_MODEL
    if encoder_key == "bge-large":
        return LARGE_MODEL
    raise ValueError(f"Unknown encoder_key: {encoder_key}")


def mean_pairwise_cosine(vectors: np.ndarray) -> float:
    """Mean pairwise cosine similarity within a cluster (vectors assumed L2-normalized)."""
    n = vectors.shape[0]
    if n < 2:
        return np.nan
    sims = [float(np.dot(vectors[i], vectors[j])) for i, j in combinations(range(n), 2)]
    return float(np.mean(sims)) if sims else np.nan


def cluster_geometry_stats(
    df_labels: pd.DataFrame, emb_col: str, cluster_cols: list[str]
) -> dict:
    """
    Computes:
      - mean_within_similarity: mean over clusters (size >=2) of mean pairwise cosine
      - mean_nearest_centroid_similarity: mean over clusters of similarity to nearest other centroid
      - n_clusters_used: number of clusters with >=2 labels
    """
    within_scores: list[float] = []
    centroids: list[np.ndarray] = []

    for _, group in df_labels.groupby(cluster_cols):
        vecs = np.vstack(group[emb_col].values)
        if vecs.shape[0] < 2:
            continue

        within_scores.append(mean_pairwise_cosine(vecs))

        c = vecs.mean(axis=0)
        norm = np.linalg.norm(c)
        if norm > 0:
            c = c / norm
        centroids.append(c)

    nn_sims: list[float] = []
    if len(centroids) > 1:
        C = np.vstack(centroids)
        S = C @ C.T
        np.fill_diagonal(S, -1.0)
        nn_sims = list(np.max(S, axis=1))

    mean_within = float(np.nanmean(within_scores)) if within_scores else np.nan
    mean_nn = float(np.mean(nn_sims)) if nn_sims else np.nan

    return {
        "mean_within_similarity": mean_within,
        "mean_nearest_centroid_similarity": mean_nn,
        # separation: higher (closer to 0) is better
        "separation": float(mean_within - mean_nn)
        if (pd.notna(mean_within) and pd.notna(mean_nn))
        else np.nan,
        "n_clusters_used": int(len(centroids)),
    }


def to_latex_table(df: pd.DataFrame, title: str) -> None:
    out = df.copy()

    # Sort by separation (descending: higher is better)
    out = out.sort_values(by="separation", ascending=False).reset_index(drop=True)

    # Format for LaTeX
    for col in [
        "mean_within_similarity",
        "mean_nearest_centroid_similarity",
        "separation",
    ]:
        out[col] = out[col].map(lambda x: f"{x:.3f}" if pd.notna(x) else "")
    out["n_clusters_used"] = out["n_clusters_used"].astype(int)

    print(f"\n{title}\n")
    print(out.to_latex(index=False, escape=False))


def main() -> None:
    taxonomy_files = [
        os.path.join(TAXONOMIES_DIR, f)
        for f in os.listdir(TAXONOMIES_DIR)
        if f.endswith(".csv")
    ]

    layer1_rows = []
    layer2_rows = []

    for fpath in sorted(taxonomy_files):
        fname = os.path.basename(fpath)

        encoder_key = infer_encoder_from_filename(fname)
        model = get_model_for_encoder(encoder_key)

        taxonomy = pd.read_csv(fpath)

        # Validate columns
        for col in ["label", "layer_1_cluster", "layer_2_cluster"]:
            if col not in taxonomy.columns:
                raise ValueError(f"Missing '{col}' column in {fname}")

        # Prepare labels
        taxonomy = taxonomy.dropna(subset=["label"]).copy()
        taxonomy["label"] = taxonomy["label"].astype(str).str.strip()

        # Embed unique labels and map back to rows (avoids index alignment issues)
        unique_labels = sorted(taxonomy["label"].unique().tolist())
        label_embs = model.encode(unique_labels, normalize_embeddings=True)
        label_to_emb = dict(zip(unique_labels, label_embs))
        taxonomy["embedding"] = taxonomy["label"].map(label_to_emb)

        # Deduplicate by label so each label counts once
        taxonomy_u = taxonomy.drop_duplicates(subset=["label"], keep="first").copy()

        # --- Layer 1 ---
        l1_df = taxonomy_u.dropna(subset=["layer_1_cluster"]).copy()
        stats_l1 = cluster_geometry_stats(
            l1_df[["label", "layer_1_cluster", "embedding"]],
            emb_col="embedding",
            cluster_cols=["layer_1_cluster"],
        )
        layer1_rows.append({"taxonomy": fname, **stats_l1})

        # --- Layer 2 (leaf clusters) ---
        l2_df = taxonomy_u.dropna(subset=["layer_1_cluster", "layer_2_cluster"]).copy()
        stats_l2 = cluster_geometry_stats(
            l2_df[["label", "layer_1_cluster", "layer_2_cluster", "embedding"]],
            emb_col="embedding",
            cluster_cols=["layer_1_cluster", "layer_2_cluster"],
        )
        layer2_rows.append({"taxonomy": fname, **stats_l2})

    layer1_df = pd.DataFrame(layer1_rows)
    layer2_df = pd.DataFrame(layer2_rows)

    to_latex_table(layer1_df, "LaTeX table: Layer 1 Geometry (sorted by separation)")
    to_latex_table(layer2_df, "LaTeX table: Layer 2 Geometry (sorted by separation)")


if __name__ == "__main__":
    main()
