import os
import numpy as np
import pandas as pd

TAXONOMIES_DIR = "../data/unlabelled-taxonomies"


def _balance_stats_from_counts(counts: pd.Series) -> dict:
    """
    Given a Series of cluster sizes (counts), compute:
      - K
      - normalized entropy
      - largest/smallest cluster proportions
    """
    counts = counts.astype(float)
    K = int(counts.shape[0])

    if K == 0 or counts.sum() <= 0:
        return {
            "no_of_clusters": 0,
            "normalized_entropy": np.nan,
            "largest_cluster_proportion": np.nan,
            "smallest_cluster_proportion": np.nan,
        }

    proportions = counts / counts.sum()

    p = proportions[proportions > 0]
    entropy = -float(np.sum(p * np.log(p)))
    max_entropy = float(np.log(K))
    normalized_entropy = float(entropy / max_entropy) if max_entropy > 0 else 0.0

    return {
        "no_of_clusters": K,
        "normalized_entropy": normalized_entropy,
        "largest_cluster_proportion": float(proportions.max()),
        "smallest_cluster_proportion": float(proportions.min()),
    }


def layer1_balance_stats(taxonomy_df: pd.DataFrame) -> dict:
    """
    Layer-1 balance statistics based on number of UNIQUE labels per layer_1_cluster.
    """
    required_cols = {"label", "layer_1_cluster"}
    missing = required_cols - set(taxonomy_df.columns)
    if missing:
        raise ValueError(f"Missing required columns: {missing}")

    df = (
        taxonomy_df[["label", "layer_1_cluster"]]
        .dropna(subset=["label", "layer_1_cluster"])
        .copy()
    )
    df["label"] = df["label"].astype(str)

    # Unique labels only
    df = df.drop_duplicates(subset=["label"])

    counts = df.groupby("layer_1_cluster")["label"].nunique()
    return _balance_stats_from_counts(counts)


def layer12_balance_stats(taxonomy_df: pd.DataFrame) -> dict:
    """
    (Layer-1, Layer-2) balance statistics based on number of UNIQUE labels per
    (layer_1_cluster, layer_2_cluster) pair.
    """
    required_cols = {"label", "layer_1_cluster", "layer_2_cluster"}
    missing = required_cols - set(taxonomy_df.columns)
    if missing:
        raise ValueError(f"Missing required columns: {missing}")

    df = (
        taxonomy_df[["label", "layer_1_cluster", "layer_2_cluster"]]
        .dropna(subset=["label", "layer_1_cluster", "layer_2_cluster"])
        .copy()
    )
    df["label"] = df["label"].astype(str)

    # Unique labels only
    df = df.drop_duplicates(subset=["label"])

    counts = df.groupby(["layer_1_cluster", "layer_2_cluster"])["label"].nunique()
    return _balance_stats_from_counts(counts)


def main():
    taxonomy_files = [
        os.path.join(TAXONOMIES_DIR, f)
        for f in os.listdir(TAXONOMIES_DIR)
        if f.endswith(".csv")
    ]

    rows = []

    for fpath in sorted(taxonomy_files):
        taxonomy = pd.read_csv(fpath)

        s1 = layer1_balance_stats(taxonomy)
        s12 = layer12_balance_stats(taxonomy)

        rows.append(
            {
                "taxonomy": os.path.basename(fpath),
                "layer": "L1",
                "no_of_clusters": s1["no_of_clusters"],
                "normalized_entropy": s1["normalized_entropy"],
                "largest_cluster_proportion": s1["largest_cluster_proportion"],
                "smallest_cluster_proportion": s1["smallest_cluster_proportion"],
            }
        )
        rows.append(
            {
                "taxonomy": os.path.basename(fpath),
                "layer": "L1+L2",
                "no_of_clusters": s12["no_of_clusters"],
                "normalized_entropy": s12["normalized_entropy"],
                "largest_cluster_proportion": s12["largest_cluster_proportion"],
                "smallest_cluster_proportion": s12["smallest_cluster_proportion"],
            }
        )

    results_df = pd.DataFrame(rows)

    # Sort for stable output
    results_df = results_df.sort_values(
        by=["layer", "normalized_entropy"], ascending=[True, False]
    )

    # Format numeric columns for LaTeX
    for col in [
        "normalized_entropy",
        "largest_cluster_proportion",
        "smallest_cluster_proportion",
    ]:
        results_df[col] = results_df[col].map(
            lambda x: f"{x:.4f}" if pd.notna(x) else ""
        )

    results_df["no_of_clusters"] = results_df["no_of_clusters"].astype(int)

    # --- Print two LaTeX tables: one for L1 and one for L1+L2 ---
    for layer_key, layer_title in [("L1", "Layer 1"), ("L1+L2", "Layer 1 + Layer 2")]:
        sub = results_df[results_df["layer"] == layer_key].copy()

        # Optional: drop the 'layer' column since it's constant within each table
        sub = sub.drop(columns=["layer"])

        print(f"\nLaTeX table ({layer_title}):\n")
        print(sub.to_latex(index=False, escape=False))


if __name__ == "__main__":
    main()
