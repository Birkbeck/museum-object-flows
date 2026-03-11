import os
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.metrics import adjusted_rand_score

TAXONOMIES_DIR = Path("../data/unlabelled-taxonomies")
OUTDIR = Path(".")  # current directory


def load_taxonomy(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    required = {"label", "layer_1_cluster", "layer_2_cluster"}
    missing = required - set(df.columns)
    if missing:
        raise ValueError(f"{path.name} missing columns: {sorted(missing)}")

    df = df.dropna(subset=["label"]).copy()
    df["label"] = df["label"].astype(str).str.strip()

    # One row per label (your CSVs typically already satisfy this, but be safe)
    df = df.drop_duplicates(subset=["label"], keep="first").copy()
    return df


def layer2_leaf_id(df: pd.DataFrame) -> pd.Series:
    """
    Define a leaf cluster id as the pair (layer_1_cluster, layer_2_cluster).
    Cast to string to avoid float/int mismatches between files.
    """
    l1 = df["layer_1_cluster"].astype(str)
    l2 = df["layer_2_cluster"].astype(str)
    return l1 + "||" + l2


def pairwise_ari(assignments: dict[str, pd.Series]) -> pd.DataFrame:
    """
    assignments: {taxonomy_name: Series indexed by label with cluster_id values}
    Returns a square DataFrame of pairwise ARI.
    """
    names = list(assignments.keys())
    ari = pd.DataFrame(index=names, columns=names, dtype=float)

    for i, a in enumerate(names):
        ari.loc[a, a] = 1.0
        for j in range(i + 1, len(names)):
            b = names[j]

            sa = assignments[a]
            sb = assignments[b]

            common = sa.index.intersection(sb.index)
            if len(common) < 2:
                score = np.nan
            else:
                score = adjusted_rand_score(
                    sa.loc[common].values, sb.loc[common].values
                )

            ari.loc[a, b] = score
            ari.loc[b, a] = score

    return ari


def save_matrix(matrix: pd.DataFrame, stem: str) -> None:
    csv_path = OUTDIR / f"{stem}.csv"
    tex_path = OUTDIR / f"{stem}.tex"

    matrix.to_csv(csv_path)

    # LaTeX (shorten filenames for readability in the paper)
    short = matrix.copy()
    short.index = [simplify_name(x) for x in short.index]
    short.columns = [simplify_name(x) for x in short.columns]

    # format floats
    tex = short.to_latex(
        float_format=lambda x: "" if pd.isna(x) else f"{x:.3f}",
        na_rep="",
        escape=False,
    )
    tex_path.write_text(tex)

    print(f"Saved:\n - {csv_path}\n - {tex_path}")


def simplify_name(filename: str) -> str:
    """
    Make table labels shorter for LaTeX.
    Example:
      taxonomy-BAAI-bge-small-en-v1.5-The new use of the building is-llm-2-10-20.csv
    -> bge-small + llm
    """
    name = filename.replace(".csv", "")

    enc = (
        "bge-small"
        if "bge-small" in name
        else ("bge-large" if "bge-large" in name else "encoder?")
    )
    aug = (
        "llm"
        if "-llm-" in name
        else (
            "null"
            if "-null-" in name
            else (
                "note" if "-note-" in name else ("wiki" if "-wiki-" in name else "aug?")
            )
        )
    )

    return f"{enc} + {aug}"


def main() -> None:
    paths = sorted([p for p in TAXONOMIES_DIR.iterdir() if p.suffix == ".csv"])
    if not paths:
        raise FileNotFoundError(f"No .csv files found in {TAXONOMIES_DIR}")

    # Load
    dfs = {p.name: load_taxonomy(p) for p in paths}

    # Build label-indexed assignments
    layer1_assignments: dict[str, pd.Series] = {}
    layer2_assignments: dict[str, pd.Series] = {}

    for name, df in dfs.items():
        # Layer 1
        l1 = (
            df.dropna(subset=["layer_1_cluster"])
            .set_index("label")["layer_1_cluster"]
            .astype(str)
        )
        layer1_assignments[name] = l1

        # Layer 2 leaf
        df2 = df.dropna(subset=["layer_1_cluster", "layer_2_cluster"]).copy()
        l2 = layer2_leaf_id(df2)
        l2.index = df2["label"].values
        l2 = pd.Series(l2.values, index=df2["label"].values, name="leaf").astype(str)
        layer2_assignments[name] = l2

    # Pairwise ARI
    ari_l1 = pairwise_ari(layer1_assignments)
    ari_l2 = pairwise_ari(layer2_assignments)

    # Save
    save_matrix(ari_l1, "ari_layer1")
    save_matrix(ari_l2, "ari_layer2")


if __name__ == "__main__":
    main()
