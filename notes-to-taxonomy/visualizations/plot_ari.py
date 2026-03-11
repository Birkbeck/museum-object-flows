import os
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
from sklearn.metrics import adjusted_rand_score

TAXONOMIES_DIR = Path("../data/unlabelled-taxonomies")
OUTDIR = Path(".")  # current directory

# Output filenames
L1_CSV = OUTDIR / "ari_layer1.csv"
L2_CSV = OUTDIR / "ari_layer2.csv"
L1_PNG = OUTDIR / "ari_layer1_heatmap.png"
L2_PNG = OUTDIR / "ari_layer2_heatmap.png"


def load_taxonomy(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    required = {"label", "layer_1_cluster", "layer_2_cluster"}
    missing = required - set(df.columns)
    if missing:
        raise ValueError(f"{path.name} missing columns: {sorted(missing)}")

    df = df.dropna(subset=["label"]).copy()
    df["label"] = df["label"].astype(str).str.strip()

    # One row per label (be safe)
    df = df.drop_duplicates(subset=["label"], keep="first").copy()
    return df


def layer2_leaf_id(df: pd.DataFrame) -> pd.Series:
    """Leaf cluster id as (layer_1_cluster, layer_2_cluster)."""
    l1 = df["layer_1_cluster"].astype(str)
    l2 = df["layer_2_cluster"].astype(str)
    return l1 + "||" + l2


def simplify_name(filename: str) -> str:
    """
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
        "Llama"
        if "-llm-" in name
        else (
            "null"
            if "-null-" in name
            else (
                "note" if "-note-" in name else ("Wiki" if "-wiki-" in name else "aug?")
            )
        )
    )

    return f"{enc} + {aug}"


def pairwise_ari(assignments: dict[str, pd.Series]) -> pd.DataFrame:
    """
    assignments: {taxonomy_name: Series indexed by label with cluster_id values}
    Returns a square DataFrame of pairwise ARI (computed over label intersection).
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


def reorder_methods(labels: list[str]) -> list[str]:
    def key(s: str) -> tuple:
        # expected format: "bge-large + llm"
        parts = [p.strip() for p in s.split("+")]
        enc = parts[0] if parts else s
        aug = parts[1] if len(parts) > 1 else ""

        aug_rank = {"Llama": 0, "null": 1, "Wiki": 2, "note": 3}.get(aug, 99)
        enc_rank = {"bge-large": 0, "bge-small": 1}.get(enc, 99)

        return (aug_rank, enc_rank, s)

    return sorted(labels, key=key)


def plot_heatmap(
    matrix: pd.DataFrame,
    title: str,
    outpath: Path,
    annotate: bool = True,
    vmin: float = 0.0,
    vmax: float = 1.0,
) -> None:
    """
    Plot a tile chart heatmap using matplotlib with a white → lime green scale.
    """

    # Custom white → lime colormap
    white_lime = LinearSegmentedColormap.from_list(
        "white_lime",
        ["#ffffff", "#32CD32"],  # white → lime green
    )

    short = matrix.copy()
    short.index = [simplify_name(x) for x in short.index]
    short.columns = [simplify_name(x) for x in short.columns]

    order = reorder_methods(short.index.tolist())
    short = short.loc[order, order]

    data = short.values.astype(float)

    fig, ax = plt.subplots(figsize=(10, 8))
    im = ax.imshow(
        data,
        cmap=white_lime,
        vmin=vmin,
        vmax=vmax,
        aspect="equal",
    )

    ax.set_title(title)
    ax.set_xticks(np.arange(short.shape[1]))
    ax.set_yticks(np.arange(short.shape[0]))
    ax.set_xticklabels(short.columns, rotation=45, ha="right")
    ax.set_yticklabels(short.index)

    # Tile gridlines
    ax.set_xticks(np.arange(-0.5, short.shape[1], 1), minor=True)
    ax.set_yticks(np.arange(-0.5, short.shape[0], 1), minor=True)
    ax.grid(which="minor", linewidth=0.5)
    ax.tick_params(which="minor", bottom=False, left=False)

    if annotate:
        for i in range(short.shape[0]):
            for j in range(short.shape[1]):
                val = data[i, j]
                text = "" if np.isnan(val) else f"{val:.3f}"
                ax.text(j, i, text, ha="center", va="center", fontsize=8)

    cbar = fig.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
    cbar.set_label("Adjusted Rand Index (ARI)")

    fig.tight_layout()
    fig.savefig(outpath, dpi=300, bbox_inches="tight")
    plt.close(fig)


def main() -> None:
    paths = sorted([p for p in TAXONOMIES_DIR.iterdir() if p.suffix == ".csv"])
    if not paths:
        raise FileNotFoundError(f"No .csv files found in {TAXONOMIES_DIR}")

    dfs = {p.name: load_taxonomy(p) for p in paths}

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

        # Layer 2 leaf (layer_1, layer_2)
        df2 = df.dropna(subset=["layer_1_cluster", "layer_2_cluster"]).copy()
        leaf = layer2_leaf_id(df2)
        layer2_assignments[name] = pd.Series(
            leaf.values, index=df2["label"].values, name="leaf"
        ).astype(str)

    ari_l1 = pairwise_ari(layer1_assignments)
    ari_l2 = pairwise_ari(layer2_assignments)

    # Save matrices as CSV (useful for debugging / reproducibility)
    ari_l1.to_csv(L1_CSV)
    ari_l2.to_csv(L2_CSV)

    # Plot heatmaps
    plot_heatmap(
        ari_l1,
        title="Pairwise clustering agreement (ARI) — Layer 1",
        outpath=L1_PNG,
        annotate=True,
        vmin=0.0,
        vmax=1.0,
    )
    plot_heatmap(
        ari_l2,
        title="Pairwise clustering agreement (ARI) — Layer 2 (leaf clusters)",
        outpath=L2_PNG,
        annotate=True,
        vmin=0.0,
        vmax=1.0,
    )

    print("Saved:")
    print(f" - {L1_CSV}")
    print(f" - {L2_CSV}")
    print(f" - {L1_PNG}")
    print(f" - {L2_PNG}")


if __name__ == "__main__":
    main()
