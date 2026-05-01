from pathlib import Path
from itertools import combinations

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
from scipy.stats import spearmanr


def build_similarity_vector(
    clusters: dict[str, set[str]]
) -> tuple[list[tuple[str, str]], np.ndarray]:
    """
    Returns:
        pairs: list of (item1, item2)
        sims:  array of Jaccard similarities
    """
    items = sorted(clusters.keys())
    pairs = list(combinations(items, 2))

    sims = []
    for i, j in pairs:
        a, b = clusters[i], clusters[j]
        union = a | b
        sim = 0.0 if len(union) == 0 else len(a & b) / len(union)
        sims.append(sim)

    return pairs, np.array(sims)


def mantel_test(
    df_a: pd.DataFrame,
    df_b: pd.DataFrame,
    item_col: str = "name",
    cluster_col: str = "layer_1_label",
    n_permutations: int = 1000,
    seed: int = 42,
) -> tuple[float, float]:
    """
    Returns:
        r_obs: observed Spearman correlation
        p_value: permutation p-value (two-sided)
    """
    rng = np.random.default_rng(seed)

    clusters_a = dataframe_to_item_clusters(df_a, item_col, cluster_col)
    clusters_b = dataframe_to_item_clusters(df_b, item_col, cluster_col)

    common = sorted(set(clusters_a) & set(clusters_b))
    if len(common) < 2:
        return np.nan, np.nan

    clusters_a = {k: clusters_a[k] for k in common}
    clusters_b = {k: clusters_b[k] for k in common}

    pairs, sims_a = build_similarity_vector(clusters_a)
    _, sims_b = build_similarity_vector(clusters_b)

    if len(set(sims_a)) < 2 or len(set(sims_b)) < 2:
        return np.nan, np.nan

    r_obs = spearmanr(sims_a, sims_b).correlation

    # Permutation test
    permuted_rs = []
    items = list(common)

    for _ in range(n_permutations):
        perm = rng.permutation(items)
        perm_map = dict(zip(items, perm))

        # permute clustering B
        clusters_b_perm = {i: clusters_b[perm_map[i]] for i in items}

        _, sims_b_perm = build_similarity_vector(clusters_b_perm)
        r_perm = spearmanr(sims_a, sims_b_perm).correlation

        if not np.isnan(r_perm):
            permuted_rs.append(r_perm)

    permuted_rs = np.array(permuted_rs)

    # two-sided p-value
    p_value = np.mean(np.abs(permuted_rs) >= np.abs(r_obs))

    return r_obs, p_value


def simplify_name(x: str) -> str:
    mapping = {
        "gpt_5_2": "ChatGPT 5.2",
        "gpt_5_4": "ChatGPT 5.4",
        "pipeline_best": "Pipeline\n(BGE-large+null)",
        "pipeline_second": "Pipeline\n(BGE-small+Llama)",
    }
    return mapping.get(x, x)


def reorder_methods(names: list[str]) -> list[str]:
    preferred = ["GPT-5.2", "GPT-5.4", "Pipeline best", "Pipeline second"]
    return [x for x in preferred if x in names] + [
        x for x in names if x not in preferred
    ]


def plot_heatmap(
    matrix: pd.DataFrame,
    title: str,
    outpath: Path,
    annotate: bool = True,
    vmin: float = -1.0,
    vmax: float = 1.0,
) -> None:
    """
    Plot a tile-chart heatmap using matplotlib with a white → lime green scale.
    NaNs are shown in light grey.
    """
    white_lime = LinearSegmentedColormap.from_list(
        "white_lime",
        ["#ffffff", "#32CD32"],
    )
    white_lime = white_lime.copy()
    white_lime.set_bad(color="#f0f0f0")

    short = matrix.copy()
    short.index = [simplify_name(x) for x in short.index]
    short.columns = [simplify_name(x) for x in short.columns]

    order = reorder_methods(short.index.tolist())
    short = short.loc[order, order]

    data = short.values.astype(float)

    fig, ax = plt.subplots(figsize=(8, 6))
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

    # gridlines
    ax.set_xticks(np.arange(-0.5, short.shape[1], 1), minor=True)
    ax.set_yticks(np.arange(-0.5, short.shape[0], 1), minor=True)
    ax.grid(which="minor", color="black", linewidth=0.5)
    ax.tick_params(which="minor", bottom=False, left=False)

    if annotate:
        for i in range(short.shape[0]):
            for j in range(short.shape[1]):
                val = data[i, j]
                text = "" if np.isnan(val) else f"{val:.3f}"
                ax.text(j, i, text, ha="center", va="center", fontsize=9)

    cbar = fig.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
    cbar.set_label("Spearman correlation of pairwise Jaccard similarities")

    fig.tight_layout()
    fig.savefig(outpath, dpi=300, bbox_inches="tight")
    plt.close(fig)


def pairwise_jaccard_similarity(
    clusters: dict[str, set[str]]
) -> dict[tuple[str, str], float]:
    sims = {}
    for m1, m2 in combinations(sorted(clusters.keys()), 2):
        a, b = clusters[m1], clusters[m2]
        union = a | b
        sim = 0.0 if len(union) == 0 else len(a & b) / len(union)
        sims[(m1, m2)] = sim
    return sims


def dataframe_to_item_clusters(
    df: pd.DataFrame,
    item_col: str = "name",
    cluster_col: str = "layer_1_label",
) -> dict[str, set[str]]:
    return (
        df[[item_col, cluster_col]]
        .dropna(subset=[item_col, cluster_col])
        .assign(
            **{
                item_col: lambda x: x[item_col].astype(str).str.strip(),
                cluster_col: lambda x: x[cluster_col].astype(str).str.strip(),
            }
        )
        .drop_duplicates()
        .groupby(item_col)[cluster_col]
        .apply(set)
        .to_dict()
    )


def compare_clusterings(
    df_a: pd.DataFrame,
    df_b: pd.DataFrame,
    item_col: str = "name",
    cluster_col: str = "layer_1_label",
) -> float:
    clusters_a = dataframe_to_item_clusters(
        df_a, item_col=item_col, cluster_col=cluster_col
    )
    clusters_b = dataframe_to_item_clusters(
        df_b, item_col=item_col, cluster_col=cluster_col
    )

    common = sorted(set(clusters_a) & set(clusters_b))
    if len(common) < 2:
        return np.nan

    sims_a = pairwise_jaccard_similarity({k: clusters_a[k] for k in common})
    sims_b = pairwise_jaccard_similarity({k: clusters_b[k] for k in common})

    v1 = [sims_a[k] for k in sims_a]
    v2 = [sims_b[k] for k in sims_a]

    if len(v1) == 0 or len(set(v1)) < 2 or len(set(v2)) < 2:
        return np.nan

    return spearmanr(v1, v2).correlation


def results_to_matrix(
    results: dict[tuple[str, str], dict[str, float]],
    names: list[str],
    value_key: str,
    diagonal: float = np.nan,
) -> pd.DataFrame:
    mat = pd.DataFrame(np.nan, index=names, columns=names, dtype=float)

    for name in names:
        mat.loc[name, name] = diagonal

    for (a, b), vals in results.items():
        mat.loc[a, b] = vals[value_key]
        mat.loc[b, a] = vals[value_key]

    return mat


buildings_notes_file = "buildings_validation_set.csv"
gpt_5_2_labels = "../data/building_notes_labeled_5.2.csv"
gpt_5_4_labels = "../data/building_notes_labeled_5.4.csv"
pipeline_best_csv = "../data/unlabelled-taxonomies/taxonomy-BAAI-bge-large-en-v1.5-The new use of the building is-null-2-10-20.csv"
pipeline_second_csv = "../data/unlabelled-taxonomies/taxonomy-BAAI-bge-small-en-v1.5-The new use of the building is-llm-2-10-20.csv"

taxonomy_5_2 = {
    "Cultural & heritage": ["Cultural/arts/heritage", "Still cultural/museum use"],
    "Public & civic": ["Government/public services", "Community & civic"],
    "Education": ["Education & training"],
    "Health & care": ["Healthcare & social care"],
    "Religion": ["Religious use"],
    "Commercial": [
        "Hospitality & food/drink",
        "Retail & consumer services",
        "Office/business use",
    ],
    "Residential": ["Residential"],
    "Industrial": ["Industrial/storage"],
    "Leisure": ["Leisure & sport"],
    "Inactive / removed": ["Vacant/derelict", "Demolished/cleared"],
    "Redevelopment pattern": ["Mixed-use redevelopment"],
    "Unclear": ["Unknown/unspecified"],
}

# standardise representations
df = (
    pd.read_csv(buildings_notes_file, encoding="latin1")
    .reset_index()
    .rename(columns={"index": "note_id", "change_in_use": "human_labels"})
)[["note_id", "name"]]

gpt_5_2_df = (
    pd.read_csv(gpt_5_2_labels).rename(columns={"label_str": "gpt_5_2_labels"})
)[["note_id", "gpt_5_2_labels"]]
gpt_5_2_df["note_id"] = gpt_5_2_df["note_id"].map(lambda x: x - 1)
gpt_5_2_df = gpt_5_2_df.merge(df, on="note_id", how="left")
gpt_5_2_df = gpt_5_2_df.assign(
    gpt_5_2_labels=gpt_5_2_df["gpt_5_2_labels"].str.split(";")
).explode("gpt_5_2_labels")
gpt_5_2_df["layer_2_label"] = gpt_5_2_df["gpt_5_2_labels"].str.strip()
gpt_5_2_df["layer_1_label"] = gpt_5_2_df["layer_2_label"].map(
    lambda x: next(
        (k for k, v in taxonomy_5_2.items() if x in v),
        "Unmapped label: " + str(x),
    )
)
gpt_5_2_df = gpt_5_2_df[["name", "layer_1_label"]]

gpt_5_4_df = pd.read_csv(gpt_5_4_labels).rename(
    columns={"first_level_types": "layer_1_labels"}
)[["note_id", "layer_1_labels"]]
gpt_5_4_df = gpt_5_4_df.merge(df, on="note_id", how="left")
gpt_5_4_df = gpt_5_4_df.assign(
    layer_1_labels=gpt_5_4_df["layer_1_labels"].str.split("|")
).explode("layer_1_labels")
gpt_5_4_df["layer_1_label"] = gpt_5_4_df["layer_1_labels"].str.strip()
gpt_5_4_df = gpt_5_4_df[["name", "layer_1_label"]]

pipeline_best_df = pd.read_csv(pipeline_best_csv)[["name", "layer_1_cluster"]].rename(
    columns={"layer_1_cluster": "layer_1_label"}
)
pipeline_second_df = pd.read_csv(pipeline_second_csv)[
    ["name", "layer_1_cluster"]
].rename(columns={"layer_1_cluster": "layer_1_label"})

taxonomies = {
    "gpt_5_2": gpt_5_2_df,
    "gpt_5_4": gpt_5_4_df,
    "pipeline_best": pipeline_best_df,
    "pipeline_second": pipeline_second_df,
}

results = {}
for (name_a, df_a), (name_b, df_b) in combinations(taxonomies.items(), 2):
    r, p = mantel_test(df_a, df_b, n_permutations=1000)
    results[(name_a, name_b)] = {"r": r, "p": p}
    print(f"{name_a} vs {name_b}: r = {r:.4f}, p = {p:.4f}")


matrix = results_to_matrix(
    results, names=list(taxonomies.keys()), value_key="r", diagonal=1.0
)

plot_heatmap(
    matrix=matrix,
    title="Correlation of pairwise item Jaccard similarities",
    outpath=Path("jaccard_similarity_correlation_heatmap.png"),
    annotate=True,
    vmin=0.0,
    vmax=1.0,
)
