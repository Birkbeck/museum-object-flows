from itertools import combinations
import numpy as np
import pandas as pd
from scipy.stats import spearmanr


def pairwise_jaccard_similarity(
    clusters: dict[str, set[str]]
) -> dict[tuple[str, str], float]:
    sims = {}
    for m1, m2 in combinations(sorted(clusters.keys()), 2):
        a, b = clusters[m1], clusters[m2]
        if len(a | b) == 0:
            sim = 0.0
        else:
            sim = len(a & b) / len(a | b)
        sims[(m1, m2)] = sim
    return sims


def dataframe_to_item_clusters(
    df: pd.DataFrame,
    item_col: str = "name",
    cluster_col: str = "layer_1_label",
) -> dict[str, set[str]]:
    """
    Convert a long dataframe of item-cluster memberships into:
        {item_name: {cluster1, cluster2, ...}}
    """
    out = (
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
    return out


def compare_clusterings(
    df_a: pd.DataFrame,
    df_b: pd.DataFrame,
    item_col: str = "name",
    cluster_col: str = "layer_1_label",
) -> float:
    """
    Compare two overlapping clusterings by:
    1. building item -> set(clusters)
    2. computing pairwise Jaccard similarity between items within each clustering
    3. correlating those pairwise similarities across the two clusterings

    Returns Spearman correlation.
    """
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

    v1 = []
    v2 = []
    for k in sims_a:
        v1.append(sims_a[k])
        v2.append(sims_b[k])

    # If one side is constant, Spearman is undefined
    if len(set(v1)) < 2 or len(set(v2)) < 2:
        return np.nan

    return spearmanr(v1, v2).correlation


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
        "Unmapped label: " + x,
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

for (name_a, df_a), (name_b, df_b) in combinations(taxonomies.items(), 2):
    score = compare_clusterings(df_a, df_b)
    print(
        f"Pairwise Jaccard similarity correlation between {name_a} and {name_b}: {score:.4f}"
    )
