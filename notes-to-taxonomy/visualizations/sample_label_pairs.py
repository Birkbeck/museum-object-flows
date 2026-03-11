from pathlib import Path

import pandas as pd

labelled_taxonomies_dir = Path("..") / "data" / "labelled-taxonomies"

label_dfs = []
for csv_file in labelled_taxonomies_dir.glob("*.csv"):
    df = pd.read_csv(csv_file)
    label_dfs.append(
        df[["layer_1_label", "label", "layer_1_similarity"]].rename(
            columns={
                "label": "document_label",
                "layer_1_label": "taxonomy_label",
                "layer_1_similarity": "similarity",
            }
        )
    )

    label_dfs.append(
        df[["layer_2_label", "label", "layer_2_similarity"]].rename(
            columns={
                "label": "document_label",
                "layer_2_label": "taxonomy_label",
                "layer_2_similarity": "similarity",
            }
        )
    )
labels_df = (
    pd.concat(label_dfs, ignore_index=True)
    .dropna(subset=["taxonomy_label"])
    .drop_duplicates()
)
# filter to only rows with similarity < 1
labels_df = labels_df[labels_df["similarity"] < 1]

# sample 20 random rows, 1 for each 20-cile of similarity score
labels_df["similarity_cile"] = pd.qcut(labels_df["similarity"], 20, labels=False)
sampled_labels_df = (
    labels_df.groupby("similarity_cile")
    .apply(lambda x: x.sample(1, random_state=1))
    .reset_index(drop=True)
)

example_labels_and_scores = (
    sampled_labels_df[["taxonomy_label", "document_label", "similarity"]]
    .sort_values(by="similarity", ascending=False)
    .reset_index(drop=True)
)

# output to latex with similarity scores to 3dp
latex_output = example_labels_and_scores.to_latex(
    index=False, float_format="{:.3f}".format
)
print(latex_output)
