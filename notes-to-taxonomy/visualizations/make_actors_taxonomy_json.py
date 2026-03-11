import json

import pandas as pd

reasons_csv = "reason_types.csv"

df = pd.read_csv(reasons_csv)


df["label"] = df["super_cause_text"]
df["layer_1_label"] = df["cause_type"]
df["layer_2_label"] = df.apply(
    lambda row: row["cause_type"]
    if pd.notna(row["cause_type"])
    else row["layer_1_label"],
    axis=1,
)
df = df[df["cause_super_type"] == "building"]
df = df[["label", "layer_1_label", "layer_2_label"]]

taxonomy_string = ""
for layer_1_label, group in df.groupby("layer_1_label"):
    taxonomy_string += f"{layer_1_label}:\n"
    for layer_2_label, subgroup in group.groupby("layer_2_label"):
        labels = subgroup["label"].tolist()
        labels_str = ", ".join(labels)
        taxonomy_string += f"  {layer_2_label}: {labels_str}\n"
print(taxonomy_string)

# with open("reasons_taxonomy.txt", "w", encoding="utf-8") as f:
#    f.write(taxonomy_string)
