import json

import pandas as pd


def taxonomy_to_json(
    taxonomy_frame: pd.DataFrame,
    number_of_layers: int,
    column_name_suffix: str,
):
    return json.dumps(
        taxonomy_to_dict(
            taxonomy_frame,
            number_of_layers,
            column_name_suffix,
        ),
        indent=2,
        ensure_ascii=False,
        sort_keys=True,
    )


def taxonomy_to_dict(
    taxonomy_frame: pd.DataFrame,
    number_of_layers: int,
    column_name_suffix: str,
):
    taxonomy: dict = {}
    layer_columns = [
        f"layer_{i}_{column_name_suffix}" for i in range(1, number_of_layers + 1)
    ]
    grouped = taxonomy_frame.groupby(layer_columns, dropna=False, sort=True)
    for path, subset in grouped:
        if number_of_layers == 1:
            path = (path,)
        leaf_labels = subset["label"].dropna().astype(str).drop_duplicates().tolist()
        current_level = taxonomy
        for i, node_label in enumerate(path):
            if i == number_of_layers - 1:
                current_level[node_label] = leaf_labels
            else:
                current_level = current_level.setdefault(node_label, {})
    return taxonomy
