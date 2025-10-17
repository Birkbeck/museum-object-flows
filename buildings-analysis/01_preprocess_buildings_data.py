import pandas as pd


WORK_IN_PROGRESS_FILE_NAME = "../data/wip-sheet-2025-03-06.xlsx"
REASONS_COLUMN = "reasons for closure"
DISPERSAL_COLUMN = "Collection dispersal"
BUILDINGS_COLUMN = "Building"


def augment_buildings(row):
    buildings_text = (
        row[BUILDINGS_COLUMN] if isinstance(row[BUILDINGS_COLUMN], str) else ""
    )
    if "see reasons" in buildings_text.lower():
        buildings_text += row[REASONS_COLUMN]
    if "see dispersal" in buildings_text.lower():
        buildings_text += row[DISPERSAL_COLUMN]
    return buildings_text


df = pd.read_excel(WORK_IN_PROGRESS_FILE_NAME)
df = df[df["name"].notna() & df[BUILDINGS_COLUMN].notna()]
df = df[
    (df["name"].str.strip() != "")
    & (~df[BUILDINGS_COLUMN].str.strip().str.lower().isin(["", "tbc", "?", "unknown"]))
]

df["building_notes"] = df.apply(augment_buildings, axis=1)
df["building_notes_length"] = df["building_notes"].apply(lambda n: len(n.split()))
df = df.sort_values(by="building_notes_length", ascending=False).reset_index(drop=True)

# approximately 11% (1/9) of the data is used for development (for n-shot examples and validation)
development_rows = [i for i in range(len(df)) if i % 9 == 0]
df["development_or_test"] = "test"
df.loc[development_rows, "development_or_test"] = "development"

output_columns = [
    "name",
    "building_notes",
    "building_notes_length",
    "development_or_test",
]
df.to_csv("buildings_dataset.csv", columns=output_columns, index=False)
