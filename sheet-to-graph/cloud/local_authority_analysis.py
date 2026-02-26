import pandas as pd


def probability_happened_by_end_of_year(year_1, year_2, cut_off_year):
    if year_2 < cut_off_year:
        return 1
    if year_1 > cut_off_year:
        return 0
    return (cut_off_year - year_1 + 1) / (year_2 - year_1 + 1)


def probability_happened_before_year(year_1, year_2, cut_off_year):
    return probability_happened_by_end_of_year(year_1, year_2, cut_off_year - 1)


def museums_in_time_period_raw_figures(museums, start_year, end_year):
    years_in_period = end_year - start_year + 1
    museums["prob_opened_before_start"] = museums.apply(
        lambda x: probability_happened_before_year(
            x["year_opened_1"], x["year_opened_2"], start_year
        ),
        axis=1,
    )
    museums["prob_closed_before_start"] = museums.apply(
        lambda x: probability_happened_before_year(
            x["year_closed_1"], x["year_closed_2"], start_year
        ),
        axis=1,
    )
    museums["prob_opened_before_end"] = museums.apply(
        lambda x: probability_happened_before_year(
            x["year_opened_1"], x["year_opened_2"], end_year + 1
        ),
        axis=1,
    )
    museums["prob_closed_before_end"] = museums.apply(
        lambda x: probability_happened_before_year(
            x["year_closed_1"], x["year_closed_2"], end_year + 1
        ),
        axis=1,
    )
    museums["prob_open_at_start"] = museums.apply(
        lambda x: x["prob_opened_before_start"] * (1 - x["prob_closed_before_start"]),
        axis=1,
    )
    museums["prob_open_at_end"] = museums.apply(
        lambda x: x["prob_opened_before_end"] * (1 - x["prob_closed_before_end"]),
        axis=1,
    )
    museums["prob_closed_in_period"] = museums.apply(
        lambda x: x["prob_closed_before_end"] - x["prob_closed_before_start"],
        axis=1,
    )
    museums["prob_opened_in_period"] = museums.apply(
        lambda x: x["prob_opened_before_end"] - x["prob_opened_before_start"],
        axis=1,
    )
    museums["prob_opened_for_some_of_period"] = museums.apply(
        lambda x: x["prob_opened_before_end"] * (1 - x["prob_closed_before_start"]),
        axis=1,
    )
    return museums


museums = museums_in_time_period_raw_figures(
    pd.read_csv("../data/mapping-museums-data.csv"), 2000, 2025
)
local_authorities = museums[["lad_2023_code", "lad_2023_name"]].drop_duplicates()
museums = museums[museums["governance"] == "local authority"]

local_authority_summary = (
    museums.groupby("lad_2023_name")
    .agg(
        start_total=("prob_open_at_start", "sum"), end_total=("prob_open_at_end", "sum")
    )
    .reset_index()
)
local_authority_summary["change"] = (
    local_authority_summary["end_total"] - local_authority_summary["start_total"]
)
local_authority_summary["change_pc"] = (
    local_authority_summary["change"] / local_authority_summary["start_total"] * 100
)

local_authority_summary["start_total"] = round(
    local_authority_summary["start_total"], 1
)
local_authority_summary["end_total"] = round(local_authority_summary["end_total"], 1)
local_authority_summary["change"] = round(local_authority_summary["change"], 1)
local_authority_summary["change_pc"] = round(local_authority_summary["change_pc"], 1)

local_authority_summary = local_authorities.merge(
    local_authority_summary, on="lad_2023_name", how="left"
)
cols_to_fill = ["start_total", "end_total", "change", "change_pc"]
local_authority_summary[cols_to_fill] = local_authority_summary[cols_to_fill].fillna(0)
local_authority_summary.sort_values(by="change_pc", ascending=True, inplace=True)

local_authority_summary.to_csv("local_authority_summary.csv", index=False)
