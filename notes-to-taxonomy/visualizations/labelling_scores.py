import json
import math
from pathlib import Path

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.formula.api as smf
from statsmodels.stats.anova import anova_lm

RESULTS_FILE = "../data/cluster-labelling-results.csv"
OUTPUT_DIR = Path(".")

COMMON_XLIM = (0.0, 0.7)
HUMAN_BASELINES = {
    "coherence_score": 0.45,
    "coverage_score": 0.298,
}


def evaluation_summary(results: pd.DataFrame) -> dict:
    if len(results) < 2:
        raise Exception(
            f"Not enough runs for ANOVA (n={len(results)}). Need at least 2."
        )
    if results["coherence_score"].isna().any():
        raise Exception("Some results have no coherence score")

    candidate_cols = [
        c for c in results.columns if c not in ["coherence_score", "coverage_score"]
    ]

    varying_cols = []
    dropped = {}

    for col in candidate_cols:
        nunique = results[col].dropna().nunique()
        if nunique >= 2:
            varying_cols.append(col)
        else:
            dropped[col] = nunique

    if not varying_cols:
        raise Exception("No varying predictors to test (all factors constant).")

    factors = " + ".join([f"C({col})" for col in varying_cols])

    coherence_model = smf.ols(f"coherence_score ~ {factors}", data=results).fit()
    coverage_model = smf.ols(f"coverage_score ~ {factors}", data=results).fit()

    return {
        "dropped_predictors": dropped,
        "coherence_model_anova": anova_lm(coherence_model, typ=2),
        "coverage_model_anova": anova_lm(coverage_model, typ=2),
    }


def simplify_taxonomy_name(filename: str) -> str:
    """
    taxonomy-BAAI-bge-small-en-v1.5-The new use of the building is-llm-2-10-20.csv
    -> bge-small + Llama
    """
    name = filename.replace(".csv", "")

    encoder = (
        "bge-small"
        if "bge-small" in name
        else ("bge-large" if "bge-large" in name else "encoder?")
    )

    augmentation = (
        "Llama"
        if "-llm-" in name
        else (
            "null"
            if "-null-" in name
            else (
                "Wiki" if "-wiki-" in name else ("note" if "-note-" in name else "aug?")
            )
        )
    )

    return f"{encoder} + {augmentation}"


def clean_role(value):
    return "null" if isinstance(value, float) and math.isnan(value) else value


def compute_order(
    df: pd.DataFrame,
    category_col: str,
    score_col: str,
    ascending: bool = False,
) -> list[str]:
    plot_df = df[[category_col, score_col]].dropna().copy()
    grouped = (
        plot_df.groupby(category_col, observed=False)[score_col]
        .median()
        .sort_values(ascending=ascending)
    )
    return [str(x) for x in grouped.index.tolist()]


def prepare_plot_df(
    df: pd.DataFrame, category_col: str, score_col: str
) -> pd.DataFrame:
    plot_df = df[[category_col, score_col]].dropna().copy()
    if plot_df.empty:
        raise ValueError(f"No rows left to plot for {category_col} vs {score_col}")
    plot_df[category_col] = plot_df[category_col].astype(str)
    return plot_df


def draw_horizontal_boxplot(
    ax,
    plot_df: pd.DataFrame,
    category_col: str,
    score_col: str,
    order: list[str],
    xlabel: str,
    ylabel: str,
    title: str,
    xlim: tuple[float, float],
    human_baseline: float | None = None,
):
    sns.boxplot(
        data=plot_df,
        y=category_col,
        x=score_col,
        order=order,
        fill=False,
        showfliers=True,
        linewidth=1.5,
        ax=ax,
    )

    sns.stripplot(
        data=plot_df,
        y=category_col,
        x=score_col,
        order=order,
        color="black",
        size=2,
        alpha=0.3,
        jitter=False,
        ax=ax,
    )

    if human_baseline is not None:
        ax.axvline(
            x=human_baseline,
            color="red",
            linestyle="--",
            linewidth=2,
            alpha=0.8,
        )

    ax.set_xlim(*xlim)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.set_title(title)
    sns.despine(ax=ax, left=True)


def make_single_boxplot(
    df: pd.DataFrame,
    category_col: str,
    score_col: str,
    xlabel: str,
    ylabel: str,
    title: str,
    output_file: str,
    human_baseline: float | None,
    xlim: tuple[float, float],
    order: list[str] | None = None,
    figsize: tuple[int, int] = (10, 6),
):
    plot_df = prepare_plot_df(df, category_col, score_col)

    if order is None:
        order = compute_order(df, category_col, score_col, ascending=False)

    fig, ax = plt.subplots(figsize=figsize)

    draw_horizontal_boxplot(
        ax=ax,
        plot_df=plot_df,
        category_col=category_col,
        score_col=score_col,
        order=order,
        xlabel=xlabel,
        ylabel=ylabel,
        title=title,
        xlim=xlim,
        human_baseline=human_baseline,
    )

    fig.tight_layout()
    fig.savefig(output_file, dpi=300, bbox_inches="tight")
    plt.close(fig)


def make_taxonomy_two_panel_figure(
    df: pd.DataFrame,
    output_file: str,
    xlim: tuple[float, float],
):
    taxonomy_order = compute_order(df, "taxonomy", "coherence_score", ascending=False)

    coherence_df = prepare_plot_df(df, "taxonomy", "coherence_score")
    coverage_df = prepare_plot_df(df, "taxonomy", "coverage_score")

    fig, axes = plt.subplots(
        nrows=2,
        ncols=1,
        figsize=(10, 10),
        sharex=True,
    )

    draw_horizontal_boxplot(
        ax=axes[0],
        plot_df=coherence_df,
        category_col="taxonomy",
        score_col="coherence_score",
        order=taxonomy_order,
        xlabel="",
        ylabel="Taxonomy",
        title="Label Coherence by Taxonomy Structure",
        xlim=xlim,
        human_baseline=HUMAN_BASELINES["coherence_score"],
    )

    draw_horizontal_boxplot(
        ax=axes[1],
        plot_df=coverage_df,
        category_col="taxonomy",
        score_col="coverage_score",
        order=taxonomy_order,
        xlabel="Score",
        ylabel="Taxonomy",
        title="Label Coverage by Taxonomy Structure",
        xlim=xlim,
        human_baseline=HUMAN_BASELINES["coverage_score"],
    )

    fig.tight_layout()
    fig.savefig(output_file, dpi=300, bbox_inches="tight")
    plt.close(fig)


def write_candidate_taxonomy_list(df: pd.DataFrame, output_file: str):
    q1_coherence = df.groupby("taxonomy", observed=False)["coherence_score"].quantile(
        0.25
    )
    q1_coverage = df.groupby("taxonomy", observed=False)["coverage_score"].quantile(
        0.25
    )

    best_results = df[
        (df["coherence_score"] >= df["taxonomy"].map(q1_coherence))
        & (df["coverage_score"] >= df["taxonomy"].map(q1_coverage))
    ].copy()

    best_results["taxonomy"] = (
        best_results["taxonomy"]
        .str.replace(".csv", "", regex=False)
        .str.replace(" ", "_", regex=False)
    )
    best_results["llm"] = best_results["llm"].str.replace("/", "_", regex=False)

    best_results["file_name"] = best_results.apply(
        lambda row: (
            f"{row['taxonomy']}"
            f"-{row['llm']}"
            f"-{row['role']}"
            f"-{row['task']}"
            f"-{row['example_length']}"
            f"-{row['temperature']}.json"
        ),
        axis=1,
    )

    best_taxonomy_files = best_results["file_name"].tolist()

    with open(output_file, "w", encoding="utf-8") as f:
        json.dump(best_taxonomy_files, f, indent=2, ensure_ascii=False)


def main():
    sns.set_theme(style="ticks")

    df = pd.read_csv(RESULTS_FILE)[
        [
            "llm",
            "taxonomy",
            "role",
            "task",
            "example_length",
            "temperature",
            "seed",
            "coherence_score",
            "coverage_score",
        ]
    ]

    df = df[
        ~df["taxonomy"].isin(
            [
                "taxonomy-BAAI-bge-large-en-v1.5-The new use of the building is-note-2-10-20.csv",
                "taxonomy-BAAI-bge-small-en-v1.5-The new use of the building is-note-2-10-20.csv",
            ]
        )
    ].copy()

    df["role"] = df["role"].map(clean_role)

    print(evaluation_summary(df))

    write_candidate_taxonomy_list(
        df=df,
        output_file="../data/candidate-taxonomies.json",
    )

    df["taxonomy"] = df["taxonomy"].map(simplify_taxonomy_name)

    # Shared taxonomy ordering across both metrics
    # taxonomy_order = compute_order(df, "taxonomy", "coherence_score", ascending=False)
    taxonomy_order = [
        "bge-small + null",
        "bge-large + null",
        "bge-small + Llama",
        "bge-large + Llama",
        "bge-small + Wiki",
        "bge-large + Wiki",
    ]

    # Combined two-panel taxonomy figure
    make_taxonomy_two_panel_figure(
        df=df,
        output_file=str(OUTPUT_DIR / "label_scores_by_taxonomy.png"),
        xlim=COMMON_XLIM,
    )

    # Separate taxonomy figures, if you still want them
    make_single_boxplot(
        df=df,
        category_col="taxonomy",
        score_col="coherence_score",
        xlabel="Coherence Score",
        ylabel="Taxonomy",
        title="",
        output_file=str(OUTPUT_DIR / "label_coherence_score_by_taxonomy.png"),
        human_baseline=HUMAN_BASELINES["coherence_score"],
        xlim=COMMON_XLIM,
        order=taxonomy_order,
        figsize=(10, 6),
    )

    make_single_boxplot(
        df=df,
        category_col="taxonomy",
        score_col="coverage_score",
        xlabel="Coverage Score",
        ylabel="Taxonomy",
        title="",
        output_file=str(OUTPUT_DIR / "label_coverage_score_by_taxonomy.png"),
        human_baseline=HUMAN_BASELINES["coverage_score"],
        xlim=COMMON_XLIM,
        order=taxonomy_order,
        figsize=(10, 6),
    )

    # Example length
    example_length_order = compute_order(
        df, "example_length", "coherence_score", ascending=False
    )

    make_single_boxplot(
        df=df,
        category_col="example_length",
        score_col="coherence_score",
        xlabel="Coherence Score",
        ylabel="Example Length",
        title="Label Coherence by Number of Examples in Prompt",
        output_file=str(OUTPUT_DIR / "label_coherence_score_by_example_length.png"),
        human_baseline=HUMAN_BASELINES["coherence_score"],
        xlim=COMMON_XLIM,
        order=example_length_order,
        figsize=(10, 6),
    )

    make_single_boxplot(
        df=df,
        category_col="example_length",
        score_col="coverage_score",
        xlabel="Coverage Score",
        ylabel="Example Length",
        title="Label Coverage by Number of Examples in Prompt",
        output_file=str(OUTPUT_DIR / "label_coverage_score_by_example_length.png"),
        human_baseline=HUMAN_BASELINES["coverage_score"],
        xlim=COMMON_XLIM,
        order=example_length_order,
        figsize=(10, 6),
    )

    # Temperature
    temperature_order = compute_order(
        df, "temperature", "coherence_score", ascending=False
    )

    make_single_boxplot(
        df=df,
        category_col="temperature",
        score_col="coherence_score",
        xlabel="Coherence Score",
        ylabel="Temperature",
        title="Label Coherence by Decoding Temperature",
        output_file=str(OUTPUT_DIR / "label_coherence_score_by_temperature.png"),
        human_baseline=HUMAN_BASELINES["coherence_score"],
        xlim=COMMON_XLIM,
        order=temperature_order,
        figsize=(10, 6),
    )

    make_single_boxplot(
        df=df,
        category_col="temperature",
        score_col="coverage_score",
        xlabel="Coverage Score",
        ylabel="Temperature",
        title="Label Coverage by Decoding Temperature",
        output_file=str(OUTPUT_DIR / "label_coverage_score_by_temperature.png"),
        human_baseline=HUMAN_BASELINES["coverage_score"],
        xlim=COMMON_XLIM,
        order=temperature_order,
        figsize=(10, 6),
    )


if __name__ == "__main__":
    main()
