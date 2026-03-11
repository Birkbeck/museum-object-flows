import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from sentence_transformers import SentenceTransformer

LABELS_FILE = (
    "../data/unlabelled-taxonomies/"
    "taxonomy-BAAI-bge-large-en-v1.5-The new use of the building is-llm-2-10-20.csv"
)

SENTENCE_STRUCTURE = "The new use of the building is"

SMALL_MODEL = SentenceTransformer("BAAI/bge-small-en-v1.5")
LARGE_MODEL = SentenceTransformer("BAAI/bge-large-en-v1.5")

# Diagnostic pairs (synonyms + confounds)
SYNONYMS = [
    ("flats", "apartments"),
    ("bar", "pub"),
    ("cafe", "coffee shop"),
    ("hotel", "b&b"),
    ("warehouse", "storage"),
    ("holiday accommodation", "holiday let"),
]
CONFOUNDS = [
    ("tea room", "escape room"),
    ("coffee shop", "shop"),
    ("town hall", "market hall"),
    ("car park", "industrial park"),
    ("heritage centre", "wellbeing centre"),
]

ENCODERS = ["small", "large"]  # bge-small, bge-large

# Percentile ranks are computed within each method's own pairwise similarity distribution
METHODS = ["label", "label_null", "label_note", "label_wiki", "label_llm"]
METHOD_DISPLAY = {
    "label": "baseline",
    "label_null": "null",
    "label_note": "note",
    "label_wiki": "wiki",
    "label_llm": "llm",
}

# Letters to plot instead of shapes
LETTERS = {
    "label": "B",  # baseline
    "label_null": "S",  # sentence/null
    "label_note": "N",
    "label_wiki": "W",
    "label_llm": "L",
}

# Sampling for reference distribution (within each method+encoder)
N_RANDOM_PAIRS = 50000
RNG_SEED = 42


def safe_cat(*parts: object) -> str:
    """Safely concatenate parts into a single string, dropping empties/NaNs."""
    out = []
    for p in parts:
        if p is None:
            continue
        if isinstance(p, float) and np.isnan(p):
            continue
        s = str(p).strip()
        if s:
            out.append(s)
    return " ".join(out)


def cos_sim(u: np.ndarray, v: np.ndarray) -> float:
    """Cosine similarity for L2-normalized embeddings."""
    return float(np.dot(u, v))


def sample_pairwise_sims(embeddings: np.ndarray, n_pairs: int, seed: int) -> np.ndarray:
    """Sample cosine similarities for random pairs from a matrix of normalized embeddings."""
    rng = np.random.default_rng(seed)
    n = embeddings.shape[0]
    if n < 2:
        return np.array([], dtype=np.float32)

    max_pairs = n * (n - 1) // 2
    n_pairs = int(min(n_pairs, max_pairs))

    i = rng.integers(0, n, size=n_pairs, endpoint=False)
    j = rng.integers(0, n, size=n_pairs, endpoint=False)
    mask = i != j
    i = i[mask]
    j = j[mask]
    if i.size == 0:
        return np.array([], dtype=np.float32)

    sims = np.sum(embeddings[i] * embeddings[j], axis=1)
    return sims.astype(np.float32)


def percentile_rank(value: float, sample: np.ndarray) -> float:
    """Percentile rank of `value` within `sample`, in [0,100]. Uses empirical CDF: P(sample <= value)."""
    if sample.size == 0:
        return float("nan")
    return float(np.mean(sample <= value) * 100.0)


def plot_letters_for_encoder(
    results_df: pd.DataFrame,
    encoder: str,
    synonyms: list[tuple[str, str]],
    confounds: list[tuple[str, str]],
    methods: list[str],
    title: str,
    outpath: str,
) -> tuple[plt.Figure, plt.Axes]:
    """
    One chart per encoder:
      - y-axis: word pairs (ordered exactly as lists)
      - x-axis: percentile rank
      - letter: augmentation method
      - colour: augmentation method
    """

    # Color by augmentation method (matplotlib default cycle: C0..)
    METHOD_COLOR = {
        "label": "C0",
        "label_null": "C1",
        "label_note": "C2",
        "label_wiki": "C3",
        "label_llm": "C4",
    }

    # Ordered y categories
    pair_order = [f"{a} ↔ {b}" for a, b in synonyms] + [
        f"{a} ↔ {b}" for a, b in confounds
    ]
    pair_to_y = {p: i for i, p in enumerate(pair_order)}

    d = results_df[
        (results_df["encoder"] == encoder) & (results_df["pair"].isin(pair_order))
    ].copy()
    if d.empty:
        raise ValueError(f"No data to plot for encoder={encoder}")

    d["y"] = d["pair"].map(pair_to_y)

    fig, ax = plt.subplots(figsize=(12, 7))

    # Draw letters
    for method in methods:
        dd = d[d["method"] == method]
        if dd.empty:
            continue
        for _, row in dd.iterrows():
            ax.text(
                row["percentile"],
                row["y"],
                LETTERS.get(method, "?"),
                color=METHOD_COLOR.get(method, "black"),
                ha="center",
                va="center",
                fontweight="bold",
                fontsize=10,
            )

    ax.set_title(title)
    ax.set_xlabel(
        "Percentile rank of pair similarity (within-method reference distribution)"
    )
    ax.set_ylabel("Word pair")
    ax.set_xlim(0, 100)

    # Add padding top/bottom so text isn't clipped at edges
    n_rows = len(pair_order)
    ax.set_ylim(-1, n_rows)  # extra space above and below
    ax.set_yticks(range(n_rows))
    ax.set_yticklabels(pair_order)

    # Visual separators / guides
    if len(synonyms) > 0 and len(confounds) > 0:
        ax.axhline(len(synonyms) - 0.5, linewidth=1)
    ax.axvline(90, linewidth=1)  # top-decile guide

    # Legend (colour ↔ method)
    from matplotlib.lines import Line2D

    handles = []
    for m in methods:
        handles.append(
            Line2D(
                [0],
                [0],
                marker=None,
                linestyle="",
                color=METHOD_COLOR.get(m, "black"),
                label=f"{LETTERS.get(m,'?')} = {METHOD_DISPLAY.get(m, m)}",
            )
        )
    ax.legend(
        handles=handles, title="Augmentation (letter & colour)", loc="best", fontsize=9
    )

    fig.tight_layout()
    fig.savefig(outpath, dpi=300, bbox_inches="tight")
    plt.close(fig)
    return fig, ax


def main() -> None:
    df = pd.read_csv(LABELS_FILE).copy()
    df = df.dropna(subset=["label"]).copy()
    df["label"] = df["label"].astype(str)

    # If your CSV can contain duplicate rows per label, keep the first
    df = df.drop_duplicates(subset=["label"], keep="first").copy()

    # Build augmented texts
    df["label_null"] = df.apply(
        lambda row: safe_cat(SENTENCE_STRUCTURE, row["label"] + "."),
        axis=1,
    )
    df["label_note"] = df.apply(
        lambda row: safe_cat(
            SENTENCE_STRUCTURE, row["label"] + ".", row.get("definition_note", "")
        ),
        axis=1,
    )
    df["label_wiki"] = df.apply(
        lambda row: safe_cat(
            SENTENCE_STRUCTURE, row["label"] + ".", row.get("definition_wiki", "")
        ),
        axis=1,
    )
    df["label_llm"] = df.apply(
        lambda row: safe_cat(
            SENTENCE_STRUCTURE, row["label"] + ".", row.get("definition_llm", "")
        ),
        axis=1,
    )

    # Keep only needed text columns
    df = df[["label", "label_null", "label_note", "label_wiki", "label_llm"]].copy()
    df.to_csv("label_examples.csv", index=False)

    # --- Embed each method column with each encoder (normalized embeddings) ---
    for col in METHODS:
        unique_texts = sorted(set(v for v in df[col].dropna().astype(str) if v.strip()))
        small_embs = SMALL_MODEL.encode(unique_texts, normalize_embeddings=True)
        large_embs = LARGE_MODEL.encode(unique_texts, normalize_embeddings=True)

        map_small = dict(zip(unique_texts, small_embs))
        map_large = dict(zip(unique_texts, large_embs))

        df[f"{col}_embedding_small"] = df[col].map(map_small)
        df[f"{col}_embedding_large"] = df[col].map(map_large)

    # Index by raw label for easy lookup of a label's augmented text embedding
    df_idx = df.set_index("label")

    # Precompute reference similarity samples per (encoder, method)
    ref_sims: dict[tuple[str, str], np.ndarray] = {}
    for enc in ENCODERS:
        for method in METHODS:
            embs = np.vstack(df[f"{method}_embedding_{enc}"].values)
            sims = sample_pairwise_sims(embs, n_pairs=N_RANDOM_PAIRS, seed=RNG_SEED)
            ref_sims[(enc, method)] = sims
            print(
                f"Reference sims: encoder={enc}, method={method}, "
                f"n={sims.size}, mean={float(np.mean(sims)):.4f}, sd={float(np.std(sims)):.4f}"
            )

    # Compute percentile ranks for all pairs (synonyms + confounds)
    records = []
    for pair_type, pairs in [("synonym", SYNONYMS), ("confound", CONFOUNDS)]:
        for a, b in pairs:
            if a not in df_idx.index or b not in df_idx.index:
                print(f"Skipping missing pair: ({a}, {b})")
                continue

            for enc in ENCODERS:
                for method in METHODS:
                    ea = df_idx.loc[a, f"{method}_embedding_{enc}"]
                    eb = df_idx.loc[b, f"{method}_embedding_{enc}"]
                    sim = cos_sim(ea, eb)
                    pct = percentile_rank(sim, ref_sims[(enc, method)])

                    records.append(
                        {
                            "pair_type": pair_type,
                            "pair": f"{a} ↔ {b}",
                            "a": a,
                            "b": b,
                            "encoder": enc,
                            "method": method,
                            "cosine": sim,
                            "percentile": pct,
                        }
                    )

    results_df = pd.DataFrame(records)
    results_df.to_csv("pair_similarity_percentiles.csv", index=False)
    print("Wrote: pair_similarity_percentiles.csv")

    # Two charts: one per encoder (includes both synonyms + confounds on y-axis)
    plot_letters_for_encoder(
        results_df=results_df,
        encoder="small",
        synonyms=SYNONYMS,
        confounds=CONFOUNDS,
        methods=METHODS,
        title="bge-small: pair similarity percentile (letter=method; colour=method)",
        outpath="pair_percentiles_letters_bge_small.png",
    )
    plot_letters_for_encoder(
        results_df=results_df,
        encoder="large",
        synonyms=SYNONYMS,
        confounds=CONFOUNDS,
        methods=METHODS,
        title="bge-large: pair similarity percentile (letter=method; colour=method)",
        outpath="pair_percentiles_letters_bge_large.png",
    )

    print("Saved:")
    print(" - pair_percentiles_letters_bge_small.png")
    print(" - pair_percentiles_letters_bge_large.png")


if __name__ == "__main__":
    main()
