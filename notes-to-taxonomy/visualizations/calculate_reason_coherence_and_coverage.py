import pandas as pd
from sentence_transformers import SentenceTransformer

sentence_model = SentenceTransformer("all-MiniLM-L6-v2")

df = pd.read_csv("reason_types.csv")

df["layer_1_label"] = df["cause_super_type"]
df["layer_2_label"] = df.apply(
    lambda row: row["cause_type"]
    if pd.notna(row["cause_type"])
    else row["cause_super_type"],
    axis=1,
)
df["doc_label"] = df["super_cause_text"]

df = df[["layer_1_label", "layer_2_label", "doc_label"]].drop_duplicates()

df["layer_1_embedding"] = df["layer_1_label"].apply(lambda x: sentence_model.encode(x))
df["layer_2_embedding"] = df["layer_2_label"].apply(lambda x: sentence_model.encode(x))
df["doc_label_embedding"] = df["doc_label"].apply(lambda x: sentence_model.encode(x))

# similarity between each layer_1_embedding and doc_label_embedding
df["layer_1_similarity"] = sentence_model.similarity_pairwise(
    list(df["layer_1_embedding"]), list(df["doc_label_embedding"])
).tolist()
# similarity between each layer_2_embedding and doc_label_embedding
df["layer_2_similarity"] = sentence_model.similarity_pairwise(
    list(df["layer_2_embedding"]), list(df["doc_label_embedding"])
).tolist()
df["layer_1_similarity"] = pd.to_numeric(df["layer_1_similarity"])
df["layer_2_similarity"] = pd.to_numeric(df["layer_2_similarity"])

# five rows with lowest similarity for layer_1
print("5 rows with lowest similarity for layer_1:")
print(
    df.nsmallest(5, "layer_1_similarity")[
        ["layer_1_label", "doc_label", "layer_1_similarity"]
    ]
)
print("\n5 rows with lowest similarity for layer_2:")

# overall mean similarity and q10 of similarity weighted by cluster size
layer_1_stats = df.groupby("layer_1_label").agg(
    cluster_size=("layer_1_label", "size"),
    mean_sim=("layer_1_similarity", "mean"),
    q10_sim=("layer_1_similarity", lambda x: x.quantile(0.1)),
)
layer_1_weights = layer_1_stats["cluster_size"].astype(float)
layer_1_coherence = (
    layer_1_stats["mean_sim"] * layer_1_weights
).sum() / layer_1_weights.sum()
layer_1_coverage = (
    layer_1_stats["q10_sim"] * layer_1_weights
).sum() / layer_1_weights.sum()

layer_2_stats = df.groupby("layer_2_label").agg(
    cluster_size=("layer_2_label", "size"),
    mean_sim=("layer_2_similarity", "mean"),
    q10_sim=("layer_2_similarity", lambda x: x.quantile(0.1)),
)
layer_2_weights = layer_2_stats["cluster_size"].astype(float)
layer_2_coherence = (
    layer_2_stats["mean_sim"] * layer_2_weights
).sum() / layer_2_weights.sum()
layer_2_coverage = (
    layer_2_stats["q10_sim"] * layer_2_weights
).sum() / layer_2_weights.sum()

overall_coherence = (layer_1_coherence + layer_2_coherence) / 2
overall_coverage = (layer_1_coverage + layer_2_coverage) / 2

print(
    "coherence (weighted mean of intra-cluster mean pairwise cosine similarity):",
    overall_coherence,
)
print(
    "coverage (weighted mean of q10 of intra-cluster pairwise cosine similarity):",
    overall_coverage,
)
