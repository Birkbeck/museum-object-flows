import numpy as np
import pandas as pd
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score
from sklearn.preprocessing import normalize


class TaxonomyBuilder:
    """Builds a single taxonomy to structure a set of labels."""

    def __init__(
        self,
        encoder: "SentenceModel",
        sentence_structure: str,
        label_definer: "LabelDefiner",
        number_of_layers: int = 2,
        min_k: int = 10,
        max_k: int = 20,
    ):
        self.encoder = encoder
        self.sentence_structure = sentence_structure
        self.label_definer = label_definer
        if number_of_layers < 1:
            raise Exception(
                "number_of_layers < 1: The taxonomy must have at least one layer."
            )
        self.number_of_layers = number_of_layers
        if min_k < 2:
            raise Exception("min_k < 2: The taxonomy must have at least two clusters.")
        self.min_k = min_k
        if max_k < 2:
            raise Exception("max_k < 2: The taxonomy must have at least two clusters.")
        self.max_k = max_k

    def generate_taxonomy(self, labelled_texts_file: str):
        labelled_texts = self._get_embeddings(labelled_texts_file)
        for i in range(self.number_of_layers):
            labelled_texts = self._cluster(labelled_texts, layer=i + 1)
        return labelled_texts

    def _get_embeddings(self, labelled_texts_file: str):
        if labelled_texts_file.split(".")[-1] == "parquet":
            labelled_texts = pd.read_parquet(labelled_texts_file)
        elif labelled_texts_file.split(".")[-1] == "csv":
            labelled_texts = pd.read_csv(labelled_texts_file)
        if "augmented_label" not in labelled_texts.columns:
            labelled_texts["augmented_label"] = labelled_texts.apply(
                lambda row: self.sentence_structure
                + " "
                + row["label"]
                + ". "
                + self.label_definer.define(row),
                axis=1,
            )
        if "embedding" not in labelled_texts.columns:
            labelled_texts["embedding"] = self.encoder.encode(
                labelled_texts["augmented_label"].tolist(), normalize_embedding=True
            )
        labelled_texts.to_parquet(
            labelled_texts_file.split(".")[0] + ".parquet",
            index=False,
            engine="pyarrow",  # necessary for storing numpy arrays
        )
        return labelled_texts

    def _cluster(self, labelled_texts: pd.DataFrame, layer: int):
        if layer == 1:
            # cluster all labels
            clusters, score = self._kmeans(
                labelled_texts,
                layer_number=layer,
                min_k=self.min_k,
                max_k=self.max_k,
            )
            return clusters
        # otherwise cluster labels within each cluster
        prev_layer = layer - 1
        cluster_frames = []
        for cluster_id, subset in labelled_texts.groupby(f"layer_{prev_layer}_cluster"):
            clusters, score = self._kmeans(
                subset,
                layer_number=layer,
                min_k=max(2, len(subset)),
                max_k=min(10, len(subset) - 1),
            )
            cluster_frames.append(clusters)
        labelled_texts = pd.concat(cluster_frames)
        return labelled_texts

    def _kmeans(
        self,
        labelled_texts: pd.DataFrame,
        layer_number,
        min_k: int,
        max_k: int,
        random_state: int = 42,
    ):
        working_frame = labelled_texts.copy()
        embeddings = normalize(
            np.vstack(
                working_frame["embedding"]
                .apply(lambda v: np.asarray(v, dtype=np.float32))
                .to_numpy()
            ),
            norm="l2",
        )
        highest_score = 0.0
        best_clusters = None
        for k in range(min_k, max_k + 1):
            estimator = KMeans(n_clusters=k, n_init=10, random_state=random_state)
            clusters = estimator.fit_predict(embeddings)
            score = (
                silhouette_score(embeddings, clusters)
                if k > 1 and len(np.unique(clusters)) > 1
                else np.nan
            )
            if score > highest_score:
                highest_score = score
                best_clusters = clusters
        labelled_texts[f"layer_{layer_number}_cluster"] = best_clusters
        return labelled_texts, highest_score
