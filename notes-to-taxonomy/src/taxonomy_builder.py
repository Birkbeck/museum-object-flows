import pandas as pd


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
        self.label_definer = (label_definer,)
        if number_of_layers < 1:
            raise Exception("The taxonomy must have at least one layer")
        self.number_of_layers = number_of_layers
        if min_k < 2:
            raise Exception("The taxonomy must have at least two clusters")
        self.min_k = min_k
        if max_k < 2:
            raise Exception("The taxonomy must have at least two clusters")
        self.max_k = max_k

    def generate_taxonomy(self, labelled_texts: pd.DataFrame):
        labelled_texts = self._get_embeddings(labelled_texts)
        for i in range(self.number_of_layers):
            labelled_texts = self._cluster(labelled_texts, layer=i)
        return labelled_texts

    def _get_embeddings(self, labelled_texts: pd.DataFrame):
        pass

    def _cluster(self, labelled_texts: pd.DataFrame, layer: int):
        if layer == 1:
            # cluster all labels
            clusters, score = self._kmeans(
                labelled_texts,
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
                min_k=max(2, len(subset)),
                max_k=min(10, len(subset) - 1),
            )
            cluster_frames.append(clusters)
        labelled_texts = pd.concat(cluster_frames)
        return labelled_texts

    def _kmeans(self, labelled_texts: pd.DataFrame, min_k: int, max_k: int):
        pass
