import pandas as pd


class Pipeline:
    def __init__(
        self,
        note_labeller: "NoteLabeller",
        taxonomy_builder: "TaxonomyBuilder",
        cluster_labeller: "ClusterLabeller",
        labelled_texts_file: str,
        taxonomy_file: str,
    ):
        self.note_labeller = note_labeller
        self.taxonomy_builder = taxonomy_builder
        self.cluster_labeller = cluster_labeller
        self.labelled_texts_file = labelled_texts_file
        self.taxonomy_file = taxonomy_file

    def run(self, notes: pd.DataFrame):
        notes["label"] = notes["note"].map(self.note_labeller.label_note)
        notes.to_csv(self.labelled_texts_file)
        taxonomy = self.taxonomy_builder.generate_taxonomy(self.labelled_texts_file)
        taxonomy = self._label_taxonomy(taxonomy)
        taxonomy.to_csv(self.taxonomy_file)
        # TODO: draw the taxonomy and save png
        return taxonomy

    def _label_taxonomy(self, taxonomy: pd.DataFrame):
        cluster_cols = [col for col in taxonomy.columns if col.endswith("_cluster")]
        for cluster_col in cluster_cols:
            cluster_names = {}
            for cluster_id, subset in taxonomy.groupby(cluster_col):
                cluster_names[cluster_id] = self.cluster_labeller(subset["label"])
            taxonomy[f"{cluster_col}_label"] = taxonomy[cluster_col].map(cluster_names)
        return taxonomy
