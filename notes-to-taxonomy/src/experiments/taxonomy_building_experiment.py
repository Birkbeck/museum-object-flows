from typing import Iterable

import pandas as pd
from sentence_transformers import SentenceTransformer

from src import Experiment
from src import TaxonomyBuilder
from src.label_definers import LabelDefinerLLM, LabelDefinerWiki


class TaxonomyBuildingExperiment(Experiment):
    def __init__(
        self,
        config: dict,
        dataset: pd.DataFrame,
        encoders: dict,
        label_definers: dict,
        evaluation_llm,
    ):
        super().__init__()
        self.dataset = dataset
        self.encoders = config["encoders"]
        self.sentence_templates = config["sentence_templates"]
        self.definers = config["definers"]
        self.layer_counts = config["layer_counts"]
        self.min_ks = config["min_ks"]
        self.max_ks = config["max_ks"]
        self.encoders = encoders
        self.label_definers = label_definers
        self.evaluation_llm = evaluation_llm

    @classmethod
    def from_config(cls, config: dict, dataset: pd.DataFrame, evaluation_llm):
        encoders = {
            encoder: SentenceTransformer(encoder) for encoder in config["encoders"]
        }
        label_definers = {
            "note": None,
            "wiki": LabelDefinerWiki(),
            "llm": LabelDefinerLLM(),
        }
        return cls(config, dataset, encoders, label_definers, evaluation_llm)

    def parameter_combinations(self) -> Iterable:
        return (
            {
                "encoder": encoder,
                "sentence_template": sentence_template,
                "definer": definer,
                "layer_count": layer_count,
                "min_k": min_k,
                "max_k": max_k,
            }
            for encoder in self.encoders
            for sentence_template in self.sentence_templates
            for definer in self.definers
            for layer_count in self.layer_counts
            for min_k in self.min_ks
            for max_k in self.max_ks
        )

    def run_test(self, configuration: dict) -> dict:
        # TODO: label definer's need to memoize label definitions
        # TaxonomyBuilder.generate_taxonomy should therefore accept a dataframe, not a file
        encoder = self.encoders[configuration["encoder"]]
        label_definer = self.label_definers[configuration["label_definer"]]
        taxonomy_builder = TaxonomyBuilder(
            configuration["encoder"],
            configuration["sentence_template"],
            configuration["label_definer"],
            configuration["number_of_layers"],
            configuration["min_k"],
            configuration["max_k"],
        )
        taxonomy = taxonomy_builder.generate_taxonomy(self.dataset.copy())
        coherence_score, comments = self._evaluate_taxonomy(taxonomy)
        raise NotImplementedError

    def evaluation_summary(self):
        raise NotImplementedError

    def best_configuration(self, performance_metric: str) -> dict:
        raise NotImplementedError

    def _evaluate_taxonomy(self):
        pass
