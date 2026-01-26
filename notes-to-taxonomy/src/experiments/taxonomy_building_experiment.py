import json
import re
from typing import Iterable

import pandas as pd
from sentence_transformers import SentenceTransformer
import statsmodels.formula.api as smf
from statsmodels.stats.anova import anova_lm

from src import Experiment
from src import TaxonomyBuilder
from src.label_definers import LabelDefinerLLM, LabelDefinerNote, LabelDefinerWiki
from src.llms import CompletionLLM


class TaxonomyBuildingExperiment(Experiment):
    """Instantiates multiple taxonomy builders, evaluates their performance on dataset."""

    def __init__(
        self,
        config: dict,
        dataset: pd.DataFrame,
        encoders: dict,
        evaluation_llm,
    ):
        output_file_name = f"{config['output_directory']}/{config['output_file_name']}"
        super().__init__(output_file_name)
        self.dataset = dataset
        self.encoder_names = config["encoders"]
        self.sentence_templates = config["sentence_templates"]
        self.definition_types = config["definition_types"]
        self.layer_counts = config["layer_counts"]
        self.min_ks = config["min_ks"]
        self.max_ks = config["max_ks"]
        self.encoders = encoders
        self.evaluation_llm = evaluation_llm
        self.evaluation_prompt = config["evaluation_prompt"]
        self.evaluation_temperature = config["evaluation_temperature"]
        self.evaluation_top_p = config["evaluation_top_p"]
        self.evaluation_seed = config["evaluation_seed"]
        self.output_directory = config["output_directory"]

    @classmethod
    def from_config(cls, config: dict, save_label_definitions: bool = True):
        if config["evaluation_llm"] == config["label_defining_llm"]:
            shared_llm = CompletionLLM.from_model_name(
                config["evaluation_llm"], trust_remote=True
            )
            evaluation_llm = shared_llm
            definer_llm = shared_llm
        else:
            evaluation_llm = CompletionLLM.from_model_name(
                config["evaluation_llm"], trust_remote=True
            )
            definer_llm = CompletionLLM.from_model_name(
                config["label_defining_llm"], trust_remote=True
            )
        encoders = {
            encoder: SentenceTransformer(encoder) for encoder in config["encoders"]
        }
        wiki_encoder = (
            encoders[config["label_definer_sentence_model"]]
            if config["label_definer_sentence_model"] in encoders
            else SentenceTransformer(config["label_definer_sentence_model"])
        )
        label_definer_note = LabelDefinerNote()
        label_definer_wiki = LabelDefinerWiki(
            wiki_encoder,
            config["email"],
            config["label_definer_wiki_top_k"],
            config["label_definer_wiki_min_relevance"],
        )
        label_definer_llm = LabelDefinerLLM(
            definer_llm,
            config["label_definer_prompt"],
            config["label_definer_max_new_tokens"],
            config["label_definer_temperature"],
            config["label_definer_top_p"],
            config["label_definer_seed"],
        )
        dataset = pd.read_csv(config["dataset"]).dropna(how="any", axis=0)
        dataset["label"] = dataset["label"].astype(str).str.split(r"\s*;\s*")
        dataset = dataset.explode("label", ignore_index=True)
        dataset["label"] = dataset["label"].map(lambda x: cls._normalize_label(x))
        dataset = dataset[dataset["label"] != ""]
        new_label_definitions_added = False
        if "definition_note" not in dataset.columns:
            dataset["definition_note"] = dataset.apply(
                lambda row: label_definer_note.get_label_definition(
                    row["label"], row["note"]
                ),
                axis=1,
            )
            new_label_definitions_added = True
        if "definition_wiki" not in dataset.columns:
            dataset["definition_wiki"] = dataset.apply(
                lambda row: label_definer_wiki.get_label_definition(
                    row["label"], row["note"]
                ),
                axis=1,
            )
            new_label_definitions_added = True
        if "definition_llm" not in dataset.columns:
            dataset["definition_llm"] = dataset.apply(
                lambda row: label_definer_llm.get_label_definition(
                    row["label"], row["note"]
                ),
                axis=1,
            )
            new_label_definitions_added = True
        if save_label_definitions and new_label_definitions_added:
            dataset.to_csv(config["dataset"], index=False)
        dataset["definition_null"] = ""
        return cls(config, dataset, encoders, evaluation_llm)

    @classmethod
    def _normalize_label(cls, label):
        if not isinstance(label, str):
            return ""
        label = (
            label.replace("cafã©", "cafe")
            .replace("cafã", "cafe")
            .replace("?", " ")
            .replace("\n", " ")
        )
        label = re.sub(r"\s+", " ", label).strip()
        return label

    def parameter_combinations(self) -> Iterable:
        return (
            {
                "encoder": encoder,
                "sentence_template": sentence_template,
                "definition_type": definition_type,
                "layer_count": layer_count,
                "min_k": min_k,
                "max_k": max_k,
            }
            for encoder in self.encoder_names
            for sentence_template in self.sentence_templates
            for definition_type in self.definition_types
            for layer_count in self.layer_counts
            for min_k in self.min_ks
            for max_k in self.max_ks
        )

    def run_test(self, configuration: dict) -> dict:
        encoder = self.encoders[configuration["encoder"]]
        taxonomy_builder = TaxonomyBuilder(
            encoder,
            configuration["sentence_template"],
            configuration["definition_type"],
            configuration["layer_count"],
            configuration["min_k"],
            configuration["max_k"],
        )
        taxonomy = taxonomy_builder.generate_taxonomy(self.dataset.copy())
        encoder_name = configuration["encoder"].replace("/", "-")
        taxonomy.to_csv(
            f"{self.output_directory}/taxonomy"
            f"-{encoder_name}"
            f"-{configuration['sentence_template']}"
            f"-{configuration['definition_type']}"
            f"-{configuration['layer_count']}"
            f"-{configuration['min_k']}"
            f"-{configuration['max_k']}"
            f".csv"
        )
        coherence_score, comments = self._evaluate_taxonomy(
            taxonomy, configuration["layer_count"]
        )
        return configuration | {
            "coherence_score": coherence_score,
            "comments": comments,
        }

    def evaluation_summary(self):
        """
        Returns a summary of a factorial ANOVA test
        of configuration parameters
        as predictors of the evaluation LLM's coherence score.
        """
        results = pd.DataFrame(self.results)
        if results["coherence_score"].isna().any():
            return "Some results have no coherence score"
        factors = " + ".join(
            [
                f"C({column})"
                for column in results.columns
                if column not in ["coherence_score", "comments"]
            ]
        )
        model_formula = f"coherence_score ~ {factors}"
        model = smf.ols(
            model_formula,
            data=results,
        ).fit()
        return anova_lm(model, typ=2)

    def _evaluate_taxonomy(self, taxonomy: pd.DataFrame, number_of_layers: int):
        taxonomy_json = self._taxonomy_to_json(taxonomy, number_of_layers)
        prompt = (
            f"{taxonomy_json}\n\n"
            f"{self.evaluation_prompt}\n\n"
            "Structure your response in the following way\n\n"
            "Coherence score: ...\n"
            "Comments: ..."
        )
        response = self.evaluation_llm.get_response(
            prompt,
            temperature=self.evaluation_temperature,
            top_p=self.evaluation_top_p,
            seed=self.evaluation_seed,
        )
        try:
            coherence_score_line = response.split("\n")[0]
            coherence_score = float(
                coherence_score_line[len("Coherence score:") :].strip()
            )
            comments = response[len(f"{coherence_score_line}\nComments:") :].strip()
        except (IndexError, ValueError):
            coherence_score = None
            comments = response
        return coherence_score, comments

    def _taxonomy_to_json(self, taxonomy_frame: pd.DataFrame, number_of_layers: int):
        return json.dumps(
            self._taxonomy_to_dict(taxonomy_frame, number_of_layers),
            indent=2,
            ensure_ascii=False,
            sort_keys=True,
        )

    def _taxonomy_to_dict(self, taxonomy_frame: pd.DataFrame, number_of_layers: int):
        taxonomy = {}
        layer_columns = [f"layer_{i}_cluster" for i in range(1, number_of_layers + 1)]
        for _, row in taxonomy_frame.iterrows():
            current_level = taxonomy
            for i, col in enumerate(layer_columns):
                cluster_id = row[col]
                # Last layer → assign label
                if i == number_of_layers - 1:
                    current_level[cluster_id] = row["label"]
                else:
                    if cluster_id not in current_level:
                        current_level[cluster_id] = {}
                    current_level = current_level[cluster_id]
        return taxonomy
