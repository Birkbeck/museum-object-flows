from typing import Iterable

import pandas as pd
from sentence_transformers import SentenceTransformer, util

from src import Experiment


class DocLabellingExperiment(Experiment):
    def __init__(
        self, config: dict, dataset: pd.DataFrame, sentence_model: SentenceTransformer
    ):
        super().__init__()
        self.dataset = dataset
        self.models = config["models"]
        self.roles = config["roles"]
        self.tasks = config["tasks"]
        self.example_lengths = config["example_lengths"]
        self.examples = config["examples"]
        self.temperatures = config["temperatures"]
        self.seeds = config["seeds"]
        self.sentence_model = sentence_model
        self.cached_embeddings: dict = {}

    def parameter_combinations(self) -> Iterable:
        return (
            {
                "model": model,
                "role": role,
                "task": task,
                "example_length": example_length,
                "temperature": temperature,
                "seed": seed,
            }
            for model in self.models
            for role in self.roles
            for task in self.tasks
            for example_length in self.example_lengths
            for temperature in self.temperatures
            for seed in self.seed
        )

    def run_test(self, configuration: dict):
        dataset = self.dataset.copy()
        dataset["llm_prompt"] = dataset.apply(
            lambda row: self._generate_prompt(configuration, row["note"]), axis=1
        )
        model = self.models[configuration["model"]]
        dataset["llm_response"] = dataset.apply(
            lambda row: model.get_response(
                row["prompt"],
                configuration["temperature"],
                configuration["seed"],
            ),
            axis=1,
        )
        dataset["predicted_change_in_status"] = dataset.apply(
            lambda row: self._extract_labels(row["llm_response"], "status"),
            axis=1,
        )
        dataset["predicted_change_in_use"] = dataset.apply(
            lambda row: self._extract_labels(row["llm_response"], "use"),
            axis=1,
        )
        dataset["predicted_change_in_responsibility"] = dataset.apply(
            lambda row: self._extract_labels(row["llm_response"], "responsibility"),
            axis=1,
        )
        dataset["status_similarity"] = dataset.apply(
            lambda row: self._evaluate_labels(
                row["predicted_change_in_status"], row["change_in_status"]
            ),
            axis=1,
        )
        dataset["use_similarity"] = dataset.apply(
            lambda row: self._evaluate_labels(
                row["predicted_change_in_use"], row["change_in_use"]
            ),
            axis=1,
        )
        dataset["responsibility_similarity"] = dataset.apply(
            lambda row: self._evaluate_labels(
                row["predicted_change_in_responsibility"],
                row["change_in_responsibility"],
            ),
            axis=1,
        )
        dataset["mean_similarity"] = dataset[
            ["status_similarity", "use_similarity", "responsibility_similarity"]
        ].mean(axis=1)

    def evaluation_summary(self):
        raise NotImplementedError

    def best_configuration(self, peformance_metric: str) -> dict:
        raise NotImplementedError

    def _generate_prompt(self, configuration: dict, note: str):
        role = self.roles[configuration["role"]]
        task = self.tasks[configuration["task"]]
        examples = self.examples[: configuration["example_length"]]
        response_format = [
            "Format your response like this:",
            "change in status: ...",
            "change in use: ...",
            "change in responsibility: ...",
        ]
        return "\n\n".join(
            [role, task] + response_format + examples + [f"Notes: {note}"]
        )

    def _extract_labels(self, response: str, label_type: str):
        """Returns a sub-string of text containing the labels of the required type"""
        label_lists = response.lower().split("change in ")
        try:
            sub_string = [l for l in label_lists if l[: len(label_type)] == label_type][
                0
            ]
            sub_string_no_label_type = sub_string[len(label_type) + 1 :]
            sub_string_no_notes = sub_string_no_label_type.split("\nnotes")[0]
            return sub_string_no_notes.strip()
        except IndexError:
            return ""

    def _evaluate_labels(self, prediction: str, expected: str):
        """Returns cosine similarity of predicted and expected labels"""
        if prediction == "":
            return 0.0
        prediction_embedding = self.sentence_model.encode(
            prediction, convert_to_tensor=True
        )
        expected_embedding = self._get_cached_embedding(expected)
        similarity = util.cos_sim(prediction, expected)
        return similarity.item()

    def _get_cached_embedding(self, text: str):
        try:
            return self.cached_embeddings[text]
        except KeyError:
            self.cached_embeddings[text] = self.sentence_model.encode(
                text, convert_to_tensor=True
            )
            return self.cached_embeddings[text]
