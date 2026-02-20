import json
import os
from typing import Iterable

import numpy as np
import pandas as pd
from sentence_transformers import SentenceTransformer, util
import statsmodels.formula.api as smf
from statsmodels.stats.anova import anova_lm
import torch

from src import ClusterLabeller, Experiment
from src.llms import make_llm_from_name


class ClusterLabellingExperiment(Experiment):
    def __init__(self, config: dict, evaluation_sentence_model):
        output_file_name = f"{config['output_directory']}/{config['output_file_name']}"
        super().__init__(output_file_name)
        self.taxonomies_directory = config["taxonomies_directory"]
        self.llms = config["llms"]
        self.roles = config["roles"]
        self.tasks = config["tasks"]
        self.examples = config["examples"]
        self.example_lengths = config["example_lengths"]
        self.examples = config["examples"]
        self.temperatures = config["temperatures"]
        self.top_p = config["top_p"]
        self.seeds = config["seeds"]
        self.layer_count = config["layer_count"]
        self.output_directory = config["output_directory"]
        self.evaluation_sentence_model = evaluation_sentence_model
        self.language_models = {}

    @classmethod
    def from_config(cls, config: dict):
        evaluation_sentence_model = SentenceTransformer(
            config["evaluation_sentence_model"]
        )
        return cls(config, evaluation_sentence_model)

    def parameter_combinations(self) -> Iterable:
        taxonomies = [f for f in os.listdir(self.taxonomies_directory)]
        self.language_models = {llm: make_llm_from_name(llm) for llm in self.llms}
        return (
            {
                "llm": llm,
                "taxonomy": taxonomy,
                "role": role,
                "task": task,
                "example_length": example_length,
                "temperature": temperature,
                "seed": seed,
            }
            for llm in self.llms
            for taxonomy in taxonomies
            for role in self.roles
            for task in self.tasks
            for example_length in self.example_lengths
            for temperature in self.temperatures
            for seed in self.seeds
        )

    def run_test(self, configuration: dict) -> dict:
        cluster_labeller = ClusterLabeller(
            llm=self.llms[configuration["llm"]],
            role_description=self.roles[configuration["role"]],
            task_description=self.tasks[configuration["task"]],
            examples=self.examples[: configuration["example_length"]],
            temperature=configuration["temperature"],
            top_p=self.top_p,
            seed=configuration["seed"],
        )
        taxonomy_columns = ["label"] + [
            f"layer_{count}_cluster" for count in range(1, self.layer_count + 1)
        ]
        taxonomy = pd.read_csv(
            f"{self.taxonomies_directory}/{configuration['taxonomy']}"
        )[taxonomy_columns].drop_duplicates()
        labelled_taxonomy = self._label_clusters(cluster_labeller, taxonomy)
        taxonomy_name = configuration["taxonomy"].replace(".csv", "")
        llm_name = configuration["llm"].replace("/", "-")
        taxonomy_file_name = (
            f"{self.output_directory}/labelled-taxonomies/"
            f"-{taxonomy_name}"
            f"-{llm_name}"
            f"-{configuration['role']}"
            f"-{configuration['task']}"
            f"-{configuration['example_length']}"
            f"-{configuration['temperature']}"
        )
        labelled_taxonomy.to_csv(f"{taxonomy_file_name}.csv")
        taxonomy_json = self._taxonomy_to_json(labelled_taxonomy, self.layer_count)
        with open(f"{taxonomy_file_name}.json", "w") as f:
            f.write(taxonomy_json)
        evaluated_taxonomy = self._evaluate_labelled_taxonomy(labelled_taxonomy)
        coherence_score, coverage_score = self._aggregate_taxonomy_scores(
            evaluated_taxonomy
        )
        return configuration | {
            "coherence_score": coherence_score,
            "coverage_score": coverage_score,
        }

    def evaluation_summary(self):
        """
        Returns a summary of a factorial ANOVA test
        of configuration parameters
        as predictors of the coherence and coverage scores.
        """
        results = pd.DataFrame(self.results)
        if results["coherence_score"].isna().any():
            return "Some results have no coherence score"
        factors = " + ".join(
            [
                f"C({column})"
                for column in results.columns
                if column not in ["coherence_score", "coverage_score"]
            ]
        )
        coherence_model_formula = f"coherence_score ~ {factors}"
        coherence_model = smf.ols(
            coherence_model_formula,
            data=results,
        ).fit()
        coherence_model_anova = anova_lm(coherence_model, typ=2)
        coverage_model_formula = f"coverage_score ~ {factors}"
        coverage_model = smf.ols(
            coverage_model_formula,
            data=results,
        ).fit()
        coverage_model_anova = anova_lm(coverage_model, typ=2)
        return {
            "coherence_model_anova": coherence_model_anova,
            "coverage_model_anova": coverage_model_anova,
        }

    def _label_clusters(
        self, cluster_labeller: ClusterLabeller, taxonomy: pd.DataFrame
    ) -> pd.DataFrame:
        out = taxonomy.copy()
        for layer in range(1, self.layer_count + 1):
            cluster_id_column = f"layer_{layer}_cluster"
            cluster_label_column = f"layer_{layer}_label"
            cluster_to_label = {}
            for cluster_id, subset in taxonomy.groupby(
                cluster_id_column, dropna=False, sort=True
            ):
                unique_labels = sorted(set(subset["label"].dropna()))
                cluster_to_label[cluster_id] = cluster_labeller.label_cluster(
                    unique_labels
                )
            out[cluster_label_column] = out[cluster_id_column].map(cluster_to_label)
        return out

    def _evaluate_labelled_taxonomy(self, taxonomy: pd.DataFrame) -> pd.DataFrame:
        out = taxonomy.copy()

        def _embed_texts(texts: list[str]) -> torch.Tensor:
            return self.evaluation_sentence_model.encode(
                texts,
                convert_to_tensor=True,
                normalize_embeddings=True,
                show_progress_bar=False,
            )

        for layer in range(1, self.layer_count + 1):
            cluster_id_column = f"layer_{layer}_cluster"
            cluster_label_column = f"layer_{layer}_label"
            similarity_column = f"layer_{layer}_similarity"
            cluster_mean_similarity_column = f"layer_{layer}_cluster_mean_similarity"
            cluster_q10_similarity_column = f"layer_{layer}_cluster_q10_similarity"
            valid_mask = out["label"].notna() & out[cluster_label_column].notna()
            if valid_mask.sum() == 0:
                out[similarity_column] = np.nan
                out[cluster_mean_similarity_column] = np.nan
                out[cluster_q10_similarity_column] = np.nan
                continue
            leaf_labels = out.loc[valid_mask, "label"].astype(str)
            cluster_labels = out.loc[valid_mask, cluster_label_column].astype(str)
            unique_texts = (
                pd.Index(leaf_labels.tolist() + cluster_labels.tolist())
                .unique()
                .tolist()
            )
            embeddings = _embed_texts(unique_texts)
            text_to_idx = {t: i for i, t in enumerate(unique_texts)}
            leaf_idx = torch.tensor(
                [text_to_idx[t] for t in leaf_labels.tolist()],
                dtype=torch.long,
            )
            cluster_idx = torch.tensor(
                [text_to_idx[t] for t in cluster_labels.tolist()],
                dtype=torch.long,
            )
            leaf_embedding = embeddings[leaf_idx]
            cluster_embedding = embeddings[cluster_idx]
            cosine_similarities = (
                (leaf_embedding * cluster_embedding).sum(dim=1).detach().cpu().numpy()
            )
            out[similarity_column] = np.nan
            out.loc[valid_mask, similarity_column] = cosine_similarities
            cluster_grouped = out.loc[
                valid_mask, [cluster_id_column, similarity_column]
            ].groupby(cluster_id_column, dropna=False)[similarity_column]
            cluster_means = cluster_grouped.mean()
            cluster_q10 = cluster_grouped.quantile(0.10)
            out[cluster_mean_similarity_column] = out[cluster_id_column].map(
                cluster_means
            )
            out[cluster_q10_similarity_column] = out[cluster_id_column].map(cluster_q10)
        return out

    def _aggregate_taxonomy_scores(
        self, evaluated_taxonomy: pd.DataFrame
    ) -> tuple[float, float]:
        """
        Returns (coherence_score, coverage_score).
        Coherence: size-weighted mean of per-cluster mean similarity, averaged across layers.
        Coverage:  size-weighted mean of per-cluster q10 similarity, averaged across layers.
        """
        layer_coherences: list[float] = []
        layer_coverages: list[float] = []
        for layer in range(1, self.layer_count + 1):
            cluster_id_column = f"layer_{layer}_cluster"
            mean_col = f"layer_{layer}_cluster_mean_similarity"
            q10_col = f"layer_{layer}_cluster_q10_similarity"
            df = evaluated_taxonomy[[cluster_id_column, mean_col, q10_col]].dropna(
                subset=[cluster_id_column]
            )
            if df.empty:
                continue
            cluster_stats = (
                df.groupby(cluster_id_column, dropna=False)
                .agg(
                    cluster_size=(cluster_id_column, "size"),
                    mean_sim=(mean_col, "first"),
                    q10_sim=(q10_col, "first"),
                )
                .dropna(subset=["mean_sim", "q10_sim"])
            )
            if cluster_stats.empty:
                continue
            weights = cluster_stats["cluster_size"].astype(float)
            coherence_layer = (
                cluster_stats["mean_sim"] * weights
            ).sum() / weights.sum()
            coverage_layer = (cluster_stats["q10_sim"] * weights).sum() / weights.sum()
            layer_coherences.append(float(coherence_layer))
            layer_coverages.append(float(coverage_layer))
        coherence_score = (
            float(sum(layer_coherences) / len(layer_coherences))
            if layer_coherences
            else float("nan")
        )
        coverage_score = (
            float(sum(layer_coverages) / len(layer_coverages))
            if layer_coverages
            else float("nan")
        )
        return coherence_score, coverage_score

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
