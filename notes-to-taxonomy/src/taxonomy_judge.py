from __future__ import annotations

import json
import os
import random
from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Sequence, Set, Tuple, Union

import choix
import pandas as pd

from src.llms import LLM, make_llm_from_name


@dataclass(frozen=True)
class TaxonomyItem:
    filename: str
    data: dict


class TaxonomyJudge:
    """
    LLM-as-a-judge for ranking taxonomy JSON files in a directory, using pairwise comparisons
    aggregated with a Bradley–Terry model via `choix`.
    """

    def __init__(self, config: dict, judge_llm: "LLM"):
        self.taxonomies_directory = config["taxonomies_directory"]
        self.output_file_name = (
            f"{config['output_directory']}/{config['output_file_name']}"
        )
        self.judgements_file_name = (
            f"{config['output_directory']}/{config['judgements_file_name']}"
        )
        self.max_new_judgements = config.get("max_new_judgements")
        # LLM parameters
        self.judge_llm = judge_llm
        self.temperature = config["temperature"]
        self.top_p = config["top_p"]
        self.max_new_tokens = config["max_new_tokens"]
        self.seed = config["seed"]
        self.task = config["task"]
        self.examples_with_winner_a = config["examples_with_winner_a"]
        self.examples_with_winner_b = config["examples_with_winner_b"]
        # Bradley-Terry parameters
        self.pair_limit = config["pair_limit"]
        if self.pair_limit <= 0:
            raise ValueError("pair_limit must be positive")
        self.min_appearances = config["min_appearances"]
        if self.min_appearances < 0:
            raise ValueError("min_appearances must be non-negative")

    @classmethod
    def from_config(cls, config: dict) -> TaxonomyJudge:
        judge_llm = make_llm_from_name(config["judge_llm"])
        with open(config["task_file"], "r", encoding="utf-8") as f:
            config["task"] = f.read()
        config["examples_with_winner_a"] = {}
        config["examples_with_winner_b"] = {}
        for example_file in config["example_files"]:
            with open(example_file, "r", encoding="utf-8") as f:
                example_winner = example_file.split("/")[-1].split(".")[0]
                example, winner = example_winner.split("-")
                if winner == "a":
                    config["examples_with_winner_a"][example_file] = f.read()
                elif winner == "b":
                    config["examples_with_winner_b"][example_file] = f.read()
                else:
                    raise ValueError(
                        f"Invalid example file name {example_file}: "
                        f"expected format '$example-$winner.txt' where winner is 'a' or 'b'"
                    )
        return cls(config=config, judge_llm=judge_llm)

    def rank_taxonomies(self, taxonomies_list: List[str]) -> List[str]:
        items = self._load_taxonomies(taxonomies_list)
        n = len(items)
        if n < 2:
            return [it.filename for it in items]
        sampled_pairs = self._sample_pairs(n)
        sampled_pairs_with_examples = self._add_examples_to_pairs(sampled_pairs)
        existing_judgements = self._load_existing_judgements()
        completed_keys = {
            (
                row["taxonomy_a"],
                row["taxonomy_b"],
                row["example_1"],
                row["example_2"],
            )
            for row in existing_judgements
        }
        filename_by_index = {i: item.filename for i, item in enumerate(items)}
        new_judgements = 0
        for i, j, example_1, example_2 in sampled_pairs_with_examples:
            key = (
                filename_by_index[i],
                filename_by_index[j],
                f"{example_1[0]}-{example_1[1]}",
                f"{example_2[0]}-{example_2[1]}",
            )
            if key in completed_keys:
                continue
            if (
                self.max_new_judgements is not None
                and new_judgements >= self.max_new_judgements
            ):
                break
            taxonomy_a = items[i]
            taxonomy_b = items[j]
            result = self._elicit_judgement(
                taxonomy_a, taxonomy_b, example_1, example_2
            )
            row = {
                "taxonomy_a": taxonomy_a.filename,
                "taxonomy_b": taxonomy_b.filename,
                "example_1": f"{example_1[0]}-{example_1[1]}",
                "example_2": f"{example_2[0]}-{example_2[1]}",
                "best_taxonomy": result["best_taxonomy"],
                "comments": result["comments"],
            }
            self._append_judgement(row)
            existing_judgements.append(row)
            completed_keys.add(key)
            new_judgements += 1
            if new_judgements % 50 == 0:
                print(f"Added {new_judgements} new judgements so far...")
        comparisons: List[Tuple[int, int]] = []
        none_count = 0
        filename_to_index = {item.filename: i for i, item in enumerate(items)}
        for row in existing_judgements:
            taxonomy_a = row["taxonomy_a"]
            taxonomy_b = row["taxonomy_b"]
            best_taxonomy = row["best_taxonomy"]
            if (
                taxonomy_a not in filename_to_index
                or taxonomy_b not in filename_to_index
            ):
                continue
            i = filename_to_index[taxonomy_a]
            j = filename_to_index[taxonomy_b]
            if best_taxonomy is None:
                none_count += 1
            elif best_taxonomy == 0:
                comparisons.append((i, j))
            else:
                comparisons.append((j, i))
        none_rate = none_count / len(existing_judgements) if existing_judgements else 0
        skill_scores = choix.ilsr_pairwise(n_items=n, data=comparisons)
        ranked_indices = sorted(range(n), key=lambda k: skill_scores[k], reverse=True)
        ranked_taxonomies = [items[k].filename for k in ranked_indices]
        with open(self.output_file_name, "w", encoding="utf-8") as f:
            json.dump(ranked_taxonomies, f, indent=2, ensure_ascii=False)
        print("New judgements added this run:", new_judgements)
        print("Total stored judgements:", len(existing_judgements))
        print("None rate of LLM judgements:", none_rate)
        return ranked_taxonomies

    def _load_existing_judgements(self) -> List[dict]:
        if not os.path.exists(self.judgements_file_name):
            return []
        judgements: List[dict] = []
        with open(self.judgements_file_name, "r", encoding="utf-8") as f:
            for line in f:
                line = line.strip()
                if line:
                    judgements.append(json.loads(line))
        return judgements

    def _append_judgement(self, row: dict) -> None:
        os.makedirs(os.path.dirname(self.judgements_file_name), exist_ok=True)
        with open(self.judgements_file_name, "a", encoding="utf-8") as f:
            f.write(json.dumps(row, ensure_ascii=False) + "\n")

    def _load_taxonomies(self, taxonomies_list: List[str]) -> List[TaxonomyItem]:
        items: List[TaxonomyItem] = []
        for taxonomy in taxonomies_list:
            path = os.path.join(self.taxonomies_directory, taxonomy)
            df = pd.read_csv(path)
            taxonomy_string = ""
            for layer_1_label, group in df.groupby("layer_1_label"):
                taxonomy_string += f"{layer_1_label}:\n"
                for layer_2_label, subgroup in group.groupby("layer_2_label"):
                    labels = subgroup["label"].tolist()
                    labels = sorted(set(labels))
                    labels_str = ", ".join(labels)
                    taxonomy_string += f"  {layer_2_label}: {labels_str}\n"
            items.append(TaxonomyItem(filename=taxonomy, data=taxonomy_string))
        return items

    def _sample_pairs(self, n: int) -> List[Tuple[int, int]]:
        """
        Sample a sparse set of *directed* pairs (i, j) with i != j.

        Key differences from the previous version:
          - (i, j) and (j, i) are DISTINCT and may both appear.
          - min_appearances is the minimum number of times each i appears
            in position 0.

        Returns:
          List of (i, j)

        Notes:
          - Maximum number of unique directed pairs is n*(n-1).
          - If self.pair_limit is too small to satisfy min_appearances for all
        """
        if n < 2:
            raise ValueError("Need at least 2 items to sample pairs")
        max_directed_pairs = n * (n - 1)
        if self.pair_limit > max_directed_pairs:
            raise ValueError(
                f"pair_limit of {self.pair_limit} "
                f"exceeds maximum of {max_directed_pairs} "
                f"for n={n}"
            )
        if self.min_appearances > 0 and self.pair_limit < n * self.min_appearances:
            raise ValueError(
                f"pair_limit of {self.pair_limit} "
                f"is too small to meet min_appearances of {self.min_appearances} "
                f"for all {n} items (need at least {n * self.min_appearances})"
            )
        local_random = random.Random(self.seed)
        directed_pairs: set[Tuple[int, int]] = set()
        appearance_counts = [0] * n
        right_side_pairings: List[Set[int]] = [set() for _ in range(n)]

        def add_directed(i: int, j: int) -> bool:
            """Add (i, j) if new and i != j.
            Updates counts. Returns True if added."""
            if i == j:
                return False
            key = (i, j)
            if key in directed_pairs:
                return False
            directed_pairs.add(key)
            right_side_pairings[i].add(j)
            appearance_counts[i] += 1
            return True

        # Coverage phase: ensure each i appears min_appearances times on left
        indices = list(range(n))
        local_random.shuffle(indices)
        for i in indices:
            if len(directed_pairs) >= self.pair_limit:
                break
            need = self.min_appearances - appearance_counts[i]
            if need <= 0:
                continue
            available = [
                j for j in range(n) if j != i and j not in right_side_pairings[i]
            ]
            if not available:
                continue
            remaining_budget = self.pair_limit - len(directed_pairs)
            k = min(need, remaining_budget, len(available))
            for j in local_random.sample(available, k=k):
                add_directed(i, j)
        # Fill phase: sample additional directed pairs uniformly from remaining
        remaining_budget = self.pair_limit - len(directed_pairs)
        if remaining_budget > 0:
            remaining_candidates: List[Tuple[int, int]] = []
            for i in range(n):
                for j in range(n):
                    if i == j:
                        continue
                    if (i, j) not in directed_pairs:
                        remaining_candidates.append((i, j))
            if remaining_candidates:
                k = min(remaining_budget, len(remaining_candidates))
                for i, j in local_random.sample(remaining_candidates, k=k):
                    add_directed(i, j)
        pairs = list(directed_pairs)
        local_random.shuffle(pairs)
        return pairs

    def _add_examples_to_pairs(
        self, directed_pairs: Iterable[Tuple[int, int]]
    ) -> List[Tuple[int, int, Tuple[str, str], Tuple[str, str]]]:
        local_random = random.Random(self.seed)
        pairs = list(directed_pairs)
        local_random.shuffle(pairs)
        result: List[Tuple[int, int, Tuple[str, str], Tuple[str, str]]] = []
        for i, j in pairs:
            example_with_winner_a = local_random.choice(
                list(self.examples_with_winner_a.keys())
            )
            example_with_winner_b = local_random.choice(
                [
                    k
                    for k in self.examples_with_winner_b.keys()
                    if k != example_with_winner_a
                ]
            )
            example_a = (example_with_winner_a, "a")
            example_b = (example_with_winner_b, "b")
            example_1 = local_random.choice([example_a, example_b])
            example_2 = example_b if example_1 == example_a else example_a
            result.append((i, j, example_1, example_2))
        return result

    def _elicit_judgement(
        self,
        a: TaxonomyItem,
        b: TaxonomyItem,
        example_1: Tuple[str, str],
        example_2: Tuple[str, str],
    ) -> Dict[str, Optional[int] | str]:
        taxonomy_a = a.data
        taxonomy_b = b.data
        example_1_text = (
            self.examples_with_winner_a[example_1[0]]
            if example_1[1] == "a"
            else self.examples_with_winner_b[example_1[0]]
        )
        example_2_text = (
            self.examples_with_winner_a[example_2[0]]
            if example_2[1] == "a"
            else self.examples_with_winner_b[example_2[0]]
        )
        prompt = (
            f"{self.task}\n\n"
            f"{example_1_text}"
            "\n"
            f"{example_2_text}"
            "\n"
            "Taxonomy A:\n"
            f"{taxonomy_a}"
            "\n\n"
            "Taxonomy B:\n"
            f"{taxonomy_b}"
        )
        try:
            response = self.judge_llm.get_response(
                prompt,
                num_return_sequences=1,
                max_new_tokens=self.max_new_tokens,
                temperature=self.temperature,
                top_p=self.top_p,
                seed=self.seed,
            ).lower()
        except Exception as e:
            print(f"Error eliciting judgement for {a.filename} vs {b.filename}: {e}")
            return {
                "best_taxonomy": None,
                "comments": f"Error eliciting judgement: {e}",
            }
        try:
            comments = response.split("comments:")[1].split("best taxonomy:")[0].strip()
            choice_text = response.split("best taxonomy:")[1].strip().splitlines()[0]
            choice_text = choice_text.strip().lower().rstrip(".:")
        except IndexError:
            return {"best_taxonomy": None, "comments": response}
        best_taxonomy: Optional[int]
        if "taxonomy a" in choice_text:
            best_taxonomy = 0
        elif "taxonomy b" in choice_text:
            best_taxonomy = 1
        elif choice_text in ["a", "taxonomy a", "1"]:
            best_taxonomy = 0
        elif choice_text in ["b", "taxonomy b", "2"]:
            best_taxonomy = 1
        else:
            best_taxonomy = None
        return {"best_taxonomy": best_taxonomy, "comments": comments}
