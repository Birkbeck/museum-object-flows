from __future__ import annotations

import json
import os
import random
from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Sequence, Set, Tuple, Union

import choix

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
        self.output_file_name = (
            f"{config['output_directory']}/{config['output_file_name']}"
        )
        # LLM parameters
        self.judge_llm = judge_llm
        self.temperature = config["temperature"]
        self.top_p = config["top_p"]
        self.max_new_tokens = config["max_new_tokens"]
        self.seed = config["seed"]
        self.task = config["task"]
        self.examples = config["examples"]
        if not self.examples:
            raise ValueError(
                "self.examples is empty; cannot sample examples for prompts"
            )
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
        return cls(config=config, judge_llm=judge_llm)

    def rank_taxonomies(self, taxonomies_directory: str) -> List[str]:
        items = self._load_taxonomies(taxonomies_directory)
        n = len(items)
        if n < 2:
            return [it.filename for it in items]
        sampled_pairs = self._sample_pairs(n)
        sampled_pairs_with_examples = self._add_examples_to_pairs(sampled_pairs)
        comparisons: List[Tuple[int, int]] = []
        none_count = 0
        for i, j, example in sampled_pairs_with_examples:
            taxonomy_a = items[i]
            taxonomy_b = items[j]
            result = self._elicit_judgement(taxonomy_a, taxonomy_b, example)
            choice = result["choice"]
            if choice is None:
                none_count += 1
            elif choice == 0:
                comparisons.append((i, j))
            else:
                comparisons.append((j, i))
        none_rate = none_count / len(sampled_pairs_with_examples)
        skill_scores = choix.ilsr_pairwise(n_items=n, data=comparisons)
        ranked_indices = sorted(range(n), key=lambda k: skill_scores[k], reverse=True)
        ranked_taxonomies = [items[k].filename for k in ranked_indices]
        with open(self.output_file_name, "w", encoding="utf-8") as f:
            json.dump(ranked_taxonomies, f, indent=2, ensure_ascii=False)
        print("None rate of LLM judgements:", none_rate)
        return ranked_taxonomies

    def _load_taxonomies(self, taxonomies_directory: str) -> List[TaxonomyItem]:
        files = [
            f
            for f in os.listdir(taxonomies_directory)
            if f.lower().endswith(".json")
            and os.path.isfile(os.path.join(taxonomies_directory, f))
        ]
        files.sort()
        items: List[TaxonomyItem] = []
        for fn in files:
            path = os.path.join(taxonomies_directory, fn)
            with open(path, "r", encoding="utf-8") as f:
                data = json.load(f)
            items.append(TaxonomyItem(filename=fn, data=data))
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
    ) -> List[Tuple[int, int, str]]:
        local_random = random.Random(self.seed)
        pairs = list(directed_pairs)
        local_random.shuffle(pairs)
        result: List[Tuple[int, int, str]] = []
        for i, j in pairs:
            example = local_random.choice(self.examples)
            result.append((i, j, example))
        return result

    def _elicit_judgement(
        self, a: TaxonomyItem, b: TaxonomyItem, example: str
    ) -> Dict[str, Optional[int] | str]:
        taxonomy_a = json.dumps(a.data, indent=2, ensure_ascii=False)
        taxonomy_b = json.dumps(b.data, indent=2, ensure_ascii=False)
        prompt = (
            f"{self.task}\n\n"
            f"{example}"
            "\n\n"
            "Taxonomy A:\n"
            f"{taxonomy_a}"
            "\n\n"
            "Taxonomy B:\n"
            f"{taxonomy_b}"
            "\n\n"
        )
        response = self.judge_llm.get_response(
            prompt,
            num_return_sequences=1,
            max_new_tokens=self.max_new_tokens,
            temperature=self.temperature,
            top_p=self.top_p,
            seed=self.seed,
        )
        try:
            comments = response.split("Comments:")[1].split("Choice:")[0].strip()
            choice_text = response.split("Choice:")[1].strip().lower()
        except IndexError:
            return {"choice": None, "comments": ""}
        choice: Optional[int]
        if choice_text in ["a", "taxonomy a", "1"]:
            choice = 0
        elif choice_text in ["b", "taxonomy b", "2"]:
            choice = 1
        else:
            choice = None
        return {"choice": choice, "comments": comments}
