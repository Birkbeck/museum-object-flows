from __future__ import annotations

import json
import os
import random
from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Sequence, Tuple

import choix
from openai import OpenAI


@dataclass(frozen=True)
class TaxonomyItem:
    filename: str
    data: dict


class TaxonomyJudge:
    """
    LLM-as-a-judge for ranking taxonomy JSON files in a directory, using pairwise comparisons
    aggregated with a Bradley–Terry model via `choix`.

    Defaults:
      - model: gpt-5-mini
      - temperature: 0
      - max_output_tokens: 32
      - pair_limit: 15_000 (≈52 comparisons/item for n=576)
      - min_appearances: 40 (try to ensure each item appears in at least this many comparisons)
    """

    def __init__(self, config: dict):
        self.client = OpenAI(
            api_key=config.get("api_key") or os.getenv("OPENAI_API_KEY")
        )
        self.model: str = config.get("model", "gpt-5-mini")
        self.temperature: float = float(config.get("temperature", 0.0))
        self.max_output_tokens: int = int(config.get("max_output_tokens", 32))
        self.seed: Optional[int] = config.get("seed", None)
        self.truncation: str = config.get("truncation", "auto")
        # For 576 items: 15k pairs ~ 52 comps/item average
        self.pair_limit: int = int(config.get("pair_limit", 15_000))
        self.min_appearances: int = int(config.get("min_appearances", 40))
        self.system_msg: str = config["system_msg"]
        self.rubric: str = config["rubric"]
        # Simple in-run cache to avoid duplicate calls if a pair repeats
        self._cache: Dict[Tuple[int, int, int, int], int] = {}
        # Seed RNG for reproducibility (pair sampling + optional API seed)
        if self.seed is not None:
            random.seed(self.seed)

    def rank_taxonomies(self, taxonomies_directory: str) -> List[str]:
        items = self._load_taxonomies(taxonomies_directory)
        n = len(items)
        if n < 2:
            return [it.filename for it in items]
        sampled_pairs = self._sample_sparse_pairs(
            n=n, pair_limit=self.pair_limit, min_appearances=self.min_appearances
        )
        comparisons: List[Tuple[int, int]] = []
        number_of_ties = 0
        for i, j in sampled_pairs:
            winner = self._elicit_judgement(items[i], items[j])
            if winner is None:
                number_of_ties += 1
                comparisons.append((i, j))
                comparisons.append((j, i))
            elif winner == 0:
                comparisons.append((i, j))
            else:
                comparisons.append((j, i))
        skill_scores = choix.ilsr_pairwise(n_items=n, data=comparisons)
        ranked_indices = sorted(range(n), key=lambda k: skill_scores[k], reverse=True)
        tie_rate = number_of_ties / len(comparisons)
        if tie_rate > 0:
            raise Warning(f"Tie rate of {tie_rate}")
        return [items[k].filename for k in ranked_indices]

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

    def _sample_sparse_pairs(
        self, n: int, pair_limit: int, min_appearances: int
    ) -> List[Tuple[int, int]]:
        """
        Sample a sparse set of unique unordered pairs (i, j), i < j, for a tournament.

        Goals:
          1) Coverage: try to ensure each item appears in at least `min_appearances` pairs.
          2) Budget: never exceed `pair_limit` pairs total.
          3) No retries: sample opponents without replacement in the coverage phase.
          4) Reproducible: uses a local RNG seeded by `self.seed` (does not touch global random state).

        Notes:
          - If `pair_limit` is too small to meet `min_appearances` for all items, the target is scaled down.
          - With n=576 and pair_limit=15_000, avg appearances ≈ 52/item.
        """
        if n < 2:
            return []
        if pair_limit <= 0:
            raise ValueError("pair_limit must be positive")
        if min_appearances < 0:
            raise ValueError("min_appearances must be non-negative")
        max_pairs = n * (n - 1) // 2
        pair_limit = min(pair_limit, max_pairs)
        # If budget can't satisfy target coverage, scale target down best-effort.
        min_pairs_needed = (n * min_appearances + 1) // 2
        if min_appearances > 0 and pair_limit < min_pairs_needed:
            min_appearances = max(1, (2 * pair_limit) // n)
        rng = random.Random(self.seed)
        pairs_set: set[Tuple[int, int]] = set()
        appearances = [0] * n
        neighbors: List[set[int]] = [set() for _ in range(n)]

        def add_pair(i: int, j: int) -> bool:
            """Add unordered pair if not present. Returns True if added."""
            if i == j:
                return False
            a, b = (i, j) if i < j else (j, i)
            if (a, b) in pairs_set:
                return False
            pairs_set.add((a, b))
            neighbors[a].add(b)
            neighbors[b].add(a)
            appearances[a] += 1
            appearances[b] += 1
            return True

        indices = list(range(n))
        rng.shuffle(indices)
        for i in indices:
            if len(pairs_set) >= pair_limit:
                break
            need = min_appearances - appearances[i]
            if need <= 0:
                continue
            if len(neighbors[i]) >= n - 1:
                continue
            available = [j for j in range(n) if j != i and j not in neighbors[i]]
            if not available:
                continue
            remaining_budget = pair_limit - len(pairs_set)
            k = min(need, remaining_budget, len(available))
            for j in rng.sample(available, k=k):
                add_pair(i, j)
        remaining_budget = pair_limit - len(pairs_set)
        if remaining_budget <= 0:
            pairs = list(pairs_set)
            rng.shuffle(pairs)
            return pairs
        remaining_candidates: List[Tuple[int, int]] = []
        for i in range(n):
            for j in range(i + 1, n):
                if (i, j) not in pairs_set:
                    remaining_candidates.append((i, j))
        if remaining_candidates:
            k = min(remaining_budget, len(remaining_candidates))
            pairs_set.update(rng.sample(remaining_candidates, k=k))
        pairs = list(pairs_set)
        rng.shuffle(pairs)
        return pairs

    def _elicit_judgement(self, a: TaxonomyItem, b: TaxonomyItem) -> Optional[int]:
        """
        Returns:
          0  -> a is better
          1  -> b is better
          None -> tie / indeterminate

        Makes two calls to reduce order effects:
          - Call 1 sees (a, b)
          - Call 2 sees (b, a)
        """
        ha, hb = a.content_hash, b.content_hash
        if ha <= hb:
            key = (ha, hb)
            flip = False
        else:
            key = (hb, ha)
            flip = True

        if key in self._cache:
            cached = self._cache[key]  # Optional[int]
            return (None if cached is None else (1 - cached)) if flip else cached

        def judge(first: TaxonomyItem, second: TaxonomyItem) -> str:
            user_msg = (
                f"{self.rubric}\n\n"
                "Taxonomy A (JSON):\n"
                f"{first.prompt_text}\n\n"
                "Taxonomy B (JSON):\n"
                f"{second.prompt_text}\n"
            )
            resp = self.client.responses.create(
                model=self.model,
                input=[
                    {"role": "system", "content": self.system_msg},
                    {"role": "user", "content": user_msg},
                ],
                temperature=self.temperature,
                max_output_tokens=self.max_output_tokens,
                truncation=self.truncation,
                seed=self.seed,
            )
            raw = (resp.output_text or "").strip().upper()
            # Be forgiving: extract first valid token
            for ch in raw:
                if ch in ("A", "B", "T"):
                    return ch
            raise ValueError(f"Model returned unexpected output: {resp.output_text!r}")

        def to_winner_index(result_token: str, swapped: bool) -> Optional[int]:
            if result_token == "T":
                return None
            if not swapped:
                return 0 if result_token == "A" else 1
            return 1 if result_token == "A" else 0

        winner_1 = to_winner_index(judge(a, b), swapped=False)
        winner_2 = to_winner_index(judge(b, a), swapped=True)
        winner_final: Optional[int] = winner_1 if winner_1 == winner_2 else None

        return winner_final

    @staticmethod
    def _stable_hash(obj: object) -> int:
        s = json.dumps(obj, sort_keys=True, ensure_ascii=False, separators=(",", ":"))
        return hash(s)
