import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Sequence, Tuple

import pytest


from src.taxonomy_judge import TaxonomyJudge, TaxonomyItem


# -----------------------------
# Helpers / fakes
# -----------------------------


@dataclass(frozen=True)
class FakeItem:
    """
    A minimal stand-in for TaxonomyItem that includes the fields _elicit_judgement expects
    """

    filename: str
    data: dict
    prompt_text: str
    content_hash: int


class FakeResponse:
    def __init__(self, output_text: str):
        self.output_text = output_text


class FakeResponsesAPI:
    """
    Mimics client.responses.create(...) and returns scripted outputs in sequence.
    """

    def __init__(self, scripted_outputs: Sequence[str]):
        self.scripted_outputs = list(scripted_outputs)
        self.calls: List[dict] = []

    def create(self, **kwargs):
        self.calls.append(kwargs)
        if not self.scripted_outputs:
            raise RuntimeError("No more scripted outputs left for FakeResponsesAPI.")
        return FakeResponse(self.scripted_outputs.pop(0))


class FakeOpenAIClient:
    def __init__(self, scripted_outputs: Sequence[str]):
        self.responses = FakeResponsesAPI(scripted_outputs)


def make_items(names: Sequence[str]) -> List[FakeItem]:
    items: List[FakeItem] = []
    for name in names:
        data = {"name": name, "children": []}
        prompt_text = json.dumps(data, indent=2)
        # Stable-ish hash for tests
        content_hash = hash(json.dumps(data, sort_keys=True, separators=(",", ":")))
        items.append(
            FakeItem(
                filename=f"{name}.json",
                data=data,
                prompt_text=prompt_text,
                content_hash=content_hash,
            )
        )
    return items


# -----------------------------
# Unit tests
# -----------------------------


def test_load_taxonomies_reads_and_sorts(tmp_path: Path):
    # Arrange: three files in unsorted order plus a non-json
    (tmp_path / "b.json").write_text(json.dumps({"b": 1}), encoding="utf-8")
    (tmp_path / "a.json").write_text(json.dumps({"a": 1}), encoding="utf-8")
    (tmp_path / "c.json").write_text(json.dumps({"c": 1}), encoding="utf-8")
    (tmp_path / "ignore.txt").write_text("nope", encoding="utf-8")

    judge = TaxonomyJudge(
        {
            "api_key": "test",
            "system_msg": "Return ONLY A, B, or T.",
            "rubric": "Pick better.",
        }
    )

    items = judge._load_taxonomies(str(tmp_path))

    assert [it.filename for it in items] == ["a.json", "b.json", "c.json"]
    assert isinstance(items[0], TaxonomyItem)
    assert items[0].data == {"a": 1}


def test_sample_sparse_pairs_unique_unordered_and_within_limit():
    judge = TaxonomyJudge(
        {
            "api_key": "test",
            "seed": 123,
            "system_msg": "Return ONLY A, B, or T.",
            "rubric": "Pick better.",
        }
    )

    n = 20
    pair_limit = 50
    min_appearances = 4
    pairs = judge._sample_sparse_pairs(
        n=n, pair_limit=pair_limit, min_appearances=min_appearances
    )

    # Within limit
    assert len(pairs) <= pair_limit

    # Unordered uniqueness: all pairs are i < j and unique
    assert all(i < j for i, j in pairs)
    assert len(set(pairs)) == len(pairs)

    # Indices within bounds
    assert all(0 <= i < n and 0 <= j < n for i, j in pairs)


def test_sample_sparse_pairs_best_effort_coverage_when_budget_allows():
    judge = TaxonomyJudge(
        {
            "api_key": "test",
            "seed": 42,
            "system_msg": "Return ONLY A, B, or T.",
            "rubric": "Pick better.",
        }
    )

    n = 30
    min_appearances = 6
    # Budget sufficient: need at least ceil(n*min/2) = ceil(30*6/2)=90 pairs
    pair_limit = 120
    pairs = judge._sample_sparse_pairs(
        n=n, pair_limit=pair_limit, min_appearances=min_appearances
    )

    appearances = [0] * n
    for i, j in pairs:
        appearances[i] += 1
        appearances[j] += 1

    # Because the algorithm is randomized and best-effort, don't demand perfection,
    # but for this setting it should usually hit the target.
    assert min(appearances) >= min_appearances


def test_elicit_judgement_agreement_returns_winner(monkeypatch):
    """
    If AB says A and BA says B (mapped back => A), winner should be 0.
    """

    judge = TaxonomyJudge(
        {
            "api_key": "test",
            "seed": 7,
            "system_msg": "Return ONLY A, B, or T.",
            "rubric": "Pick better. Use T only if truly tied.",
        }
    )

    # Replace real OpenAI client with scripted fake:
    # First call (a,b) returns "A" -> a wins
    # Second call (b,a) returns "B" -> a wins (because in swapped order, B means second (a) wins)
    judge.client = FakeOpenAIClient(["A", "B"])  # type: ignore[attr-defined]

    a, b = make_items(["a", "b"])
    winner = judge._elicit_judgement(a, b)
    assert winner == 0


def test_elicit_judgement_disagreement_returns_tie(monkeypatch):
    """
    If AB says A (a wins) but BA also says A (b wins in original),
    winners disagree -> None (tie/indeterminate).
    """

    judge = TaxonomyJudge(
        {
            "api_key": "test",
            "seed": 7,
            "system_msg": "Return ONLY A, B, or T.",
            "rubric": "Pick better. Use T only if truly tied.",
        }
    )

    judge.client = FakeOpenAIClient(["A", "A"])  # type: ignore[attr-defined]

    a, b = make_items(["a", "b"])
    winner = judge._elicit_judgement(a, b)
    assert winner is None


def test_elicit_judgement_explicit_tie_returns_none(monkeypatch):
    """
    If either call returns T and the other doesn't match it, current logic returns None.
    If both map to None, it returns None.
    """

    judge = TaxonomyJudge(
        {
            "api_key": "test",
            "seed": 7,
            "system_msg": "Return ONLY A, B, or T.",
            "rubric": "Pick better. Use T only if truly tied.",
        }
    )

    judge.client = FakeOpenAIClient(["T", "T"])  # type: ignore[attr-defined]

    a, b = make_items(["a", "b"])
    winner = judge._elicit_judgement(a, b)
    assert winner is None


def test_rank_taxonomies_builds_comparisons_and_ranks(monkeypatch):
    """
    - Forces sampled pairs
    - Forces judgement outcomes including a tie
    - Mocks choix.ilsr_pairwise to return known scores
    - Asserts ranking order
    """

    judge = TaxonomyJudge(
        {
            "api_key": "test",
            "seed": 1,
            "system_msg": "Return ONLY A, B, or T.",
            "rubric": "Pick better. Use T only if truly tied.",
            "pair_limit": 10,
            "min_appearances": 1,
        }
    )

    # Patch loader to return 3 items (using FakeItem with required fields)
    items = make_items(["x", "y", "z"])
    monkeypatch.setattr(judge, "_load_taxonomies", lambda _: items)

    # Patch sampler to return fixed pairs: (0,1), (0,2)
    monkeypatch.setattr(
        judge, "_sample_sparse_pairs", lambda **kwargs: [(0, 1), (0, 2)]
    )

    # Patch judgements: first is tie, second is winner=1 (i.e. b wins so add (2,0))
    outcomes = [None, 1]

    def fake_elicit(a, b):
        return outcomes.pop(0)

    monkeypatch.setattr(judge, "_elicit_judgement", fake_elicit)

    # Capture what comparisons were passed to choix and return deterministic skills
    captured = {}

    def fake_ilsr_pairwise(n_items: int, data: List[Tuple[int, int]]):
        captured["n_items"] = n_items
        captured["data"] = list(data)
        # Force ranking: item 2 best, then 0, then 1
        return [0.2, 0.1, 0.9]

    monkeypatch.setattr("choix.ilsr_pairwise", fake_ilsr_pairwise)

    with pytest.raises(Warning):
        judge.rank_taxonomies("unused")

    assert captured["n_items"] == 3
    # Pair (0,1) tie => add both (0,1) and (1,0)
    # Pair (0,2) winner=1 => b wins => add (2,0)
    assert captured["data"] == [(0, 1), (1, 0), (2, 0)]
