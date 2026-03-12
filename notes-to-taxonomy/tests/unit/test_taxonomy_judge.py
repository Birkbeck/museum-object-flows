import json
from pathlib import Path
from typing import List, Tuple

import pandas as pd
import pytest

import choix
from src.taxonomy_judge import TaxonomyJudge, TaxonomyItem


class FakeLLM:
    def __init__(self, responses: List[str] | None = None, raise_error: bool = False):
        self.responses = list(responses or [])
        self.raise_error = raise_error
        self.calls: List[dict] = []

    def get_response(
        self,
        prompt: str,
        num_return_sequences: int,
        max_new_tokens: int,
        temperature: float,
        top_p: float,
        seed: int,
    ) -> str:
        self.calls.append(
            {
                "prompt": prompt,
                "num_return_sequences": num_return_sequences,
                "max_new_tokens": max_new_tokens,
                "temperature": temperature,
                "top_p": top_p,
                "seed": seed,
            }
        )
        if self.raise_error:
            raise RuntimeError("LLM failure")
        if not self.responses:
            raise RuntimeError("No fake responses left")
        return self.responses.pop(0)


def make_config(tmp_path: Path) -> dict:
    output_dir = tmp_path / "outputs"
    output_dir.mkdir(parents=True, exist_ok=True)
    return {
        "taxonomies_directory": str(tmp_path),
        "output_directory": str(output_dir),
        "output_file_name": "ranking.json",
        "judgements_file_name": "judgements.jsonl",
        "temperature": 0.0,
        "top_p": 1.0,
        "max_new_tokens": 32,
        "seed": 123,
        "task": "Compare the taxonomies.",
        "examples": ["Example 1", "Example 2"],
        "pair_limit": 4,
        "min_appearances": 1,
    }


def write_taxonomy_csv(path: Path, rows: list[dict]) -> None:
    pd.DataFrame(rows).to_csv(path, index=False)


def test_load_taxonomies_builds_taxonomy_strings(tmp_path: Path):
    csv_path = tmp_path / "taxonomy_a.csv"
    write_taxonomy_csv(
        csv_path,
        [
            {
                "layer_1_label": "building",
                "layer_2_label": "condition",
                "label": "damp",
            },
            {
                "layer_1_label": "building",
                "layer_2_label": "condition",
                "label": "damp",
            },
            {
                "layer_1_label": "building",
                "layer_2_label": "condition",
                "label": "leaking roof",
            },
            {"layer_1_label": "building", "layer_2_label": "safety", "label": "unsafe"},
            {
                "layer_1_label": "staff",
                "layer_2_label": "loss",
                "label": "resignations",
            },
        ],
    )

    judge = TaxonomyJudge(make_config(tmp_path), FakeLLM())
    items = judge._load_taxonomies(["taxonomy_a.csv"])

    assert len(items) == 1
    assert isinstance(items[0], TaxonomyItem)
    assert items[0].filename == "taxonomy_a.csv"
    assert (
        items[0].data == "building:\n"
        "  condition: damp, leaking roof\n"
        "  safety: unsafe\n"
        "staff:\n"
        "  loss: resignations\n"
    )


def test_sample_pairs_returns_directed_unique_pairs_and_respects_constraints(
    tmp_path: Path,
):
    config = make_config(tmp_path)
    config["pair_limit"] = 8
    config["min_appearances"] = 2
    judge = TaxonomyJudge(config, FakeLLM())

    pairs = judge._sample_pairs(4)

    assert len(pairs) == 8
    assert len(set(pairs)) == 8
    assert all(i != j for i, j in pairs)
    assert all(0 <= i < 4 and 0 <= j < 4 for i, j in pairs)

    left_counts = [0] * 4
    for i, _ in pairs:
        left_counts[i] += 1
    assert min(left_counts) >= 2


def test_add_examples_to_pairs_is_deterministic(tmp_path: Path):
    judge = TaxonomyJudge(make_config(tmp_path), FakeLLM())
    pairs = [(0, 1), (1, 0), (2, 1)]

    result_1 = judge._add_examples_to_pairs(pairs)
    result_2 = judge._add_examples_to_pairs(pairs)

    assert result_1 == result_2
    assert all(len(t) == 3 for t in result_1)
    assert all(example in judge.examples for _, _, example in result_1)


def test_elicit_judgement_parses_taxonomy_a_choice(tmp_path: Path):
    llm = FakeLLM(
        responses=["Comments: Taxonomy A is clearer.\nBest taxonomy: Taxonomy A"]
    )
    judge = TaxonomyJudge(make_config(tmp_path), llm)

    a = TaxonomyItem(filename="a.csv", data="A taxonomy")
    b = TaxonomyItem(filename="b.csv", data="B taxonomy")

    result = judge._elicit_judgement(a, b, "Example 1")

    assert result == {
        "best_taxonomy": 0,
        "comments": "taxonomy a is clearer.",
    }
    assert "Taxonomy A:\nA taxonomy" in llm.calls[0]["prompt"]
    assert "Taxonomy B:\nB taxonomy" in llm.calls[0]["prompt"]


def test_elicit_judgement_parses_taxonomy_b_choice_with_punctuation(tmp_path: Path):
    llm = FakeLLM(
        responses=[
            "Comments: Taxonomy B is better separated.\nBest taxonomy: Taxonomy B."
        ]
    )
    judge = TaxonomyJudge(make_config(tmp_path), llm)

    a = TaxonomyItem(filename="a.csv", data="A taxonomy")
    b = TaxonomyItem(filename="b.csv", data="B taxonomy")

    result = judge._elicit_judgement(a, b, "Example 1")

    assert result == {
        "best_taxonomy": 1,
        "comments": "taxonomy b is better separated.",
    }


def test_elicit_judgement_returns_none_for_unparseable_response(tmp_path: Path):
    llm = FakeLLM(responses=["Something unstructured"])
    judge = TaxonomyJudge(make_config(tmp_path), llm)

    a = TaxonomyItem(filename="a.csv", data="A taxonomy")
    b = TaxonomyItem(filename="b.csv", data="B taxonomy")

    result = judge._elicit_judgement(a, b, "Example 1")

    assert result == {"best_taxonomy": None, "comments": ""}


def test_elicit_judgement_returns_none_on_llm_error(tmp_path: Path):
    llm = FakeLLM(raise_error=True)
    judge = TaxonomyJudge(make_config(tmp_path), llm)

    a = TaxonomyItem(filename="a.csv", data="A taxonomy")
    b = TaxonomyItem(filename="b.csv", data="B taxonomy")

    result = judge._elicit_judgement(a, b, "Example 1")

    assert result == {"best_taxonomy": None, "comments": ""}


def test_rank_taxonomies_writes_ranking_and_judgements_store(
    tmp_path: Path, monkeypatch
):
    config = make_config(tmp_path)
    config["pair_limit"] = 2
    config["min_appearances"] = 1
    llm = FakeLLM()
    judge = TaxonomyJudge(config, llm)

    items = [
        TaxonomyItem(filename="x.csv", data="X"),
        TaxonomyItem(filename="y.csv", data="Y"),
        TaxonomyItem(filename="z.csv", data="Z"),
    ]
    monkeypatch.setattr(judge, "_load_taxonomies", lambda _: items)
    monkeypatch.setattr(judge, "_sample_pairs", lambda n: [(0, 1), (0, 2)])
    monkeypatch.setattr(
        judge,
        "_add_examples_to_pairs",
        lambda pairs: [(0, 1, "Example 1"), (0, 2, "Example 2")],
    )

    results = [
        {"best_taxonomy": 0, "comments": "x beats y"},
        {"best_taxonomy": 1, "comments": "z beats x"},
    ]

    def fake_elicit(a, b, example):
        return results.pop(0)

    monkeypatch.setattr(judge, "_elicit_judgement", fake_elicit)
    monkeypatch.setattr(choix, "ilsr_pairwise", lambda n_items, data: [0.5, 0.1, 0.9])

    ranked = judge.rank_taxonomies(["x.csv", "y.csv", "z.csv"])

    assert ranked == ["z.csv", "x.csv", "y.csv"]

    ranking_path = Path(judge.output_file_name)
    assert ranking_path.exists()
    assert json.loads(ranking_path.read_text(encoding="utf-8")) == ranked

    judgements_path = Path(judge.judgements_file_name)
    stored_rows = [
        json.loads(line)
        for line in judgements_path.read_text(encoding="utf-8").splitlines()
    ]
    assert stored_rows == [
        {
            "taxonomy_a": "x.csv",
            "taxonomy_b": "y.csv",
            "example": "Example 1",
            "best_taxonomy": 0,
            "comments": "x beats y",
        },
        {
            "taxonomy_a": "x.csv",
            "taxonomy_b": "z.csv",
            "example": "Example 2",
            "best_taxonomy": 1,
            "comments": "z beats x",
        },
    ]


def test_rank_taxonomies_resumes_from_existing_judgements(tmp_path: Path, monkeypatch):
    config = make_config(tmp_path)
    config["pair_limit"] = 2
    config["min_appearances"] = 1
    llm = FakeLLM()
    judge = TaxonomyJudge(config, llm)

    items = [
        TaxonomyItem(filename="x.csv", data="X"),
        TaxonomyItem(filename="y.csv", data="Y"),
        TaxonomyItem(filename="z.csv", data="Z"),
    ]
    monkeypatch.setattr(judge, "_load_taxonomies", lambda _: items)
    monkeypatch.setattr(judge, "_sample_pairs", lambda n: [(0, 1), (0, 2)])
    monkeypatch.setattr(
        judge,
        "_add_examples_to_pairs",
        lambda pairs: [(0, 1, "Example 1"), (0, 2, "Example 2")],
    )

    existing_row = {
        "taxonomy_a": "x.csv",
        "taxonomy_b": "y.csv",
        "example": "Example 1",
        "best_taxonomy": 0,
        "comments": "existing",
    }
    Path(judge.judgements_file_name).parent.mkdir(parents=True, exist_ok=True)
    with open(judge.judgements_file_name, "w", encoding="utf-8") as f:
        f.write(json.dumps(existing_row) + "\n")

    calls: list[Tuple[str, str, str]] = []

    def fake_elicit(a, b, example):
        calls.append((a.filename, b.filename, example))
        return {"best_taxonomy": 1, "comments": "new"}

    monkeypatch.setattr(judge, "_elicit_judgement", fake_elicit)
    monkeypatch.setattr(choix, "ilsr_pairwise", lambda n_items, data: [0.5, 0.1, 0.9])

    judge.rank_taxonomies(["x.csv", "y.csv", "z.csv"])

    assert calls == [("x.csv", "z.csv", "Example 2")]

    stored_rows = [
        json.loads(line)
        for line in Path(judge.judgements_file_name)
        .read_text(encoding="utf-8")
        .splitlines()
    ]
    assert len(stored_rows) == 2
    assert stored_rows[0] == existing_row
    assert stored_rows[1] == {
        "taxonomy_a": "x.csv",
        "taxonomy_b": "z.csv",
        "example": "Example 2",
        "best_taxonomy": 1,
        "comments": "new",
    }


def test_rank_taxonomies_respects_max_new_judgements(tmp_path: Path, monkeypatch):
    config = make_config(tmp_path)
    config["pair_limit"] = 3
    config["min_appearances"] = 1
    config["max_new_judgements"] = 1
    llm = FakeLLM()
    judge = TaxonomyJudge(config, llm)

    items = [
        TaxonomyItem(filename="x.csv", data="X"),
        TaxonomyItem(filename="y.csv", data="Y"),
        TaxonomyItem(filename="z.csv", data="Z"),
    ]
    monkeypatch.setattr(judge, "_load_taxonomies", lambda _: items)
    monkeypatch.setattr(judge, "_sample_pairs", lambda n: [(0, 1), (0, 2), (1, 2)])
    monkeypatch.setattr(
        judge,
        "_add_examples_to_pairs",
        lambda pairs: [(0, 1, "Example 1"), (0, 2, "Example 2"), (1, 2, "Example 1")],
    )

    calls: list[Tuple[str, str, str]] = []

    def fake_elicit(a, b, example):
        calls.append((a.filename, b.filename, example))
        return {"best_taxonomy": 0, "comments": "ok"}

    monkeypatch.setattr(judge, "_elicit_judgement", fake_elicit)
    monkeypatch.setattr(choix, "ilsr_pairwise", lambda n_items, data: [0.9, 0.5, 0.1])

    judge.rank_taxonomies(["x.csv", "y.csv", "z.csv"])

    assert len(calls) == 1

    stored_rows = [
        json.loads(line)
        for line in Path(judge.judgements_file_name)
        .read_text(encoding="utf-8")
        .splitlines()
    ]
    assert len(stored_rows) == 1
