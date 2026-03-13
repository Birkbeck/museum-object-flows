import json
from pathlib import Path
from typing import List, Tuple

import choix
import pandas as pd
import pytest

import src.taxonomy_judge as taxonomy_judge_module
from src.taxonomy_judge import TaxonomyItem, TaxonomyJudge


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
        "examples_with_winner_a": {
            "example_a_1.txt": "Example A1",
            "example_a_2.txt": "Example A2",
        },
        "examples_with_winner_b": {
            "example_b_1.txt": "Example B1",
            "example_b_2.txt": "Example B2",
        },
        "pair_limit": 4,
        "min_appearances": 1,
    }


def write_taxonomy_csv(path: Path, rows: list[dict]) -> None:
    pd.DataFrame(rows).to_csv(path, index=False)


def test_from_config_loads_task_and_examples(tmp_path: Path, monkeypatch):
    task_file = tmp_path / "task.txt"
    task_file.write_text("Task text", encoding="utf-8")

    example_a = tmp_path / "demo-a.txt"
    example_b = tmp_path / "demo-b.txt"
    example_a.write_text("Example winner A", encoding="utf-8")
    example_b.write_text("Example winner B", encoding="utf-8")

    fake_llm = FakeLLM()
    monkeypatch.setattr(taxonomy_judge_module, "make_llm_from_name", lambda _: fake_llm)

    config = {
        "judge_llm": "fake",
        "task_file": str(task_file),
        "example_files": [str(example_a), str(example_b)],
        "taxonomies_directory": str(tmp_path),
        "output_directory": str(tmp_path / "outputs"),
        "output_file_name": "ranking.json",
        "judgements_file_name": "judgements.jsonl",
        "temperature": 0.0,
        "top_p": 1.0,
        "max_new_tokens": 32,
        "seed": 1,
        "pair_limit": 4,
        "min_appearances": 1,
    }

    judge = TaxonomyJudge.from_config(config)

    assert judge.task == "Task text"
    assert judge.examples_with_winner_a == {str(example_a): "Example winner A"}
    assert judge.examples_with_winner_b == {str(example_b): "Example winner B"}


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


def test_add_examples_to_pairs_uses_one_a_and_one_b_example(tmp_path: Path):
    judge = TaxonomyJudge(make_config(tmp_path), FakeLLM())
    pairs = [(0, 1), (1, 0), (2, 1)]

    result = judge._add_examples_to_pairs(pairs)

    assert len(result) == 3
    for i, j, example_1, example_2 in result:
        assert (i, j) in pairs
        assert example_1[1] != example_2[1]
        assert {example_1[1], example_2[1]} == {"a", "b"}
        if example_1[1] == "a":
            assert example_1[0] in judge.examples_with_winner_a
            assert example_2[0] in judge.examples_with_winner_b
        else:
            assert example_1[0] in judge.examples_with_winner_b
            assert example_2[0] in judge.examples_with_winner_a


def test_elicit_judgement_parses_taxonomy_a_choice(tmp_path: Path):
    llm = FakeLLM(
        responses=["Comments: Taxonomy A is clearer.\nBest taxonomy: Taxonomy A"]
    )
    judge = TaxonomyJudge(make_config(tmp_path), llm)

    a = TaxonomyItem(filename="a.csv", data="A taxonomy")
    b = TaxonomyItem(filename="b.csv", data="B taxonomy")
    example_1 = ("example_a_1.txt", "a")
    example_2 = ("example_b_1.txt", "b")

    result = judge._elicit_judgement(a, b, example_1, example_2)

    assert result == {
        "best_taxonomy": 0,
        "comments": "taxonomy a is clearer.",
    }
    prompt = llm.calls[0]["prompt"]
    assert "Example A1" in prompt
    assert "Example B1" in prompt
    assert "Taxonomy A:\nA taxonomy" in prompt
    assert "Taxonomy B:\nB taxonomy" in prompt


def test_elicit_judgement_parses_taxonomy_b_choice_with_punctuation(tmp_path: Path):
    llm = FakeLLM(
        responses=[
            "Comments: Taxonomy B is better separated.\nBest taxonomy: Taxonomy B."
        ]
    )
    judge = TaxonomyJudge(make_config(tmp_path), llm)

    a = TaxonomyItem(filename="a.csv", data="A taxonomy")
    b = TaxonomyItem(filename="b.csv", data="B taxonomy")

    result = judge._elicit_judgement(
        a,
        b,
        ("example_b_1.txt", "b"),
        ("example_a_1.txt", "a"),
    )

    assert result == {
        "best_taxonomy": 1,
        "comments": "taxonomy b is better separated.",
    }


def test_elicit_judgement_returns_none_for_unparseable_response(tmp_path: Path):
    llm = FakeLLM(responses=["Something unstructured"])
    judge = TaxonomyJudge(make_config(tmp_path), llm)

    a = TaxonomyItem(filename="a.csv", data="A taxonomy")
    b = TaxonomyItem(filename="b.csv", data="B taxonomy")

    result = judge._elicit_judgement(
        a,
        b,
        ("example_a_1.txt", "a"),
        ("example_b_1.txt", "b"),
    )

    assert result == {"best_taxonomy": None, "comments": "something unstructured"}


def test_elicit_judgement_returns_none_on_llm_error(tmp_path: Path):
    llm = FakeLLM(raise_error=True)
    judge = TaxonomyJudge(make_config(tmp_path), llm)

    a = TaxonomyItem(filename="a.csv", data="A taxonomy")
    b = TaxonomyItem(filename="b.csv", data="B taxonomy")

    result = judge._elicit_judgement(
        a,
        b,
        ("example_a_1.txt", "a"),
        ("example_b_1.txt", "b"),
    )

    assert result["best_taxonomy"] is None
    assert "Error eliciting judgement" in result["comments"]


def test_rank_taxonomies_writes_ranking_and_judgements_store(
    tmp_path: Path, monkeypatch
):
    config = make_config(tmp_path)
    config["pair_limit"] = 2
    config["min_appearances"] = 1
    judge = TaxonomyJudge(config, FakeLLM())

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
        lambda pairs: [
            (0, 1, ("example_a_1.txt", "a"), ("example_b_1.txt", "b")),
            (0, 2, ("example_b_2.txt", "b"), ("example_a_2.txt", "a")),
        ],
    )

    results = [
        {"best_taxonomy": 0, "comments": "x beats y"},
        {"best_taxonomy": 1, "comments": "z beats x"},
    ]

    def fake_elicit(a, b, example_1, example_2):
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
            "example_1": "example_a_1.txt-a",
            "example_2": "example_b_1.txt-b",
            "best_taxonomy": 0,
            "comments": "x beats y",
        },
        {
            "taxonomy_a": "x.csv",
            "taxonomy_b": "z.csv",
            "example_1": "example_b_2.txt-b",
            "example_2": "example_a_2.txt-a",
            "best_taxonomy": 1,
            "comments": "z beats x",
        },
    ]


def test_rank_taxonomies_resumes_from_existing_judgements(tmp_path: Path, monkeypatch):
    config = make_config(tmp_path)
    config["pair_limit"] = 2
    config["min_appearances"] = 1
    judge = TaxonomyJudge(config, FakeLLM())

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
        lambda pairs: [
            (0, 1, ("example_a_1.txt", "a"), ("example_b_1.txt", "b")),
            (0, 2, ("example_b_2.txt", "b"), ("example_a_2.txt", "a")),
        ],
    )

    existing_row = {
        "taxonomy_a": "x.csv",
        "taxonomy_b": "y.csv",
        "example_1": "example_a_1.txt-a",
        "example_2": "example_b_1.txt-b",
        "best_taxonomy": 0,
        "comments": "existing",
    }
    Path(judge.judgements_file_name).parent.mkdir(parents=True, exist_ok=True)
    with open(judge.judgements_file_name, "w", encoding="utf-8") as f:
        f.write(json.dumps(existing_row) + "\n")

    calls: list[Tuple[str, str, Tuple[str, str], Tuple[str, str]]] = []

    def fake_elicit(a, b, example_1, example_2):
        calls.append((a.filename, b.filename, example_1, example_2))
        return {"best_taxonomy": 1, "comments": "new"}

    monkeypatch.setattr(judge, "_elicit_judgement", fake_elicit)
    monkeypatch.setattr(choix, "ilsr_pairwise", lambda n_items, data: [0.5, 0.1, 0.9])

    judge.rank_taxonomies(["x.csv", "y.csv", "z.csv"])

    assert calls == [
        ("x.csv", "z.csv", ("example_b_2.txt", "b"), ("example_a_2.txt", "a"))
    ]

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
        "example_1": "example_b_2.txt-b",
        "example_2": "example_a_2.txt-a",
        "best_taxonomy": 1,
        "comments": "new",
    }


def test_rank_taxonomies_respects_max_new_judgements(tmp_path: Path, monkeypatch):
    config = make_config(tmp_path)
    config["pair_limit"] = 3
    config["min_appearances"] = 1
    config["max_new_judgements"] = 1
    judge = TaxonomyJudge(config, FakeLLM())

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
        lambda pairs: [
            (0, 1, ("example_a_1.txt", "a"), ("example_b_1.txt", "b")),
            (0, 2, ("example_a_2.txt", "a"), ("example_b_2.txt", "b")),
            (1, 2, ("example_b_1.txt", "b"), ("example_a_1.txt", "a")),
        ],
    )

    calls: list[Tuple[str, str, Tuple[str, str], Tuple[str, str]]] = []

    def fake_elicit(a, b, example_1, example_2):
        calls.append((a.filename, b.filename, example_1, example_2))
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
