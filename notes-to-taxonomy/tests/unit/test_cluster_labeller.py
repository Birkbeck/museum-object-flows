import pytest

from src.cluster_labeller import ClusterLabeller


class FakeLLM:
    def __init__(self, response_text="FAKE_RESPONSE"):
        self.response_text = response_text
        self.calls = []

    def get_response(
        self, prompt, num_return_sequences, max_new_tokens, temperature, seed
    ):
        self.calls.append(
            {
                "prompt": prompt,
                "num_return_sequences": num_return_sequences,
                "max_new_tokens": max_new_tokens,
                "temperature": temperature,
                "seed": seed,
            }
        )
        return self.response_text


def test_generate_prompt_contains_role_task_examples_and_members():
    llm = FakeLLM()
    labeller = ClusterLabeller(
        llm=llm,
        role_description="ROLE",
        task_description="TASK",
        examples=["EX1", "EX2"],
        temperature=0.3,
    )

    prompt = labeller._generate_prompt(["roof", "door"])

    assert "ROLE" in prompt
    assert "TASK" in prompt
    assert "EX1" in prompt
    assert "EX2" in prompt
    assert "Sub-categories: roof, door" in prompt
    assert prompt.endswith("\nCategory:")


def test_label_cluster_calls_llm_with_expected_parameters_and_returns_response():
    llm = FakeLLM(response_text="Buildings / Structure")
    labeller = ClusterLabeller(
        llm=llm,
        role_description="ROLE",
        task_description="TASK",
        examples=[],
        temperature=0.7,
        num_return_sequences=3,
        max_new_tokens=50,
        seed=999,
    )

    result = labeller.label_cluster(["gallery", "cafe", "shop"])

    assert result == "Buildings / Structure"
    assert len(llm.calls) == 1

    call = llm.calls[0]
    assert "Sub-categories: gallery, cafe, shop" in call["prompt"]
    assert call["num_return_sequences"] == 3
    assert call["max_new_tokens"] == 50
    assert call["temperature"] == 0.7
    assert call["seed"] == 999


def test_generate_prompt_with_no_examples_still_formats_correctly():
    llm = FakeLLM()
    labeller = ClusterLabeller(
        llm=llm,
        role_description="ROLE",
        task_description="TASK",
        examples=[],
        temperature=0.3,
    )

    prompt = labeller._generate_prompt(["a", "b"])

    assert prompt.startswith("ROLE\nTASK\n\n")
    assert prompt.endswith("Sub-categories: a, b\nCategory:")


def test_label_cluster_works_with_single_member():
    llm = FakeLLM()
    labeller = ClusterLabeller(
        llm=llm,
        role_description="ROLE",
        task_description="TASK",
        examples=["EX"],
        temperature=0.3,
    )

    labeller.label_cluster(["roof"])
    prompt = llm.calls[0]["prompt"]

    assert "Sub-categories: roof" in prompt
