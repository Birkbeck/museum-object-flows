import pytest

from src.label_definers import LabelDefinerLLM


class FakeLLM:
    """
    Minimal fake LLM returning a controlled response.
    This allows us to test NoteLabeller without using a real model.
    """

    def __init__(self, output):
        self.output = output
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
        return self.output


def test_get_label_definition():
    """Full test of get_class_definition() using a fake LLM response."""
    fake_output = "A house is a place where people live."
    fake_llm = FakeLLM(fake_output)
    definer = LabelDefinerLLM(
        llm=fake_llm,
        role_description="Role text",
        task_description="Task text",
        examples=[],
        temperature=0.7,
        seed=99,
    )
    definition = definer.get_label_definition("label", "Some note.")

    assert definition == fake_output
    assert len(fake_llm.calls) == 1
    call = fake_llm.calls[0]
    assert "Some note." in call["prompt"]
    assert "label" in call["prompt"]
