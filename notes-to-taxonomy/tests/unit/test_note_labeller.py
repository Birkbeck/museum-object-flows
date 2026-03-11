import pytest

from src.note_labeller import NoteLabeller


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
        # Record call arguments for optional debugging
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


def test_extract_labels():
    """Test that _extract_labels correctly extracts individual label sections."""
    response = """
    change in status: active
    change in use: storage
    change in responsibility: sold
    """

    labeller = NoteLabeller(
        llm=None,
        role_description="role",
        task_description="task",
        examples=[],
        temperature=0.0,
        seed=0,
    )

    assert labeller._extract_labels(response, "status") == "active"
    assert labeller._extract_labels(response, "use") == "storage"
    assert labeller._extract_labels(response, "responsibility") == "sold"


def test_generate_prompt_structure():
    """Test that prompt generation includes role, task, examples, and the note text."""

    labeller = NoteLabeller(
        llm=None,
        role_description="You are a classifier.",
        task_description="Label the note.",
        examples=["Example A", "Example B"],
        temperature=0.3,
        seed=123,
    )

    prompt = labeller._generate_prompt("This is a note.")

    assert "You are a classifier." in prompt
    assert "Label the note." in prompt
    assert "Example A" in prompt
    assert "Example B" in prompt
    assert "Notes: This is a note." in prompt


def test_label_note_end_to_end():
    """Full test of label_note() using a fake LLM response."""
    fake_output = """
    change in status: refurbished
    change in use: flats
    change in responsibility: new tenant
    """

    fake_llm = FakeLLM(fake_output)

    labeller = NoteLabeller(
        llm=fake_llm,
        role_description="Role text",
        task_description="Task text",
        examples=[],
        temperature=0.7,
        seed=99,
    )

    labels = labeller.label_note("Some note.")

    assert labels["status"] == "refurbished"
    assert labels["use"] == "flats"
    assert labels["responsibility"] == "new tenant"

    # Optionally verify that LLM was called once
    assert len(fake_llm.calls) == 1

    call = fake_llm.calls[0]
    assert "Some note." in call["prompt"]  # ensure prompt was passed through
