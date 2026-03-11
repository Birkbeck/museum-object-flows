from typing import List


class NoteLabeller:
    def __init__(
        self,
        llm: "LLM",
        role_description: str,
        task_description: str,
        examples: List[str],
        temperature: float,
        num_return_sequences: int = 1,
        max_new_tokens: int = 20,
        seed: int = 123,
    ):
        self.llm = llm
        self.role_description = role_description
        self.task_description = task_description
        self.examples = examples
        self.temperature = temperature
        self.num_return_sequences = num_return_sequences
        self.max_new_tokens = max_new_tokens
        self.seed = seed

    def label_note(self, note: str):
        prompt = self._generate_prompt(note)
        response = self.llm.get_response(
            prompt,
            self.num_return_sequences,
            self.max_new_tokens,
            self.temperature,
            self.seed,
        )
        labels = {
            "status": self._extract_labels(response, "status"),
            "use": self._extract_labels(response, "use"),
            "responsibility": self._extract_labels(response, "responsibility"),
        }
        return labels

    def _generate_prompt(self, note):
        response_format = [
            "Format your response like this:",
            "change in status: ...",
            "change in use: ...",
            "change in responsibility: ...",
        ]
        return "\n\n".join(
            [self.role_description, self.task_description]
            + response_format
            + self.examples
            + [f"Notes: {note}"]
        )

    def _extract_labels(self, text, label_type):
        """Returns a sub-string of text containing the labels of the required type"""
        label_lists = text.lower().split("change in ")
        try:
            sub_string = [l for l in label_lists if l[: len(label_type)] == label_type][
                0
            ]
            sub_string_no_label_type = sub_string[len(label_type) + 1 :]
            sub_string_no_notes = sub_string_no_label_type.split("\nnotes")[0]
            return sub_string_no_notes.strip()
        except IndexError:
            return ""
