from typing import List

from src import LabelDefiner, LLM


class LabelDefinerLLM(LabelDefiner):
    def __init__(
        self,
        llm: LLM,
        role_description: str,
        task_description: str,
        examples: List[str],
        temperature: float,
        seed: int,
    ):
        self.llm = llm
        self.role_description = role_description
        self.task_description = task_description
        self.examples = examples
        self.temperature = temperature
        self.seed = seed

    def get_label_definition(self, label: str, note: str):
        prompt = (
            "The following text has been summarised with a building use type label."
            # TODO: prompt for shorter outputs, e.g. 2-3 sentences
            "Provide a concise and general definition of the building use type label in this context."
            "Do not include details from the original text.\n\n"
            f"Text: {note}\n\n"
            f"Building use type: {label}\n\n"
            "Definition:"
        )
        response = self.llm.get_response(
            prompt,
            num_return_sequences=1,
            max_new_tokens=100,  # TODO: and then avoid hard cut-offs
            temperature=0.7,
            seed=self.seed,
        )
        return response
