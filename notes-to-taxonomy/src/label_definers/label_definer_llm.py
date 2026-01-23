from typing import List

from src import LabelDefiner, LLM


class LabelDefinerLLM(LabelDefiner):
    def __init__(
        self,
        llm: LLM,
        prompt: str,
        max_new_tokens: int,
        temperature: float,
        top_p: float,
        seed: int,
    ):
        self.llm = llm
        self.prompt = prompt
        self.max_new_tokens = max_new_tokens
        self.temperature = temperature
        self.top_p = top_p
        self.seed = seed

    def get_label_definition(self, label: str, note: str):
        prompt = (
            f"{self.prompt}\n\n"
            f"Text: {note}\n"
            f"Building use type: {label}\n"
            "Definition:"
        )
        response = self.llm.get_response(
            prompt,
            num_return_sequences=1,
            max_new_tokens=self.max_new_tokens,
            temperature=self.temperature,
            top_p=self.top_p,
            seed=self.seed,
        )
        return response
