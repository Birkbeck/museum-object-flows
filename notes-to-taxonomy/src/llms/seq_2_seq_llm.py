from src.llm import LLM


class Seq2SeqLLM(LLM):
    def __init__(self, model):
        self.model = model

    def get_response(
        self,
        task_input: str,
        num_return_sequences: int,
        max_new_tokens: int,
        temperature: float,
        seed: int,
    ) -> str:
        out = self.model(task_input)
        return out
