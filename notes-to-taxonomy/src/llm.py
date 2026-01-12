from abc import ABC, abstractmethod


class LLM(ABC):
    @abstractmethod
    def get_response(
        self,
        task_input: str,
        num_return_sequences: int,
        max_new_tokens: int,
        temperature: float,
        seed: int,
    ) -> str:
        raise NotImplementedError
