from abc import ABC, abstractmethod


class LLM(ABC):
    @abstractmethod
    def get_response(self, task_input: str) -> str:
        raise NotImplementedError
