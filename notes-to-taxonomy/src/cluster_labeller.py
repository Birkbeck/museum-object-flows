from typing import List


class ClusterLabeller:
    def __init__(
        self,
        llm: "LLM",
        role_description: str,
        task_description: str,
        examples: List[str],
        temperature: float,
        top_p: float,
        num_return_sequences: int = 1,
        max_new_tokens: int = 20,
        seed: int = 123,
    ):
        self.llm = llm
        self.role_description = role_description
        self.task_description = task_description
        self.examples = examples
        self.temperature = temperature
        self.top_p = top_p
        self.num_return_sequences = num_return_sequences
        self.max_new_tokens = max_new_tokens
        self.seed = seed

    def label_cluster(self, members: List[str]):
        prompt = self._generate_prompt(members)
        response = self.llm.get_response(
            prompt,
            self.num_return_sequences,
            self.max_new_tokens,
            self.temperature,
            self.top_p,
            self.seed,
        )
        return response.strip()

    def _generate_prompt(self, members):
        return (
            self.role_description
            + "\n"
            + self.task_description
            + "\n"
            + "\n\n".join(self.examples)
            + "\n\n"
            + "Sub-categories: "
            + ", ".join(members)
            + "\nCategory:"
        )
