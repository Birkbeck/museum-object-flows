from typing import List


class ClusterLabeller:
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

    def label_cluster(self, members):
        prompt = self._generate_prompt(members)
        response = self.llm.get_response(
            prompt,
            self.num_return_sequences,
            self.max_new_tokens,
            self.temperature,
            self.seed,
        )
        return response

    def _generate_prompt(self, members):
        return (
            self.role_description
            + "\n"
            + self.task_description
            + "\n\n"
            + "\n\n".join(self.examples)
            + "Sub-categories: "
            + ", ".join(members)
            + "\nCategory:"
        )
