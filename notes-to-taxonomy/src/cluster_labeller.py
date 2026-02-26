from typing import List, Callable, Any, Dict
from datasets import Dataset


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

    def label_cluster(self, members: List[str]) -> str:
        prompt = self._generate_prompt(members)
        response = self.llm.get_response(
            prompt,
            self.num_return_sequences,
            self.max_new_tokens,
            self.temperature,
            self.top_p,
            self.seed,
        )
        return self._first_nonempty_line(response)

    def label_clusters(
        self,
        lists_of_members: List[List[str]],
        *,
        batch_size: int = 16,
        num_proc: int | None = None,
        desc: str = "Labelling clusters",
    ) -> List[str]:
        prompts = [self._generate_prompt(members) for members in lists_of_members]
        dataset = Dataset.from_dict({"prompt": prompts})

        def _infer_batch(batch: Dict[str, List[Any]]) -> Dict[str, List[str]]:
            batch_prompts: List[str] = batch["prompt"]
            raw = self.llm.get_responses(
                batch_prompts,
                max_new_tokens=self.max_new_tokens,
                temperature=self.temperature,
                top_p=self.top_p,
                seed=self.seed,
                batch_size=batch_size,
            )
            labels = [self._first_nonempty_line(r) for r in raw]
            return {"label": labels}

        dataset = dataset.map(
            _infer_batch,
            batched=True,
            batch_size=batch_size,
            num_proc=num_proc,
            desc=desc,
        )
        return list(dataset["label"])

    @staticmethod
    def _first_nonempty_line(response: str) -> str:
        for line in response.split("\n"):
            line = line.strip()
            if line:
                return line
        return ""

    def _generate_prompt(self, members: List[str]) -> str:
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
