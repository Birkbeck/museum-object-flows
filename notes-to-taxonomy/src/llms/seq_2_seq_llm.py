import torch
from transformers import AutoModelForSeq2SeqLM, AutoTokenizer, pipeline, set_seed

from src.llm import LLM


class Seq2SeqLLM(LLM):
    def __init__(self, model):
        self.model = model

    @classmethod
    def from_model_name(cls, name: str, trust_remote: bool = False):
        return cls(
            pipeline(
                "text2text-generation",
                model=AutoModelForSeq2SeqLM.from_pretrained(
                    name,
                    device_map="auto",
                    dtype=(
                        torch.float16
                        if torch.backends.mps.is_available()
                        else torch.float32
                    ),
                    trust_remote_code=trust_remote,
                ),
                tokenizer=AutoTokenizer.from_pretrained(
                    name, trust_remote_code=trust_remote
                ),
            )
        )

    def get_response(
        self,
        task_input: str,
        num_return_sequences: int = None,
        max_new_tokens: int = None,
        temperature: float = None,
        top_p: float = None,
        seed: int = None,
    ) -> str:
        set_seed(seed)
        out = self.model(
            task_input,
            num_return_sequences=num_return_sequences,
            max_new_tokens=max_new_tokens,
            temperature=temperature,
            top_p=top_p,
        )
        return out
