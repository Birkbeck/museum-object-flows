from transformers import AutoModelForCausalLM, AutoTokenizer, pipeline, set_seed

from src.llm import LLM


class CompletionLLM(LLM):
    def __init__(self, model):
        self.model = model

    @classmethod
    def from_model_name(cls, name: str, trust_remote: bool = False):
        return cls(
            pipeline(
                "text-generation",
                model=AutoModelForCausalLM.from_pretrained(
                    name,
                    trust_remote_code=trust_remote,
                ),
                tokenizer=AutoTokenizer.from_pretrained(
                    name,
                    trust_remote_code=trust_remote,
                ),
                device=0,
                pad_token_id=0,
            )
        )

    def get_response(
        self,
        task_input: str,
        num_return_sequences: int = 1,
        max_new_tokens: int = 200,
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
        return out[0]["generated_text"][len(task_input) :]
