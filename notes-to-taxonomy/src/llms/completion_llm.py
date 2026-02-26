from typing import List, Optional, Sequence, Union
from transformers import AutoModelForCausalLM, AutoTokenizer, pipeline, set_seed

from .base import LLM


class CompletionLLM(LLM):
    def __init__(self, model):
        self.model = model

    @classmethod
    def from_model_name(cls, name: str, trust_remote: bool = False):
        model = AutoModelForCausalLM.from_pretrained(
            name, trust_remote_code=trust_remote
        )
        tokenizer = AutoTokenizer.from_pretrained(name, trust_remote_code=trust_remote)
        tokenizer.padding_side = "left"
        if tokenizer.pad_token_id is None:
            if tokenizer.eos_token_id is None:
                tokenizer.add_special_tokens({"pad_token": "[PAD]"})
                model.resize_token_embeddings(len(tokenizer))
            else:
                tokenizer.pad_token = tokenizer.eos_token
        if getattr(model.config, "pad_token_id", None) is None:
            model.config.pad_token_id = tokenizer.pad_token_id
        pipe = pipeline(
            "text-generation",
            model=model,
            tokenizer=tokenizer,
            device=0,
        )
        return cls(pipe)

    def get_response(
        self,
        task_input: str,
        num_return_sequences: int = 1,
        max_new_tokens: int = 200,
        temperature: float | None = None,
        top_p: float | None = None,
        seed: int | None = None,
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

    def get_responses(
        self,
        task_inputs: List[str],
        max_new_tokens: int = 200,
        temperature: float | None = None,
        top_p: float | None = None,
        seed: int | None = None,
        batch_size: int = 16,
    ) -> List[str]:
        """
        True batched inference: one pipeline call with a list.
        Returns one response per input (assumes num_return_sequences == 1).
        """
        set_seed(seed)
        outs = self.model(
            task_inputs,
            num_return_sequences=1,
            max_new_tokens=max_new_tokens,
            temperature=temperature,
            top_p=top_p,
            batch_size=batch_size,
        )
        responses: List[str] = []
        for inp, out_i in zip(task_inputs, outs):
            gen_text = out_i[0]["generated_text"]
            responses.append(gen_text[len(inp) :])
        return responses
