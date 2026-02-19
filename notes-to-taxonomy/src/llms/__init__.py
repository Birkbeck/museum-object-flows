from .base import LLM
from .completion_llm import CompletionLLM
from .seq_2_seq_llm import Seq2SeqLLM

SEQ2SEQ_LLMS = ["flan-t5-base"]
COMPLETION_LLMS = ["gpt2", "llama3.1-8B"]


def make_llm_from_name(name: str) -> LLM:
    short_name = name.split("/")[-1]
    if short_name in COMPLETION_LLMS:
        return CompletionLLM.from_model_name(name)
    if short_name in SEQ2SEQ_LLMS:
        return Seq2SeqLLM.from_model_name(name)
    raise ValueError(f"Unknown LLM: {name}")
