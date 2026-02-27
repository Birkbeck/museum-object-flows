from .base import LLM
from .completion_llm import CompletionLLM
from .seq_2_seq_llm import Seq2SeqLLM

SEQ2SEQ_LLMS = ["flan-t5"]
COMPLETION_LLMS = ["gpt", "llama"]


def make_llm_from_name(name: str) -> LLM:
    for completion_llm in COMPLETION_LLMS:
        if completion_llm in name.lower():
            return CompletionLLM.from_model_name(name)
    for seq2seq_llm in SEQ2SEQ_LLMS:
        if seq2seq_llm in name.lower():
            return Seq2SeqLLM.from_model_name(name)
    raise ValueError(f"Unknown LLM: {name}")
