from sentence_transformers import SentenceTransformer


class SentenceModel:
    def __init__(self, model_name):
        self.saved_embeddings = {}
        self.transformer = SentenceTransformer(model_name)

    def encode(self, texts: list, normalize_embeddings: bool = True):
        pass
