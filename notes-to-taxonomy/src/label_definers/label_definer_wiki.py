import re

import numpy as np
import requests
from requests.adapters import HTTPAdapter
from sentence_transformers import SentenceTransformer
from urllib3.util.retry import Retry

from src import LabelDefiner


class LabelDefinerWiki(LabelDefiner):
    """Handles connection to Wikipedia for generating concept definitions."""

    def __init__(
        self,
        sentence_model: SentenceTransformer,
        email,
        top_k=5,
        minimum_relevance=0.7,
        lang="en",
    ):
        self.session = requests.Session()
        self.session.headers.update({"User-Agent": f"notes-to-taxonomy/0.1 ({email})"})
        self.retries = Retry(
            total=5,
            backoff_factor=0.5,  # 0.5s, 1s, 2s, ...
            status_forcelist=[429, 500, 502, 503, 504],
            allowed_methods=["GET"],
            raise_on_status=False,
        )
        self.session.mount("https://", HTTPAdapter(max_retries=self.retries))
        self.top_k = top_k
        self.minimum_relevance = minimum_relevance
        self.lang = lang
        self.sentence_model = sentence_model

    def get_label_definition(self, label: str, note: str):
        titles = self._search(label)
        # remove titles which are proper nouns with same name as label
        titles = [t for t in titles if len(label.split()) == 1 or t != label.title()]
        if not titles:
            return ""
        label_embedding = self.sentence_model.encode([label], normalize_embeddings=True)
        candidate_texts = [self._get_page_intro(t) for t in titles]
        pairs = [(t, c) for t, c in zip(titles, candidate_texts)]
        if not pairs:
            return ""
        candidate_embeddings = self.sentence_model.encode(
            [c for _, c in pairs], normalize_embeddings=True
        )
        similarity_scores = (candidate_embeddings @ label_embedding.T).ravel()
        order = np.argsort(-similarity_scores)
        total_relevance = 0
        context_texts = []
        for i in order[: self.top_k]:
            context_texts.append(pairs[i][1])
            total_relevance += similarity_scores[i]
            if total_relevance >= self.minimum_relevance:
                break
        snippet = " ".join(context_texts)
        return snippet

    def _search(self, term):
        params = {
            "action": "query",
            "list": "search",
            "srsearch": term,
            "srlimit": self.top_k,
            "format": "json",
            "utf8": 1,
        }
        r = self.session.get(
            f"https://{self.lang}.wikipedia.org/w/api.php", params=params, timeout=10
        )
        r.raise_for_status()  # will retry on 429/5xx due to adapter
        data = r.json()
        return [hit["title"] for hit in data.get("query", {}).get("search", [])]

    def _get_page_intro(self, title):
        r = self.session.get(
            f"https://{self.lang}.wikipedia.org/api/rest_v1/page/summary/{requests.utils.quote(title)}",
            headers={"accept": "application/json"},
            timeout=10,
        )
        if r.status_code != 200:
            return ""
        data = r.json()
        if data.get("type") == "disambiguation":
            return ""
        txt = (data.get("extract") or "").strip()
        return re.sub(r"\s+", " ", txt)
