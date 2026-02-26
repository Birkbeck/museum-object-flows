import io
import re
from typing import List, Optional, Pattern, Set, Protocol

import pandas as pd
import snowballstemmer
from scipy.io import mmwrite
from sklearn.feature_extraction.text import ENGLISH_STOP_WORDS, TfidfVectorizer


class Stemmer(Protocol):
    def stemWords(self, words: List[str]) -> List[str]:
        ...


class MuseumSearchPreprocessor:
    """
    TF-IDF vectorizer for museums with:
      - regex tokenization
      - stopword removal
      - stemming (Snowball)
    """

    def __init__(
        self,
        museums: pd.DataFrame,
        document_columns: List[str],
        *,
        stemmer: Stemmer,
        stopwords: Set[str],
        token_regex: Pattern[str],
        ngram_range: tuple[int, int] = (1, 2),
        min_df: int = 2,
        max_df: float = 0.95,
        norm: str = "l2",
        sublinear_tf: bool = True,
    ):
        self.museums = museums
        self.document_columns = document_columns
        self.stemmer = stemmer
        self.stopwords = stopwords
        self.token_regex = token_regex
        self.ngram_range = ngram_range
        self.min_df = min_df
        self.max_df = max_df
        self.norm = norm
        self.sublinear_tf = sublinear_tf

    @classmethod
    def setup(
        cls,
        museums: pd.DataFrame,
        document_columns: List[str],
        *,
        language: str = "english",
        token_pattern: str = r"[a-z0-9]+",
        stopwords: Optional[Set[str]] = None,
        ngram_range: tuple[int, int] = (1, 2),
        min_df: int = 2,
        max_df: float = 0.95,
        norm: str = "l2",
        sublinear_tf: bool = True,
    ) -> "MuseumSearchPreprocessor":
        """
        Convenience constructor that builds default dependencies.
        Still DI-friendly because __init__ accepts injected deps.
        """
        stemmer = snowballstemmer.stemmer(language)
        token_regex = re.compile(token_pattern)
        stopwords_set = set(ENGLISH_STOP_WORDS) if stopwords is None else set(stopwords)

        return cls(
            museums,
            document_columns,
            stemmer=stemmer,
            stopwords=stopwords_set,
            token_regex=token_regex,
            ngram_range=ngram_range,
            min_df=min_df,
            max_df=max_df,
            norm=norm,
            sublinear_tf=sublinear_tf,
        )

    def vectorize_museums(self):
        ids = self.museums["museum_id"].fillna("").astype(str).to_numpy()
        documents = self._build_documents()
        vectorizer = TfidfVectorizer(
            tokenizer=self._tokenizer,
            preprocessor=None,
            token_pattern=None,
            ngram_range=self.ngram_range,
            min_df=self.min_df,
            max_df=self.max_df,
            norm=self.norm,
            sublinear_tf=self.sublinear_tf,
        )
        X = vectorizer.fit_transform(documents)
        return {
            "matrix": X,
            "ids": ids,
            "vocab": vectorizer.get_feature_names_out(),
            "idf": vectorizer.idf_,
            "vectorizer": vectorizer,
        }

    @staticmethod
    def sparse_to_mtx_bytes(X) -> bytes:
        buf = io.BytesIO()
        mmwrite(buf, X.tocoo())
        return buf.getvalue()

    def _tokenizer(self, text: str) -> List[str]:
        tokens = self.token_regex.findall((text or "").lower())
        tokens = [t for t in tokens if t and t not in self.stopwords]
        return self.stemmer.stemWords(tokens)

    def _build_documents(self) -> List[str]:
        return (
            self.museums[self.document_columns]
            .fillna("")
            .astype(str)
            .agg(" ".join, axis=1)
            .to_list()
        )
