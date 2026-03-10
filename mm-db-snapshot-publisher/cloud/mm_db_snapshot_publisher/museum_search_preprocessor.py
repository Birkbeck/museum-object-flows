import io
from typing import List

import pandas as pd
from scipy.io import mmwrite
from sklearn.feature_extraction.text import TfidfVectorizer


class MuseumSearchPreprocessor:
    """
    TF-IDF vectorizer for museums with character n-grams to support partial matches
    """

    def __init__(
        self,
        museums: pd.DataFrame,
        document_columns: List[str],
        *,
        ngram_range: tuple[int, int] = (3, 5),
        min_df: int = 1,
        max_df: float = 1.0,
        norm: str = "l2",
        sublinear_tf: bool = True,
    ):
        self.museums = museums
        self.document_columns = document_columns
        self.ngram_range = ngram_range
        self.min_df = min_df
        self.max_df = max_df
        self.norm = norm
        self.sublinear_tf = sublinear_tf

    def vectorize_museums(self):
        ids = self.museums["museum_id"].fillna("").astype(str).to_numpy()
        documents = self._build_documents()
        vectorizer = TfidfVectorizer(
            analyzer="char_wb",
            ngram_range=self.ngram_range,
            min_df=self.min_df,
            max_df=self.max_df,
            lowercase=True,
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

    def _build_documents(self) -> List[str]:
        return (
            self.museums[self.document_columns]
            .fillna("")
            .astype(str)
            .agg(" ".join, axis=1)
            .to_list()
        )
