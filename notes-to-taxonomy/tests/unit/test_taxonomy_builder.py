import numpy as np
import pandas as pd
import pytest

from src.taxonomy_builder import TaxonomyBuilder


class FakeSentenceModel:
    def __init__(self):
        self.calls = []

    def encode(self, texts, normalize_embedding=True):
        self.calls.append(("encode", len(texts), normalize_embedding))
        # deterministic embeddings: 5D vectors based on hash
        out = []
        for t in texts:
            rng = np.random.default_rng(abs(hash(t)) % (2**32))
            v = rng.normal(size=5).astype(np.float32)
            out.append(v / np.linalg.norm(v))
        return out


@pytest.fixture
def df_labels():
    return pd.DataFrame(
        {
            "label": ["roof", "door", "window", "gallery", "cafe", "shop"],
            "definition_deftype": ["def", "def", "def", "def", "def", "def"],
        }
    )


def test_init_validation_raises():
    encoder = FakeSentenceModel()

    with pytest.raises(Exception):
        TaxonomyBuilder(encoder, "X", "deftype", number_of_layers=0)

    with pytest.raises(Exception):
        TaxonomyBuilder(encoder, "X", "deftype", min_k=1)

    with pytest.raises(Exception):
        TaxonomyBuilder(encoder, "X", "deftype", max_k=1)


def test_get_embeddings_creates_augmented_label_and_embedding(tmp_path, df_labels):
    encoder = FakeSentenceModel()
    tb = TaxonomyBuilder(
        encoder=encoder,
        sentence_structure="This label refers to",
        definition_type="deftype",
        number_of_layers=1,
        min_k=2,
        max_k=3,
    )

    out = tb._get_embeddings(df_labels)

    assert "augmented_label" in out.columns
    assert "embedding" in out.columns

    # Confirm encoder was used
    assert encoder.calls and encoder.calls[0][0] == "encode"


def test_kmeans_adds_cluster_column(df_labels):
    df = df_labels.copy()
    df["embedding"] = [
        np.array([i, i + 1, i + 2, i + 3, i + 4], dtype=np.float32)
        for i in range(len(df))
    ]

    tb = TaxonomyBuilder(
        encoder=FakeSentenceModel(),
        sentence_structure="S",
        definition_type="deftype",
        number_of_layers=1,
        min_k=2,
        max_k=3,
    )

    clustered, score = tb._kmeans(df, layer_number=1, min_k=2, max_k=3, random_state=42)

    assert "layer_1_cluster" in clustered.columns
    assert clustered["layer_1_cluster"].notna().all()
    assert isinstance(score, float)


def test_cluster_first_layer_clusters_all(df_labels):
    df = df_labels.copy()
    df["embedding"] = [
        np.random.default_rng(i).normal(size=5).astype(np.float32)
        for i in range(len(df))
    ]

    tb = TaxonomyBuilder(
        encoder=FakeSentenceModel(),
        sentence_structure="S",
        definition_type="deftype",
        number_of_layers=2,
        min_k=2,
        max_k=3,
    )

    out = tb._cluster(df, layer=1)
    assert "layer_1_cluster" in out.columns


def test_generate_taxonomy_creates_cluster_columns(tmp_path, df_labels):
    encoder = FakeSentenceModel()

    tb = TaxonomyBuilder(
        encoder=encoder,
        sentence_structure="S",
        definition_type="deftype",
        number_of_layers=2,
        min_k=2,
        max_k=3,
    )

    out = tb.generate_taxonomy(df_labels)
    assert "layer_1_cluster" in out.columns
    assert "layer_2_cluster" in out.columns
