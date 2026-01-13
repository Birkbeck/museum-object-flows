import re
import numpy as np
import pytest

from src.label_definers import LabelDefinerWiki


class FakeResponse:
    def __init__(self, status_code=200, json_data=None):
        self.status_code = status_code
        self._json_data = json_data or {}

    def json(self):
        return self._json_data

    def raise_for_status(self):
        # emulate requests' behavior
        if self.status_code >= 400:
            raise Exception(f"HTTP {self.status_code}")


class FakeSession:
    """
    Minimal stand-in for requests.Session that records calls and returns
    queued FakeResponse objects.
    """

    def __init__(self, responses):
        self.responses = list(responses)
        self.calls = []
        self.headers = {}

    def mount(self, *_args, **_kwargs):
        # LabelDefinerWiki calls mount() during init
        return None

    def get(self, url, params=None, headers=None, timeout=None):
        self.calls.append(
            {"url": url, "params": params, "headers": headers, "timeout": timeout}
        )
        if not self.responses:
            raise RuntimeError("No more fake responses queued")
        return self.responses.pop(0)


class FakeSentenceModel:
    """
    encode(texts) -> numpy array embeddings.
    We return deterministic vectors so similarity is predictable.
    """

    def __init__(self, mapping):
        # mapping: text -> vector
        self.mapping = mapping
        self.calls = []

    def encode(self, texts, normalize_embeddings=True):
        self.calls.append((tuple(texts), normalize_embeddings))
        vecs = []
        for t in texts:
            v = np.asarray(self.mapping.get(t, [0.0, 0.0, 0.0]), dtype=np.float32)
            if normalize_embeddings:
                n = np.linalg.norm(v)
                if n > 0:
                    v = v / n
            vecs.append(v)
        return np.vstack(vecs)


@pytest.fixture
def cfg():
    return {"task_name": "notes-to-taxonomy", "email": "george@example.com"}


def test__search_parses_titles(monkeypatch, cfg):
    # Wikipedia search API response
    search_json = {"query": {"search": [{"title": "house"}, {"title": "home"}]}}
    fake_session = FakeSession([FakeResponse(200, search_json)])

    # sentence model not used in _search
    sm = FakeSentenceModel(mapping={})

    wiki = LabelDefinerWiki(cfg, sm)
    wiki.session = fake_session  # override real session

    titles = wiki._search("house", n=2, lang="en")

    assert titles == ["house", "home"]
    assert fake_session.calls[0]["url"].endswith("en.wikipedia.org/w/api.php")
    assert fake_session.calls[0]["params"]["srsearch"] == "house"
    assert fake_session.calls[0]["params"]["srlimit"] == 2


def test__get_page_intro_non_200_returns_empty(monkeypatch, cfg):
    fake_session = FakeSession([FakeResponse(status_code=404, json_data={})])
    sm = FakeSentenceModel(mapping={})
    wiki = LabelDefinerWiki(cfg, sm)
    wiki.session = fake_session

    txt = wiki._get_page_intro("house", lang="en")
    assert txt == ""


def test__get_page_intro_disambiguation_returns_empty(cfg):
    fake_session = FakeSession(
        [FakeResponse(status_code=200, json_data={"type": "disambiguation"})]
    )
    sm = FakeSentenceModel(mapping={})
    wiki = LabelDefinerWiki(cfg, sm)
    wiki.session = fake_session

    txt = wiki._get_page_intro("house", lang="en")
    assert txt == ""


def test__get_page_intro_strips_and_normalizes_whitespace(cfg):
    fake_session = FakeSession(
        [
            FakeResponse(
                status_code=200,
                json_data={"extract": "A house is a place where people live"},
            )
        ]
    )
    sm = FakeSentenceModel(mapping={})
    wiki = LabelDefinerWiki(cfg, sm)
    wiki.session = fake_session

    txt = wiki._get_page_intro("A house is a place where people live", lang="en")
    assert txt == "A house is a place where people live"
    assert not re.search(r"\s{2,}", txt)  # no repeated whitespace


def test_get_label_definition_empty_when_no_titles(cfg, monkeypatch):
    sm = FakeSentenceModel(mapping={"house": [1, 0, 0]})
    wiki = LabelDefinerWiki(cfg, sm)

    # If your base class exposes search/get_page_intro, remove these monkeypatches.
    monkeypatch.setattr(wiki, "_search", lambda label, n=5, lang="en": [])
    monkeypatch.setattr(wiki, "_get_page_intro", lambda title, lang="en": "ignored")

    assert wiki.get_label_definition("house") == ""


def test_get_label_definition_filters_title_case_multi_word_label(cfg, monkeypatch):
    sm = FakeSentenceModel(
        mapping={
            "Chocolate Factory is an album": [1, 0, 0],
            "chocolate factory is a factory": [1, 0, 0],
        }
    )
    wiki = LabelDefinerWiki(cfg, sm)

    # Titles include exact Title() match; for single-word labels it should be filtered out
    monkeypatch.setattr(
        wiki,
        "_search",
        lambda label, n=5, lang="en": ["Chocolate Factory", "chocolate factory"],
    )
    monkeypatch.setattr(
        wiki,
        "_get_page_intro",
        lambda title, lang="en": (
            "Chocolate Factory is an album"
            if title == "Chocolate Factory"
            else "chocolate factory is a factory"
        ),
    )

    snippet = wiki.get_label_definition(
        "chocolate factory", top_k=5, minimum_relevance=0.1
    )

    # Because "Chocolate Factory" should be filtered, we expect only "chocolate factory" intro
    assert snippet == "chocolate factory is a factory"


def test_get_label_definition_selects_most_similar_and_stops_at_minimum_relevance(
    cfg, monkeypatch
):
    # label embedding aligns best with "A", then "B", then "C"
    sm = FakeSentenceModel(
        mapping={
            "house": [1, 0, 0],
            "A": [1, 0, 0],  # similarity 1.0
            "B": [0.6, 0.8, 0],  # similarity 0.6 (after normalization)
            "C": [0, 1, 0],  # similarity 0.0
        }
    )
    wiki = LabelDefinerWiki(cfg, sm)

    monkeypatch.setattr(
        wiki, "_search", lambda label, n=5, lang="en": ["T1", "T2", "T3"]
    )
    monkeypatch.setattr(
        wiki,
        "_get_page_intro",
        lambda title, lang="en": {"T1": "A", "T2": "B", "T3": "C"}[title],
    )

    # minimum_relevance=1.0 means it should stop after including "A" only
    snippet = wiki.get_label_definition("house", top_k=3, minimum_relevance=1.0)
    assert snippet == "A"

    # minimum_relevance=1.4 means it needs A (1.0) + B (~0.6) -> reaches >= 1.4, so returns "A B"
    snippet2 = wiki.get_label_definition("house", top_k=3, minimum_relevance=1.4)
    assert snippet2 == "A B"


def test_get_label_definition_ignores_empty_intros(cfg, monkeypatch):
    sm = FakeSentenceModel(mapping={"house": [1, 0, 0], "Good intro": [1, 0, 0]})
    wiki = LabelDefinerWiki(cfg, sm)

    monkeypatch.setattr(wiki, "_search", lambda label, n=5, lang="en": ["T1", "T2"])
    monkeypatch.setattr(
        wiki,
        "_get_page_intro",
        lambda title, lang="en": "" if title == "T1" else "Good intro",
    )

    snippet = wiki.get_label_definition("house", top_k=2, minimum_relevance=0.1)
    assert snippet == "Good intro"
