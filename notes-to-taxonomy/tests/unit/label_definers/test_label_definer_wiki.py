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
def email():
    return "george.wright@bbk.ac.uk"


def test__search_parses_titles(monkeypatch, email):
    # Wikipedia search API response
    search_json = {"query": {"search": [{"title": "house"}, {"title": "home"}]}}
    fake_session = FakeSession([FakeResponse(200, search_json)])

    # sentence model not used in _search
    sentence_model = FakeSentenceModel(mapping={})

    wiki = LabelDefinerWiki(sentence_model, email, top_k=2)
    wiki.session = fake_session  # override real session

    titles = wiki._search("house")

    assert titles == ["house", "home"]
    assert fake_session.calls[0]["url"].endswith("en.wikipedia.org/w/api.php")
    assert fake_session.calls[0]["params"]["srsearch"] == "house"
    assert fake_session.calls[0]["params"]["srlimit"] == 2


def test__get_page_intro_non_200_returns_empty(monkeypatch, email):
    fake_session = FakeSession([FakeResponse(status_code=404, json_data={})])
    sentence_model = FakeSentenceModel(mapping={})
    wiki = LabelDefinerWiki(sentence_model, email)
    wiki.session = fake_session

    txt = wiki._get_page_intro("house")
    assert txt == ""


def test__get_page_intro_disambiguation_returns_empty(email):
    fake_session = FakeSession(
        [FakeResponse(status_code=200, json_data={"type": "disambiguation"})]
    )
    sentence_model = FakeSentenceModel(mapping={})
    wiki = LabelDefinerWiki(sentence_model, email)
    wiki.session = fake_session

    txt = wiki._get_page_intro("house")
    assert txt == ""


def test__get_page_intro_strips_and_normalizes_whitespace(email):
    fake_session = FakeSession(
        [
            FakeResponse(
                status_code=200,
                json_data={"extract": "A house is a place where people live"},
            )
        ]
    )
    sentence_model = FakeSentenceModel(mapping={})
    wiki = LabelDefinerWiki(sentence_model, email)
    wiki.session = fake_session

    txt = wiki._get_page_intro("A house is a place where people live")
    assert txt == "A house is a place where people live"
    assert not re.search(r"\s{2,}", txt)  # no repeated whitespace


def test_get_label_definition_empty_when_no_titles(email, monkeypatch):
    sentence_model = FakeSentenceModel(mapping={"house": [1, 0, 0]})
    wiki = LabelDefinerWiki(sentence_model, email)
    monkeypatch.setattr(wiki, "_search", lambda label: [])
    monkeypatch.setattr(wiki, "_get_page_intro", lambda title: "ignored")

    result = wiki.get_label_definition("house", "the museum was converted into a house")
    assert "" == result


def test_get_label_definition_filters_title_case_multi_word_label(email, monkeypatch):
    sentence_model = FakeSentenceModel(
        mapping={
            "Chocolate Factory is an album": [1, 0, 0],
            "chocolate factory is a factory": [1, 0, 0],
        }
    )
    wiki = LabelDefinerWiki(sentence_model, email, top_k=5, minimum_relevance=0.1)

    # Titles include exact Title() match; for single-word labels it should be filtered out
    monkeypatch.setattr(
        wiki,
        "_search",
        lambda label: ["Chocolate Factory", "chocolate factory"],
    )
    monkeypatch.setattr(
        wiki,
        "_get_page_intro",
        lambda title: (
            "Chocolate Factory is an album"
            if title == "Chocolate Factory"
            else "chocolate factory is a factory"
        ),
    )

    snippet = wiki.get_label_definition(
        "chocolate factory", "the museum was converted into a chocolate factory"
    )

    # Because "Chocolate Factory" should be filtered, we expect only "chocolate factory" intro
    assert snippet == "chocolate factory is a factory"


def test_get_label_definition_selects_most_similar_and_stops_at_minimum_relevance(
    email, monkeypatch
):
    # label embedding aligns best with "A", then "B", then "C"
    sentence_model = FakeSentenceModel(
        mapping={
            "house": [1, 0, 0],
            "A": [1, 0, 0],  # similarity 1.0
            "B": [0.6, 0.8, 0],  # similarity 0.6 (after normalization)
            "C": [0, 1, 0],  # similarity 0.0
        }
    )
    wiki = LabelDefinerWiki(sentence_model, email, top_k=5)
    monkeypatch.setattr(wiki, "_search", lambda label: ["T1", "T2", "T3"])
    monkeypatch.setattr(
        wiki,
        "_get_page_intro",
        lambda title: {"T1": "A", "T2": "B", "T3": "C"}[title],
    )

    # minimum_relevance=1.0 means it should stop after including "A" only
    wiki.minimum_relevance = 1.0
    snippet = wiki.get_label_definition(
        "house", "the museum was converted into a house"
    )
    assert snippet == "A"

    # minimum_relevance=1.4 means it needs A (1.0) + B (~0.6) -> reaches >= 1.4, so returns "A B"
    wiki.minimum_relevance = 1.4
    snippet2 = wiki.get_label_definition(
        "house", "the museum was converted into a house"
    )
    assert snippet2 == "A B"


def test_get_label_definition_ignores_empty_intros(email, monkeypatch):
    sentence_model = FakeSentenceModel(
        mapping={"house": [1, 0, 0], "Good intro": [1, 0, 0]}
    )
    wiki = LabelDefinerWiki(sentence_model, email, top_k=2, minimum_relevance=0.1)
    monkeypatch.setattr(wiki, "_search", lambda label: ["T1", "T2"])
    monkeypatch.setattr(
        wiki,
        "_get_page_intro",
        lambda title: "" if title == "T1" else "Good intro",
    )

    snippet = wiki.get_label_definition(
        "house", "the museum was converted into a house"
    )
    assert snippet == "Good intro"
