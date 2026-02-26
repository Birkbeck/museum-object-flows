import json
from pathlib import Path

import pytest

from sheet_to_graph.cypher_translator import CypherTranslator


class DummyColumn:
    def __init__(
        self,
        name,
        *,
        primary_key=False,
        type_label=None,
        ignore=False,
        type_label_of=None,
        property_of=None,
        relation_from=None,
        relation_to=None,
        reference_column=None,
    ):
        self.name = name
        self.primary_key = primary_key
        self.type_label = type_label
        self.ignore = ignore
        self.type_label_of = type_label_of
        self.property_of = property_of
        self.relation_from = relation_from
        self.relation_to = relation_to
        self.reference_column = reference_column


class DummyTable:
    """Minimal stand-in for sheet_to_graph.Table."""

    def __init__(self, columns, rows):
        # columns: dict[name -> DummyColumn]
        # rows: list[dict]
        self.columns = columns
        self._rows = rows

    def __iter__(self):
        return iter(self._rows)


def test_translate_single_table_creates_basic_node(tmp_path: Path):
    """A simple table with one primary key column becomes a MERGE node query."""
    id_col = DummyColumn(
        "id",
        primary_key=True,
        type_label="Person",
        ignore=False,
    )
    columns = {"id": id_col}
    rows = [{"id": "123"}]
    table = DummyTable(columns, rows)

    output_file = tmp_path / "cypher.cql"
    translator = CypherTranslator(str(output_file))

    lines = translator.translate_tables(table)

    assert len(lines) == 1
    assert lines[0].strip() == 'MERGE (node:Person {id: "123"})'


def test_node_properties_and_extra_type_labels(tmp_path: Path):
    """type_label_of and property_of add extra labels and properties to nodes."""
    id_col = DummyColumn(
        "id",
        primary_key=True,
        type_label="Person",
    )
    kind_col = DummyColumn("kind", type_label_of="id")
    name_col = DummyColumn("name", property_of="id")

    columns = {"id": id_col, "kind": kind_col, "name": name_col}
    rows = [{"id": "123", "kind": "Curator", "name": "Alice"}]
    table = DummyTable(columns, rows)

    output_file = tmp_path / "nodes.cql"
    translator = CypherTranslator(str(output_file))

    lines = translator.translate_tables(table)

    expected = 'MERGE (node:Person:Curator {id: "123", name: "Alice"})'
    assert lines[0].strip() == expected


def test_basic_relationship_with_property(tmp_path: Path):
    """
    A relation_from column plus a property_of column should produce a MATCH/MERGE
    relationship Cypher query with the correct property.
    """
    person_id = DummyColumn(
        "person_id",
        primary_key=True,
        type_label="Person",
    )

    friend_id = DummyColumn(
        "friend_id",
        type_label="FRIENDS_WITH",
        relation_from="person_id",
        reference_column=person_id,
    )

    since = DummyColumn(
        "since",
        property_of="friend_id",
    )

    columns = {
        "person_id": person_id,
        "friend_id": friend_id,
        "since": since,
    }
    rows = [{"person_id": "1", "friend_id": "2", "since": "2020"}]
    table = DummyTable(columns, rows)

    output_file = tmp_path / "rels.cql"
    translator = CypherTranslator(str(output_file))

    lines = translator.translate_tables(table)

    assert lines[0].strip() == 'MERGE (node:Person {person_id: "1"})'

    expected_rel = (
        'MATCH (from:Person {person_id: "1"})'
        " WITH from"
        ' MATCH (to:Person {person_id: "2"})'
        ' MERGE (from)-[:FRIENDS_WITH {since: "2020"}]->(to)'
    )
    assert lines[1].strip() == expected_rel


def test_relationship_skips_empty_values(tmp_path: Path):
    """Relationships with missing from/to values are skipped."""
    person_id = DummyColumn(
        "person_id",
        primary_key=True,
        type_label="Person",
    )
    friend_id = DummyColumn(
        "friend_id",
        type_label="FRIENDS_WITH",
        relation_from="person_id",
        reference_column=person_id,
    )

    columns = {"person_id": person_id, "friend_id": friend_id}
    rows = [
        {"person_id": "1", "friend_id": "2"},
        {"person_id": "3", "friend_id": ""},
    ]
    table = DummyTable(columns, rows)

    output_file = tmp_path / "rels_skip.cql"
    translator = CypherTranslator(str(output_file))

    lines = translator.translate_tables(table)

    node_lines = [line for line in lines if line.startswith("MERGE (node")]
    rel_lines = [line for line in lines if line.startswith("MATCH (from")]

    assert len(node_lines) == 2
    assert len(rel_lines) == 1
