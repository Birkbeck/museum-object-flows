import pytest

from sheet_to_graph.rules import MutuallyRequiredColumns


@pytest.mark.parametrize(
    "column_names, row, expected_message",
    [
        (["date_from", "date_to"], {"date_from": "2000", "date_to": "2001"}, None),
        (
            ["date_from", "date_to"],
            {"date_from": "2000", "date_to": ""},
            "date_from filled in with '2000', but date_to left blank.",
        ),
        (
            ["date_from", "date_to"],
            {"date_from": "", "date_to": "2001"},
            "date_to filled in with '2001', but date_from left blank.",
        ),
        (
            ["item_id", "item_name", "item_description"],
            {
                "item_id": "123",
                "item_name": "pot",
                "item_description": "ancient pottery",
            },
            None,
        ),
        (
            ["item_id", "item_name", "item_description"],
            {
                "item_id": "",
                "item_name": "pot",
                "item_description": "ancient pottery",
            },
            "item_name, item_description filled in with 'pot', 'ancient pottery', but item_id left blank.",
        ),
        (
            ["item_id", "item_name", "item_description"],
            {
                "item_id": "123",
                "item_name": "",
                "item_description": "ancient pottery",
            },
            "item_id, item_description filled in with '123', 'ancient pottery', but item_name left blank.",
        ),
        (
            ["item_id", "item_name", "item_description"],
            {
                "item_id": "",
                "item_name": "",
                "item_description": "ancient pottery",
            },
            "item_description filled in with 'ancient pottery', but item_id, item_name left blank.",
        ),
    ],
)
def test_validation_fails_when_not_all_columns_are_supplied(
    column_names, row, expected_message
):
    rule = MutuallyRequiredColumns(column_names)
    assert expected_message == rule.validate(row)
