import pytest

from sheet_to_graph.rules import MutuallyExclusiveColumns


@pytest.mark.parametrize(
    "column_1_name, column_2_name, row, expected_message",
    [
        (
            "date",
            "date_from",
            {"date": "2000", "date_from": ""},
            None,
        ),
        (
            "date",
            "date_from",
            {"date": "", "date_from": "2000"},
            None,
        ),
        (
            "date",
            "date_from",
            {"date": "2000", "date_from": "2000"},
            "Row cannot contain a value for both date ('2000') and date_from ('2000')",
        ),
    ],
)
def test_validation_fails_when_conflicting_columns_are_supplied(
    column_1_name, column_2_name, row, expected_message
):
    rule = MutuallyExclusiveColumns(column_1_name, column_2_name)
    assert expected_message == rule.validate(row)
