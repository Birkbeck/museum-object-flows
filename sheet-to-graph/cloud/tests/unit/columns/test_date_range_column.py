import pytest

from sheet_to_graph.columns import DateRangeColumn


@pytest.mark.parametrize(
    "input_value, expected_message",
    [
        (
            "2010:2015:2020",
            "Column [date] Date range 2010:2015:2020 contains too many dates",
        ),
        (
            "not a date",
            "Column [date] Date range must contain only numbers, dashes and a colon e.g. yyyy:yyyy, yyyy-mm-dd, yyyy-mm, or yyyy",
        ),
        (
            "200",
            "Column [date] Date year must be 4 digits long (dates are formatted yyyy-mm-dd).",
        ),
        (
            "200-03-20",
            "Column [date] Date year must be 4 digits long (dates are formatted yyyy-mm-dd).",
        ),
        (
            "2000-20-03",
            "Column [date] Date month must be between 1 and 12 (dates are formatted yyyy-mm-dd).",
        ),
        (
            "2000-03-00",
            "Column [date] Date day must be between 1 and 31 (dates are formatted yyyy-mm-dd).",
        ),
        ("", None),
        ("2000", None),
        ("2000-03", None),
        ("2000-03-20", None),
        ("2000-03-20:2000-04-14", None),
        ("2000-03:2000-05", None),
        ("2000:2001", None),
    ],
)
def test_validate(input_value, expected_message):
    column = DateRangeColumn("date")
    assert expected_message == column.validate(input_value)
