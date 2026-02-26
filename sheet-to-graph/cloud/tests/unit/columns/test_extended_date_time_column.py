import pytest

from sheet_to_graph.columns import ExtendedDateTimeColumn


@pytest.mark.skip
@pytest.mark.parametrize(
    "input_value, expected_message",
    [
        (
            "2010:2015",
            "Column [date] Date '2010:2015' does not conform to LoC Extended Date Time Format",
        ),
        (
            "not a date",
            "Column [date] Date 'not a date' does not conform to LoC Extended Date Time Format",
        ),
        ("", None),
        ("2000", None),
        ("2000-03", None),
        ("2000-03-20", None),
        ("2000-03-20/2000-04-14", None),
        ("2000-03~/2000-05~", None),
        ("2000/2001", None),
    ],
)
def test_validate(input_value, expected_message):
    column = ExtendedDateTimeColumn("date")
    assert expected_message == column.validate(input_value)
