import pytest

from sheet_to_graph import Column
from sheet_to_graph import Table


def test_raises_exception_if_columns_missing_from_file():
    col_1 = Column("col_1")
    col_2 = Column("col_2")
    col_3 = Column("col_3")
    tab = Table("test", [col_1, col_2, col_3])
    table_data = [{"col_1": "a"}, {"col_1": "b"}]

    with pytest.raises(Exception) as e:
        tab.import_from_list_of_dicts(table_data)
    assert e.value.args[0] == "test: columns missing from file: col_2, col_3"


@pytest.mark.parametrize(
    "table_data, filter_terms, expected_results",
    [
        (
            [
                {"col_1": "a", "col_2": "x"},
                {"col_1": "a", "col_2": "y"},
                {"col_1": "a", "col_2": "z"},
                {"col_1": "b", "col_2": "x"},
                {"col_1": "b", "col_2": "y"},
                {"col_1": "b", "col_2": "z"},
                {"col_1": "c", "col_2": "x"},
                {"col_1": "c", "col_2": "y"},
                {"col_1": "c", "col_2": "z"},
            ],
            {"col_1": "a"},
            [
                {"col_1": "a", "col_2": "x"},
                {"col_1": "a", "col_2": "y"},
                {"col_1": "a", "col_2": "z"},
            ],
        ),
        (
            [
                {"col_1": "a", "col_2": "x"},
                {"col_1": "a", "col_2": "y"},
                {"col_1": "a", "col_2": "z"},
                {"col_1": "b", "col_2": "x"},
                {"col_1": "b", "col_2": "y"},
                {"col_1": "b", "col_2": "z"},
                {"col_1": "c", "col_2": "x"},
                {"col_1": "c", "col_2": "y"},
                {"col_1": "c", "col_2": "z"},
            ],
            {"col_1": "a", "col_2": "y"},
            [
                {"col_1": "a", "col_2": "y"},
            ],
        ),
        (
            [
                {"col_1": "a", "col_2": "x"},
                {"col_1": "a", "col_2": "y"},
                {"col_1": "a", "col_2": "z"},
                {"col_1": "b", "col_2": "x"},
                {"col_1": "b", "col_2": "y"},
                {"col_1": "b", "col_2": "z"},
                {"col_1": "c", "col_2": "x"},
                {"col_1": "c", "col_2": "y"},
                {"col_1": "c", "col_2": "z"},
            ],
            {"col_2": "z"},
            [
                {"col_1": "a", "col_2": "z"},
                {"col_1": "b", "col_2": "z"},
                {"col_1": "c", "col_2": "z"},
            ],
        ),
    ],
)
def test_filter(table_data, filter_terms, expected_results):
    col_1 = Column("col_1")
    col_2 = Column("col_2")
    tab = Table("test", [col_1, col_2])
    tab.import_from_list_of_dicts(table_data)

    assert expected_results == tab.filter(**filter_terms)
