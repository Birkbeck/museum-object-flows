from sheet_to_graph.columns import SplitColumn


def test_split_value():
    split_on_column = SplitColumn("col", [None, None], split_on=";")
    assert ["xxx"] == split_on_column._split_value("xxx")
    assert ["xxx", ""] == split_on_column._split_value("xxx;")
    assert ["xxx", "yyy"] == split_on_column._split_value("xxx;yyy")

    split_before_column = SplitColumn("col", [None, None], split_before="?")
    assert ["xxx"] == split_before_column._split_value("xxx")
    assert ["xxx", "?"] == split_before_column._split_value("xxx?")
    assert ["xxx", "?+"] == split_before_column._split_value("xxx?+")


def test_validate():
    class MockColumn:
        def __init__(self, name):
            self.name = name

        def _validate(self, value):
            return f"{self.name} error message for {value}"

    sub_column_1 = MockColumn("col_one")
    sub_column_2 = MockColumn("col_two")
    split_column = SplitColumn("split", [sub_column_1, sub_column_2], split_before="?")

    assert (
        "col_one error message for type; col_two error message for ?+"
        == split_column._validate("type?+")
    )


def test_format_as_dict():
    class MockColumn:
        def __init__(self, name):
            self.name = name

        def format(self, value):
            return value

    sub_column_1 = MockColumn("col_one")
    sub_column_2 = MockColumn("col_two")
    split_column = SplitColumn("split", [sub_column_1, sub_column_2], split_before="?")

    assert {
        "split": "type?+",
        "col_one": "type",
        "col_two": "?+",
    } == split_column.format_as_dict("type?+")
