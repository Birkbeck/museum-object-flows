from sheet_to_graph import Column


def test_uniqueness():
    class MockTable:
        def __init__(self, name, columns):
            self.name = name
            self.columns = columns
            self.rows = []

    non_unique_column_1 = Column("non_unique_1", unique=False)
    non_unique_column_2 = Column("non_unique_2", unique=False)
    unique_column_1 = Column("unique_1", unique=True)
    unique_column_2 = Column("unique_2", unique=True)
    table = MockTable(
        "table",
        {
            "non_unique_1": non_unique_column_1,
            "non_unique_2": non_unique_column_2,
            "unique_column_1": unique_column_1,
            "unique_column_2": unique_column_2,
        },
    )
    for n, c in table.columns.items():
        c.parent_table = table

    row_1 = {"non_unique_1": "1", "non_unique_2": "2", "unique_1": "3", "unique_2": "4"}
    row_2 = {"non_unique_1": "1", "non_unique_2": "1", "unique_1": "4", "unique_2": "4"}

    table.rows.append(row_1)

    assert non_unique_column_1.validate(row_2["non_unique_1"]) is None
    assert non_unique_column_2.validate(row_2["non_unique_2"]) is None
    assert unique_column_1.validate(row_2["unique_1"]) is None
    assert (
        "Column [unique_2] must be unique, but '4' already appears above."
        == unique_column_2.validate(row_2["unique_2"])
    )
