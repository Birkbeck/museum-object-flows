import pytest

from sheet_to_graph import Column
from sheet_to_graph.columns import ReferenceColumn


@pytest.mark.parametrize(
    "input_values, reference_contents, expected_message",
    [
        (["item1"], ["item1", "item2", "item3"], None),
        (["item2"], ["item1", "item2", "item3"], None),
        (["item3"], ["item1", "item2", "item3"], None),
        ([""], ["item1", "item2", "item3"], None),
        (
            ["item4"],
            ["item1", "item2", "item3"],
            "Column table->item_id does not contain values item4",
        ),
        (
            ["item1", "item4", "item5"],
            ["item1", "item2", "item3"],
            "Column table->item_id does not contain values item4, item5",
        ),
    ],
)
def test_validate_entire_column(input_values, reference_contents, expected_message):
    item_id = Column("item_id")

    class MockTable:
        def __init__(self, name, columns):
            self.name = name
            self.columns = columns

    reference_table = MockTable("table", {"item_id": item_id})
    reference_table.rows = [{"item_id": cell} for cell in reference_contents]
    item_id.parent_table = reference_table

    related_item = ReferenceColumn(
        "related_item", "item_id", reference_table=reference_table
    )
    table = MockTable("table2", {"related_id": related_item})
    table.rows = [{"related_item": input_value} for input_value in input_values]
    related_item.parent_table = table

    assert expected_message == related_item._validate_entire_column()
