from sheet_to_graph import Rule


class FillCellsWithValueWhen(Rule):
    """Specify a column that should be filled with a value when a test is met."""

    def __init__(self, fill_column: str, with_value: str, when: callable):
        self.fill_column = fill_column
        self.with_value = with_value
        self.when = when

    def make_inference(self, row: dict):
        if self.when(row):
            row[self.fill_column] = self.with_value
        return row
