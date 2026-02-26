from sheet_to_graph import Rule


class MutuallyExclusiveColumns(Rule):
    """Specify two columns which are mutually exclusive for a given row.
    For each row, if a cell in one column has a value,
    the cell in the other column must be empty."""

    def __init__(self, column_1_name: str, column_2_name: str):
        self.column_1_name = column_1_name
        self.column_2_name = column_2_name

    def validate(self, row) -> str:
        cell_1 = row[self.column_1_name]
        cell_2 = row[self.column_2_name]
        if cell_1 == "" or cell_2 == "":
            return None
        return (
            f"Row cannot contain a value for both {self.column_1_name} ('{cell_1}') "
            + f"and {self.column_2_name} ('{cell_2}')"
        )
