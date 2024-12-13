from sheet_to_graph import Rule


class MutuallyRequiredColumns(Rule):
    """Specify a list of columns which are mutually required.
    For each row, if a cell in one column has a value,
    so must the cells in the other columns."""

    def __init__(self, column_names: list):
        self.column_names = column_names

    def validate(self, row) -> str:
        relevant_cells = {
            column_name: row[column_name] for column_name in self.column_names
        }
        cells_with_value = {k: v for k, v in relevant_cells.items() if v != ""}
        cells_without_value = {k: v for k, v in relevant_cells.items() if v == ""}
        if len(cells_with_value) == 0 or len(cells_without_value) == 0:
            return None
        columns_with_value = ", ".join([k for k in cells_with_value])
        columns_with_value_values = ", ".join(
            [f"'{v}'" for _, v in cells_with_value.items()]
        )
        columns_without_value = ", ".join([k for k in cells_without_value])
        return f"{columns_with_value} filled in with {columns_with_value_values}, but {columns_without_value} left blank."
