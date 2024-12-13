from sheet_to_graph import Rule


class RequiredColumns(Rule):
    """Specify a list of columns which are required given a main column.
    For each row, if the cell in the main column has a value,
    so must the cells in the required columns."""

    def __init__(self, main_column: str, required_columns: list):
        self.main_column = main_column
        self.required_columns = required_columns

    def validate(self, row) -> str:
        main_column_value = row[self.main_column]
        if main_column_value == "":
            return None
        relevant_cells = {
            column_name: row[column_name] for column_name in self.required_columns
        }
        cells_without_value = {k: v for k, v in relevant_cells.items() if v == ""}
        if len(cells_without_value) == 0:
            return None
        columns_without_value = ", ".join([k for k in cells_without_value])
        return f"{self.main_column} filled in with {main_column_value}, but {columns_without_value} left blank."
