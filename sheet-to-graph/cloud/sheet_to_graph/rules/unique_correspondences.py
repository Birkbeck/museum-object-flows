from sheet_to_graph import Rule


class UniqueCorrespondences(Rule):
    """Specify a pair of columns which should have a one-to-one correspondence.
    If column 1 appears in multiple rows,
    column 2 must contain the same value in each of those rows"""

    def __init__(self, column_1_name: str, column_2_name: str):
        self.column_1_name = column_1_name
        self.column_2_name = column_2_name
        self.previously_checked_rows = {}

    def validate(self, row) -> str:
        column_1_value = row[self.column_1_name]
        column_2_value = row[self.column_2_name]
        try:
            existing_matches = self.previously_checked_rows[column_1_value]
            if column_2_value not in existing_matches:
                existing_matches_string = ", ".join(existing_matches)
                existing_matches.append(column_2_value)
                return (
                    f"When adding {self.column_1_name}: {column_1_value}; "
                    + f"{self.column_2_name}: {column_2_value} "
                    + f"found existing match {self.column_2_name}: {existing_matches_string}"
                )
        except KeyError:
            self.previously_checked_rows[column_1_value] = [column_2_value]
