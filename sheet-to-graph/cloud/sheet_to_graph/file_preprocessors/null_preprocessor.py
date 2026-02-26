from sheet_to_graph import FilePreprocessor


class NullPreprocessor(FilePreprocessor):
    """Converts a list of lists into a list of dicts using column names in header_row.
    Ignores rows above the header_row.
    Adds a row_number element to each dict recording the original row number."""

    def preprocess(self, rows: list, header_row: int = 0) -> list:
        header = rows[header_row]
        return [
            {"row_number": index + header_row + 2} | dict(zip(header, row))
            for index, row in enumerate(rows[header_row:])
        ]
