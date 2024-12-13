class FilePreprocessor:
    """Converts a list of lists into a list of dicts using column names in header_row
    or their equivalent values from header_mapping if header_mapping is supplied.
    Ignores rows above the header_row.
    Adds a row_number element to each dict recording the original row number."""

    def preprocess(
        self, rows: list, header_row: int = 0, header_mapping: dict = None
    ) -> list:
        header = (
            rows[header_row]
            if header_mapping is None
            else [
                header_mapping[h] if h in header_mapping else h
                for h in rows[header_row]
            ]
        )
        return [
            {"row_number": index + header_row + 2}
            | dict(zip(header, [element.strip() for element in row]))
            for index, row in enumerate(rows[header_row + 1 :])
        ]
