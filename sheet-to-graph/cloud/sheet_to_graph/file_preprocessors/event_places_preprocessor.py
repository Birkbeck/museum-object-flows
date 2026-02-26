from sheet_to_graph import FilePreprocessor


class EventPlacesPreprocessor(FilePreprocessor):
    """Performs basic preprocessing and
    ignores rows with no place information."""

    def preprocess(
        self, rows: list, header_row: int = 0, header_mapping: dict = None
    ) -> list:
        preprocessed_rows = super().preprocess(
            rows, header_row=header_row, header_mapping=header_mapping
        )
        return [
            row
            for row in preprocessed_rows
            if not all(
                [row[column_name] == "" for column_name in header_mapping.values()]
            )
        ]
