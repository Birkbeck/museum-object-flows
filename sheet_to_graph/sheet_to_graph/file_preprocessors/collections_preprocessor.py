from .super_events_preprocessor import SuperEventsPreprocessor


class CollectionsPreprocessor(SuperEventsPreprocessor):
    """Performs basic preprocessing and
    Removes duplicate referencess to the same collection/object"""

    def preprocess(
        self, rows: list, header_row: int = 0, header_mapping: dict = None
    ) -> list:
        preprocessed_rows = super().preprocess(
            rows, header_row=header_row, header_mapping=header_mapping
        )
        super_event_ids = set()
        collection_ids = set()
        unique_rows = []
        for row in preprocessed_rows:
            if (
                row["super_event_id"] in super_event_ids
                and row["collection_id"] in collection_ids
            ):
                continue
            unique_rows.append(row)
            super_event_ids.add(row["super_event_id"])
            collection_ids.add(row["collection_id"])
        return unique_rows
