from sheet_to_graph import FilePreprocessor


class SuperEventsPreprocessor(FilePreprocessor):
    """Performs basic preprocessing and
    splits events with multiple event types into multiple rows."""

    def preprocess(
        self, rows: list, header_row: int = 0, header_mapping: dict = None
    ) -> list:
        preprocessed_rows = super().preprocess(
            rows, header_row=header_row, header_mapping=header_mapping
        )

        # separate single rows with multiple events into multiple rows
        separated_rows = []
        for row in preprocessed_rows:
            sub_event_types = row["event_type"].split(";")
            for index, sub_event_type in enumerate(sub_event_types):
                # copy event row but replace event type and
                sub_event_row = {k: v for k, v in row.items()}
                sub_event_row["event_type"] = sub_event_type
                # don't repeat recipient which is sender in following event
                if index > 0:
                    sub_event_row["actor_recipient_id"] = ""
                separated_rows.append(sub_event_row)

        return separated_rows
