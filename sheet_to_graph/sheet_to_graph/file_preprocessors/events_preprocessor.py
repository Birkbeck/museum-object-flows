from sheet_to_graph import FilePreprocessor


class EventsPreprocessor(FilePreprocessor):
    """Performs basic preprocessing and
    splits events with multiple event types into multiple rows."""

    def __init__(self, default_recipient_types, actors, places, event_types):
        self.default_recipient_types = default_recipient_types
        self.actors = actors
        self.places = places
        self.event_types = event_types

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

        # infer unspecified recipients for events with a default recipient type
        new_actors = []
        for index, row in enumerate(separated_rows):
            event_type_name = row["event_type"].split("?")[0].strip()
            if event_type_name == "":
                continue
            try:
                event_type = self.event_types.filter(type_name=event_type_name)[0]
            except IndexError as e:
                raise Exception(f"{event_type_name} not in event types sheet")
            if row["actor_recipient_id"] == "" and any(
                [
                    event_type["change_of_ownership"],
                    event_type["change_of_custody"],
                    event_type["end_of_existence"],
                ]
            ):
                try:
                    actor_type = self.default_recipient_types.filter(
                        event_type=event_type_name
                    )[0]["default_recipient_type"]
                except IndexError:
                    actor_type = "unknown"
                actor_sector = "unknown"
                actor_id = f"unknown_{actor_type}_{index}"
                actor_name = f"unknown {actor_type} {index}"
                new_actors.append(
                    {
                        "actor_id": actor_id,
                        "actor_name": actor_name,
                        "actor_type": actor_type,
                        "actor_sector": actor_sector,
                        "actor_quantity": "1",
                        "mm_id": "",
                        "actor_address1": "",
                        "actor_address2": "",
                        "actor_town_city": "",
                        "actor_county": "",
                        "actor_postcode": "",
                        "actor_country": "",
                    }
                )
                row["actor_recipient_id"] = actor_id
        self.actors.import_from_list_of_dicts(new_actors)

        return separated_rows
