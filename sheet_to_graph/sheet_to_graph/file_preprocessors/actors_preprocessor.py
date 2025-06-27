from sheet_to_graph import FilePreprocessor


class ActorsPreprocessor(FilePreprocessor):
    """Performs basic preprocessing:
    - merges actors table and mapping museums data
    - adds empty museum attribute columns to non-museum actors
    - validation is avoided at this stage (e.g. duplicate museums are not prevented)
    - but invalid mm_ids cause an exception
    """

    def __init__(self, museums: list, events: list):
        self.museums = museums
        self.events = events

    def preprocess(
        self, rows: list, header_row: int = 0, header_mapping: dict = None
    ) -> list:
        preprocessed_actor_rows = super().preprocess(
            rows, header_row=header_row, header_mapping=header_mapping
        )
        for row in preprocessed_actor_rows:
            row["actor_quantity"] = "1"
            try:
                row["actor_quantity"] = str(
                    [e for e in self.events if e[26] == row["actor_id"]][0][25]
                )
            except IndexError:
                pass
            if row["actor_quantity"].strip() == "":
                row["actor_quantity"] = "1"
            row["size"] = ""
            row["governance"] = ""
            row["accreditation"] = ""
            row["subject_matter"] = ""
            row["region"] = ""
            row["country"] = ""

        preprocessed_museum_rows = [
            {
                "actor_id": "museum" + museum["museum_id"],
                "actor_name": museum["museum_name"],
                "actor_type": "museum",
                "actor_sector": self._governance_to_sector(museum["governance_broad"]),
                "actor_quantity": "1",
                "mm_id": museum["museum_id"],
                "actor_address1": museum["address_1"],
                "actor_address2": museum["address_2"],
                "actor_address3": museum["address_3"],
                "actor_town_city": museum["village_town_city"],
                "actor_postcode": museum["postcode"],
                "actor_county": museum["english_county"],
                "actor_country": museum["country"],
                "size": museum["size"],
                "governance": museum["governance"],
                "governance_broad": museum["governance_broad"],
                "accreditation": museum["accreditation"],
                "subject": museum["subject"],
                "subject_broad": museum["subject_broad"],
                "region": museum["region"],
                "country": museum["country"],
                "year_opened_1": museum["year_opened_1"],
                "year_opened_2": museum["year_opened_2"],
                "year_closed_1": museum["year_closed_1"],
                "year_closed_2": museum["year_closed_2"],
            }
            for museum in super().preprocess(self.museums, header_row=0)
        ]
        added_museums = set()

        museums_and_actors = []

        individuals_map = {}
        for actor_index, original_actor_row in enumerate(preprocessed_actor_rows):
            if original_actor_row["actor_type"] == "individual":
                actor_name = original_actor_row["actor_name"]
                try:
                    original_actor_row["actor_name"] = individuals_map[actor_name]
                except KeyError:
                    individuals_map[actor_name] = "Person" + str(len(individuals_map))
                    original_actor_row["actor_name"] = individuals_map[actor_name]
            if original_actor_row["mm_id"] != "":
                mm_id_matched = False
                for museum in preprocessed_museum_rows:
                    if museum["mm_id"] == original_actor_row["mm_id"]:
                        # update museum's actor_id with user-specified value
                        new_row = museum.copy()
                        new_row["actor_id"] = original_actor_row["actor_id"]
                        preprocessed_actor_rows[actor_index] = new_row
                        added_museums.add(museum["mm_id"])
                        mm_id_matched = True
                if not mm_id_matched:
                    raise Exception(
                        "There is no museum with id: ", original_actor_row["mm_id"]
                    )
            museums_and_actors.append(preprocessed_actor_rows[actor_index])

        for museum in preprocessed_museum_rows:
            if museum["mm_id"] in added_museums:
                continue
            museums_and_actors.append(museum)

        return museums_and_actors

    def _governance_to_sector(self, gov):
        if gov in ("national", "local authority", "other government"):
            return "public"
        if gov == "private":
            return "private"
        if "independent" in gov:
            return "third"
        if gov == "university":
            return "university"
        return "unknown"
