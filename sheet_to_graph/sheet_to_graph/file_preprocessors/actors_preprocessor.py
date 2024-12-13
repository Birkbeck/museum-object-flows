from sheet_to_graph import FilePreprocessor


class ActorsPreprocessor(FilePreprocessor):
    """Performs basic preprocessing:
    - merges actors table and mapping museums data
    - adds empty museum attribute columns to non-museum actors
    - validation is avoided at this stage (e.g. duplicate museums are not prevented)
    - but invalid mm_ids cause an exception
    """

    def __init__(self, museums: list):
        self.museums = museums

    def preprocess(
        self, rows: list, header_row: int = 0, header_mapping: dict = None
    ) -> list:
        preprocessed_actor_rows = super().preprocess(
            rows, header_row=header_row, header_mapping=header_mapping
        )
        for row in preprocessed_actor_rows:
            row["size"] = ""
            row["governance"] = ""
            row["accreditation"] = ""
            row["subject_matter"] = ""
            row["region"] = ""
            row["country"] = ""

        preprocessed_museum_rows = [
            {
                "actor_id": "museum" + museum["museum_id"],
                "actor_name": museum["name_of_museum"],
                "actor_type": "museum",
                "actor_sector": self._governance_to_sector(museum["governance"]),
                "mm_id": museum["museum_id"],
                "actor_address1": museum["address_line_1"],
                "actor_address2": museum["address_line_2"],
                "actor_address3": museum["address_line_3"],
                "actor_town_city": museum["village_town_or_city"],
                "actor_postcode": museum["postcode"],
                "actor_county": museum["english_county"],
                "actor_country": museum["nation"],
                "size": museum["size"],
                "governance": museum["governance"],
                "accreditation": museum["accreditation"],
                "subject_matter": museum["subject_matter"],
                "region": museum["region"],
                "country": museum["nation"],
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
        if gov in ("National", "Local_Authority", "Other_Government"):
            return "public"
        if gov == "Private":
            return "private"
        if "Independent" in gov:
            return "third"
        if gov == "University":
            return "university"
        return "unknown"
