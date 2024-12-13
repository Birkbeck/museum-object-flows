from collections import defaultdict, Counter
import csv
import neo4j

from .queries import Queries


class Neo4jConnection:
    """Manages a connection with a neo4j database."""

    def __init__(self, credentials: dict):
        self.uri = credentials["uri"]
        self.user = credentials["user"]
        self.password = credentials["password"]
        self.driver = None

    def open(self):
        self.driver = neo4j.GraphDatabase.driver(
            self.uri, auth=(self.user, self.password)
        )

    def close(self):
        self.driver.close()

    def delete_everything(self):
        self.run_query(Queries.delete_everything)

    def run_query(self, query: str, arguments: dict = None):
        def _run_query(query, arguments):
            arguments = {} if arguments is None else arguments
            with self.driver.session() as session:
                return [record.data() for record in session.run(query, **arguments)]

        try:
            return _run_query(query, arguments)
        except (AttributeError, neo4j.exceptions.DriverError):
            self.open()
            return _run_query(query, arguments)

    def get_event_type_paths(self):
        records = self.run_query(Queries.get_all_event_paths_as_event_types)

        sequence_counts = Counter(
            [">>".join(record["nodeNames"]) for record in records]
        )

        steps_table = []
        collection = 0
        for record in records:
            collection += 1
            stage_in_path = 0
            for node in record["nodeNames"]:
                stage_in_path += 1
                steps_table.append(
                    {
                        "stage_in_path": stage_in_path,
                        "event_type": node,
                        "freq": 1,
                        "collection": collection,
                    }
                )
        with open("../data/query_results/event_type_steps.csv", "w") as f:
            writer = csv.DictWriter(
                f, fieldnames=["stage_in_path", "event_type", "freq", "collection"]
            )
            writer.writeheader()
            writer.writerows(steps_table)

    def get_type_instance_counts(self, type_name: str):
        records = self.run_query(
            Queries.get_type_instance_counts, {"type_type": type_name}
        )
        with open(f"../data/query_results/{type_name}_instance_counts.csv", "w") as f:
            writer = csv.DictWriter(
                f,
                fieldnames=[
                    "type_name",
                    "instance_count",
                ],
            )
            writer.writeheader()
            writer.writerows(records)

    def get_all_transfer_event_details(self):
        records = self.run_query(Queries.get_all_transfer_event_details)
        for record in records:
            if record["sender_governance"] is not None:
                record["sender_type"] = self._governance_to_type_name(
                    record["sender_governance"]
                )
            if record["recipient_governance"] is not None:
                record["recipient_type"] = self._governance_to_type_name(
                    record["recipient_governance"]
                )
            record["from"] = record["sender_type"] + str(record["stage_in_path"])
            record["to"] = record["recipient_type"] + str(record["stage_in_path"] + 1)
        with open("../data/query_results/events.csv", "w") as f:
            writer = csv.DictWriter(
                f,
                fieldnames=[
                    "from",
                    "to",
                    "event_type",
                    "stage_in_path",
                    "sender_id",
                    "sender_governance",
                    "sender_type",
                    "recipient_id",
                    "recipient_governance",
                    "recipient_type",
                    "collection_size",
                ],
            )
            writer.writeheader()
            writer.writerows(records)

    def get_actor_and_event_type_paths(
        self, start_governance: str = None, start_subject: str = None
    ):
        if start_governance is not None and start_subject is not None:
            raise NotImplementedError
        if start_governance is not None:
            raw_records = self.run_query(
                Queries.get_all_event_paths_as_actor_and_event_types_from_start_gov,
                {"start_governance": start_governance},
            )
            output_file_name_base = (
                f"../data/query_results/"
                + f"actor_and_event_type_sequences_from_{start_governance}_"
            )
        elif start_subject is not None:
            raw_records = self.run_query(
                Queries.get_all_event_paths_as_actor_and_event_types_from_start_subj,
                {"start_subject": start_subject},
            )
            output_file_name_base = (
                f"../data/query_results/"
                + f"actor_and_event_type_sequences_from_{start_subject}_"
            )
        else:
            raw_records = self.run_query(
                Queries.get_all_event_paths_as_actor_and_event_types
            )
            output_file_name_base = (
                "../data/query_results/actor_and_event_type_sequences_"
            )
        records = []
        for record in raw_records:
            new_record = []
            for index, a in enumerate(record["actors_and_events"]):
                if index == 0:
                    # record describes super event museum
                    actor_type = self._governance_to_type_name(a[1])
                    new_record.append(("actor", actor_type))
                else:
                    # record describes event and recipient
                    event_type = a[0]
                    new_record.append(("event", event_type))
                    actor_type = (
                        self._governance_to_type_name(a[2])
                        if a[2] is not None
                        else a[3]
                    )
                    new_record.append(("actor", actor_type))
            records.append(new_record)

        sequence_counts = Counter(
            [
                ">>".join([a[1] if a[1] is not None else "unknown" for a in record])
                for record in records
            ]
        )
        sequence_counts_table = []
        for sequence, count in sequence_counts.items():
            sequence_counts_table.append(
                {
                    "sequence": sequence,
                    "freq": count,
                }
            )
        with open(f"{output_file_name_base}counts.csv", "w") as f:
            writer = csv.DictWriter(f, fieldnames=["sequence", "freq"])
            writer.writeheader()
            writer.writerows(sequence_counts_table)

        steps_table = []
        collection = 0
        for record in records:
            collection += 1
            stage_in_path = 0
            for node in record:
                stage_in_path += 1
                steps_table.append(
                    {
                        "stage_in_path": stage_in_path,
                        "actor_or_event": node[0],
                        "type": node[1] if node[1] is not None else "unknown",
                        "freq": 1,
                        "collection": collection,
                    }
                )
        with open(f"{output_file_name_base}steps.csv", "w") as f:
            writer = csv.DictWriter(
                f,
                fieldnames=[
                    "stage_in_path",
                    "actor_or_event",
                    "type",
                    "freq",
                    "collection",
                ],
            )
            writer.writeheader()
            writer.writerows(steps_table)

    def get_actor_type_paths(
        self, start_governance: str = None, start_subject: str = None
    ):
        if start_governance is not None:
            output_file_name_base = (
                f"../data/query_results/actor_type_sequences_from_{start_governance}_"
            )
        elif start_subject is not None:
            output_file_name_base = (
                f"../data/query_results/actor_type_sequences_from_{start_subject}_"
            )
        else:
            output_file_name_base = "../data/query_results/actor_type_sequences_"

        actor_sequences = self._query_actor_type_paths(
            start_governance=start_governance, start_subject=start_subject
        )

        self._actor_sequences_to_counts_table(actor_sequences, output_file_name_base)
        self._actor_sequences_to_steps_table(actor_sequences, output_file_name_base)
        self._actor_sequences_to_paths_table(actor_sequences, output_file_name_base)

    def get_actor_type_paths_ending_in(
        self, end_type, start_governance: str = None, start_subject: str = None
    ):
        if start_governance is not None:
            output_file_name_base = (
                "../data/query_results/actor_type_sequences_from_"
                + f"{start_governance}_to_{end_type}_"
            )
        elif start_subject is not None:
            output_file_name_base = (
                "../data/query_results/actor_type_sequences_from_"
                + f"{start_subject}_to_{end_type}_"
            )
        else:
            output_file_name_base = (
                f"../data/query_results/actor_type_sequences_to_{end_type}_"
            )

        actor_sequences = [
            sequence
            for sequence in self._query_actor_type_paths(
                start_governance=start_governance, start_subject=start_subject
            )
            if sequence[-1]["type"] == end_type
        ]

        self._actor_sequences_to_counts_table(actor_sequences, output_file_name_base)
        self._actor_sequences_to_reverse_paths_table(
            actor_sequences, output_file_name_base
        )

    def _query_actor_type_paths(
        self, start_governance: str = None, start_subject: str = None
    ):
        if start_governance is not None and start_subject is not None:
            raise NotImplementedError
        if start_governance is not None:
            raw_actor_sequences = self.run_query(
                Queries.get_all_event_paths_as_actor_types_from_start_gov,
                {"start_governance": start_governance},
            )
        elif start_subject is not None:
            raw_actor_sequences = self.run_query(
                Queries.get_all_event_paths_as_actor_types_from_start_subj,
                {"start_subject": start_subject},
            )
        else:
            raw_actor_sequences = self.run_query(
                Queries.get_all_event_paths_as_actor_types
            )
        actor_sequences = []
        for raw_actor_sequence in raw_actor_sequences:
            actor_sequence = []
            for index, a in enumerate(raw_actor_sequence["actors"]):
                if (
                    index > 0
                    and raw_actor_sequence["actors"][index - 1]["actor_id"]
                    == a["actor_id"]
                ):
                    # actor is the same as previous element in sequence
                    continue
                if a["actor_id"] is None:
                    # event has no recipient
                    continue
                actor_sequence.append(
                    {
                        "actor_id": a["actor_id"],
                        "sector": a["sector"],
                        "actor_size": a["actor_size"],
                        # if actor is museum use governance type as actor type
                        "type": self._governance_to_type_name(a["governance"])
                        if a["governance"] is not None
                        else a["type"]
                        if a["type"] is not None
                        else "unknown",
                        # if actor is museum use governance type as actor type
                        "basic_level_type": self._governance_to_type_name(
                            a["governance"]
                        )
                        if a["governance"] is not None
                        else a["basic_level_type"]
                        if a["type"] is not None
                        else "unknown",
                        "collection_id": a["collection_id"],
                        "collection_size": a["collection_size"],
                    }
                )
            # when retracing sub-collection steps, do not duplicate the size of their parent collection
            for a in actor_sequence:
                a["collection_size"] = actor_sequence[-1]["collection_size"]
            actor_sequences.append(actor_sequence)
        return actor_sequences

    def _actor_sequences_to_counts_table(self, actor_sequences, output_file_name_base):
        sequence_counts = Counter(
            [
                ">>".join([a["type"] for a in actor_sequence])
                for actor_sequence in actor_sequences
            ]
        )
        sequence_counts_table = []
        for sequence, count in sequence_counts.items():
            sequence_counts_table.append(
                {
                    "sequence": sequence,
                    "freq": count,
                }
            )
        with open(f"{output_file_name_base}counts.csv", "w") as f:
            writer = csv.DictWriter(f, fieldnames=["sequence", "freq"])
            writer.writeheader()
            writer.writerows(sequence_counts_table)

    def _actor_sequences_to_steps_table(self, actor_sequences, output_file_name_base):
        steps_table = []
        first_last_steps_table = []
        for collection_number, actor_sequence in enumerate(actor_sequences):
            for stage_in_path, actor in enumerate(actor_sequence):
                if stage_in_path == 0 or stage_in_path == len(actor_sequence):
                    first_last_steps_table.append(
                        {
                            "stage_in_path": 1 if stage_in_path == 0 else 2,
                            "actor_type": actor["type"],
                            "actor_basic_type": actor["basic_level_type"],
                            "actor_sector": actor["sector"],
                            "actor_size": actor["actor_size"],
                            "freq": 1,
                            "collection_size": actor["collection_size"],
                            "collection": collection_number,
                        }
                    )
                steps_table.append(
                    {
                        "stage_in_path": stage_in_path + 1,
                        "actor_type": actor["type"],
                        "actor_basic_type": actor["basic_level_type"],
                        "actor_sector": actor["sector"],
                        "actor_size": actor["actor_size"],
                        "freq": 1,
                        "collection_size": actor["collection_size"],
                        "collection": collection_number,
                    }
                )
        with open(f"{output_file_name_base}steps.csv", "w") as f:
            writer = csv.DictWriter(
                f,
                fieldnames=[
                    "stage_in_path",
                    "actor_type",
                    "actor_basic_type",
                    "actor_sector",
                    "actor_size",
                    "freq",
                    "collection_size",
                    "collection",
                ],
            )
            writer.writeheader()
            writer.writerows(steps_table)
        with open(f"{output_file_name_base}first_last_steps.csv", "w") as f:
            writer = csv.DictWriter(
                f,
                fieldnames=[
                    "stage_in_path",
                    "actor_type",
                    "actor_basic_type",
                    "actor_sector",
                    "actor_size",
                    "freq",
                    "collection_size",
                    "collection",
                ],
            )
            writer.writeheader()
            writer.writerows(first_last_steps_table)

    def _actor_sequences_to_paths_table(self, actor_sequences, output_file_name_base):
        paths_table = []
        for actor_sequence in actor_sequences:
            for index, actor in enumerate(actor_sequence):
                if index + 1 == len(actor_sequence):
                    continue
                stage_in_path = {
                    "from": ">>".join(
                        [
                            a["type"] + "@" + a["sector"]
                            for a in actor_sequence[: index + 1]
                        ]
                    ),
                    "from_label": actor["type"],
                    "from_sector": actor["sector"],
                    "to": ">>".join(
                        [
                            a["type"] + "@" + a["sector"]
                            for a in actor_sequence[: index + 2]
                        ]
                    ),
                    "to_label": actor_sequence[index + 1]["type"],
                    "to_sector": actor_sequence[index + 1]["sector"],
                }
                paths_table.append(stage_in_path)
        with open(f"{output_file_name_base}tree.csv", "w") as f:
            writer = csv.DictWriter(
                f,
                fieldnames=[
                    "from",
                    "from_label",
                    "from_sector",
                    "to",
                    "to_label",
                    "to_sector",
                ],
            )
            writer.writeheader()
            writer.writerows(paths_table)

    def _actor_sequences_to_reverse_paths_table(
        self, actor_sequences, output_file_name_base
    ):
        reverse_paths_table = []
        for actor_sequence in actor_sequences:
            reversed_sequence = list(reversed(actor_sequence))
            for index, actor in enumerate(reversed_sequence):
                if index + 1 == len(reversed_sequence):
                    continue
                stage_in_path = {
                    "from": ">>".join(
                        [
                            a["type"] + "@" + a["sector"]
                            for a in reversed(reversed_sequence[: index + 1])
                        ]
                    ),
                    "from_label": actor["type"],
                    "from_sector": actor["sector"],
                    "to": ">>".join(
                        [
                            a["type"] + "@" + a["sector"]
                            for a in reversed(reversed_sequence[: index + 2])
                        ]
                    ),
                    "to_label": reversed_sequence[index + 1]["type"],
                    "to_sector": reversed_sequence[index + 1]["sector"],
                }
                reverse_paths_table.append(stage_in_path)
        with open(f"{output_file_name_base}tree_reversed.csv", "w") as f:
            writer = csv.DictWriter(
                f,
                fieldnames=[
                    "from",
                    "from_label",
                    "from_sector",
                    "to",
                    "to_label",
                    "to_sector",
                ],
            )
            writer.writeheader()
            writer.writerows(reverse_paths_table)

    def _governance_to_type_name(self, governance: str):
        governance = governance.lower()
        return f"museum-{governance}"

    def _infer_collection_sizes(self, session):
        session.run(Queries.infer_collection_sizes)
