"""
This file defines formulae used in the upload script to infer new columns
"""

from .enumerated_types import (
    collection_sizes,
    collection_max_sizes,
    collection_min_sizes,
    museum_sizes,
    museum_max_sizes,
    museum_min_sizes,
    uk_constituents,
)


def get_type_id(table, row_index, id_prefix, type_name_column="type_name"):
    type_name = table[row_index][type_name_column]
    return f"{id_prefix}-{type_name}"


def get_sub_type_of_id(table, row_index, id_prefix):
    type_name = table[row_index]["sub_type_of"]
    return f"{id_prefix}-{type_name}"


def get_super_cause_types(table, row_index, super_causes_hierarchy):
    def _tidy_individual_cause(individual_cause) -> str:
        # find cause description in hierarchy
        if individual_cause in super_causes_hierarchy.columns["cause"]:
            row = super_causes_hierarchy.filter(cause=individual_cause)[0]
            return (
                row["cause_super_type"]
                + " - "
                + row["cause_type"]
                + " - "
                + row["cause"]
            )
            return individual_cause
        if individual_cause in super_causes_hierarchy.columns["cause_type"]:
            row = super_causes_hierarchy.filter(cause_type=individual_cause)[0]
            return row["cause_super_type"] + " - " + row["cause_type"]
        if individual_cause in super_causes_hierarchy.columns["cause_super_type"]:
            row = super_causes_hierarchy.filter(cause_super_type=individual_cause)[0]
            return row["cause_super_type"]
        # find how cause description maps onto hierarchy
        row = super_causes_hierarchy.filter(super_cause_text=individual_cause)[0]
        if row["cause"] != "":
            return (
                row["cause_super_type"]
                + " - "
                + row["cause_type"]
                + " - "
                + row["cause"]
            )
        if row["cause_type"] != "":
            return row["cause_super_type"] + " - " + row["cause_type"]
        if row["cause_super_type"] != "":
            return row["cause_super_type"]

    super_causes = table[row_index]["super_causes"]
    if super_causes is None:
        return None
    super_causes_list = super_causes.split(";")
    tidy_causes = []
    for cause in super_causes_list:
        cause_name = cause.strip().split("?")[0]
        if cause_name == "":
            continue
        try:
            tidy_cause_name = _tidy_individual_cause(cause_name)
            tidy_causes.append(tidy_cause_name)
        except Exception:
            print(row_index, cause_name)
    return "; ".join(tidy_causes)


def get_place_id(table, row_index):
    return f"place{row_index}"


def get_longitude(table, row_index, postcode_to_lat_long):
    postcode = table[row_index]["postcode"]
    return postcode_to_lat_long.get_longitude(postcode)


def get_latitude(table, row_index, postcode_to_lat_long):
    postcode = table[row_index]["postcode"]
    return postcode_to_lat_long.get_latitude(postcode)


def get_bng_x(table, row_index, postcode_to_lat_long):
    postcode = table[row_index]["postcode"]
    return postcode_to_lat_long.get_bng_x(postcode)


def get_bng_y(table, row_index, postcode_to_lat_long):
    postcode = table[row_index]["postcode"]
    return postcode_to_lat_long.get_bng_y(postcode)


def get_region(table, row_index, postcode_to_lat_long):
    postcode = table[row_index]["postcode"]
    return postcode_to_lat_long.get_region(postcode)


def get_local_authority_code(table, row_index, postcode_to_lat_long):
    postcode = table[row_index]["postcode"]
    return postcode_to_lat_long.get_local_authority_code(postcode)


def get_local_authority_name(table, row_index, postcode_to_lat_long):
    postcode = table[row_index]["postcode"]
    return postcode_to_lat_long.get_local_authority_name(postcode)


def get_subject_matter_broad(table, row_index):
    """If subject matter is e.g. "Arts-fine_arts", returns "Arts"."""
    actor = table[row_index]
    if actor["subject_matter"] is None:
        return None
    try:
        return actor["subject_matter"].split("-")[0]
    except IndexError:
        return actor["subject_matter"]


def get_governance_broad(table, row_index):
    """If governance is e.g. "Independent-Not_for_profit", returns "Not_for_profit"."""
    actor = table[row_index]
    if actor["governance"] is None:
        return None
    try:
        return actor["governance"].split("-")[0]
    except IndexError:
        return actor["governance"]


def get_actor_location(table, row_index, places):
    actor = table[row_index]
    return places.filter(
        address_1=actor["actor_address1"],
        address_2=actor["actor_address2"],
        address_3=actor["actor_address3"],
        village_town_city=actor["actor_town_city"],
        county=actor["actor_county"],
        postcode=actor["actor_postcode"],
        country=actor["actor_country"],
    )[0]["place_id"]


def is_uk_based(table, row_index):
    row = table[row_index]
    return row["actor_country"].lower() in uk_constituents


def get_actor_size_number(table, row_index):
    row = table[row_index]
    if row["size"] == "":
        return None
    return museum_sizes[row["size"]]


def get_actor_size_number_max(table, row_index):
    row = table[row_index]
    if row["size"] == "":
        return None
    return museum_max_sizes[row["size"]]


def get_actor_size_number_min(table, row_index):
    row = table[row_index]
    if row["size"] == "":
        return None
    return museum_min_sizes[row["size"]]


def get_collection_or_object_id(table, row_index):
    row = table[row_index]
    if row["collection_id"] == "":
        return None
    return row["super_event_id"] + row["collection_id"]


def get_collection_was_removed_from(table, row_index):
    row = table[row_index]
    if row["coll_subset_of"] == "":
        return None
    return row["super_event_id"] + row["coll_subset_of"]


def get_collection_size_number(table, row_index):
    row = table[row_index]
    if row["coll_size_name"] == "":
        return None
    return collection_sizes[row["coll_size_name"]]


def get_collection_size_number_max(table, row_index):
    row = table[row_index]
    if row["coll_size_name"] == "":
        return None
    return collection_max_sizes[row["coll_size_name"]]


def get_collection_size_number_min(table, row_index):
    row = table[row_index]
    if row["coll_size_name"] == "":
        return None
    return collection_min_sizes[row["coll_size_name"]]


def get_collection_status(table, row_index):
    row = table[row_index]
    was_removed_from = row["was_removed_from"]
    if was_removed_from is not None:
        parent_collection = table.filter(collection_or_object_id=was_removed_from)[0]
        return parent_collection["collection_status"]
    if row["coll_status"] == "":
        return "collection"
    return row["coll_status"]


def get_super_event_name(table, row_index):
    row = table[row_index]
    super_event_type = row["super_event_type"]
    super_event_date = row["super_date"]
    return super_event_type + super_event_date


def get_concerned_actor(table, row_index, actors):
    museum_id = table[row_index]["museum_id"]
    return actors.filter(mm_id=museum_id)[0]["actor_id"]


def get_involves(table, row_index):
    row = table[row_index]
    if row["event_type"] == "":
        return None
    return row["super_event_id"] + row["collection_id"]


def get_event_id(table, row_index):
    row = table[row_index]
    if row["event_type"] == "":
        return None
    return f"event{row_index}"


def get_event_name(table, row_index):
    row = table[row_index]
    event_type = row["event_type"]
    event_date = (
        row["event_date"]
        if row["event_date_from"] == ""
        else row["event_date_from"] + "-" + row["event_date_to"]
    )
    return event_type + event_date


def get_stage_in_path(table, row_index):
    row = table[row_index]
    previous_event_id = row["previous_event_id"]
    if previous_event_id is None:
        return 0
    previous_event = [
        r for r in table[:row_index] if r["event_id"] == row["previous_event_id"]
    ][0]
    return previous_event["stage_in_path"] + 1


def get_previous_event_id(table, row_index):
    # the event_id of the last row referring to the same collection or super collection
    row = table[row_index]
    super_event_id = row["super_event_id"]
    if row["collection_id"] == "":
        return None
    try:
        return [
            r
            for r in table.rows[:row_index]
            if r["super_event_id"] == super_event_id
            and r["collection_id"] != ""
            and (
                r["collection_id"] == row["collection_id"]
                or r["collection_id"] == row["coll_subset_of"]
            )
        ][-1]["event_id"]
    except IndexError:
        return None


def determine_if_collection_or_object(table, row_index):
    row = table[row_index]
    # if row does not have a collection/object, return nothing
    if row["collection_id"] == "":
        return None
    # if collection/object details are stored above, copy those details
    if row["previous_event_id"] is not None:
        previous_event = table.filter(event_id=row["previous_event_id"])[0]
        if row["collection_id"] == previous_event["collection_id"]:
            return previous_event["collection_or_object"]
    # if this is first mention of the collection/object, check quantity
    if row["object_qty"] == "1":
        return "Object"
    return "Collection"


def get_recipient_id(table, row_index, actors, event_types):
    event = table[row_index]
    if event["event_type_name"] == "":
        return None
    event_type = event_types.filter(type_name=event["event_type_name"])[0]
    if not any(
        [
            event_type["change_of_ownership"],
            event_type["change_of_custody"],
            event_type["end_of_existence"],
        ]
    ):
        return None
    return event["actor_recipient_id"]


def get_sender_id(table, row_index, actors, event_types):
    def row_is_transfer_event(row):
        event_type = event_types.filter(type_name=row["event_type_name"])[0]
        return (
            event_type["change_of_ownership"]
            or event_type["change_of_custody"]
            or event_type["end_of_existence"]
        )

    event = table[row_index]
    # if event has no type, then it has no sender
    if event["event_type_name"] == "":
        return None
    if event["previous_event_id"] is None:
        # this is the first event involving this collection
        return actors.filter(mm_id=event["museum_id"])[0]["actor_id"]
    try:
        return [
            r
            for r in table.rows[:row_index]
            if r["collection_id"] != ""
            and (
                r["collection_id"] == event["collection_id"]
                or r["collection_id"] == event["coll_subset_of"]
            )
            and row_is_transfer_event(r)
            and r["actor_recipient_id"] != ""
        ][-1]["actor_recipient_id"]
    # no previous event had a recipient, the sender is the museum
    except IndexError:
        return actors.filter(mm_id=event["museum_id"])[0]["actor_id"]


def get_event_destination(table, row_index, places, actors, event_types):
    event = table[row_index]
    # if event has no recipient and no specified location, then no destination
    if all(
        [
            event["actor_recipient_id"] == "",
            event["street"] == "",
            event["town"] == "",
            event["county"] == "",
            event["postcode"] == "",
        ]
    ):
        return None
    # if location=stays, then no destination (even if there is a recipient)
    if event["location"] == "stays":
        return None
    # if event's type is not a change of physical custody, then there is no destination
    event_type = event_types.filter(type_name=event["event_type_name"])[0]
    if not event_type["change_of_custody"]:
        return None
    # if event has a specified location, destination is that location
    if not all(
        [
            event["street"] == "",
            event["town"] == "",
            event["county"] == "",
            event["postcode"] == "",
        ]
    ):
        return places.filter(
            address_1=event["street"],
            village_town_city=event["town"],
            county=event["county"],
            postcode=event["postcode"],
        )[0]["place_id"]
    # if event has a recipient and no specified location, destination is recipient location
    return actors.filter(actor_id=event["actor_recipient_id"])[0]["has_location"]


def get_event_origin(table, row_index, places, actors):
    event = table[row_index]
    # if event has no destination, then it also has no origin
    if event["has_destination"] is None:
        return None
    # if it is the first event in the chain, then the origin is the museum's location
    if event["previous_event_id"] is None:
        return actors.filter(mm_id=event["museum_id"])[0]["has_location"]
    # the origin is the last destination in the chain
    try:
        return [
            r
            for r in table.rows[:row_index]
            if r["collection_id"] != ""
            and (
                r["collection_id"] == event["collection_id"]
                or r["collection_id"] == event["coll_subset_of"]
            )
            and r["has_destination"] is not None
        ][-1]["has_destination"]
    # no previous event had a destination, the origin is the museum's location
    except IndexError:
        return actors.filter(mm_id=event["museum_id"])[0]["has_location"]
