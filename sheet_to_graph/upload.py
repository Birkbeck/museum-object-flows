"""
This script uploads data from spreadsheets into a neo4j database.
The script does the following:
- it specifies a schema for a set of spreadsheets (or csv files);
- it reads files containing the sheets and validates them;
- it infers extra fields in the tables;
- it uploads the data into a neo4j database.
"""

import json

from sheet_to_graph import Column, FileLoader, PostcodeToLatLong, Table
from sheet_to_graph.columns import (
    BooleanColumn,
    ExtendedDateTimeColumn,
    EnumColumn,
    FormulaColumn,
    ListColumn,
    OptionalColumn,
    ReferenceColumn,
    SplitColumn,
)
from sheet_to_graph.connection_managers import TablesToGraph
import sheet_to_graph.enumerated_types as enums
from sheet_to_graph.file_preprocessors import (
    ActorsPreprocessor,
    CollectionsPreprocessor,
    EventsPreprocessor,
    EventPlacesPreprocessor,
    SuperEventsPreprocessor,
)
import sheet_to_graph.formulae as formulae
from sheet_to_graph.rules import (
    FillCellsWithValueWhen,
    RequiredColumns,
    MutuallyExclusiveColumns,
    MutuallyRequiredColumns,
    UniqueCorrespondences,
)

if __name__ == "__main__":
    with open("config.json") as f:
        config = json.load(f)
        credentials_file_name = config["credentials_file"]
        file_loader = FileLoader(config)

    postcode_to_lat_long = PostcodeToLatLong(
        "postcode_directory.json",
        "../data/ONSPD_FEB_2024_UK",
    )

    print("Defining Tables")
    actor_types = Table(
        "Actor Types",
        columns=[
            Column("type_name", property_of="type_id"),
            ReferenceColumn(
                "sub_type_of",
                "type_name",
                ignore=True,
            ),
            BooleanColumn("is_core_category", property_of="type_id"),
            FormulaColumn(
                "type_id",
                formula=lambda table, row_index: formulae.get_type_id(
                    table, row_index, "actor"
                ),
                unique=True,
                primary_key=True,
                type_label="Type",
            ),
            FormulaColumn(
                "sub_type_of_id",
                formula=lambda table, row_index: formulae.get_sub_type_of_id(
                    table, row_index, "actor"
                ),
                reference_column="type_id",
                relation_from="type_id",
                type_label="SUB_TYPE_OF",
            ),
        ],
    )

    event_types = Table(
        "Event Types",
        columns=[
            Column("type_name", property_of="type_id"),
            ReferenceColumn("sub_type_of", "type_name", ignore=True),
            BooleanColumn("is_core_category", property_of="type_id"),
            BooleanColumn("change_of_ownership", property_of="type_id"),
            BooleanColumn("change_of_custody", property_of="type_id"),
            BooleanColumn("end_of_existence", property_of="type_id"),
            Column("definition", property_of="type_id"),
            FormulaColumn(
                "type_id",
                formula=lambda table, row_index: formulae.get_type_id(
                    table, row_index, "event"
                ),
                unique=True,
                primary_key=True,
                type_label="Type",
            ),
            FormulaColumn(
                "sub_type_of_id",
                formula=lambda table, row_index: formulae.get_sub_type_of_id(
                    table, row_index, "event"
                ),
                reference_column="type_id",
                relation_from="type_id",
                type_label="SUB_TYPE_OF",
            ),
        ],
    )

    super_event_types = Table(
        "Super-Event Types",
        columns=[
            Column("type_name", unique=True, primary_key=True, type_label="Type"),
            ReferenceColumn(
                "sub_type_of",
                "type_name",
                relation_from="type_name",
                type_label="SUB_TYPE_OF",
            ),
        ],
    )

    default_recipient_types = Table(
        "Default Recipient Types",
        columns=[
            ReferenceColumn("event_type", "type_name", reference_table=event_types),
            ReferenceColumn(
                "default_recipient_type", "type_name", reference_table=actor_types
            ),
        ],
    )

    super_causes_hierarchy = Table(
        "Super Causes Hierarchy",
        columns=[
            Column("super_cause_text"),
            Column("cause"),
            Column("cause_type"),
            Column("cause_super_type"),
        ],
    )

    places = Table(
        "Places",
        columns=[
            Column("address_1", property_of="place_id"),
            OptionalColumn("address_2", property_of="place_id"),
            OptionalColumn("address_3", property_of="place_id"),
            Column("village_town_city", property_of="place_id"),
            Column("county", property_of="place_id"),
            OptionalColumn("country", property_of="place_id"),
            Column("postcode", property_of="place_id"),
            FormulaColumn(
                "longitude",
                formula=lambda table, row_index: formulae.get_longitude(
                    table, row_index, postcode_to_lat_long
                ),
                property_of="place_id",
            ),
            FormulaColumn(
                "latitude",
                formula=lambda table, row_index: formulae.get_latitude(
                    table, row_index, postcode_to_lat_long
                ),
                property_of="place_id",
            ),
            FormulaColumn(
                "bng_x",
                formula=lambda table, row_index: formulae.get_bng_x(
                    table, row_index, postcode_to_lat_long
                ),
                property_of="place_id",
            ),
            FormulaColumn(
                "bng_y",
                formula=lambda table, row_index: formulae.get_bng_y(
                    table, row_index, postcode_to_lat_long
                ),
                property_of="place_id",
            ),
            FormulaColumn(
                "region",
                formula=lambda table, row_index: formulae.get_region(
                    table, row_index, postcode_to_lat_long
                ),
                property_of="place_id",
            ),
            FormulaColumn(
                "local_authority_code",
                formula=lambda table, row_index: formulae.get_local_authority_code(
                    table, row_index, postcode_to_lat_long
                ),
                property_of="place_id",
            ),
            FormulaColumn(
                "local_authority_name",
                formula=lambda table, row_index: formulae.get_local_authority_name(
                    table, row_index, postcode_to_lat_long
                ),
                property_of="place_id",
            ),
            FormulaColumn(
                "place_id",
                formula=formulae.get_place_id,
                unique=True,
                primary_key=True,
                type_label="Place",
            ),
        ],
    )

    actors = Table(
        "Actors",
        columns=[
            Column("actor_id", unique=True, primary_key=True, type_label="Actor"),
            Column("actor_name", property_of="actor_id"),
            SplitColumn(
                "actor_type",
                [
                    ReferenceColumn(
                        "actor_type_name",
                        "type_name",
                        reference_table=actor_types,
                        ignore=True,
                    ),
                    EnumColumn(
                        "actor_type_uncertainty",
                        enums.uncertainty_values,
                        property_of="actor_type_name",
                    ),
                ],
                split_before="?",
            ),
            FormulaColumn(
                "actor_type_id",
                formula=lambda table, row_index: formulae.get_type_id(
                    table, row_index, "actor", "actor_type_name"
                ),
                reference_column="type_id",
                reference_table=actor_types,
                relation_from="actor_id",
                type_label="INSTANCE_OF",
            ),
            SplitColumn(
                "actor_sector",
                [
                    EnumColumn(
                        "actor_sector_name",
                        enums.actor_sector_values,
                        property_of="actor_id",
                    ),
                    EnumColumn(
                        "actor_sector_uncertainty",
                        enums.uncertainty_values,
                        property_of="actor_id",
                    ),
                ],
                split_before="?",
            ),
            OptionalColumn("actor_quantity", property_of="actor_id"),
            Column("mm_id", unique=True, optional=True, property_of="actor_id"),
            Column("actor_address1", ignore=True),
            Column("actor_address2", ignore=True),
            OptionalColumn("actor_address3", ignore=True),
            Column("actor_town_city", ignore=True),
            Column("actor_county", ignore=True),
            Column("actor_postcode", ignore=True),
            Column("actor_country", ignore=True),
            OptionalColumn("size", property_of="actor_id"),
            OptionalColumn("governance", property_of="actor_id"),
            OptionalColumn("accreditation", property_of="actor_id"),
            OptionalColumn("subject_matter", property_of="actor_id"),
            OptionalColumn("region", property_of="actor_id"),
            OptionalColumn("country", property_of="actor_id"),
            FormulaColumn(
                "subject_matter_broad",
                formula=formulae.get_subject_matter_broad,
                property_of="actor_id",
            ),
            FormulaColumn(
                "governance_broad",
                formula=formulae.get_governance_broad,
                property_of="actor_id",
            ),
            FormulaColumn(
                "has_location",
                formula=lambda table, row_index: formulae.get_actor_location(
                    table, row_index, places
                ),
                relation_from="actor_id",
                type_label="HAS_LOCATION",
                reference_table=places,
                reference_column="place_id",
            ),
            FormulaColumn(
                "is_uk_based", formula=formulae.is_uk_based, property_of="actor_id"
            ),
            FormulaColumn(
                "size_num",
                formula=formulae.get_actor_size_number,
                property_of="actor_id",
            ),
            FormulaColumn(
                "size_num_max",
                formula=formulae.get_actor_size_number_max,
                property_of="actor_id",
            ),
            FormulaColumn(
                "size_num_min",
                formula=formulae.get_actor_size_number_min,
                property_of="actor_id",
            ),
        ],
    )

    super_events = Table(
        "Super Events",
        columns=[
            ReferenceColumn(
                "museum_id", "mm_id", reference_table=actors, fill=True, ignore=True
            ),
            ReferenceColumn(
                "super_event_type",
                "type_name",
                reference_table=super_event_types,
                fill=True,
                property_of="super_event_id",
            ),
            Column(
                "super_event_id",
                unique=True,
                fill=True,
                primary_key=True,
                type_label="SuperEvent",
            ),
            ExtendedDateTimeColumn(
                "super_date", fill=True, property_of="super_event_id"
            ),
            Column("super_causes", fill=True, property_of="super_event_id"),
            FormulaColumn(
                "super_cause_types",
                formula=lambda table, row_index: formulae.get_super_cause_types(
                    table, row_index, super_causes_hierarchy
                ),
                property_of="super_event_id",
            ),
            FormulaColumn(
                "super_event_name",
                formula=formulae.get_super_event_name,
                property_of="super_event_id",
            ),
            FormulaColumn(
                "concerned_actor",
                formula=lambda table, row_index: formulae.get_concerned_actor(
                    table, row_index, actors
                ),
                reference_table=actors,
                reference_column="actor_id",
                relation_from="super_event_id",
                type_label="CONCERNS",
            ),
        ],
        error_rules=[
            MutuallyRequiredColumns(["museum_id", "super_event_id"]),
        ],
    )

    collections_and_objects = Table(
        "Collections and Objects",
        columns=[
            ReferenceColumn(
                "super_event_id",
                unique=True,
                fill=True,
                reference_table=super_events,
                reference_column="super_event_id",
                ignore=True,
            ),
            SplitColumn(
                "coll_size",
                [
                    EnumColumn(
                        "coll_size_name",
                        enums.collection_size_values,
                        property_of="collection_or_object_id",
                    ),
                    EnumColumn(
                        "coll_size_uncertainty",
                        enums.uncertainty_values,
                        property_of="collection_or_object_id",
                    ),
                ],
                split_before="?",
            ),
            Column("object_qty", property_of="collection_or_object_id"),
            ListColumn("coll_type", property_of="collection_or_object_id"),
            ListColumn("coll_wiki_type_url", property_of="collection_or_object_id"),
            Column("coll_wiki_instance", property_of="collection_or_object_id"),
            Column("coll_wiki_instance_url", property_of="collection_or_object_id"),
            Column("coll_desc", property_of="collection_or_object_id"),
            EnumColumn(
                "coll_status",
                enums.collection_statuses,
                ignore=True,
            ),
            Column(
                "collection_id",
                ignore=True,
            ),
            FormulaColumn(
                "collection_or_object_id",
                formula=formulae.get_collection_or_object_id,
                primary_key=True,
                type_label="CollectionOrObject",
            ),
            ReferenceColumn(
                "coll_subset_of",
                "collection_id",
                ignore=True,
            ),
            FormulaColumn(
                "was_removed_from",
                formula=formulae.get_collection_was_removed_from,
                reference_column="collection_or_object_id",
                relation_from="collection_or_object_id",
                type_label="WAS_REMOVED_FROM",
            ),
            FormulaColumn(
                "collection_status",
                formula=formulae.get_collection_status,
                property_of="collection_or_object_id",
            ),
            FormulaColumn(
                "coll_size_num",
                formula=formulae.get_collection_size_number,
                property_of="collection_or_object_id",
            ),
            FormulaColumn(
                "coll_size_num_max",
                formula=formulae.get_collection_size_number_max,
                property_of="collection_or_object_id",
            ),
            FormulaColumn(
                "coll_size_num_min",
                formula=formulae.get_collection_size_number_min,
                property_of="collection_or_object_id",
            ),
            SplitColumn(
                "event_type",
                [
                    ReferenceColumn(
                        "event_type_name",
                        "type_name",
                        reference_table=event_types,
                        ignore=True,
                    ),
                    EnumColumn(
                        "event_type_uncertainty", enums.uncertainty_values, ignore=True
                    ),
                ],
                split_before="?",
            ),
            ExtendedDateTimeColumn("event_date", ignore=True),
            ExtendedDateTimeColumn("event_date_from", ignore=True),
            ExtendedDateTimeColumn("event_date_to", ignore=True),
            FormulaColumn("event_id", formula=formulae.get_event_id, ignore=True),
            FormulaColumn(
                "previous_event_id", formula=formulae.get_previous_event_id, ignore=True
            ),
            FormulaColumn(
                "collection_or_object",
                formula=formulae.determine_if_collection_or_object,
                type_label_of="collection_or_object_id",
            ),
        ],
    )

    events = Table(
        "Events",
        columns=[
            ReferenceColumn(
                "museum_id", "mm_id", reference_table=actors, fill=True, ignore=True
            ),
            ReferenceColumn(
                "super_event_id",
                unique=True,
                fill=True,
                relation_from="event_id",
                type_label="SUB_EVENT_OF",
                reference_table=super_events,
                reference_column="super_event_id",
            ),
            ReferenceColumn(
                "collection_id",
                reference_table=collections_and_objects,
                reference_column="collection_id",
                ignore=True,
            ),
            ReferenceColumn(
                "coll_subset_of",
                reference_column="collection_id",
                ignore=True,
            ),
            FormulaColumn(
                "involves",
                formula=formulae.get_involves,
                reference_table=collections_and_objects,
                reference_column="collection_or_object_id",
                relation_from="event_id",
                type_label="INVOLVES",
            ),
            SplitColumn(
                "event_type",
                [
                    ReferenceColumn(
                        "event_type_name",
                        "type_name",
                        reference_table=event_types,
                        ignore=True,
                    ),
                    EnumColumn(
                        "event_type_uncertainty",
                        enums.uncertainty_values,
                        property_of="event_type_id",
                    ),
                ],
                split_before="?",
            ),
            FormulaColumn(
                "event_type_id",
                formula=lambda table, row_index: formulae.get_type_id(
                    table, row_index, "event", "event_type_name"
                ),
                reference_column="type_id",
                reference_table=event_types,
                relation_from="event_id",
                type_label="INSTANCE_OF",
            ),
            ExtendedDateTimeColumn("event_date", property_of="event_id"),
            ExtendedDateTimeColumn("event_date_from", property_of="event_id"),
            ExtendedDateTimeColumn("event_date_to", property_of="event_id"),
            ReferenceColumn(
                "actor_recipient_id",
                "actor_id",
                reference_table=actors,
                ignore=True,
            ),
            Column("location", ignore=True),
            Column("street", ignore=True),
            Column("town", ignore=True),
            Column("county", ignore=True),
            Column("postcode", ignore=True),
            Column("notes", property_of="event_id"),
            FormulaColumn(
                "event_id",
                formula=formulae.get_event_id,
                primary_key=True,
                type_label="Event",
            ),
            FormulaColumn(
                "event_name", formula=formulae.get_event_name, property_of="event_id"
            ),
            FormulaColumn(
                "stage_in_path",
                formula=formulae.get_stage_in_path,
                property_of="event_id",
            ),
            FormulaColumn(
                "previous_event_id",
                formula=formulae.get_previous_event_id,
                relation_to="event_id",
                type_label="PRECEDES",
                reference_column="event_id",
            ),
            FormulaColumn(
                "recipient_id",
                formula=lambda table, row_index: formulae.get_recipient_id(
                    table, row_index, actors, event_types
                ),
                relation_from="event_id",
                type_label="HAS_RECIPIENT",
                reference_table=actors,
                reference_column="actor_id",
            ),
            FormulaColumn(
                "sender_id",
                formula=lambda table, row_index: formulae.get_sender_id(
                    table, row_index, actors, event_types
                ),
                relation_from="event_id",
                type_label="HAS_SENDER",
                reference_table=actors,
                reference_column="actor_id",
            ),
            FormulaColumn(
                "has_destination",
                formula=lambda table, row_index: formulae.get_event_destination(
                    table, row_index, places, actors, event_types
                ),
                relation_from="event_id",
                type_label="HAS_DESTINATION",
                reference_table=places,
                reference_column="place_id",
            ),
            FormulaColumn(
                "has_origin",
                formula=lambda table, row_index: formulae.get_event_origin(
                    table, row_index, places, actors
                ),
                relation_from="event_id",
                type_label="HAS_ORIGIN",
                reference_table=places,
                reference_column="place_id",
            ),
        ],
        error_rules=[
            MutuallyRequiredColumns(["museum_id", "super_event_id"]),
            RequiredColumns("event_type_name", required_columns=["collection_id"]),
            MutuallyRequiredColumns(["event_date_from", "event_date_to"]),
            MutuallyExclusiveColumns("event_date", "event_date_from"),
            MutuallyExclusiveColumns("event_date", "event_date_to"),
        ],
        warning_rules=[
            RequiredColumns("collection_id", required_columns=["event_type_name"]),
            UniqueCorrespondences("museum_id", "super_event_id"),
        ],
        inference_rules=[
            FillCellsWithValueWhen(
                fill_column="event_type_name",
                with_value="event",
                when=lambda row: row["event_type_name"] == ""
                and row["collection_id"] != "",
            )
        ],
    )

    print("Loading data from files")
    actor_types.import_from_list_of_lists(
        file_loader.get_sheet_as_list_of_lists("actor types")
    )
    event_types.import_from_list_of_lists(
        file_loader.get_sheet_as_list_of_lists("event types")
    )
    super_event_types.import_from_list_of_lists(
        file_loader.get_sheet_as_list_of_lists("super-event types")
    )
    default_recipient_types.import_from_list_of_lists(
        file_loader.get_sheet_as_list_of_lists("default recipient types")
    )
    super_causes_hierarchy.import_from_list_of_lists(
        file_loader.get_sheet_as_list_of_lists("super causes hierarchy")
    )

    places.import_from_list_of_lists(
        file_loader.get_sheet_as_list_of_lists("actors"),
        header_mapping={
            "actor_address1": "address_1",
            "actor_address2": "address_2",
            "actor_town_city": "village_town_city",
            "actor_county": "county",
            "actor_postcode": "postcode",
            "actor_country": "country",
        },
    )
    places.import_from_list_of_lists(
        file_loader.get_sheet_as_list_of_lists("museums"),
        header_mapping={
            "address_line_1": "address_1",
            "address_line_2": "address_2",
            "address_line_3": "address_3",
            "village_town_or_city": "village_town_city",
            "english_county": "county",
            "postcode": "postcode",
            "nation": "country",
        },
    )
    places.import_from_list_of_lists(
        file_loader.get_sheet_as_list_of_lists("events"),
        preprocessor=EventPlacesPreprocessor(),
        header_mapping={
            "street": "address_1",
            "town": "village_town_city",
            "county": "county",
            "postcode": "postcode",
        },
    )
    places.remove_duplicates()

    actors.import_from_list_of_lists(
        file_loader.get_sheet_as_list_of_lists("actors"),
        preprocessor=ActorsPreprocessor(
            file_loader.get_sheet_as_list_of_lists("museums"),
            file_loader.get_sheet_as_list_of_lists("events"),
        ),
    )

    super_events.import_from_list_of_lists(
        file_loader.get_sheet_as_list_of_lists("events"),
        preprocessor=SuperEventsPreprocessor(),
    )

    collections_and_objects.import_from_list_of_lists(
        file_loader.get_sheet_as_list_of_lists("events"),
        preprocessor=CollectionsPreprocessor(),
    )

    events.import_from_list_of_lists(
        file_loader.get_sheet_as_list_of_lists("events"),
        preprocessor=EventsPreprocessor(
            default_recipient_types, actors, places, event_types
        ),
    )

    infer_collection_sizes = """
MATCH (c:Collection)<-[:INVOLVES]-(:Event)-[:SUB_EVENT_OF]->(:SuperEvent)-[:CONCERNS]->(m:Actor)
WITH c, m
SET c.estimated_size = c.coll_size_num * m.size_num
    """
    infer_collection_max_sizes = """
MATCH (c:Collection)<-[:INVOLVES]-(:Event)-[:SUB_EVENT_OF]->(:SuperEvent)-[:CONCERNS]->(m:Actor)
WITH c, m
SET c.max_estimated_size = c.coll_size_num_max * m.size_num_max
    """
    infer_collection_min_sizes = """
MATCH (c:Collection)<-[:INVOLVES]-(:Event)-[:SUB_EVENT_OF]->(:SuperEvent)-[:CONCERNS]->(m:Actor)
WITH c, m
SET c.min_estimated_size = c.coll_size_num_min * m.size_num_min
    """

    remove_sent_to_auction_events_where_same_collection_is_sold = """
MATCH (:Type {type_name: "sent-to-auction"})<-[:INSTANCE_OF]-
    (sent_to_auction:Event)-[p:PRECEDES]->(sold_at_auction:Event)
    -[:INSTANCE_OF]->(:Type {type_name: "sold-at-auction"})
WHERE (sent_to_auction)-[:INVOLVES]->(:CollectionOrObject)<-[:INVOLVES]-(sold_at_auction)
WITH sent_to_auction, sold_at_auction
OPTIONAL MATCH (preceding_event:Event)-[:PRECEDES]->(sent_to_auction)
MATCH (sender:Actor)<-[:HAS_SENDER]-(sent_to_auction)-[:HAS_RECIPIENT]->(auction_house:Actor)
    <-[has_auction_house_sender:HAS_SENDER]-(sold_at_auction)
DELETE has_auction_house_sender
SET sold_at_auction.stage_in_path = sent_to_auction.stage_in_path
CREATE (preceding_event)-[:PRECEDES]->(sold_at_auction)
CREATE (sender)<-[:HAS_SENDER]-(sold_at_auction)-[:HAS_ENABLER]->(auction_house)
DETACH DELETE sent_to_auction
    """  # this will orphan events involving a sub-collection that are not sold after being sent to auction house

    separate_sent_and_sold_auction_events_where_sub_collection_is_sold_and_sent_to_auction_is_first_event = """
MATCH (:Type {type_name: "sent-to-auction"})<-[:INSTANCE_OF]-(sent_to_auction:Event {stage_in_path: 0})
    -[:INVOLVES]->(:CollectionOrObject)<-[was_removed_from:WAS_REMOVED_FROM]-(:CollectionOrObject)<-[:INVOLVES]-
    (sold_at_auction:Event)-[:INSTANCE_OF]->(:Type {type_name: "sold-at-auction"})
WITH sent_to_auction, was_removed_from, sold_at_auction
MATCH (sent_to_auction)-[precedes:PRECEDES]->(sold_at_auction)
WITH sent_to_auction, precedes, was_removed_from, sold_at_auction
MATCH (sender:Actor)<-[:HAS_SENDER]-(sent_to_auction)-[:HAS_RECIPIENT]->(auction_house:Actor)
    <-[has_auction_house_sender:HAS_SENDER]-(sold_at_auction)
DELETE has_auction_house_sender
SET sold_at_auction.stage_in_path = sent_to_auction.stage_in_path
CREATE (sender)<-[:HAS_SENDER]-(sold_at_auction)-[:HAS_ENABLER]->(auction_house)
DELETE was_removed_from
DELETE precedes 
    """

    separate_sent_and_sold_auction_events_where_sub_collection_is_sold = """
MATCH (:Type {type_name: "sent-to-auction"})<-[:INSTANCE_OF]-(sent_to_auction:Event)
    -[:INVOLVES]->(:CollectionOrObject)<-[was_removed_from:WAS_REMOVED_FROM]-(:CollectionOrObject)<-[:INVOLVES]-
    (sold_at_auction:Event)-[:INSTANCE_OF]->(:Type {type_name: "sold-at-auction"})
WHERE sent_to_auction.stage_in_path > 0
WITH sent_to_auction, was_removed_from, sold_at_auction
MATCH (sent_to_auction)-[precedes:PRECEDES]->(sold_at_auction)
WITH sent_to_auction, precedes, was_removed_from, sold_at_auction
OPTIONAL MATCH (preceding_event:Event)-[:PRECEDES]->(sent_to_auction)
MATCH (sender:Actor)<-[:HAS_SENDER]-(sent_to_auction)-[:HAS_RECIPIENT]->(auction_house:Actor)
    <-[has_auction_house_sender:HAS_SENDER]-(sold_at_auction)
DELETE has_auction_house_sender
DELETE precedes 
SET sold_at_auction.stage_in_path = sent_to_auction.stage_in_path
CREATE (preceding_event)-[:PRECEDES]->(sold_at_auction)
CREATE (sender)<-[:HAS_SENDER]-(sold_at_auction)-[:HAS_ENABLER]->(auction_house)
    """

    sheet_to_graph = TablesToGraph(
        actor_types,
        event_types,
        super_event_types,
        default_recipient_types,
        places,
        actors,
        super_events,
        collections_and_objects,
        events,
        inference_queries=[
            infer_collection_sizes,
            infer_collection_max_sizes,
            infer_collection_min_sizes,
            remove_sent_to_auction_events_where_same_collection_is_sold,
            separate_sent_and_sold_auction_events_where_sub_collection_is_sold_and_sent_to_auction_is_first_event,
            separate_sent_and_sold_auction_events_where_sub_collection_is_sold,
        ],
        credentials_file_name=credentials_file_name,
    )
    sheet_to_graph.translate_and_upload(
        output_spreadsheet_name="output.xlsx", stop_if_validation_fails=True
    )
