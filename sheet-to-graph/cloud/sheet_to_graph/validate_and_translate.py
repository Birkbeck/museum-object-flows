import os
import time
from typing import Any, Dict, Tuple

from geopy.distance import geodesic
import numpy as np
import pandas as pd

from sheet_to_graph import (
    Column,
    FileLoader,
    GoogleUtils,
    PostcodeToLatLong,
    Table,
    WikidataConnection,
)
from sheet_to_graph.anonymize_rows import anonymize_actor_and_event_rows
from sheet_to_graph.cloud_storage_utils import (
    _gcs_bucket,
    _upload_bytes,
    _upload_df_csv,
)
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
from sheet_to_graph.graph_traversal import (
    make_get_ancestors,
    make_get_core_type,
    make_get_ultimate_ancestor,
)
from sheet_to_graph.rules import (
    FillCellsWithValueWhen,
    RequiredColumns,
    MutuallyExclusiveColumns,
    MutuallyRequiredColumns,
    UniqueCorrespondences,
)


def validate_and_translate_data(
    file_loader, postcode_to_lat_long
) -> tuple[Dict[str, str], str]:
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
            BooleanColumn("contributes_to_length_calculation", property_of="type_id"),
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
            OptionalColumn("actor_country", property_of="place_id"),
            Column("postcode", property_of="place_id"),
            FormulaColumn(
                "longitude",
                formula=lambda table, row_index: formulae.get_longitude(
                    postcode_to_lat_long,
                    table,
                    row_index,
                    postcode_column="postcode",
                    town_city_column="village_town_city",
                    county_column="county",
                    country_column="actor_country",
                ),
                property_of="place_id",
            ),
            FormulaColumn(
                "latitude",
                formula=lambda table, row_index: formulae.get_latitude(
                    postcode_to_lat_long,
                    table,
                    row_index,
                    postcode_column="postcode",
                    town_city_column="village_town_city",
                    county_column="county",
                    country_column="actor_country",
                ),
                property_of="place_id",
            ),
            FormulaColumn(
                "bng_x",
                formula=lambda table, row_index: formulae.get_bng_x(
                    postcode_to_lat_long,
                    table,
                    row_index,
                    postcode_column="postcode",
                    town_city_column="village_town_city",
                    county_column="county",
                    country_column="actor_country",
                ),
                property_of="place_id",
            ),
            FormulaColumn(
                "bng_y",
                formula=lambda table, row_index: formulae.get_bng_y(
                    postcode_to_lat_long,
                    table,
                    row_index,
                    postcode_column="postcode",
                    town_city_column="village_town_city",
                    county_column="county",
                    country_column="actor_country",
                ),
                property_of="place_id",
            ),
            FormulaColumn(
                "region",
                formula=lambda table, row_index: formulae.get_region(
                    postcode_to_lat_long,
                    table,
                    row_index,
                    postcode_column="postcode",
                    town_city_column="village_town_city",
                    county_column="county",
                    country_column="actor_country",
                ),
                property_of="place_id",
            ),
            FormulaColumn(
                "country",
                formula=lambda table, row_index: formulae.get_country(table, row_index),
                property_of="place_id",
            ),
            FormulaColumn(
                "local_authority_code",
                formula=lambda table, row_index: formulae.get_local_authority_code(
                    postcode_to_lat_long,
                    table,
                    row_index,
                    postcode_column="postcode",
                    town_city_column="village_town_city",
                    county_column="county",
                    country_column="actor_country",
                ),
                property_of="place_id",
            ),
            FormulaColumn(
                "local_authority_name",
                formula=lambda table, row_index: formulae.get_local_authority_name(
                    postcode_to_lat_long,
                    table,
                    row_index,
                    postcode_column="postcode",
                    town_city_column="village_town_city",
                    county_column="county",
                    country_column="actor_country",
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
            Column("actor_note", property_of="actor_id"),
            OptionalColumn("size", property_of="actor_id"),
            OptionalColumn("governance", property_of="actor_id"),
            OptionalColumn("governance_broad", property_of="actor_id"),
            OptionalColumn("accreditation", property_of="actor_id"),
            OptionalColumn("subject", property_of="actor_id"),
            OptionalColumn("subject_broad", property_of="actor_id"),
            OptionalColumn("year_opened_1", property_of="actor_id"),
            OptionalColumn("year_opened_2", property_of="actor_id"),
            OptionalColumn("year_closed_1", property_of="actor_id"),
            OptionalColumn("year_closed_2", property_of="actor_id"),
            FormulaColumn(
                "region",
                formula=lambda table, row_index: formulae.get_region(
                    postcode_to_lat_long,
                    table,
                    row_index,
                    postcode_column="actor_postcode",
                    town_city_column="actor_town_city",
                    county_column="actor_county",
                    country_column="actor_country",
                ),
                property_of="actor_id",
            ),
            OptionalColumn("country", property_of="actor_id"),
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
            BooleanColumn(
                "has_collection", default="Yes", fill=True, property_of="super_event_id"
            ),
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
                "previous_event_id",
                formula=formulae.get_previous_event_id,
                relation_to="event_id",
                type_label="PRECEDES",
                reference_column="event_id",
            ),
            FormulaColumn(
                "stage_in_path",
                formula=formulae.get_stage_in_path,
                property_of="event_id",
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
    actor_type_rows = file_loader.get_sheet_as_list_of_lists(
        os.environ["DISPERSAL_SHEET_ID"],
        os.environ["ACTOR_TYPES_SHEET"],
    )
    event_type_rows = file_loader.get_sheet_as_list_of_lists(
        os.environ["DISPERSAL_SHEET_ID"],
        os.environ["EVENT_TYPES_SHEET"],
    )
    super_event_type_rows = file_loader.get_sheet_as_list_of_lists(
        os.environ["DISPERSAL_SHEET_ID"],
        os.environ["SUPER_EVENT_TYPES_SHEET"],
    )
    default_recipient_type_rows = file_loader.get_sheet_as_list_of_lists(
        os.environ["DISPERSAL_SHEET_ID"],
        os.environ["DEFAULT_RECIPIENT_TYPES_SHEET"],
    )
    super_cause_type_rows = file_loader.get_sheet_as_list_of_lists(
        os.environ["DISPERSAL_SHEET_ID"],
        os.environ["SUPER_CAUSES_SHEET"],
    )
    actor_rows = file_loader.get_sheet_as_list_of_lists(
        os.environ["DISPERSAL_SHEET_ID"],
        os.environ["ACTORS_SHEET_NAME"],
    )
    event_rows = file_loader.get_sheet_as_list_of_lists(
        os.environ["DISPERSAL_SHEET_ID"],
        os.environ["EVENTS_SHEET_NAME"],
    )
    actor_rows, event_rows = anonymize_actor_and_event_rows(actor_rows, event_rows)
    museum_rows = file_loader.get_csv_as_list_of_lists(os.environ["MUSEUMS_CSV_URL"])

    actor_types.import_from_list_of_lists(actor_type_rows)
    event_types.import_from_list_of_lists(event_type_rows)
    super_event_types.import_from_list_of_lists(super_event_type_rows)
    default_recipient_types.import_from_list_of_lists(default_recipient_type_rows)
    super_causes_hierarchy.import_from_list_of_lists(super_cause_type_rows)

    places.import_from_list_of_lists(
        actor_rows,
        header_mapping={
            "actor_address1": "address_1",
            "actor_address2": "address_2",
            "actor_town_city": "village_town_city",
            "actor_county": "county",
            "actor_postcode": "postcode",
            "actor_country": "actor_country",
        },
    )
    places.import_from_list_of_lists(
        museum_rows,
        header_mapping={
            "address_1": "address_1",
            "address_2": "address_2",
            "address_3": "address_3",
            "village_town_city": "village_town_city",
            "lad": "county",
            "postcode": "postcode",
            "country": "actor_country",
        },
    )
    places.import_from_list_of_lists(
        event_rows,
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
        actor_rows,
        preprocessor=ActorsPreprocessor(museum_rows, event_rows),
    )

    super_events.import_from_list_of_lists(
        event_rows,
        preprocessor=SuperEventsPreprocessor(),
    )
    super_events.remove_duplicates()

    collections_and_objects.import_from_list_of_lists(
        event_rows,
        preprocessor=CollectionsPreprocessor(),
    )
    collections_and_objects.remove_duplicates()

    events.import_from_list_of_lists(
        event_rows,
        preprocessor=EventsPreprocessor(
            default_recipient_types, actors, places, event_types
        ),
    )

    actor_types_df = actor_types.to_pandas_dataframe()
    event_types_df = event_types.to_pandas_dataframe()
    super_event_types_df = super_event_types.to_pandas_dataframe()
    default_recipient_types_df = default_recipient_types.to_pandas_dataframe()
    super_causes_hierarchy_df = super_causes_hierarchy.to_pandas_dataframe()
    places_df = places.to_pandas_dataframe()
    actors_df = actors.to_pandas_dataframe()
    super_events_df = super_events.to_pandas_dataframe()
    collections_and_objects_df = collections_and_objects.to_pandas_dataframe()
    events_df = events.to_pandas_dataframe()

    actor_type_parents = dict(
        zip(
            actor_types_df["type_id"],
            actor_types_df["sub_type_of_id"],
        )
    )
    get_core_actor_type = make_get_core_type(
        actor_type_parents,
        actor_types_df[actor_types_df["is_core_category"] == True]["type_id"].tolist(),
    )
    get_parent_actor_types = make_get_ancestors(actor_type_parents)
    actors_df["actor_types"] = actors_df["actor_type_id"].map(
        lambda type_id: [type_id] + get_parent_actor_types(type_id)
    )
    actors_df["core_type"] = actors_df["actor_type_id"].map(get_core_actor_type)
    actors_df["core_type_name"] = actors_df["core_type"].map(
        actor_types_df.set_index("type_id")["type_name"]
    )

    core_actor_types = (
        actor_types_df[actor_types_df["is_core_category"]]["type_name"]
        .unique()
        .tolist()
    )
    core_actor_types_with_children = (
        actor_types_df[actor_types_df["sub_type_of"].isin(core_actor_types)][
            "sub_type_of"
        ]
        .unique()
        .tolist()
    )

    event_type_parents = dict(
        zip(
            event_types_df["type_id"],
            event_types_df["sub_type_of_id"],
        )
    )
    get_core_event_type = make_get_core_type(
        event_type_parents,
        event_types_df[event_types_df["is_core_category"] == True]["type_id"].tolist(),
    )
    get_parent_event_types = make_get_ancestors(event_type_parents)
    event_types_df["core_type"] = event_types_df["type_id"].map(get_core_event_type)
    event_types_df["core_type"] = event_types_df["core_type"].map(
        event_types_df.set_index("type_id")["type_name"]
    )

    places_df["x"] = places_df["bng_x"]
    places_df["y"] = places_df["bng_y"]
    places_df["lad"] = places_df["local_authority_name"]

    museums_df = actors_df[actors_df["mm_id"] != ""].merge(
        places_df,
        left_on="has_location",
        right_on="place_id",
        how="left",
    )

    super_events_df["super_event_causes"] = super_events_df["super_causes"]
    super_events_df["super_event_cause_types"] = super_events_df["super_cause_types"]
    super_events_df["super_event_date"] = super_events_df["super_date"]

    dispersal_events = events_df[events_df["event_id"].notna()].copy()
    dispersal_events["initial_museum_id"] = dispersal_events["museum_id"]
    dispersal_events = (
        dispersal_events.merge(
            event_types_df.add_prefix("event_type_"),
            left_on="event_type_name",
            right_on="event_type_type_name",
            how="left",
        )
        .merge(
            super_events_df,
            left_on="super_event_id",
            right_on="super_event_id",
            how="left",
        )
        .merge(
            museums_df.add_prefix("initial_museum_"),
            left_on="initial_museum_id",
            right_on="initial_museum_mm_id",
            how="left",
        )
        .merge(
            actors_df.add_prefix("sender_"),
            left_on="sender_id",
            right_on="sender_actor_id",
            how="left",
        )
        .merge(
            actors_df.add_prefix("recipient_"),
            left_on="recipient_id",
            right_on="recipient_actor_id",
            how="left",
        )
        .merge(
            places_df.add_prefix("origin_"),
            left_on="has_origin",
            right_on="origin_place_id",
            how="left",
        )
        .merge(
            places_df.add_prefix("destination_"),
            left_on="has_destination",
            right_on="destination_place_id",
            how="left",
        )
        .merge(
            collections_and_objects_df.add_prefix("collection_"),
            left_on="collection_id",
            right_on="collection_collection_id",
            how="left",
        )
    )

    dispersal_events["event_stage_in_path"] = dispersal_events["stage_in_path"] + 1
    dispersal_events["event_core_type"] = dispersal_events["event_type_core_type"]
    dispersal_events["event_types"] = dispersal_events["event_type_type_id"].map(
        lambda type_id: [type_id] + get_parent_event_types(type_id)
    )
    dispersal_events["event_type"] = dispersal_events["event_type_name"]
    dispersal_events["event_is_change_of_ownership"] = dispersal_events[
        "event_type_change_of_ownership"
    ]
    dispersal_events["event_is_change_of_custody"] = (
        dispersal_events["event_type_change_of_custody"]
        & dispersal_events["has_destination"].notna()
    )
    dispersal_events["event_is_end_of_existence"] = dispersal_events[
        "event_type_end_of_existence"
    ]
    dispersal_events["initial_museum_name"] = dispersal_events[
        "initial_museum_actor_name"
    ]
    dispersal_events["initial_museum_all"] = "all"
    dispersal_events["initial_museum_town"] = dispersal_events[
        "initial_museum_actor_town_city"
    ]
    dispersal_events["initial_museum_country"] = dispersal_events[
        "initial_museum_country_x"
    ]
    dispersal_events["initial_museum_region"] = dispersal_events[
        "initial_museum_region_x"
    ]
    dispersal_events["initial_museum_sector"] = dispersal_events[
        "initial_museum_actor_sector_name"
    ]
    dispersal_events["initial_museum_type"] = dispersal_events[
        "initial_museum_actor_type_name"
    ]
    dispersal_events["initial_museum_core_type"] = dispersal_events[
        "initial_museum_actor_type_name"
    ]
    dispersal_events["sender_name"] = dispersal_events["sender_actor_name"]
    dispersal_events["sender_all"] = None
    dispersal_events["sender_all"] = dispersal_events["sender_all"].mask(
        dispersal_events["sender_size"] != "", "all"
    )
    dispersal_events["sender_town"] = dispersal_events["sender_actor_town_city"]
    dispersal_events["sender_county"] = dispersal_events["sender_actor_county"]
    dispersal_events["sender_postcode"] = dispersal_events["sender_actor_postcode"]
    dispersal_events["sender_quantity"] = dispersal_events["sender_actor_quantity"]
    dispersal_events["sender_sector"] = dispersal_events["sender_actor_sector_name"]
    dispersal_events["sender_type"] = dispersal_events["sender_actor_type_name"]
    dispersal_events["sender_core_type"] = dispersal_events["sender_core_type_name"]
    dispersal_events["recipient_name"] = dispersal_events["recipient_actor_name"]
    dispersal_events["recipient_all"] = None
    dispersal_events["recipient_all"] = dispersal_events["recipient_all"].mask(
        dispersal_events["recipient_size"] != "", "all"
    )
    dispersal_events["recipient_town"] = dispersal_events["recipient_actor_town_city"]
    dispersal_events["recipient_county"] = dispersal_events["recipient_actor_county"]
    dispersal_events["recipient_postcode"] = dispersal_events[
        "recipient_actor_postcode"
    ]
    dispersal_events["recipient_quantity"] = dispersal_events[
        "recipient_actor_quantity"
    ]
    dispersal_events["recipient_sector"] = dispersal_events[
        "recipient_actor_sector_name"
    ]
    dispersal_events["recipient_type"] = dispersal_events["recipient_actor_type_name"]
    dispersal_events["recipient_core_type"] = dispersal_events[
        "recipient_core_type_name"
    ]
    dispersal_events["origin_id"] = dispersal_events["origin_place_id"]
    dispersal_events["destination_id"] = dispersal_events["destination_place_id"]
    dispersal_events["collection_id"] = dispersal_events[
        "collection_collection_or_object_id"
    ]
    dispersal_events["parent_collection_id"] = dispersal_events[
        "collection_was_removed_from"
    ]
    dispersal_events["collection_description"] = dispersal_events[
        "collection_coll_desc"
    ]
    dispersal_events["collection_types"] = dispersal_events["collection_coll_type"]
    dispersal_events["collection_status"] = "NA"
    dispersal_events["collection_status"] = dispersal_events["collection_status"].mask(
        dispersal_events["collection_collection_status"] == "collection",
        "Objects from a museum collection",
    )
    dispersal_events["collection_status"] = dispersal_events["collection_status"].mask(
        dispersal_events["collection_collection_status"] == "loan",
        "Objects on loan to a museum",
    )
    dispersal_events["collection_status"] = dispersal_events["collection_status"].mask(
        dispersal_events["collection_collection_status"] == "handling",
        "Handling objects",
    )
    dispersal_events["collection_status"] = dispersal_events["collection_status"].mask(
        dispersal_events["collection_collection_status"] == "museum-stuff",
        "Other objects (e.g. display cases)",
    )
    dispersal_events["collection_size"] = dispersal_events["collection_coll_size_name"]
    dispersal_events["collection_quantity"] = dispersal_events["collection_object_qty"]

    dispersal_events["recipient_type"] = dispersal_events["recipient_type"].mask(
        dispersal_events["recipient_type"] == "actor", "unspecified actor"
    )
    dispersal_events["recipient_type"] = dispersal_events["recipient_type"].mask(
        dispersal_events["recipient_type"].isin(core_actor_types_with_children),
        "unspecified"
        + dispersal_events.loc[
            dispersal_events["recipient_type"].isin(core_actor_types_with_children),
            "recipient_type",
        ].astype(str),
    )
    dispersal_events["recipient_type"] = dispersal_events["recipient_type"].mask(
        dispersal_events["recipient_type"].isna(), "N/A"
    )
    dispersal_events["recipient_core_type"] = dispersal_events[
        "recipient_core_type"
    ].mask(
        dispersal_events["recipient_type"] == "unspecified actor", "unspecified actor"
    )
    dispersal_events["recipient_core_type"] = dispersal_events[
        "recipient_core_type"
    ].mask(dispersal_events["recipient_type"] == "N/A", "N/A")

    dispersal_events["sender_type"] = dispersal_events["sender_type"].mask(
        dispersal_events["sender_type"] == "actor", "unspecified actor"
    )
    dispersal_events["sender_type"] = dispersal_events["sender_type"].mask(
        dispersal_events["sender_type"].isin(core_actor_types_with_children),
        "unspecified"
        + dispersal_events.loc[
            dispersal_events["sender_type"].isin(core_actor_types_with_children),
            "sender_type",
        ].astype(str),
    )
    dispersal_events["sender_type"] = dispersal_events["sender_type"].mask(
        dispersal_events["sender_type"].isna(), "N/A"
    )
    dispersal_events["sender_core_type"] = dispersal_events["sender_core_type"].mask(
        dispersal_events["sender_type"] == "unspecified actor", "unspecified actor"
    )
    dispersal_events["sender_core_type"] = dispersal_events["sender_core_type"].mask(
        dispersal_events["sender_type"] == "N/A", "N/A"
    )

    dispersal_events["sender_region_original"] = dispersal_events["sender_region"]
    dispersal_events["sender_region"] = "unknown"
    dispersal_events["sender_region"] = dispersal_events["sender_region"].mask(
        dispersal_events["origin_country"].notna()
        & (dispersal_events["origin_country"] != ""),
        dispersal_events["origin_country"],
    )
    dispersal_events["sender_region"] = dispersal_events["sender_region"].mask(
        dispersal_events["origin_region"].notna()
        & (dispersal_events["origin_region"] != ""),
        dispersal_events["origin_region"],
    )
    dispersal_events["sender_region"] = dispersal_events["sender_region"].mask(
        dispersal_events["sender_country"].notna()
        & (dispersal_events["sender_country"] != ""),
        dispersal_events["sender_country"],
    )
    dispersal_events["sender_region"] = dispersal_events["sender_region"].mask(
        dispersal_events["sender_region_original"].notna()
        & (dispersal_events["sender_region_original"] != ""),
        dispersal_events["sender_region_original"],
    )
    dispersal_events["sender_region"] = dispersal_events["sender_region"].mask(
        dispersal_events["sender_type"] == "end of existence",
        "end of existence",
    )

    dispersal_events["recipient_region_original"] = dispersal_events["recipient_region"]
    dispersal_events["recipient_region"] = "unknown"
    dispersal_events["recipient_region"] = dispersal_events["recipient_region"].mask(
        dispersal_events["destination_country"].notna()
        & (dispersal_events["destination_country"] != ""),
        dispersal_events["destination_country"],
    )
    dispersal_events["recipient_region"] = dispersal_events["recipient_region"].mask(
        dispersal_events["destination_region"].notna()
        & (dispersal_events["destination_region"] != ""),
        dispersal_events["destination_region"],
    )
    dispersal_events["recipient_region"] = dispersal_events["recipient_region"].mask(
        dispersal_events["recipient_country"].notna()
        & (dispersal_events["recipient_country"] != ""),
        dispersal_events["recipient_country"],
    )
    dispersal_events["recipient_region"] = dispersal_events["recipient_region"].mask(
        dispersal_events["recipient_region_original"].notna()
        & (dispersal_events["recipient_region_original"] != ""),
        dispersal_events["recipient_region_original"],
    )
    dispersal_events["recipient_region"] = dispersal_events["recipient_region"].mask(
        dispersal_events["recipient_type"] == "end of existence",
        "end of existence",
    )

    dispersal_events["distance"] = dispersal_events.apply(
        lambda row: geodesic(
            (row["origin_latitude"], row["origin_longitude"]),
            (row["destination_latitude"], row["destination_longitude"]),
        ).miles
        if pd.notnull(row["origin_latitude"])
        and pd.notnull(row["destination_latitude"])
        else np.nan,
        axis=1,
    )
    dispersal_events["distance_category"] = "1,000+"
    dispersal_events["distance_category"] = dispersal_events["distance_category"].mask(
        dispersal_events["distance"] < 1000, "100 - 1,000"
    )
    dispersal_events["distance_category"] = dispersal_events["distance_category"].mask(
        dispersal_events["distance"] < 100, "10 - 100"
    )
    dispersal_events["distance_category"] = dispersal_events["distance_category"].mask(
        dispersal_events["distance"] < 10, "1 - 10"
    )
    dispersal_events["distance_category"] = dispersal_events["distance_category"].mask(
        dispersal_events["distance"] < 1, "0 - 1"
    )
    dispersal_events["distance_category"] = dispersal_events["distance_category"].mask(
        dispersal_events["distance"] == 0, "0"
    )
    dispersal_events["distance_category"] = dispersal_events["distance_category"].mask(
        dispersal_events["distance"].isna(), "unknown"
    )
    dispersal_events["distance_category"] = dispersal_events["distance_category"].mask(
        dispersal_events["recipient_type"] == "end of existence", "end of existence"
    )

    dispersal_events["distance_from_initial_museum"] = dispersal_events.apply(
        lambda row: geodesic(
            (row["initial_museum_latitude"], row["initial_museum_longitude"]),
            (row["destination_latitude"], row["destination_longitude"]),
        ).miles
        if pd.notnull(row["initial_museum_latitude"])
        and pd.notnull(row["destination_latitude"])
        else np.nan,
        axis=1,
    )
    dispersal_events["distance_from_initial_museum_category"] = "1,000+"
    dispersal_events["distance_from_initial_museum_category"] = dispersal_events[
        "distance_from_initial_museum_category"
    ].mask(dispersal_events["distance_from_initial_museum"] < 1000, "100 - 1,000")
    dispersal_events["distance_from_initial_museum_category"] = dispersal_events[
        "distance_from_initial_museum_category"
    ].mask(dispersal_events["distance_from_initial_museum"] < 100, "10 - 100")
    dispersal_events["distance_from_initial_museum_category"] = dispersal_events[
        "distance_from_initial_museum_category"
    ].mask(dispersal_events["distance_from_initial_museum"] < 10, "1 - 10")
    dispersal_events["distance_from_initial_museum_category"] = dispersal_events[
        "distance_from_initial_museum_category"
    ].mask(dispersal_events["distance_from_initial_museum"] < 1, "0 - 1")
    dispersal_events["distance_from_initial_museum_category"] = dispersal_events[
        "distance_from_initial_museum_category"
    ].mask(dispersal_events["distance_from_initial_museum"] == 0, "0")
    dispersal_events["distance_from_initial_museum_category"] = dispersal_events[
        "distance_from_initial_museum_category"
    ].mask(dispersal_events["distance_from_initial_museum"].isna(), "unknown")
    dispersal_events["distance_from_initial_museum_category"] = dispersal_events[
        "distance_from_initial_museum_category"
    ].mask(dispersal_events["recipient_type"] == "end of existence", "end of existence")

    # infer collection sizes
    mask = dispersal_events["collection_collection_or_object"] == "Collection"
    dispersal_events["collection_estimated_size"] = np.nan
    dispersal_events["collection_estimated_size_min"] = np.nan
    dispersal_events["collection_estimated_size_max"] = np.nan
    dispersal_events.loc[mask, "collection_estimated_size"] = (
        dispersal_events.loc[mask, "collection_coll_size_num"]
        * dispersal_events.loc[mask, "initial_museum_size_num"]
    )
    dispersal_events.loc[mask, "collection_estimated_size_min"] = (
        dispersal_events.loc[mask, "collection_coll_size_num_min"]
        * dispersal_events.loc[mask, "initial_museum_size_num_min"]
    )
    dispersal_events.loc[mask, "collection_estimated_size_max"] = (
        dispersal_events.loc[mask, "collection_coll_size_num_max"]
        * dispersal_events.loc[mask, "initial_museum_size_num_max"]
    )

    event_ancestors = dict(
        zip(dispersal_events.event_id, dispersal_events.previous_event_id)
    )
    collection_ancestors = dict(
        zip(
            collections_and_objects_df.collection_or_object_id,
            collections_and_objects_df.was_removed_from,
        )
    )
    get_event_ancestors = make_get_ancestors(event_ancestors)
    get_collection_ancestors = make_get_ancestors(collection_ancestors)

    dispersal_events["ancestor_events"] = dispersal_events["event_id"].apply(
        lambda event_id: [event_id] + get_event_ancestors(event_id)
    )
    dispersal_events["ancestor_collections"] = dispersal_events["collection_id"].apply(
        lambda collection_id: [collection_id] + get_collection_ancestors(collection_id)
    )
    dispersal_events["original_collection_id"] = dispersal_events[
        "ancestor_collections"
    ].apply(lambda ancestors: ancestors[-1])

    # actor types
    actor_type_columns = [
        "type_name",
        "sub_type_of",
        "is_core_category",
        "total_instances",
        "public_instances",
        "private_instances",
        "third_instances",
        "university_instances",
        "hybrid_instances",
        "unknown_instances",
    ]
    # group actors in actors_df by parent_types and sector. Count sector instances per type.
    type_sector_counts = (
        actors_df.explode("actor_types")
        .rename(columns={"actor_types": "type", "actor_sector_name": "sector"})
        .groupby(["type", "sector"])
        .size()
        .reset_index(name="count")
        .pivot_table(index="type", columns="sector", values="count", fill_value=0)
    )
    type_sector_counts = type_sector_counts.rename(
        columns={col: f"{col}_instances" for col in type_sector_counts.columns}
    )
    type_sector_counts = type_sector_counts.reset_index()
    actor_types_df = actor_types_df.merge(
        type_sector_counts, left_on="type_id", right_on="type", how="left"
    )
    actor_types_df["total_instances"] = actor_types_df[
        [
            "public_instances",
            "private_instances",
            "third_instances",
            "university_instances",
            "hybrid_instances",
            "unknown_instances",
        ]
    ].sum(axis=1)
    actor_types_df = actor_types_df[actor_type_columns]

    # event types
    event_type_columns = [
        "type_name",
        "sub_type_of",
        "core_type",
        "is_core_category",
        "change_of_ownership",
        "change_of_custody",
        "end_of_existence",
        "contributes_to_length_calculation",
        "definition",
        "total_instances",
    ]
    event_type_counts = (
        dispersal_events.explode("event_types")
        .rename(columns={"event_types": "type"})
        .groupby("type")
        .size()
        .reset_index(name="total_instances")
    )
    event_types_df = event_types_df.merge(
        event_type_counts, left_on="type_id", right_on="type", how="left"
    )
    event_types_df = event_types_df[event_type_columns]

    # super events
    super_event_columns = ["museum_id", "super_reasons", "reason", "has_collection"]
    super_events_df["super_reasons"] = super_events_df["super_causes"]
    super_events_df["reason"] = super_events_df["super_cause_types"]
    super_events_df = super_events_df[super_event_columns]

    # dispersal events
    dispersal_events_columns = [
        "initial_museum_id",
        "initial_museum_name",
        "initial_museum_all",
        "initial_museum_size",
        "initial_museum_governance_broad",
        "initial_museum_governance",
        "initial_museum_sector",
        "initial_museum_accreditation",
        "initial_museum_subject_broad",
        "initial_museum_subject",
        "initial_museum_year_opened_1",
        "initial_museum_year_opened_2",
        "initial_museum_year_closed_1",
        "initial_museum_year_closed_2",
        "initial_museum_country",
        "initial_museum_region",
        "initial_museum_town",
        "initial_museum_type",
        "initial_museum_core_type",
        "super_event_id",
        "super_event_causes",
        "super_event_cause_types",
        "super_event_date",
        "event_id",
        "previous_event_id",
        "ancestor_events",
        "event_type",
        "event_core_type",
        "event_type_uncertainty",
        "event_date",
        "event_date_from",
        "event_date_to",
        "event_stage_in_path",
        "event_is_change_of_ownership",
        "event_is_change_of_custody",
        "event_is_end_of_existence",
        "collection_id",
        "parent_collection_id",
        "ancestor_collections",
        "original_collection_id",
        "collection_description",
        "collection_types",
        "collection_status",
        "collection_size",
        "collection_quantity",
        "collection_estimated_size",
        "collection_estimated_size_min",
        "collection_estimated_size_max",
        "sender_id",
        "sender_name",
        "sender_all",
        "sender_size",
        "sender_governance",
        "sender_governance_broad",
        "sender_accreditation",
        "sender_subject",
        "sender_subject_broad",
        "sender_year_opened_1",
        "sender_year_opened_2",
        "sender_year_closed_1",
        "sender_year_closed_2",
        "sender_region",
        "sender_country",
        "sender_town",
        "sender_county",
        "sender_postcode",
        "sender_quantity",
        "sender_sector",
        "sender_type",
        "sender_core_type",
        "recipient_id",
        "recipient_name",
        "recipient_all",
        "recipient_size",
        "recipient_governance",
        "recipient_governance_broad",
        "recipient_accreditation",
        "recipient_subject",
        "recipient_subject_broad",
        "recipient_year_opened_1",
        "recipient_year_opened_2",
        "recipient_year_closed_1",
        "recipient_year_closed_2",
        "recipient_region",
        "recipient_country",
        "recipient_town",
        "recipient_county",
        "recipient_postcode",
        "recipient_quantity",
        "recipient_sector",
        "recipient_type",
        "recipient_core_type",
        "initial_museum_latitude",
        "initial_museum_longitude",
        "initial_museum_x",
        "initial_museum_y",
        "origin_id",
        "origin_latitude",
        "origin_longitude",
        "origin_x",
        "origin_y",
        "origin_lad",
        "origin_region",
        "origin_country",
        "destination_id",
        "destination_latitude",
        "destination_longitude",
        "destination_x",
        "destination_y",
        "destination_lad",
        "destination_region",
        "destination_country",
        "distance",
        "distance_category",
        "distance_from_initial_museum",
        "distance_from_initial_museum_category",
    ]
    dispersal_events = dispersal_events[dispersal_events_columns]

    # find "sold-at-auction" events
    # where the same collection_id was in a preceding "sent-to-auction" event.
    # Delete the "sent-to-auction" event and update the "sold-at-auction" event's
    # stage_in_path, sender, and origin fields with the values from the "sent-to-auction" event.
    sender_values = [
        col_name
        for col_name in dispersal_events_columns
        if col_name.startswith("sender_")
    ]
    origin_values = [
        col_name
        for col_name in dispersal_events_columns
        if col_name.startswith("origin_")
    ]
    cols_to_update = (
        ["event_stage_in_path", "previous_event_id"] + sender_values + origin_values
    )

    sent_to_auction_events = dispersal_events[
        dispersal_events["event_type"] == "sent-to-auction"
    ]
    sold_at_auction_events = dispersal_events[
        dispersal_events["event_type"] == "sold-at-auction"
    ]
    events_to_delete = []
    for _, event in sold_at_auction_events.iterrows():
        collection_id = event["collection_id"]
        sent_to_auction_event = sent_to_auction_events[
            (sent_to_auction_events["event_id"] == event["previous_event_id"])
            & (sent_to_auction_events["collection_id"] == collection_id)
            & (sent_to_auction_events["event_type"] == "sent-to-auction")
        ]
        if not sent_to_auction_event.empty:
            events_to_delete.append(sent_to_auction_event["event_id"].values[0])
            dispersal_events.loc[
                dispersal_events["event_id"] == event["event_id"], cols_to_update
            ] = sent_to_auction_event[cols_to_update].values
            event["ancestor_events"].remove(sent_to_auction_event["event_id"].values[0])
    dispersal_events = dispersal_events[
        ~(dispersal_events["event_id"].isin(events_to_delete))
    ]

    # find "sold-at-auction" and "sent-to-auction" events where
    # the collection sent-to-auction is parent collection of the collection sold-at-auction
    # update the "sold-at-auction" event's
    # stage_in_path, sender, and origin fields with the values from the "sent-to-auction" event.
    # but do not delete the "sent-to-auction" event
    for _, sold_event in sold_at_auction_events.iterrows():
        collection_id = sold_event["collection_id"]
        parent_collection_id = sold_event["parent_collection_id"]
        if parent_collection_id is None:
            continue
        sent_to_auction_event = sent_to_auction_events[
            (sent_to_auction_events["collection_id"] == parent_collection_id)
            & (sent_to_auction_events["event_id"] == sold_event["previous_event_id"])
        ]
        if not sent_to_auction_event.empty:
            dispersal_events.loc[
                dispersal_events["event_id"] == sold_event["event_id"], cols_to_update
            ] = sent_to_auction_event[cols_to_update].values
            sold_event["ancestor_events"].remove(
                sent_to_auction_event["event_id"].values[0]
            )

    # ---- GCS outputs ----
    bucket, cache_control = _gcs_bucket()
    timestamp = time.strftime("%Y-%m-%d_%H-%M-%S")

    latest_base = "latest"
    versioned_base = "snapshots"

    urls = {}

    urls["dispersal_events_latest"] = _upload_df_csv(
        bucket,
        f"{latest_base}/dispersal_events.csv",
        dispersal_events,
        cache_control=cache_control,
    )
    urls["dispersal_events_snapshot"] = _upload_df_csv(
        bucket,
        f"{versioned_base}/dispersal_events_{timestamp}.csv",
        dispersal_events,
        cache_control=cache_control,
    )

    urls["event_types_latest"] = _upload_df_csv(
        bucket,
        f"{latest_base}/event_types.csv",
        event_types_df,
        cache_control=cache_control,
    )
    urls["event_types_snapshot"] = _upload_df_csv(
        bucket,
        f"{versioned_base}/event_types_{timestamp}.csv",
        event_types_df,
        cache_control=cache_control,
    )

    urls["actor_types_latest"] = _upload_df_csv(
        bucket,
        f"{latest_base}/actor_types.csv",
        actor_types_df,
        cache_control=cache_control,
    )
    urls["actor_types_snapshot"] = _upload_df_csv(
        bucket,
        f"{versioned_base}/actor_types_{timestamp}.csv",
        actor_types_df,
        cache_control=cache_control,
    )

    urls["super_events_latest"] = _upload_df_csv(
        bucket,
        f"{latest_base}/super_events.csv",
        super_events_df,
        cache_control=cache_control,
    )
    urls["super_events_snapshot"] = _upload_df_csv(
        bucket,
        f"{versioned_base}/super_events_{timestamp}.csv",
        super_events_df,
        cache_control=cache_control,
    )

    # Return urls + timestamp so caller can include them in the HTTP response
    return urls, timestamp
