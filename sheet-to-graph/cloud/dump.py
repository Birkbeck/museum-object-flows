"""
This script queries the neo4j database and converts it into a single large csv file
"""

from collections import Counter
import csv
import os
import json
import shutil

from sheet_to_graph.connection_managers import QueryToCsv

RESULTS_DIR = ".."

actor_types_query = """
MATCH (actor_type:Type)-[:SUB_TYPE_OF*0..]->(:Type {type_name: "actor"})
WITH actor_type
OPTIONAL MATCH (actor_type)-[:SUB_TYPE_OF]->(super_type:Type)-[:SUB_TYPE_OF*0..]->(:Type {type_name: "actor"})
WITH actor_type.type_name AS type_name, 
     super_type.type_name AS sub_type_of, 
     actor_type.is_core_category AS is_core_category,
     actor_type
OPTIONAL MATCH (actor_type)<-[:SUB_TYPE_OF*0..]-(:Type)<-[:INSTANCE_OF]-(a:Actor)
WITH type_name, 
     sub_type_of, 
     is_core_category, 
     COUNT(a) AS total_instances,
     COUNT(CASE WHEN a.actor_sector_name = "public" THEN 1 END) AS public_instances,
     COUNT(CASE WHEN a.actor_sector_name = "private" THEN 1 END) AS private_instances,
     COUNT(CASE WHEN a.actor_sector_name = "third" THEN 1 END) AS third_instances,
     COUNT(CASE WHEN a.actor_sector_name = "university" THEN 1 END) AS university_instances,
     COUNT(CASE WHEN a.actor_sector_name = "hybrid" THEN 1 END) AS hybrid_instances,
     COUNT(CASE WHEN a.actor_sector_name = "unknown" THEN 1 END) AS unknown_instances
RETURN
    type_name,
    sub_type_of,
    is_core_category,
    total_instances,
    public_instances,
    private_instances,
    third_instances,
    university_instances,
    hybrid_instances,
    unknown_instances
"""

event_types_query = """
MATCH (event_type:Type)-[:SUB_TYPE_OF*0..]->(:Type {type_name: "event"})
WITH event_type
OPTIONAL MATCH (event_type)-[:SUB_TYPE_OF]->(super_type:Type)-[:SUB_TYPE_OF*0..]->(:Type {type_name: "event"})
OPTIONAL MATCH (event_type)<-[:SUB_TYPE_OF*0..]-(:Type)<-[:INSTANCE_OF]-(e:Event)
WITH event_type,
     super_type,
     COUNT(e) AS total_instances
RETURN
    event_type.type_name AS type_name,
    super_type.type_name AS sub_type_of,
    [
      (event_type)-[:SUB_TYPE_OF*0..]->(event_core_type:Type {is_core_category: TRUE})
      | event_core_type.type_name
    ][0] AS core_type,
    event_type.is_core_category AS is_core_category,
    event_type.change_of_ownership AS change_of_ownership,
    event_type.change_of_custody AS change_of_custody,
    event_type.end_of_existence AS end_of_existence,
    event_type.contributes_to_length_calculation AS contributes_to_length_calculation,
    event_type.definition AS definition,
    total_instances
"""

actor_types_data_dump_query = """
MATCH (actor_type:Type)-[:SUB_TYPE_OF*0..]->(:Type {type_name: "actor"})
WITH actor_type
OPTIONAL MATCH (actor_type)-[:SUB_TYPE_OF]->(super_type:Type)-[:SUB_TYPE_OF*0..]->(:Type {type_name: "actor"})
RETURN
    actor_type.type_name AS type_name,
    super_type.type_name AS sub_type_of,
    actor_type.is_core_category AS is_core_category
"""

event_types_data_dump_query = """
MATCH (event_type:Type)-[:SUB_TYPE_OF*0..]->(:Type {type_name: "event"})
WITH event_type
OPTIONAL MATCH (event_type)-[:SUB_TYPE_OF]->(super_type:Type)-[:SUB_TYPE_OF*0..]->(:Type {type_name: "event"})
RETURN
    event_type.type_name AS type_name,
    super_type.type_name AS sub_type_of,
    [
      (event_type)-[:SUB_TYPE_OF*0..]->(event_core_type:Type {is_core_category: TRUE})
      | event_core_type.type_name
    ][0] AS core_type,
    event_type.is_core_category AS is_core_category,
    event_type.change_of_ownership AS change_of_ownership,
    event_type.change_of_custody AS change_of_custody,
    event_type.end_of_existence AS end_of_existence
"""

museums_query = """
MATCH (museum:Actor)-[:HAS_LOCATION]->(place:Place)
WHERE museum.mm_id <> ""
RETURN
    museum.mm_id AS museum_id,
    museum.actor_name AS museum_name,
    "all" AS all,
    museum.governance AS governance,
    museum.governance_broad AS governance_broad,
    museum.size AS size,
    museum.subject AS subject,
    museum.subject_broad AS subject_broad,
    museum.accreditation AS accreditation,
    museum.region AS region,
    museum.country AS country,
    museum.year_opened_1 AS year_opened_1,
    museum.year_opened_2 AS year_opened_2,
    museum.year_closed_1 AS year_closed_1,
    museum.year_closed_2 AS year_closed_2,
    place.address_1 AS address_1,
    place.address_2 AS address_2,
    place.address_3 AS address_3,
    place.village_town_city AS village_town_city,
    place.postcode AS postcode,
    place.bng_x AS bng_x,
    place.bng_y AS bng_y
"""

super_events_query = """
MATCH (initial_museum:Actor)<-[:CONCERNS]-(super_event:SuperEvent)
RETURN
    initial_museum.mm_id AS museum_id,
    super_event.super_causes AS super_reasons,
    super_event.super_cause_types AS reason,
    super_event.has_collection AS has_collection
"""

dispersal_events_query = """
MATCH (initial_museum:Actor)<-[:CONCERNS]-(super_event:SuperEvent)<-[:SUB_EVENT_OF]-(event:Event)-[event_is_instance_of:INSTANCE_OF]->(event_type:Type)
WITH initial_museum, super_event, event, event_is_instance_of, event_type
OPTIONAL MATCH (event)-[:INVOLVES]->(collection:CollectionOrObject)
WITH initial_museum, super_event, event, event_is_instance_of, event_type, collection
OPTIONAL MATCH (event)-[:HAS_SENDER]->(sender:Actor)-[:INSTANCE_OF]->(sender_type:Type)
WITH initial_museum, super_event, event, event_is_instance_of, event_type, collection, sender, sender_type
OPTIONAL MATCH (event)-[:HAS_RECIPIENT]->(recipient:Actor)-[:INSTANCE_OF]->(recipient_type:Type)
WITH initial_museum, super_event, event, event_is_instance_of, event_type, collection, sender, sender_type, recipient, recipient_type
OPTIONAL MATCH (event)-[:HAS_ORIGIN]->(origin:Place)
WITH initial_museum, super_event, event, event_is_instance_of, event_type, collection, sender, sender_type, recipient, recipient_type, origin
OPTIONAL MATCH (event)-[:HAS_DESTINATION]->(destination:Place)
WITH initial_museum, super_event, event, event_is_instance_of, event_type, collection, sender, sender_type, recipient, recipient_type, origin, destination
OPTIONAL MATCH (sender)-[:HAS_LOCATION]->(sender_location:Place)
WITH initial_museum, super_event, event, event_is_instance_of, event_type, collection, sender, sender_type, recipient, recipient_type, origin, destination, sender_location
OPTIONAL MATCH (recipient)-[:HAS_LOCATION]->(recipient_location:Place)
WITH initial_museum, super_event, event, event_is_instance_of, event_type, collection, sender, sender_type, recipient, recipient_type, origin, destination, sender_location, recipient_location
OPTIONAL MATCH (initial_museum)-[:HAS_LOCATION]->(initial_museum_location:Place)
RETURN
    initial_museum.mm_id AS initial_museum_id,
    initial_museum.actor_name AS initial_museum_name,
    "all" AS initial_museum_all,
    initial_museum.size AS initial_museum_size,
    initial_museum.governance_broad AS initial_museum_governance_broad,
    initial_museum.governance AS initial_museum_governance,
    initial_museum.actor_sector_name AS initial_museum_sector,
    initial_museum.accreditation AS initial_museum_accreditation,
    initial_museum.subject_broad AS initial_museum_subject_broad,
    initial_museum.subject AS initial_museum_subject,
    initial_museum.year_opened_1 AS initial_museum_year_opened_1,
    initial_museum.year_opened_2 AS initial_museum_year_opened_2,
    initial_museum.year_closed_1 AS initial_museum_year_closed_1,
    initial_museum.year_closed_2 AS initial_museum_year_closed_2,
    initial_museum.country AS initial_museum_country,
    initial_museum.region AS initial_museum_region,
    initial_museum_location.village_town_city AS initial_museum_town,
    [
      (initial_museum)-[:INSTANCE_OF]->(initial_museum_type:Type)
      | initial_museum_type.type_name
    ][0] AS initial_museum_type,
    [
      (initial_museum)-[:INSTANCE_OF]->(initial_museum_type:Type)-[:SUB_TYPE_OF*0..]->(initial_museum_core_type:Type {is_core_category: TRUE})
      | initial_museum_core_type.type_name
    ][0] AS initial_museum_core_type,
    [
      (initial_museum)-[:INSTANCE_OF]->(initial_museum_type:Type)-[:SUB_TYPE_OF*0..]->(initial_museum_general_type:Type)-[:SUB_TYPE_OF]->(:Type {type_name: "actor"})
      | initial_museum_general_type.type_name
    ][0] AS initial_museum_general_type,
    super_event.super_event_id AS super_event_id,
    super_event.super_causes AS super_event_causes,
    super_event.super_cause_types AS super_event_cause_types,
    super_event.super_date AS super_event_date,
    event.event_id AS event_id,
    [
      (previous_event)-[:PRECEDES]->(event) | previous_event.event_id
    ][0] AS previous_event_id,
    [
      (event)<-[:PRECEDES*0..]-(ancestor_event:Event) | ancestor_event.event_id
    ] AS ancestor_events,
    event_type.type_name AS event_type,
    [
      (event_type)-[:SUB_TYPE_OF*0..]->(event_core_type:Type {is_core_category: TRUE})
      | event_core_type.type_name
    ][0] AS event_core_type,
    event_is_instance_of.event_type_uncertainty AS event_type_uncertainty,
    event.event_date AS event_date,
    event.event_date_from AS event_date_from,
    event.event_date_to AS event_date_to,
    event.stage_in_path AS event_stage_in_path,
    event_type.change_of_ownership AS event_is_change_of_ownership,
    event_type.change_of_custody and exists((event)-[:HAS_DESTINATION]->()) AS event_is_change_of_custody,
    event_type.end_of_existence AS event_is_end_of_existence,
    collection.collection_or_object_id AS collection_id,
    [
      (collection)-[:WAS_REMOVED_FROM]->(parent_collection:CollectionOrObject) | parent_collection.collection_or_object_id
    ][0] AS parent_collection_id,
    [
      (collection)-[:WAS_REMOVED_FROM*0..]->(ancestor_collection:CollectionOrObject) | ancestor_collection.collection_or_object_id
    ] AS ancestor_collections,
    [
      (collection)-[:WAS_REMOVED_FROM*0..]->(original_collection:CollectionOrObject)<-[:INVOLVES]-(original_event {stage_in_path: 0})
      | original_collection.collection_or_object_id
    ][0] AS original_collection_id,
    collection.coll_desc AS collection_description,
    collection.coll_type AS collection_types,
    collection.collection_status AS collection_status,
    collection.coll_size_name AS collection_size,
    collection.object_qty AS collection_quantity,
    collection.estimated_size AS collection_estimated_size,
    collection.min_estimated_size AS collection_estimated_size_min,
    collection.max_estimated_size AS collection_estimated_size_max,
    sender.actor_id AS sender_id,
    sender.actor_name AS sender_name,
    "all" AS sender_all,
    sender.size AS sender_size,
    sender.governance AS sender_governance,
    sender.governance_broad AS sender_governance_broad,
    sender.accreditation AS sender_accreditation,
    sender.subject AS sender_subject,
    sender.subject_broad AS sender_subject_broad,
    sender.year_opened_1 AS sender_year_opened_1,
    sender.year_opened_2 AS sender_year_opened_2,
    sender.year_closed_1 AS sender_year_closed_1,
    sender.year_closed_2 AS sender_year_closed_2,
    sender.region AS sender_region,
    sender.country AS sender_country,
    sender_location.village_town_city AS sender_town,
    sender_location.county AS sender_county,
    sender_location.postcode AS sender_postcode,
    sender.actor_quantity AS sender_quantity,
    sender.actor_sector_name AS sender_sector,
    sender_type.type_name AS sender_type,
    [
      (sender_type)-[:SUB_TYPE_OF*0..]->(sender_core_type:Type {is_core_category: TRUE})
      | sender_core_type.type_name
    ][0] AS sender_core_type,
    [
      (sender_type)-[:SUB_TYPE_OF*0..]->(sender_general_type:Type)-[:SUB_TYPE_OF]->(:Type {type_name: "actor"})
      | sender_general_type.type_name
    ][0] AS sender_general_type,
    recipient.actor_id AS recipient_id,
    recipient.actor_name AS recipient_name,
    "all" AS recipient_all,
    recipient.size AS recipient_size,
    recipient.governance AS recipient_governance,
    recipient.governance_broad AS recipient_governance_broad,
    recipient.accreditation AS recipient_accreditation,
    recipient.subject AS recipient_subject,
    recipient.subject_broad AS recipient_subject_broad,
    recipient.year_opened_1 AS recipient_year_opened_1,
    recipient.year_opened_2 AS recipient_year_opened_2,
    recipient.year_closed_1 AS recipient_year_closed_1,
    recipient.year_closed_2 AS recipient_year_closed_2,
    recipient.region AS recipient_region,
    recipient.country AS recipient_country,
    recipient_location.village_town_city AS recipient_town,
    recipient_location.county AS recipient_county,
    recipient_location.postcode AS recipient_postcode,
    recipient.actor_quantity AS recipient_quantity,
    recipient.actor_sector_name AS recipient_sector,
    recipient_type.type_name AS recipient_type,
    [
      (recipient_type)-[:SUB_TYPE_OF*0..]->(recipient_core_type:Type {is_core_category: TRUE})
      | recipient_core_type.type_name
    ][0] AS recipient_core_type,
    [
      (recipient_type)-[:SUB_TYPE_OF*0..]->(recipient_general_type:Type)-[:SUB_TYPE_OF]->(:Type {type_name: "actor"})
      | recipient_general_type.type_name
    ][0] AS recipient_general_type,
    initial_museum_location.latitude AS initial_museum_latitude,
    initial_museum_location.longitude AS initial_museum_longitude,
    initial_museum_location.bng_x AS initial_museum_x,
    initial_museum_location.bng_y AS initial_museum_y,
    origin.place_id AS origin_id,
    origin.latitude AS origin_latitude,
    origin.longitude AS origin_longitude,
    origin.bng_x AS origin_x,
    origin.bng_y AS origin_y,
    origin.local_authority_name AS origin_lad,
    origin.region AS origin_region,
    origin.country AS origin_country,
    destination.place_id AS destination_id,
    destination.latitude AS destination_latitude,
    destination.longitude AS destination_longitude,
    destination.bng_x AS destination_x,
    destination.bng_y AS destination_y,
    destination.local_authority_name AS destination_lad,
    destination.region AS destination_region,
    destination.country AS destination_country
"""

dispersal_events_data_dump_query = """
MATCH (initial_museum:Actor)<-[:CONCERNS]-(super_event:SuperEvent)
WITH initial_museum, super_event
OPTIONAL MATCH (super_event)<-[:SUB_EVENT_OF]-(event:Event)-[event_is_instance_of:INSTANCE_OF]->(event_type:Type)
WITH initial_museum, super_event, event, event_is_instance_of, event_type
OPTIONAL MATCH (event)-[:INVOLVES]->(collection:CollectionOrObject)
WITH initial_museum, super_event, event, event_is_instance_of, event_type, collection
OPTIONAL MATCH (event)-[:HAS_SENDER]->(sender:Actor)-[:INSTANCE_OF]->(sender_type:Type)
WITH initial_museum, super_event, event, event_is_instance_of, event_type, collection, sender, sender_type
OPTIONAL MATCH (event)-[:HAS_RECIPIENT]->(recipient:Actor)-[:INSTANCE_OF]->(recipient_type:Type)
WITH initial_museum, super_event, event, event_is_instance_of, event_type, collection, sender, sender_type, recipient, recipient_type
OPTIONAL MATCH (event)-[:HAS_ORIGIN]->(origin:Place)
WITH initial_museum, super_event, event, event_is_instance_of, event_type, collection, sender, sender_type, recipient, recipient_type, origin
OPTIONAL MATCH (event)-[:HAS_DESTINATION]->(destination:Place)
WITH initial_museum, super_event, event, event_is_instance_of, event_type, collection, sender, sender_type, recipient, recipient_type, origin, destination
OPTIONAL MATCH (sender)-[:HAS_LOCATION]->(sender_location:Place)
WITH initial_museum, super_event, event, event_is_instance_of, event_type, collection, sender, sender_type, recipient, recipient_type, origin, destination, sender_location
OPTIONAL MATCH (recipient)-[:HAS_LOCATION]->(recipient_location:Place)
WITH initial_museum, super_event, event, event_is_instance_of, event_type, collection, sender, sender_type, recipient, recipient_type, origin, destination, sender_location, recipient_location
OPTIONAL MATCH (initial_museum)-[:HAS_LOCATION]->(initial_museum_location:Place)
RETURN

    initial_museum.mm_id AS initial_museum_id,
    initial_museum.actor_name AS initial_museum_name,
    initial_museum.size AS initial_museum_size,
    initial_museum.governance AS initial_museum_governance,
    initial_museum.accreditation AS initial_museum_accreditation,
    initial_museum.subject AS initial_museum_subject,
    initial_museum_location.village_town_city AS initial_museum_town,
    initial_museum.region AS initial_museum_region,
    initial_museum_location.postcode AS initial_museum_postcode,

    super_event.super_event_id AS super_event_id,
    super_event.super_date AS super_event_date,
    super_event.super_cause_types AS super_event_reasons,

    event.event_id AS event_id,
    event.stage_in_path AS event_stage_in_path,
    event_type.type_name AS event_type,
    [
      (event_type)-[:SUB_TYPE_OF*0..]->(event_core_type:Type {is_core_category: TRUE})
      | event_core_type.type_name
    ][0] AS event_core_type,
    event_is_instance_of.event_type_uncertainty AS event_type_uncertainty,
    event_type.end_of_existence AS event_is_end_of_existence,
    event_type.change_of_ownership AS event_is_change_of_ownership,
    event_type.change_of_custody and exists((event)-[:HAS_DESTINATION]->()) AS event_is_change_of_custody,
    event.event_date AS event_date,
    event.event_date_from AS event_date_from,
    event.event_date_to AS event_date_to,
    [
      (previous_event)-[:PRECEDES]->(event) | previous_event.event_id
    ][0] AS previous_event_id,

    sender.actor_id AS sender_id,
    sender.actor_name AS sender_name,
    sender.mm_id AS sender_mm_id,
    sender_type.type_name AS sender_type,
    [
      (sender_type)-[:SUB_TYPE_OF*0..]->(sender_core_type:Type {is_core_category: TRUE})
      | sender_core_type.type_name
    ][0] AS sender_core_type,
    sender.actor_sector_name AS sender_sector,

    recipient.actor_id AS recipient_id,
    recipient.actor_name AS recipient_name,
    recipient.mm_id AS recipient_mm_id,
    recipient.actor_quantity AS recipient_quantity,
    recipient_type.type_name AS recipient_type,
    [
      (recipient_type)-[:SUB_TYPE_OF*0..]->(recipient_core_type:Type {is_core_category: TRUE})
      | recipient_core_type.type_name
    ][0] AS recipient_core_type,
    recipient.actor_sector_name AS recipient_sector,
    recipient.size AS recipient_size,
    recipient.governance AS recipient_governance,
    recipient.accreditation AS recipient_accreditation,
    recipient.subject AS recipient_subject,
    recipient_location.village_town_city AS recipient_town,
    recipient.region AS recipient_region,
    recipient_location.postcode AS recipient_postcode,

    collection.collection_or_object_id AS object_id,
    [
      (collection)-[:WAS_REMOVED_FROM]->(parent_collection:CollectionOrObject) | parent_collection.collection_or_object_id
    ][0] AS parent_object_id,
    collection.coll_size_name AS object_size,
    collection.object_qty AS object_quantity,
    collection.collection_status AS object_status,
    collection.coll_type AS object_types,
    collection.coll_desc AS object_description
"""


if __name__ == "__main__":
    with open("config.json") as f:
        config = json.load(f)
        credentials_file_name = config["credentials_file"]

    queries = {
        # for top-level data directories:
        "data-model/actor_types": actor_types_data_dump_query,
        "data-model/event_types": event_types_data_dump_query,
        "data/closure_data/dispersal_events": dispersal_events_data_dump_query,
        # for shiny app inputs:
        "shiny/mappingmuseums/data/query_results/actor_types": actor_types_query,
        "shiny/mappingmuseums/data/query_results/event_types": event_types_query,
        "shiny/mappingmuseums/data/query_results/museums": museums_query,
        "shiny/mappingmuseums/data/query_results/super_events": super_events_query,
        "shiny/mappingmuseums/data/query_results/dispersal_events": dispersal_events_query,
    }

    query_to_csv = QueryToCsv(
        queries,
        credentials_file_name=credentials_file_name,
        output_directory_name=RESULTS_DIR,
    )

    query_to_csv.make_queries_and_save_outputs()
