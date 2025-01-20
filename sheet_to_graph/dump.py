"""
This script queries the neo4j database and converts it into a single large csv file
"""

from collections import Counter
import csv
import json

from sheet_to_graph.connection_managers import QueryToCsv

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
RETURN {
    type_name: type_name,
    sub_type_of: sub_type_of,
    is_core_category: is_core_category,
    total_instances: total_instances,
    public_instances: public_instances,
    private_instances: private_instances,
    third_instances: third_instances,
    university_instances: university_instances,
    hybrid_instances: hybrid_instances,
    unknown_instances: unknown_instances
} AS record
"""

event_types_query = """
MATCH (event_type:Type)-[:SUB_TYPE_OF*0..]->(:Type {type_name: "event"})
WITH event_type
OPTIONAL MATCH (event_type)-[:SUB_TYPE_OF]->(super_type:Type)-[:SUB_TYPE_OF*0..]->(:Type {type_name: "event"})
RETURN {
    type_name: event_type.type_name,
    sub_type_of: super_type.type_name,
    is_core_category: event_type.is_core_category,
    change_of_ownership: event_type.change_of_ownership,
    change_of_custody: event_type.change_of_custody,
    end_of_existence: event_type.end_of_existence,
    definition: event_type.definition
} as record
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
RETURN {
    initial_museum_id: initial_museum.mm_id,
    initial_museum_name: initial_museum.actor_name,
    initial_museum_size: initial_museum.size,
    initial_museum_governance_broad: initial_museum.governance_broad,
    initial_museum_governance: initial_museum.governance,
    initial_museum_sector: initial_museum.actor_sector_name,
    initial_museum_accreditation: initial_museum.accreditation,
    initial_museum_subject_matter_broad: initial_museum.subject_matter_broad,
    initial_museum_subject_matter: initial_museum.subject_matter,
    initial_museum_country: initial_museum.country,
    initial_museum_region: initial_museum.region,
    initial_museum_town: initial_museum_location.village_town_city,
    initial_museum_type: [
      (initial_museum)-[:INSTANCE_OF]->(initial_museum_type:Type)
      | initial_museum_type.type_name
    ][0],
    initial_museum_core_type: [
      (initial_museum)-[:INSTANCE_OF]->(initial_museum_type:Type)-[:SUB_TYPE_OF*0..]->(initial_museum_core_type:Type {is_core_category: TRUE})
      | initial_museum_core_type.type_name
    ][0],
    initial_museum_general_type: [
      (initial_museum)-[:INSTANCE_OF]->(initial_museum_type:Type)-[:SUB_TYPE_OF*0..]->(initial_museum_general_type:Type)-[:SUB_TYPE_OF]->(:Type {type_name: "actor"})
      | initial_museum_general_type.type_name
    ][0],
    super_event_id: super_event.super_event_id,
    super_event_causes: super_event.super_causes,
    super_event_cause_types: super_event.super_cause_types,
    super_event_date: super_event.super_date,
    event_id: event.event_id,
    previous_event_id: [
      (previous_event)-[:PRECEDES]->(event) | previous_event.event_id
    ][0],
    ancestor_events: [
      (event)<-[:PRECEDES*0..]-(ancestor_event:Event) | ancestor_event.event_id
    ],
    event_type: event_type.type_name,
    event_core_type: [
      (event_type)-[:SUB_TYPE_OF*0..]->(event_core_type:Type {is_core_category: TRUE})
      | event_core_type.type_name
    ][0],
    event_type_uncertainty: event_is_instance_of.event_type_uncertainty,
    event_date: event.event_date,
    event_date_from: event.event_date_from,
    event_date_to: event.event_date_to,
    event_stage_in_path: event.stage_in_path,
    event_is_change_of_ownership: event_type.change_of_ownership,
    event_is_change_of_custody: event_type.change_of_custody and exists((event)-[:HAS_DESTINATION]->()),
    event_is_end_of_existence: event_type.end_of_existence,
    collection_id: collection.collection_or_object_id,
    parent_collection_id: [
      (collection)-[:WAS_REMOVED_FROM]->(parent_collection:CollectionOrObject) | parent_collection.collection_or_object_id
    ][0],
    ancestor_collections: [
      (collection)-[:WAS_REMOVED_FROM*0..]->(ancestor_collection:CollectionOrObject) | ancestor_collection.collection_or_object_id
    ],
    original_collection_id: [
      (collection)-[:WAS_REMOVED_FROM*0..]->(original_collection:CollectionOrObject)<-[:INVOLVES]-(original_event {stage_in_path: 0})
      | original_collection.collection_or_object_id
    ][0],
    collection_description: collection.coll_desc,
    collection_types: collection.coll_type,
    collection_status: collection.collection_status,
    collection_size: collection.coll_size_name,
    collection_estimated_size: collection.estimated_size,
    collection_estimated_size_min: collection.min_estimated_size,
    collection_estimated_size_max: collection.max_estimated_size,
    sender_id: sender.actor_id,
    sender_name: sender.actor_name,
    sender_size: sender.size,
    sender_governance: sender.governance,
    sender_governance_broad: sender.governance_broad,
    sender_accreditation: sender.accreditation,
    sender_subject: sender.subject_matter_broad,
    sender_region: sender.region,
    sender_country: sender.country,
    sender_town: sender_location.village_town_city,
    sender_county: sender_location.county,
    sender_postcode: sender_location.postcode,
    sender_quantity: sender.actor_quantity,
    sender_sector: sender.actor_sector_name,
    sender_type: sender_type.type_name,
    sender_core_type: [
      (sender_type)-[:SUB_TYPE_OF*0..]->(sender_core_type:Type {is_core_category: TRUE})
      | sender_core_type.type_name
    ][0],
    sender_general_type: [
      (sender_type)-[:SUB_TYPE_OF*0..]->(sender_general_type:Type)-[:SUB_TYPE_OF]->(:Type {type_name: "actor"})
      | sender_general_type.type_name
    ][0],
    recipient_id: recipient.actor_id,
    recipient_name: recipient.actor_name,
    recipient_governance: recipient.governance,
    recipient_governance_broad: recipient.governance_broad,
    recipient_town: recipient_location.village_town_city,
    recipient_county: recipient_location.county,
    recipient_postcode: recipient_location.postcode,
    recipient_quantity: recipient.actor_quantity,
    recipient_sector: recipient.actor_sector_name,
    recipient_type: recipient_type.type_name,
    recipient_core_type: [
      (recipient_type)-[:SUB_TYPE_OF*0..]->(recipient_core_type:Type {is_core_category: TRUE})
      | recipient_core_type.type_name
    ][0],
    recipient_general_type: [
      (recipient_type)-[:SUB_TYPE_OF*0..]->(recipient_general_type:Type)-[:SUB_TYPE_OF]->(:Type {type_name: "actor"})
      | recipient_general_type.type_name
    ][0],
    origin_id: origin.place_id,
    origin_latitude: origin.latitude,
    origin_longitude: origin.longitude,
    origin_x: origin.bng_x,
    origin_y: origin.bng_y,
    origin_region: origin.region,
    origin_country: origin.country,
    destination_id: destination.place_id,
    destination_latitude: destination.latitude,
    destination_longitude: destination.longitude,
    destination_x: destination.bng_x,
    destination_y: destination.bng_y,
    destination_region: destination.region,
    destination_country: destination.country
} as record
"""


if __name__ == "__main__":
    with open("config.json") as f:
        config = json.load(f)
        credentials_file_name = config["credentials_file"]

    queries = {
        "actor_types": actor_types_query,
        "event_types": event_types_query,
        "dispersal_events": dispersal_events_query,
    }

    query_to_csv = QueryToCsv(
        queries,
        credentials_file_name=credentials_file_name,
        output_directory_name="../data/query_results",
    )

    query_to_csv.make_queries_and_save_outputs()
