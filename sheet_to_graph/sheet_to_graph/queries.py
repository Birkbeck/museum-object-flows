class Queries:
    # Delete all nodes and relations
    delete_everything = """
MATCH (n)
OPTIONAL MATCH (n)-[r]-()
DELETE n, r
"""

    # Create type node and connect to parent type
    create_type = """
MERGE (t:Type {name: $type_name, is_basic_level: $is_basic_level})
"""

    # Connect type node to its parent type in type hierarchy
    connect_to_super_type = """
MATCH (t:Type {name: $type_name})
with t
MATCH (parent:Type {name: $sub_type_of})
MERGE (t)-[:SUB_TYPE_OF]->(parent)
"""

    # Create museum node
    # TODO: add other museum attributes, eg governance, geographic, demographic classifications
    create_museum = """
MERGE (a:Actor {mm_id: $museum_id})
SET a.name = $name_of_museum, a.mm_id = $museum_id, a.sector = $actor_sector_name, a.is_uk_based = TRUE,
a.address_1 = $address_line_1, a.address_2 = $address_line_2, a.town_city = $village_town_or_city,
a.postcode = $postcode, a.country = "United Kingdom"
WITH a
MATCH (t:Type {name: "museum"})
MERGE (a)-[:INSTANCE_OF {uncertainty: "certain"}]->(t)
"""

    # Create actor node
    create_actor = """
MERGE (a:Actor {actor_id: $actor_id})
SET a.name = $actor_name, a.mm_id = $mm_id, a.sector = $actor_sector_name, a.is_uk_based = $is_uk_based,
a.address_1 = $actor_address1, a.address_2 = $actor_address2, a.town_city = $actor_town_city,
a.county = $actor_county, a.postcode = $actor_postcode, a.country = $actor_country, a.location = $actor_location
WITH a
MATCH (t:Type {name: $actor_type_name})
MERGE (a)-[:INSTANCE_OF {uncertainty: $actor_type_uncertainty}]->(t)
"""

    add_museum_attributes_to_actor = """
OPTIONAL MATCH (a: Actor {mm_id: $museum_id})
SET a.governance = $governance, a.size = $size, a.size_num = $size_num, a.subject_matter = $subject_matter, a.sector = $sector
"""

    # Create super event node and connect to museum node
    create_super_event = """
MERGE (se:SuperEvent {super_event_id: $super_event_id})
SET se.name = $super_event_name, se.super_date = $super_date,
se.super_causes = $super_causes, se.notes = $notes
WITH se
MATCH (m:Actor {mm_id: $museum_id})
MERGE (se)-[:CONCERNS]->(m)
WITH se
MATCH (t:Type {name: $super_event_type})
MERGE (se)-[:INSTANCE_OF]->(t)
"""

    # Create collection and connect to super collection if applicable
    create_collection = """
MERGE (c:Collection {collection_id: $collection_id})
ON CREATE SET c.name = $collection_id, c.size = $coll_size_name, c.size_num = $coll_size_num,
c.quantity = $object_qty, c.estimated_size = $object_qty,
c.wiki_type = $coll_type, c.wiki_type_url = $ coll_wiki_type_url,
c.wiki_instance = $coll_wiki_instance, c.wiki_instance_url = $coll_wiki_instance_url,
c.description = $coll_desc, c.status = $coll_status
WITH c
MATCH (sc:Collection {collection_id: $coll_subset_of})
MERGE (sc)-[:CONTAINS]->(c)
"""

    # Calculate an estimated collection size according to the descriptors for collection and museum size
    # unless collection has a specified quantity
    infer_collection_sizes = """
MATCH (c:Collection {quantity: ""})<-[:HAS_OBJECT]-(:Event)-[:SUB_EVENT_OF]->(:SuperEvent)-[:CONCERNS]->(m:Actor)
WITH c, m
SET c.estimated_size = c.size_num * m.size_num
"""

    # Create event and connect to super event, and preceding event if applicable
    create_event = """
MERGE (e:Event {event_id: $event_id})
SET e.name = $event_name,
e.date = $event_date, e.date_from = $event_date_from,
e.date_to = $event_date_to, e.stage_in_path = $stage_in_path
WITH e
MATCH (se:SuperEvent {super_event_id: $super_event_id})
MERGE (e)-[:SUB_EVENT_OF]->(se)
WITH e
MATCH (t:Type {name: $event_type_name})
MERGE (e)-[:INSTANCE_OF {uncertainty: $event_type_uncertainty}]->(t)
WITH e
MATCH (pe:Event {event_id: $previous_event_id})
MERGE (pe)-[:PRECEDES]->(e)
"""

    # Connect collection as object of event
    connect_event_and_collection = """
MATCH (e:Event {event_id: $event_id})
MATCH (c:Collection {collection_id: $collection_id})
MERGE (e)-[:HAS_OBJECT]->(c)
"""

    # Connect actor recipient as recipient of event
    connect_event_and_recipient = """
MATCH (e:Event {event_id: $event_id})
MATCH (a:Actor {actor_id: $actor_recipient_id})
MERGE (e)-[:HAS_RECIPIENT]->(a)
"""

    # Connect sender to event
    connect_event_and_sender = """
MATCH (e:Event {event_id: $event_id})
MATCH (a:Actor {actor_id: $sender_id})
MERGE (e)-[:HAS_SENDER]->(a)
"""

    get_type_instance_counts = """
MATCH (i)-[:INSTANCE_OF]->(t:Type)-[:SUB_TYPE_OF*0..]-(actor:Type {name: $type_type})
RETURN
    t.name as type_name,
    count(i) as instance_count
"""

    get_all_transfer_event_details = """
MATCH (sender:Actor)<-[:HAS_SENDER]-(event:Event)-[:HAS_RECIPIENT]->(recipient:Actor)
WHERE sender <> recipient
WITH event, sender, recipient
MATCH (event)-[:HAS_OBJECT]->(collection:Collection)
MATCH (event)-[:INSTANCE_OF]->(eventType:Type)
CALL {
    WITH sender
    MATCH (sender)-[:INSTANCE_OF]->(Type)-[:SUB_TYPE_OF*0..]->(senderType:Type {is_basic_level: TRUE})
    WHERE NOT (senderType)<-[:SUB_TYPE_OF]-(:Type {is_basic_level: TRUE})
    RETURN senderType
}
CALL {
    WITH recipient
    MATCH (recipient)-[:INSTANCE_OF]->(Type)-[:SUB_TYPE_OF*0..]->(recipientType:Type {is_basic_level: TRUE})
    WHERE NOT (recipientType)<-[:SUB_TYPE_OF]-(:Type {is_basic_level: TRUE})
    RETURN recipientType
}
WITH event, eventType, sender, senderType, recipient, recipientType, collection
return 
    eventType.name AS event_type,
    event.stage_in_path AS stage_in_path,
    sender.actor_id AS sender_id,
    sender.governance AS sender_governance,
    senderType.name AS sender_type,
    recipient.actor_id AS recipient_id,
    recipient.governance AS recipient_governance,
    recipientType.name AS recipient_type,
    collection.estimated_size AS collection_size
"""

    get_all_event_paths_as_event_names = """
// Find all paths starting at stage 1 and ending at event without subsequent event
MATCH p = (e1 {stage_in_path: 1})-[r:PRECEDES*0..]->(en:Event)
WHERE NOT (en)-[:PRECEDES]->()
// Translate each path into a list of node names
WITH p, [node in nodes(p) | node.name] AS nodeNames
RETURN nodeNames
"""

    get_all_event_paths_as_event_types = """
// Find all paths starting at stage 1 and ending at event without subsequent event
MATCH p = (e1 {stage_in_path: 1})-[r:PRECEDES*0..]->(en:Event)
WHERE NOT (en)-[:PRECEDES]->()
// Translate each path into a list of event_types
WITH p, [node in nodes(p) | [(node)-[:INSTANCE_OF]->(t:Type) | t.name][0]] AS nodeNames
RETURN nodeNames
"""

    # get lists of actors that collections are transferred between
    # each element of the list is a list containing:
    # actor's id, actor's governance (for museums),
    # actor's type, actor's basic level type, actor's sector (public, private, etc)

    # re-used in below queries
    translate_path_into_actor_types = """
// Reusable stub of a query
// Translate each path into a list of actor types
WITH
[
    {
        actor: [(e1)-[:SUB_EVENT_OF]->(se:SuperEvent)-[:CONCERNS]->(a:Actor) | a][0],
        collection: [(e1)-[:HAS_OBJECT]->(c:Collection) | c][0]
    }
] AS superEvent,
[
    e in nodes(p) |
    [(c:Collection)<-[:HAS_OBJECT]-(e)-[:HAS_RECIPIENT]->(a:Actor) |
    {actor: a, collection: c}][0]
] AS events
RETURN
[
    e in superEvent + events |
    {
        actor_id: e["actor"].actor_id,
        governance: e["actor"].governance,
        actor_size: e["actor"].size,
        sector: e["actor"].sector,
        type: [(a: Actor {actor_id: e["actor"].actor_id})-[:INSTANCE_OF]->(t:Type) | t.name][0],
        basic_level_type: [
            (a: Actor {actor_id: e["actor"].actor_id})-[:INSTANCE_OF]->(:Type)-[:SUB_TYPE_OF*0..]->(t:Type {is_basic_level: TRUE}) | t.name
        ][0],
        collection_id: e["collection"].collection_id,
        collection_size: e["collection"].estimated_size
    }
] AS actors
"""

    get_all_event_paths_as_actor_types = f"""
// Find all paths starting at stage 1 and ending at event without subsequent event
MATCH p = (e1 {{stage_in_path: 1}})-[r:PRECEDES*0..]->(en:Event)
WHERE NOT (en)-[:PRECEDES]->()
{translate_path_into_actor_types}
"""

    get_all_event_paths_as_actor_types_from_start_gov = f"""
MATCH (e1: Event {{stage_in_path: 1}})-[:SUB_EVENT_OF]->(se: SuperEvent)-[:CONCERNS]->(m: Actor {{governance: $start_governance}})
WITH e1
MATCH p = (e1)-[r:PRECEDES*0..]->(en:Event)
WHERE NOT (en)-[:PRECEDES]->()
{translate_path_into_actor_types}
"""

    get_all_event_paths_as_actor_types_from_start_subj = f"""
MATCH (e1: Event {{stage_in_path: 1}})-[:SUB_EVENT_OF]->(se: SuperEvent)-[:CONCERNS]->(m: Actor)
WHERE m.subject_matter STARTS WITH $start_subject
WITH e1
MATCH p = (e1)-[r:PRECEDES*0..]->(en:Event)
WHERE NOT (en)-[:PRECEDES]->()
{translate_path_into_actor_types}
"""

    get_all_event_paths_as_actor_and_event_types = """
// Find all paths starting at stage 1 and ending at event without subsequent event
MATCH p = (e1 {stage_in_path: 1})-[r:PRECEDES*0..]->(en:Event)
WHERE NOT (en)-[:PRECEDES]->()
// Translate each path into a list of recipient types
WITH p,
    [node in nodes(p) |
        [
            [(node)-[:SUB_EVENT_OF]->(se:SuperEvent)-[:CONCERNS]->(a:Actor) | a.actor_id][0],
            [(node)-[:SUB_EVENT_OF]->(se:SuperEvent)-[:CONCERNS]->(a:Actor) | a.governance][0],
            [(node)-[:SUB_EVENT_OF]->(se:SuperEvent)-[:CONCERNS]->(a:Actor)-[:INSTANCE_OF]->(t:Type) | t.name][0]
        ]
    ][0] AS superEventMuseum,
    [node in nodes(p) |
        [
            [(node)-[:INSTANCE_OF]->(et:Type) | et.name][0],
            [(node)-[:HAS_RECIPIENT]->(a:Actor) | a.actor_id][0],
            [(node)-[:HAS_RECIPIENT]->(a:Actor) | a.governance][0],
            [(node)-[:HAS_RECIPIENT]->(a:Actor)-[:INSTANCE_OF]->(t:Type) | t.name][0]
        ]
    ] AS events_and_recipients
RETURN [superEventMuseum] + events_and_recipients AS actors_and_events
"""

    get_all_collection_types = f"""
MATCH (c:Collection)
RETURN c.wiki_type
"""

    changes_of_legal_ownership = """
MATCH (transfer:Event)-[:INSTANCE_OF]->(event_type {change_of_legal_ownership: TRUE})
WITH transfer, event_type
MATCH (sender)<-[:HAS_SENDER]-(transfer)-[:HAS_RECIPIENT]->(recipient)
RETURN {
    event_type: event_type.type_name,
    sender_type: [(sender)-[:INSTANCE_OF]->(sender_type)-[:SUB_TYPE_OF*0..]->(sender_basic_type {is_basic_level: TRUE}) | sender_basic_type.type_name][0],
    recipient_type: [(recipient)-[:INSTANCE_OF]->(recipient_type)-[:SUB_TYPE_OF*0..]->(recipient_basic_type {is_basic_level: TRUE}) | recipient_basic_type.type_name][0]
} as records
"""
