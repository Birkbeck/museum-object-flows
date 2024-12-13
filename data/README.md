# Data

This directory contains the data required to set-up the Neo4j database with the `sheet_to_graph` tool.

- `dispersal-sheet-yyyy-mm-dd.xlsx`: The dispersal sheet is an Excel file containg data concerning the flows of objects and the actors involved. This is used as input to the Neo4j database.
- `mapping-museums-yyyy-mm-dd.csv`: This is a CSV data dump from the [Mapping Museums Database](https://museweb.dcs.bbk.ac.uk/aboutapp)
- `query_results`: This directory contains CSV files of object flow data dumped from the Neo4j database.
- `ONSPD_FEB_2024_UK`: You need to create this directory and place in it the csv collection from the [ONS postcode directory](https://geoportal.statistics.gov.uk/datasets/e14b1475ecf74b58804cf667b6740706/about)

## The Dispersal Spreadsheet

The dispersal sheet contains a number of worksheets: `events`, `actors`, `event-types-hierarchy`, `actor-types-hierarchy`, `default-recipient-types`, `closure-causes-hierarchy`.

### The Events Sheet

The events sheet contains the following columns:

| **Column**           | **Description**                                                                                   |
|-----------------------|---------------------------------------------------------------------------------------------------|
| Museum ID            | The ID of the museum in the Mapping Museums database                                              |
| **SuperEvent**        |                                                                                                   |
| SuperEvent ID        | A unique identifier for the super event.                                                          |
| SuperEvent Type      | In all cases so far, this has been the *closure* of a museum                                      |
| SuperEvent Date      | A date in Extended Date/Time Format (EDTF)                                                        |
| SuperEvent Causes    | List of the causes of the museum closure                                                          |
| **Collection**        |                                                                                                   |
| Collection ID        | A unique identifier for the collection or object.                                                 |
| Object Quantity      | The precise number of objects. If 1, the row refers to an object, not a collection                |
| Collection Size      | Approximate proportion of the museum's total collection                                           |
| Collection Type      | A list of Wikidata resources describing the collection contents                                   |
| Collection Instance  | A list of Wikidata resources of specific individual objects                                       |
| Collection Description | A textual description of the collection                                                        |
| Collection Status    | Whether the museum is part of the museum collection; a loan; handling objects; or museum furniture |
| Contains            | The ID of a collection that is contained within this collection but has not been removed from it  |
| Removed from         | The ID of a super-collection that this collection was removed from                                |
| Received from        | The ID of a collection that was merged into this one                                              |
| Is same as           | The ID of an identical collection that is referred to with a different ID elsewhere in the sheet  |
| **Event**             |                                                                                                   |
| Event ID             | A unique identifier for the event.                                                                |
| Event Type           | A type from the event types hierarchy                                                             |
| Event Date           | The date of the event in EDTF (if the event happens at a point in time)                           |
| Event Date From      | The start of the event in EDTF (if the event lasts for an extended period of time)                |
| Event Date To        | The end of the event in EDTF (used with Event Date From)                                          |
| Recipient ID         | The ID of the actor who receives the collection                                                   |
| Stays                | A boolean value to indicate that if the collection in an event which would usually entail movement, in this instance stayed at its origin location. |
| Address fields       | Used if the collection does not move to the location of the actor                                 |
| Notes                | Additional notes relating to the event or super event                                             |

### The Actors Sheet

The actors sheet contains the following columns:

| **Column**        | **Description**                                                                                   |
|--------------------|---------------------------------------------------------------------------------------------------|
| Actor ID           | A unique identifier for the actor.                                                               |
| Actor name         | The actor's name.                                                                                |
| Actor quantity     | How many entities this actor represents.                                                         |
| Actor type         | A type from the actor types hierarchy.                                                           |
| Actor sector       | Which sector of the economy the actor belongs to.                                                |
| Museum ID          | If the actor is a UK museum, its ID in the Mapping Museums database.                             |
| Address fields     | The address of the actor, including street, city, postcode, country.                             |

### The Event Types Hierarchy

The event types sheet contains the following columns:

| **Column**              | **Description**                                                                    |
|--------------------------|-----------------------------------------------------------------------------------|
| Type name               | A descriptive name for the type.                                                   |
| Sub-type of             | The name of this type's super-type.                                                |
| Change of ownership?    | True if this type involves a transfer of legal ownership, false otherwise.         |
| Change of custody?      | True if this type involves a change of location, false otherwise.                  |
| End of existence?       | True if this type results in an object no longer existing, false otherwise.        |

### The Actor Types Hierarchy

The actor types sheet contains the following columns:

| **Column**       | **Description**                                                                      |
|-------------------|-------------------------------------------------------------------------------------|
| Type name         | A descriptive name for the type.                                                    |
| Sub-type of       | The name of this type's super-type.                                                 |
| Core category?    | True if this type is a core category, false otherwise.                              |

### The Default Recipient Types Sheet

The default recipient types sheet matches event types with default recipient actor types that should be inferred where the recipient is unknown. It contains the following columns:

| **Column**             | **Description**                                                                  |
|------------------------|----------------------------------------------------------------------------------|
| Event type             | The name of the event type.                                                      |
| Default recipient type | The name of this actor type that recipients are assumed by default to belong to. |

### The Closure Causes Hierarchy

The closure causes hierarchy maps unstructured notes in the SuperEvent Causes field onto a controlled vocabulary and hierarchy of types. It contains the following columns:

| **Column**             | **Description**                                                                          |
|------------------------|------------------------------------------------------------------------------------------|
| super cause text       | Text as it appears in the SuperEvent causes column.                                      |
| cause                  | The type from the lowest level in the closure causes hierarchy that they text maps onto. |
| cause type             | The type from the mid-level of the closure causes hierarchy.                             |
| cause super type       | The type from the top-level of the closure causes hierarchy.                             |
