top_home <- "<p>This database brings together information about all museums in the UK since 1960. You can search, visualise, and download information on:</p>
<ul>
  <li>Museums open since 1960: their governance, location, size, subject matter, accreditation status, years of opening and closing, among other characteristics</li>
  <li>Changes within the museum sector since 1960</li>
  <li>Museums that have closed since 2000; reasons for closure and the main outcomes of closure (e.g. stored, transferred)</li>
  <li>Object disposal: where objects from closed museums went</li>
</ul>"

top_database <- "<p>Which museums have been open in the UK in the period since 1960? This search allows you to filter museums by size, governance, accreditation, subject matter, and location and view the results as a list.</p>"

top_glossary <- "<p>Information about museums, their closure, and collections dispersal is organised into taxonomies. These taxonomies underpin all the visualisations and analyses available on this web app. In all cases the main categories of our analyses are marked by a bold circle, the sub-categories by a grey circle.</p>
<p>The taxonomies we have designed and used are as follows:</p>
<ul>
  <li><a href='#size'>Museum size</a></li>
  <li><a href='#governance'>Museum governance</a></li>
  <li><a href='#subject'>Museum subject matter</a></li>
  <li><a href='#reasons'>Reasons for closure</a></li>
  <li><a href='#actors'>Actors involved in collection dispersal</a></li>
  <li><a href='#events'>Collection dispersal events</a></li>
</ul>"

top_data <- "<p>We collected a varied amount of data on each museum. The tables below show how the amount of recorded data differed according to groups of object, events, and subject matter.</p>
<p>The charts include:</p>
<ul>
  <li><a href='#eventsPerMuseumMatrixTitle'>Groups of objects recorded vs events recorded</a></li>
  <li><a href='#eventsPerMuseumBoxplotsTitle'>Recording events per museum by subject matter</a></li>
  <li><a href='#eventsPerCollectionTitle'>Recording events by groups of objects according to subject matter</a></li>
  <li><a href='#collectionGranularityTitle'>Recording quantities of objects</a></li>
  <li><a href='#collectionGranularityHeatmapTitle'>Quantities of objects recorded according to subject matter</a></li>
</ul>"

top_snapshot <- "<p>What does the UK museum sector currently look like? What did it contain? This search enables you to visualise the distribution of museums across the UK in a single year or during a specified period.</p>
<p>Click the thumbnail image to select your preferred visualisation. You can refine your search by using View and Filters. Click the <i class='fa-solid fa-circle-info' style='color: #007bff'></i> buttons for guidance.</p>
<p>Details of open museums returned by the search are listed below the charts.</p>"

top_changes <- "<p>How has the UK museum sector changed since 1960?  This search enables you to visualise museum openings and closings over time.</p>
<p>Click the thumbnail image to select your preferred visualisation. You can refine your search by using View and Filters. Click the <i class='fa-solid fa-circle-info' style='color: #007bff'></i> buttons for guidance.</p>
<p>Details of opened and closed museums returned by the search are listed below the charts.</p>"

top_reasons <- "<p>Why do museums close? These visualisations show reasons why museums have closed since 2000. The numbers relate to reasons cited not to the number of museums (Some museums gave more than one reason).</p>
<p>Click the thumbnail image to select your preferred visualisation. You can refine your search by using View and Filters. Click the <i class='fa-solid fa-circle-info' style='color: #007bff'></i> buttons for guidance.</p>
<p>Details of museums returned by the search are listed below the charts.</p>"

top_outcomes <- "<p>What happens to the collections when museums close? These visualisations provide an overview of the outcomes. Here collections are understood to be the entirety of a museum’s holdings. The numbers relate to individual museums.</p>
<p>Click the thumbnail image to select your preferred visualisation. You can refine your search by using View and Filters. Click the <i class='fa-solid fa-circle-info' style='color: #007bff'></i> buttons for guidance.</p>
<p>Details of museums returned by the search are listed below the charts.</p>"

top_events <- "<p>What happens to museum collections after museums close? These visualisations show the various events that occur after closure. The numbers relate to recorded events, not to individual museums.</p>
<p>Click the thumbnail image to select your preferred visualisation. You can refine your search by using View and Filters. Click the <i class='fa-solid fa-circle-info' style='color: #007bff'></i> buttons for guidance.</p>
<p>Details of events returned by the search are listed below the charts.</p>"

top_dispersal <- "<p>Who are the recipients of collections from closed museums? These visualisations show where objects go after closure.</p>
<p>Click the thumbnail image to select your preferred visualisation. The pathways visualisation shows where objects from a single museum type go; the sequences visualisation enables the comparison of different museum types.</p>
<p>The numbers relate to actors, that is, to the people, groups, and organisations involved in object dispersal. A plus sign next to a number means 'at least'. For instance, a circle marked 81+ trader means that objects went to at least 81 traders.</p>
<p>You can refine your search by using View and Filters. Click the buttons for guidance.</p>
<p>Details of events returned by the search are listed below the charts.</p>"

top_length <- "<p>How long does it take to dispose of the collection after a museum closes? These visualisations plot the time between each museum closing and the last object(s) being disposed of or moved.</p>
<p>Click the thumbnail image to select your preferred visualisation. You can refine your search by using View and Filters. Click the <i class='fa-solid fa-circle-info' style='color: #007bff'></i> buttons for guidance.</p>
<p>Details of museums returned by the search are listed below the charts.</p>"

db_tooltip_search <- "<p>Enter free text to search all fields of the Mapping Museums database.</p>"
tooltip_museum_country <- "<p>The country or territory where the museum is located.</p>"
tooltip_address <- "<p>Search for museums by their address. Enter any part of the address - e.g. road, village, town, county, postcode.</p>"
tooltip_local_authority_district <- "<p>The local authority district (2023 boundaries) where the museum is located.</p>"
tooltip_existence_or_open_close <- "<p>Filter museums according to when they were open (they opened before or during the time period and closed during or after the time period) or according to their opening and closure dates (specify the time period during which their opening occurred and the time period during which their closure occurred).</p>"
tooltip_show_columns <- "<p>Select which columns should appear in the results table.</p>"

tooltip_view <- "<p>Controls the overall parameters of the visualisations.</p>"
tooltip_filter <- "<p>Allows you to refine your searches. E.g. you might choose to view museums according to size, and then further select according to governance.</p>"

tooltip_single_or_range <- "<p><strong>Single year: </strong>Shows museums open in a specified year.</p>
<p><strong>Range of years: </strong>Shows museums open in a range of years.</p>
<p>Use the slider below to choose which year or years.</p>"

tooltip_main_attribute <- "Show all museums or museums by selected attribute"

tooltip_secondary_attribute <- "Choose a second attribute to view as the horizontal axis on the heatmap"

tooltip_main_attribute_outcomes <- "<p>Select which outcomes to display</p>
<p><strong>Event:</strong> the main outcome for museum objects</p>
<p><strong>Recipient:</strong> who/what took possession of the museum objects</p>
<p><strong>Recipient count:</strong> how many recipients took possession of the museum objects</p>
<p><strong>Recipient share:</strong> how the museum objects were split</p>
<p><strong>Destination:</strong> how far the museum objects were moved</p>"

tooltip_main_attribute_events <- "<p>Select search for the vertical axis</p>
<p><strong>Event:</strong> what happened to museum objects</p>
<p><strong>Sender:</strong> where the objects came from</p>
<p><strong>Recipient:</strong> who/what took possession of the museum objects</p>
<p><strong>Object type:</strong> the specific item disposed of</p>
<p><strong>Initial museum:</strong> where the objects originally came from</p>"

tooltip_secondary_attribute_events <- "<p>Select search for the horizontal axis</p>
<p><strong>Event:</strong> what happened to museum objects</p>
<p><strong>Sender:</strong> where the objects came from</p>
<p><strong>Recipient:</strong> who/what took possession of the museum objects</p>
<p><strong>Object type:</strong> the specific item disposed of</p>
<p><strong>Initial museum:</strong> where the objects originally came from</p>"

tooltip_steps_or_last <- "<p><strong>Steps in path:</strong> View intermediate events in the sequences of events</p><p><strong>Last known:</strong> View only the last known event in the sequence.</p>"

tooltip_steps_or_first_last <- "<p>Museum objects sometimes move several times.</p>
<p><strong>Steps:</strong> View all or intermediate steps in the objects’ paths</p>
<p><strong>First and last actors:</strong> View the initial museum and last known actor in the objects’ path</p>"

tooltip_steps_in_path <- "<p>Select which steps to view in the objects’ paths. Step one shows the initial museum in the sequence. Use the slider to increase the number of steps shown.</p>"

tooltip_count_or_percentage_events <- "<p><strong>Number of events:</strong> a simple count</p>
<p><strong>Percentage of events:</strong> as a proportion of all events</p>
<p><strong>Rowwise percentages:</strong> percentage of all the events in that category, as specified on the vertical Y-axis</p>
<p><strong>Columnwise percentages:</strong> percentage of all the events in that category, as specified on the horizontal X-axis</p>"

tooltip_museum_governance <- "<p>Shows museums of a specific governance type.</p>
<p>Governance is the framework by which museums are directed and controlled. It identifies who can make decisions, who has the authority to act on behalf of the organisation, and who is accountable for how the museum and its people perform.</p>
<p>See the Taxonomies tab for the breakdown of governance types.</p>"

tooltip_museum_size <- "<p>Shows museums of a specific size</p>
<p>Estimated according to the annual number of visits.</p>
<p><strong>Small: </strong>0 - 10,000 annual visitors.</p>
<p><strong>Medium: </strong>10,000 - 50,000 annual visitors</p>
<p><strong>Large: </strong>50,000 - 1,000,000 annual visitors.</p>
<p><strong>Huge: </strong>More than 1,000,000 annual visitors.</p>"

tooltip_museum_subject <- "<p>Shows museums that focus on a specific subject.</p>
<p>Subject matter relates to the overall theme of the museum. See the Taxonomies tab for a breakdown of topics.</p>"

tooltip_museum_subject_specific <- "<p>Shows museums that focus on a specific subject at the level of sub-categories. See the Taxonomies tab for a breakdown of topics.</p>"

tooltip_museum_country_region <- "<p>Shows museums located in a specific nation or region.</p>"

tooltip_museum_accreditation <- "<p>Shows accredited and / or unaccredited museums. Accreditation is the bench-marking process whereby museums are recognised as reaching nationally agreed standards.</p>"

tooltip_stepwise_events <- "<p>Select which events to view in the objects' history. Step 1 is the first event, step 2 is the second event, and so on.</p>"

tooltip_collection_type <- "<p>Select specific types of objects</p>"

tooltip_collection_status <- "<p><strong>Objects from a museum collection:</strong> Artefacts that have been accessioned and form part of the museum collection</p>
<p><strong>Object on loan to a museum:</strong> Artefacts on loan and in the custody of the museum when it closed</p>
<p><strong>Handling objects:</strong> Non-accessioned objects used for educational purpose</p>
<p><strong>Other objects (e.g. display cases):</strong> Non-accessioned objects belonging to the museum including gallery furniture and set dressing.</p>"

tooltip_reason_type_level <- "<p>Select the level of detail. See the Taxonomies tab for a breakdown of all reasons for closure.</p>"

tooltip_group_events_level <- "<p>Select the level of detail. See the Taxonomies tab for a breakdown of all events</p>"

tooltip_group_actors_level <- "<p>Select the level of detail. See the Taxonomies tab for a breakdown of all actors involved in collection disposal</p>"

tooltip_group_museums_by <- "<p>Show all museums or show museums selected by museum attribute</p>"

tooltip_reason_filter <- "<p>Show only museums where at least one of their reasons for closure belongs to the selected reason core categories.</p>"

tooltip_event_types <- "Choose events types: See Taxonomies tab for more detail"

tooltip_sender_types <- " <p>Show all senders or show senders of a particular type.</p>"

tooltip_recipient_types <- " <p>Show all recipients or show recipients of a particular type.</p>"

tooltip_show_only_outcomes <- "<p>Select which outcomes should appear in the visualizations. Removing some outcomes could improve the readability of charts.</p>"

tooltip_include_firepower <- "Transactions involving collections originating from Firepower are automatically excluded from the diagrams. Switch on in order to include them."

tooltip_group_actors_by <- "<p>Select how to group and display actors on the diagram.</p>
<p>Other actors can be grouped according to:</p>
<p><strong>Actor sector:</strong> The sector of the economy (<i>public</i>, <i>private</i>, <i>third</i>, <i>etc.</i>) that they belong to.</p>
<p><strong>Most specific actor type:</strong> The most specific actor type that actors are known to belong to.</p>
<p><strong>Core category actor type:</strong> The core category that they belong to. Refer to the actor types hierarchy to see which types are included as core categories.</p>
<p><strong>Actor country/region:</strong> The country or UK region where actors are located.</p>"

tooltip_transaction_types <- "<p>Select which transactions should appear on the diagram.</p>
<p>Most <strong>changes of ownership</strong> are also changes of custody, but occasionally an item is sold without being sent to its new owner.</p>
<p><strong>Changes of custody</strong> include a wider range of movements than changes of ownership (e.g. <i>loaned</i> and <i>lost/stolen</i>).</p>
<p><strong>End of existence</strong> is represented as a transfer to no recipient.</p>"

tooltip_event_uncertainty <- "<p>Filter for events with certain or uncertain types.</p><p><strong>Certain: </strong>Events where the type of event is certain.</p><p><strong>?+: </strong>Events where the type of event is highly likely.</p><p><strong?: </strong>Events where the type of event is probable.</p><p><strong>?-: Events where the type of event is possible.</p>"

tooltip_initial_museum <- "<p>The initial closed museums from which the depicted sequences begin.</p><p>This field updates with a list of museums according to the filters below.</p><p>It is possible to search for and select an individual museum so that only collection transfers starting at that museum are shown in the diagram.</p>"

tooltip_final_destination <- "<p>The final actor in the sequence of transfers. The values in this field update according to how actors are grouped on the diagram.</p>"

tooltip_passes_through <- "<p>Filter sequences that only pass through specified actors at some point in the sequence of transfers. The values in this field update according to how actors are grouped on the diagram.</p>"

tooltip_example_museum <- "<p>Select an individual museum to display its closure timeline.</p>"
