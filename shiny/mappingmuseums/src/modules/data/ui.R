dataUI <- function(id) {
  fluidPage(
    text_box("DATA-TOP"),

    h3("Events and Collections per Museum"),
    p(
      "When interpreting the data in these dashboards, it is important to bear in mind that data for each museum is recorded in different amounts and at a different levels of detail. Data for about a quarter of museums records just 1 event and 1 collection, but most have more than 1 event recorded."
    ),
    plotlyOutput(NS(id, "eventsPerMuseumMatrix"), width="80%", height="1000px"),

    p(
      "Why are there 84 museums with no events/collections data? 530 museums (including Channel Islands) definitely closed in or after 2000. Our database records 458 super events with at least one event. 5 of these super events are for museums that are no longer considered closed. 7 of these super events are for museums that might not have closed in or after 2000. 530 - (458 - 5 - 7) = 84"
    ),

    hr(),

    p(
      "Often the movements of vehicles are well documented. There therefore tend to be more events and collections recorded involving museums in the transport and war and conflict categories."
    ),
    plotlyOutput(NS(id, "eventsPerMuseumBoxplots"), width="80%", height="1000px"),

    hr(),

    p(
      "Two museums in particular stand out as outliers in terms of the quantity of data collected for them: Firepower! The Royal Artillery Museum and the Electric Railway Museum."
    ),
    plotlyOutput(NS(id, "eventsPerMuseum"), width="80%", height="1000px"),

    hr(),

    h3("Collection Sizes"),
    plotlyOutput(NS(id, "collectionGranularity"), width="80%", height="1000px"),

    DTOutput(NS(id, "summaryTable"))

  )
}
