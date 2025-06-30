dataUI <- function(id) {
  fluidPage(
    text_box("DATA-TOP"),

    h3("Events and Collections per Museum"),
    p(
      "When interpreting the data in these dashboards, it is important to bear in mind that data for each museum is recorded in different amounts and at a different levels of detail. Data for about a quarter of museums records just 1 event and 1 collection, but there is a wide distribution of museums where more than one event/collection are recorded."
    ),
    plotlyOutput(NS(id, "eventsPerMuseumMatrix"), width="80%", height="1000px"),

    p(
      "Why are there 84 museums with no events/collections data? 530 museums (including Channel Islands) definitely closed in or after 2000. Our database records 458 super events with at least one event. 5 of these super events are for museums that are no longer considered closed. 7 of these super events are for museums that might not have closed in or after 2000. 530 - (458 - 5 - 7) = 84"
    ),

    p(
      "Often the movements of vehicles are well documented. There are therefore a number of outlier museums in the transport and war and conflict categories, although the median number of events for museums in these categories are in line with the rest of the dataset."
    ),
    p(
      "Two museums in particular stand out as outliers in terms of the quantity of data recorded for them: Firepower! The Royal Artillery Museum and the Electric Railway Museum."
    ),
    plotlyOutput(NS(id, "eventsPerMuseumBoxplots"), width="80%", height="1000px"),

    hr(),

    h3("Collection Sizes"),
    p(
      "Collections recorded in the data also have a wide range of sizes. The chart below shows the distribution of these. Approximately x% of collections have been given a numeric size and the remainder are described according to the proportion they took up of their original museum's total collection."
    ),
    plotlyOutput(NS(id, "collectionGranularity"), width="80%", height="1000px"),

    p(
      "The distribution of collection sizes varies widely by museum subject matter. Museum types which are more likely to have a large number of collections recorded (e.g. war & conflict, transport) are also more likely to have records of individual collections (though this is partly down to a handful of outliers). Collections from other categories such as arts, buildings, rural industry are almost always recorded as 'all' or 'most' with little or no numerical record of collection sizes."
    ),
    plotlyOutput(NS(id, "collectionGranularityHeatmap"), width="80%", height="1000px"),

    DTOutput(NS(id, "summaryTable"))

  )
}
