dataUI <- function(id) {
  fluidPage(
    text_box("DATA-TOP"),

    h3("Events per Museum"),
    plotlyOutput(NS(id, "eventsPerMuseum"), width="80%", height="1000px"),

    h3("Granularity of Object/Collection Data"),
    plotlyOutput(NS(id, "collectionGranularity"), width="80%", height="1000px")

  )
}
