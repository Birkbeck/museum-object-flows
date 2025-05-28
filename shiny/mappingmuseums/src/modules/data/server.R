source("src/modules/data/elements.R")

dataServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$eventsPerMuseum <- renderPlotly({
      events_per_museum()
    })

    output$collectionGranularity <- renderPlotly({
      collection_granularity_bars()
    })

  })
}
