source("src/modules/data/elements.R")

dataServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    data_by_museum <- get_data_by_museum(dispersal_events, museums_including_crown_dependencies)

    output$eventsPerMuseumMatrix <- renderPlotly({
      events_per_museum_matrix(data_by_museum)
    })

    output$eventsPerMuseum <- renderPlotly({
      events_per_museum()
    })

    output$collectionGranularity <- renderPlotly({
      collection_distribution_bars()
    })

    output$summaryTable <- renderDT({
      data_by_museum |>
        select(
          museum_id,
          museum_name,
          year_opened,
          year_closed,
          number_of_events,
          number_of_collections,
          size,
          governance,
          subject,
          region,
          accreditation,
        )
    })

  })
}
