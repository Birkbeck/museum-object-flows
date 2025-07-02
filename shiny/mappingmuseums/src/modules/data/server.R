source("src/modules/data/elements.R")

dataServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    data_by_museum <- get_data_by_museum(dispersal_events, museums_including_crown_dependencies)

    output$eventsPerMuseumMatrix <- renderPlotly({
      events_per_museum_matrix(data_by_museum)
    })

    output$eventsPerMuseumBoxplots <- renderPlotly({
      events_per_museum_boxplots(data_by_museum)
    })

    output$eventsPerCollection <- renderPlotly({
      events_per_collection()
    })

    output$collectionGranularity <- renderPlotly({
      collection_distribution_bars()
    })

    output$collectionGranularityHeatmap <- renderPlotly({
      collection_distribution_heatmap()
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
    }, options=list(pageLength=100))

  })
}
