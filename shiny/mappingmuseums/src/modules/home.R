homeUI <- function(id) {
  fluidPage(
    p("This application summarizes data from the Mapping Museums Database and the Dispersal Database."),
    p("Click on the buttons below to download the data in csv format."),
    downloadButton(NS(id, "downloadMuseumsTable"), label="Download all Mapping Museums data"),
    downloadButton(NS(id, "downloadEventsTable"), label="Download all dispersal data"),
  )
}

homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$downloadMuseumsTable <- downloadHandler(
      filename = function() {
        paste('mapping-museums-data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(museums_including_crown_dependencies, con)
      },
      contentType = "text/csv"
    )

    output$downloadEventsTable <- downloadHandler(
      filename = function() {
        paste('dispersal-events-data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(dispersal_events, con)
      },
      contentType = "text/csv"
    )
  })
}
