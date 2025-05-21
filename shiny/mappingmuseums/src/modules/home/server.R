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

    output$downloadActorsTable <- downloadHandler(
      filename = function() {
        paste('dispersal-actors-data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(actors, con)
      },
      contentType = "text/csv"
    )
  })
}
