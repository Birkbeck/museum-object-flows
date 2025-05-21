homeUI <- function(id) {
  fluidPage(
    p("This application summarizes data from the Mapping Museums Database and the Dispersal Database."),
    p("Click on the buttons below to download the data in csv format."),
    downloadButton(NS(id, "downloadMuseumsTable"), label="Download all Mapping Museums data"),
    downloadButton(NS(id, "downloadEventsTable"), label="Download all dispersal data"),
    downloadButton(NS(id, "downloadActorsTable"), label="Download all actors' data"),
  )
}
