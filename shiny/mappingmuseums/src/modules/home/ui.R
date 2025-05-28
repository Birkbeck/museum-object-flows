homeUI <- function(id) {
  fluidPage(
    text_box("HOME-TOP"),

    text_box("HOME-BODY"),

    p("Click on the buttons below to download the data in csv format."),
    downloadButton(NS(id, "downloadMuseumsTable"), label="Download all Mapping Museums data"),
    downloadButton(NS(id, "downloadEventsTable"), label="Download all dispersal data"),
    downloadButton(NS(id, "downloadActorsTable"), label="Download all actors' data"),

    text_box("HOME-BOTTOM")
  )
}
