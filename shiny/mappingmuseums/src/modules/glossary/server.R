source("src/modules/glossary/elements.R")

glossaryServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$actorTypes <- renderPlot({
      actors_taxonomy()
    })

    output$eventTypes <- renderPlot({
      events_taxonomy()
    })

    output$eventTypesGlossary <- renderDT({
      event_types |>
        select(
          `Type name`=type_name,
          `Sub-type of`=sub_type_of,
          `Definition`=definition,
          `Core category?`=is_core_category,
          `Usually a change of ownership?`=change_of_ownership,
          `Usually a change of custody?`=change_of_custody,
          `Usually an end of existence?`=end_of_existence
        )
    })

    output$reasonTypes <- renderPlot({
      reasons_taxonomy()
    })

  })
}
