pathwaysUI <- function(id) {
  fluidPage(
    fluidRow(
      p("The transfer of museum collections after closure. The diagram below summarizes categorizes the pathways that collections follow when leaving a closed museum."),
      p("Nodes are labelled with the number of actors belonging to the category at that time step. The width of lines is proportional to the possible number of objects involved in transfers."),
      p("Use the options to alter the way that museums and other actors are grouped together and to filter transactions according to type or collection source/destination."),
      p("Find out more about actor and event types in the actor and event types tab."),
      p("The table below the diagram provides details of the depicted transfers."),
      ),
    fluidRow(
      sidebarLayout(
        sidebarPanel(width=3, dispersalFiltersUI(NS(id, "dispersalFilters"))),
        mainPanel(
          plotlyOutput(NS(id, "pathwaysNetwork"), width="100%", height="850px"),
          tagList(
            tags$span(
              tags$strong("Display: "),
              tags$i(
                class = "fa fa-info-circle",
                style = "color: #007bff; cursor: pointer;",
                `data-toggle` = "popover",
                `data-placement` = "right",
                title = "Display steps or first and last actors",
                `data-content` = "<p><strong>Steps in path:</strong> View intermediate actors in the sequences of ownership and/or custody changes</p><p><strong>First and last actors:</strong> View only the initial museum and the last known actor in the sequence.</p>"
              )
            ),
            tags$script(popover_js),
            radioButtons(
              NS(id, "stepsOrFirstLast"),
              label="",
              choices=c("Steps in path", "First and last actors"),
              selected="Steps in path",
              inline=TRUE
            ),
          ),
          tagList(
            tags$span(
              tags$strong("Steps in path: "),
              tags$i(
                class = "fa fa-info-circle",
                style = "color: #007bff; cursor: pointer;",
                `data-toggle` = "popover",
                `data-placement` = "right",
                title = "Steps in path",
                `data-content` = "<p>Select the start and end point of sequences. Step 1 shows the initial museums where collections originated.</p><p>Use the slider to increase the number of steps away from the museum shown on the diagram.</p>"
              )
            ),
            tags$script(popover_js),
            sliderInput(
              NS(id, "stagesInOwnershipPath"),
              label="",
              value=2,
              min=1,
              max=7,
              step=1,
              ticks=FALSE,
              width="50%"
            )
          ),
          img(src='actor-sector-key.png', align="left", width="150px")
        )
      ),
    ),
    fluidRow(
      h3("Sequences Data"),
      pickerInput(
        NS(id, "tableSelect"),
        label="show columns:",
        choices=sequences_table_choices,
        selected=sequences_table_choices,
        options = pickerOptions(
          actionsBox = TRUE, 
          size = 10,
          selectedTextFormat = "count > 3"
        ), 
        multiple = TRUE
      ), 
      downloadButton(NS(id, "downloadSequencesTable"), label="Download table as CSV")
    ),
    fluidRow(
      DTOutput(NS(id, "pathwaysTable"))
    )
  )
}

pathwaysServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    steps_or_first_last <- reactive({input$stepsOrFirstLast})

    observeEvent(steps_or_first_last(), {
      if (input$stepsOrFirstLast == "First and last actors") {
        shinyjs::disable("stagesInOwnershipPath")
      } else {
        shinyjs::enable("stagesInOwnershipPath")
      }
    })

    filtered_sequences <- dispersalFiltersServer("dispersalFilters", steps_or_first_last)

    selected_columns <- reactive({input$tableSelect})

    ownershipChangesStart <- reactiveVal(1)
    ownershipChangesEnd <- reactiveVal(2)
    debouncedStagesInOwnershipPath <- debounce(
      reactive(input$stagesInOwnershipPath),
      millis=300
    )
    observeEvent(debouncedStagesInOwnershipPath(), {
      ownershipChangesEnd(debouncedStagesInOwnershipPath())
    })

    museum_grouping <- reactive({
      if (input$`dispersalFilters-groupingMuseums` == "All museums") {
        return("all")
      } else if (input$`dispersalFilters-groupingMuseums` == "Size") {
        return("size")
      } else if (input$`dispersalFilters-groupingMuseums` == "Governance") {
        return("governance_broad")
      } else if (input$`dispersalFilters-groupingMuseums` == "Accreditation") {
        return("accreditation")
      } else if (input$`dispersalFilters-groupingMuseums` == "Subject Matter") {
        return("subject_matter_broad")
      } else if (input$`dispersalFilters-groupingMuseums` == "Country/Region") {
        return("region")
      }
    })

    output$pathwaysNetwork <- renderPlotly({
      generate_pathway_dendrogram(
        filtered_sequences(),
        ownershipChangesStart(),
        ownershipChangesEnd(),
        input$`dispersalFilters-grouping`,
        museum_grouping(),
        input$`dispersalFilters-showTransactionCounts`,
        steps_or_first_last()
      )
    })

    output$downloadSequencesTable <- downloadHandler(
      filename = function() {
        paste('dispersal-sequences-data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(
          filtered_sequences() |> select(all_of(selected_columns())),
          con
        )
      },
      contentType = "text/csv"
    )

    output$pathwaysTable <- renderDT({
      pathway_table(filtered_sequences(), selected_columns())
    }, options=list(pageLength=100))

  })
}

