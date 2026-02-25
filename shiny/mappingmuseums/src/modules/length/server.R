source("src/modules/length/elements.R")

lengthServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$reset, {
      updateRadioButtons(session=session, inputId="museumGrouping", selected="All")
      updateRadioButtons(session=session, inputId="countOrPercentage", selected="count")
      updatePickerInput(
        session=session, inputId="governanceFilter", selected=governance_broad_labels()$label
      )
      updatePickerInput(
        session=session, inputId="sizeFilter", selected=size_labels()$label
      )
      updatePickerInput(
        session=session, inputId="subjectFilter", selected=subject_broad_labels()$label
      )
      updatePickerInput(
        session=session, inputId="subjectSpecificFilter", selected=subject_labels_map()$subject
      )
      updatePickerInput(
        session=session, inputId="regionFilter", selected=region_labels()$label
      )
      updatePickerInput(
        session=session, inputId="accreditationFilter", selected=accreditation_labels()$label
      )
      example_museum_name <- initial_museums()$name[1]
      updateVirtualSelect(
        session=session,
        inputId="exampleMuseum",
        choices=initial_museums()$name
      )
    })

    museum_grouping <- reactive({
      filter(field_names(), name==input$museumGrouping)$value[1]
    })
    museum_grouping_name <- reactive({input$museumGrouping})

    size_filter_choices <- reactive({ input$sizeFilter })
    governance_filter_choices <- reactive({ input$governanceFilter })
    subject_filter_choices <- reactive({ input$subjectFilter })
    specific_subject_filter_choices <- reactive({ input$subjectSpecificFilter })
    region_filter_choices <- reactive({ input$regionFilter })
    accreditation_filter_choices <- reactive({ input$accreditationFilter })
    museum_filters <- reactive({
      list(
        input$sizeFilter,
        input$governanceFilter,
        input$subjectFilter,
        input$subjectSpecificFilter,
        input$regionFilter,
        input$accreditationFilter
      )
    })

    observeEvent(subject_filter_choices(), {
      freezeReactiveValue(input, "subjectSpecificFilter")
      specific_subjects <- subject_labels_map() |>
        filter(subject_broad %in% subject_filter_choices())
      updatePickerInput(
        session=session,
        inputId="subjectSpecificFilter",
        choices=specific_subjects$subject,
        selected=specific_subjects$subject,
      )
    })

    initial_museum_ids <- reactive({
      sapply(input$initialMuseum, function(text) sub(".*\\(([^()]*)\\)$", "\\1", text))
    })

    observeEvent(museum_filters(), {
      freezeReactiveValue(input, "exampleMuseum")
      filtered_museums <- initial_museums() |>
        filter(
          size %in% size_filter_choices(),
          governance_broad %in% governance_filter_choices(),
          subject_broad %in% subject_filter_choices(),
          subject %in% specific_subject_filter_choices(),
          region %in% region_filter_choices(),
          accreditation %in% accreditation_filter_choices()
        )
      example_museum_name <- filtered_museums$name[1]
      updateVirtualSelect(
        session=session,
        inputId="exampleMuseum",
        choices=filtered_museums$name,
        selected=example_museum_name
      )
    })

    filtered_closure_lengths <- debounce(
      reactive({
        closure_lengths() |>
          filter(
            size %in% size_filter_choices(),
            governance_broad %in% governance_filter_choices(),
            accreditation %in% accreditation_filter_choices(),
            subject_broad %in% subject_filter_choices(),
            subject %in% specific_subject_filter_choices(),
            region %in% region_filter_choices()
          )
      }),
      millis=DEBOUNCE_TIME
    )
    lengths_two_way_table <- reactive({
      get_lengths_two_way_table(filtered_closure_lengths(), museum_grouping())
    })

    example_museum_id <- reactive({
      filter(
        initial_museums(),
        name==input$exampleMuseum
      )$museum_id
    })

    output$mainPlotOptions <- renderUI({
      if(currentMainPlot() == "lengthTileChart") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show number of museums" = "count",
            "Show percentage of museums" = "percentage",
            "Show percentages by museum type" = "percentage_y",
            "Show percentages by length of disposal" = "percentage_x"
          )
        )
      } else if(currentMainPlot() == "lengthLineChart") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show number of museums" = "count",
            "Show percentage of museums" = "percentage"
          )
        )
      }
    })

    count_or_percentage <- reactive({
      if (is.na(input$countOrPercentage)) {
        return("")
      }
      return(input$countOrPercentage)
    })

    currentMainPlot <- reactiveVal("lengthTileChart")
    # Update the current plot based on user clicks
    observeEvent(input$lengthTileChart, { currentMainPlot("lengthTileChart") })
    observeEvent(input$lengthLineChart, { currentMainPlot("lengthLineChart") })
    observeEvent(input$lengthScatter, { currentMainPlot("lengthScatter") })
    observeEvent(input$exampleTimelines, { currentMainPlot("exampleTimelines") })

    output$errorMessage <- renderUI({
      if (nrow(filtered_closure_lengths()) == 0) {
        p("The filters returned no results. Try less specific filters")
      }
    })

    output$mainPlot <- renderPlotly({
      if (currentMainPlot() == "lengthTileChart") {
        length_tile_chart(lengths_two_way_table(), count_or_percentage(), museum_grouping())
      } else if (currentMainPlot() == "lengthLineChart") {
        length_line_chart(filtered_closure_lengths(), count_or_percentage(), museum_grouping())
      } else if (currentMainPlot() == "lengthScatter") {
        length_scatter(filtered_closure_lengths(), museum_grouping())
      } else if (currentMainPlot() == "exampleTimelines") {
        example_timelines(closure_timeline_events(), example_museum_id())
      }
    })

    output$lengthTileChartSmall <- renderPlot({
      length_tile_chart_small(lengths_two_way_table(), museum_grouping())
    })
    output$lengthLineChartSmall <- renderPlot({
      length_line_chart_small(filtered_closure_lengths(), museum_grouping())
    })
    output$lengthScatterSmall <- renderPlot({
      length_scatter_small(filtered_closure_lengths(), museum_grouping())
    })
    output$exampleTimelinesSmall <- renderPlot({
      example_timelines_small(closure_timeline_events(), example_museum_id())
    })

    length_of_disposal_table <- reactive({
      filtered_closure_lengths() |>
        select(
          museum_id,
          museum_name,
          year_closed,
          earliest_event_date=earliest,
          latest_event_date=latest,
          length_of_disposal=length_of_closure,
          disposal_length_category=closure_length_category,
          size,
          governance,
          subject,
          region,
          accreditation
        )
    })

    output$downloadLengthsTable <- downloadHandler(
      filename = function() {
        paste('length-of-disposal-data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(length_of_disposal_table(), con)
      },
      contentType = "text/csv"
    )

    output$closureLengthsTable <- renderDT({
      length_of_disposal_table()
    }, options=list(pageLength=100))

  })
}
