source("src/modules/length/elements.R")

lengthServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$reset, {
      updateSelectInput(session=session, inputId="museumGrouping", selected="All")
      updateRadioButtons(session=session, inputId="countOrPercentage", selected="count")
      updatePickerInput(
        session=session,
        inputId="governanceFilter",
        selected=filter(governance_labels, internal_label != "Independent")$tidy_label
      )
      updatePickerInput(
        session=session,
        inputId="sizeFilter",
        selected=filter(size_labels, default_filter)$tidy_label
      )
      updatePickerInput(
        session=session,
        inputId="subjectFilter",
        selected=filter(subject_broad_labels, default_filter)$tidy_label
      )
      updatePickerInput(
        session=session,
        inputId="subjectSpecificFilter",
        selected=subject_full_labels$tidy_label
      )
      updatePickerInput(
        session=session,
        inputId="regionFilter",
        selected=filter(country_region_labels, internal_label != "England")$tidy_label
      )
      updatePickerInput(
        session=session,
        inputId="accreditationFilter",
        selected=filter(accreditation_labels, default_filter)$tidy_label
      )
      becm <- "mm.domus.SW043"
      example_museum_name <- filter(museums_list, museum_id == becm)$name
      updateVirtualSelect(
        session=session,
        inputId="exampleMuseum",
        choices=museums_list$name,
        selected=example_museum_name
      )
    })

    museum_grouping <- reactive({
      filter(field_names, name==input$museumGrouping)$value[1]
    })
    museum_grouping_name <- reactive({input$museumGrouping})

    size_filter_choices <- reactive({
      filter(
        size_labels,
        tidy_label %in% input$sizeFilter
      )$internal_label
    })
    governance_filter_choices <- reactive({
      filter(
        governance_labels,
        tidy_label %in% input$governanceFilter
      )$internal_label
    })
    subject_filter_choices <- reactive({
      filter(
        subject_broad_labels,
        tidy_label %in% input$subjectFilter
      )$internal_label
    })
    specific_subject_filter_choices <- reactive({
      filter(
        subject_full_labels,
        tidy_label %in% input$subjectSpecificFilter
      )$internal_label
    })
    region_filter_choices <- reactive({
      filter(
        country_region_labels,
        tidy_label %in% input$regionFilter
      )$internal_label
    })
    accreditation_filter_choices <- reactive({
      filter(
        accreditation_labels,
        tidy_label %in% input$accreditationFilter
      )$internal_label
    })
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
      specific_subjects <- subject_full_labels |>
        filter(subject_broad %in% subject_filter_choices())
      updatePickerInput(
        session=session,
        inputId="subjectSpecificFilter",
        choices=specific_subjects$tidy_label,
        selected=specific_subjects$tidy_label,
      )
    })

    initial_museum_ids <- reactive({
      sapply(input$initialMuseum, function(text) sub(".*\\(([^()]*)\\)$", "\\1", text))
    })

    museums_list <- dispersal_events |>
      mutate(
        name=paste0(initial_museum_name, " (", initial_museum_id, ")")
      ) |>
      arrange(name) |>
      select(
        name,
        museum_id=initial_museum_id,
        size=initial_museum_size,
        governance=initial_museum_governance,
        governance_broad=initial_museum_governance_broad,
        subject_matter_broad=initial_museum_subject_matter_broad,
        subject_matter=initial_museum_subject_matter,
        region=initial_museum_region,
        country=initial_museum_country,
        accreditation=initial_museum_accreditation
      ) |>
      distinct()

    observeEvent(museum_filters(), {
      freezeReactiveValue(input, "exampleMuseum")
      filtered_museums_list <- museums_list |>
        filter(
          size %in% size_filter_choices(),
          governance %in% governance_filter_choices() | governance_broad %in% governance_filter_choices(),
          subject_matter_broad %in% subject_filter_choices(),
          subject_matter %in% specific_subject_filter_choices(),
          region %in% region_filter_choices() | country %in% region_filter_choices(),
          accreditation %in% accreditation_filter_choices()
        )
      becm <- "mm.domus.SW043"
      if (becm %in% filtered_museums_list$museum_id) {
        example_museum_name <- filter(filtered_museums_list, museum_id == becm)$name
      } else {
        example_museum_name <- slice_sample(filtered_museums_list, n=1)$name
      }
      updateVirtualSelect(
        session=session,
        inputId="exampleMuseum",
        choices=filtered_museums_list$name,
        selected=example_museum_name
      )
    })

    event_dates_table <- reactive({get_event_dates_table()})
    lengths_table <- reactive({get_lengths_table(event_dates_table())})
    lengths_two_way_table <- reactive({
      get_lengths_two_way_table(lengths_table(), museum_grouping())
    })

    example_museum_id <- reactive({
      filter(
        museums_list,
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
            "Show rowwise percentages" = "percentage_y",
            "Show columnwise percentages" = "percentage_x"
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

    output$mainPlot <- renderPlotly({
      if (currentMainPlot() == "lengthTileChart") {
        length_tile_chart(lengths_two_way_table(), count_or_percentage(), museum_grouping())
      } else if (currentMainPlot() == "lengthLineChart") {
        length_line_chart(lengths_table(), count_or_percentage(), museum_grouping())
      } else if (currentMainPlot() == "lengthScatter") {
        length_scatter(lengths_table(), museum_grouping())
      } else if (currentMainPlot() == "exampleTimelines") {
        example_timelines(event_dates_table(), example_museum_id())
      }
    })

    output$lengthTileChartSmall <- renderPlot({
      length_tile_chart_small(lengths_two_way_table(), museum_grouping())
    })
    output$lengthLineChartSmall <- renderPlot({
      length_line_chart_small(lengths_table(), museum_grouping())
    })
    output$lengthScatterSmall <- renderPlot({
      length_scatter_small(lengths_table(), museum_grouping())
    })
    output$exampleTimelinesSmall <- renderPlot({
      example_timelines_small(event_dates_table(), example_museum_id())
    })

    output$downloadLengthsTable <- downloadHandler(
      filename = function() {
        paste('length-of-closure-data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(
          lengths_table(),
          con
        )
      },
      contentType = "text/csv"
    )

    output$closureLengthsTable <- renderDT({
      lengths_table()
    }, options=list(pageLength=100))

  })
}
