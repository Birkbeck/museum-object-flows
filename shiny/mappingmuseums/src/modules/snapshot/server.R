source("src/modules/snapshot/elements.R")

snapshotServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    small_chart_size <- 300
      x_labels <- reactive({c(
        "start_total"=paste("Open Museums at start of", input$year_range[1]),
        "end_total"=paste("Open Museums at end of", input$year_range[2]),
        "openings"=paste0("New Museum Openings ", input$year_range[1], "-", input$year_range[2]),
        "closures"=paste0("Museum Closures ", input$year_range[1], "-", input$year_range[2]),
        "change"=paste0("Change in Museum Numbers ", input$year_range[1], "-", input$year_range[2]),
        "change_pc"=paste0("Percentage Change in Museums ", input$year_range[1], "-", input$year_range[2])
      )})
      y_labels <- c(
        "all"="All Museums",
        "size"="Museum Size",
        "governance"="Museum Governance",
        "accreditation"="Museum Accreditation",
        "main_subject"="Subject Matter",
        "region"="Country/Region",
        "nation"="Country"
      )

    year_or_range <- reactive({input$yearOrRange})
    period_start <- reactive({
      if(year_or_range() == "Single year") {
        return(input$year[1])
      }
      return(input$yearRange[1])
    })
    period_end <- reactive({
      if(year_or_range() == "Single year") {
        return(input$year[1])
      }
      return(input$yearRange[2])
    })
    
    observeEvent(year_or_range(), {
      freezeReactiveValue(input, "yearRange")
      if (year_or_range() == "Single year") {
        output$yearSlider <- renderUI(
          sliderInput(
            NS(id, "year"),
            label="Year:",
            value=c(2025),
            min=1960,
            max=2025,
            step=1,
            sep="",
            ticks=TRUE,
            width="100%"
          )
        )
      } else {
        output$yearSlider <- renderUI(
          sliderInput(
            NS(id, "yearRange"),
            label="Time Period:",
            value=c(1960, 2025),
            min=1960,
            max=2025,
            step=1,
            sep="",
            ticks=TRUE,
            width="100%"
          )
        )
      }
    }) 

    main_axis <- reactive({
      req(input$mainAxis)
      return(
        filter(field_names, name==input$mainAxis)$value[1]
      )
    })
    second_axis <- reactive({
      req(input$secondAxis)
      return(
        filter(field_names, name==input$secondAxis)$value[1]
      )
    })

    mainPlot <- reactiveVal("museumMap")
    # Update the current plot based on user clicks
    observeEvent(input$museumMap, { mainPlot("museumMap") })
    observeEvent(input$museumCounts, { mainPlot("museumCounts") })
    observeEvent(input$museumHeatmap, { mainPlot("museumHeatmap") })
    
    output$mainPlotOptions <- renderUI({
      if(mainPlot() == "museumCounts") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show number of museums" = "",
            "Show percentage of museums" = "_pc"
          )
        )
      } else if(mainPlot() == "museumHeatmap") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show number of museums" = "",
            "Show percentage of museums" = "_pc",
            "Show rowwise percentages" = "_pc_x",
            "Show columnwise percentages" = "_pc_y"
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

    basic_metric <- reactive({
      if(year_or_range() == "Single year") {
        return("end_total")
      } else {
        return("period_total")
      }
    })

    x_label <- reactive({
      if (count_or_percentage() == "") {
        "Number of Museums"
      } else {
        "Percentage of Museums"
      }
    })
    y_label <- reactive({input$mainAxis})
    title <- reactive({
      if (main_axis() == "all") {
        return(x_label())
      } else {
        return(paste(x_label(), "by", y_label()))
      }
    })

    metric <- reactive({
      paste0(basic_metric(), count_or_percentage())
    })

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
    subject_specific_filter_choices <- reactive({
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

    filtered_museums <- reactive({
      get_museums_in_snapshot(
        museums,
        size_filter=size_filter_choices(),
        governance_filter=governance_filter_choices(),
        subject_filter=subject_filter_choices(),
        subject_specific_filter=subject_specific_filter_choices(),
        region_filter=region_filter_choices(),
        accreditation_filter=accreditation_filter_choices(),
        start=period_start(),
        end=period_end()
      )
    })

    museum_type_summary <- reactive({
      get_open_and_close_data(
        filtered_museums(),
        main_axis(),
        period_start(),
        period_end()
      )
    })

    museum_type_two_way_summary <- reactive({
      get_2_way_open_and_close_data(
        filtered_museums() |>
          mutate(
            dimension_1=.data[[main_axis()]],
            dimension_2=.data[[second_axis()]],
          ),
        "dimension_1",
        "dimension_2",
        period_start(),
        period_end()
      )
    })

    output$mainPlot <- renderPlotly({
      if (mainPlot() == "museumMap") {
        museum_map(
          filtered_museums(),
          main_axis(),
          year_or_range(),
          period_start(),
          period_end()
        )
      } else if(mainPlot() == "museumCounts") {
        snapshot_bar_chart(
          museum_type_summary(),
          main_axis(),
          metric(),
          title(),
          y_label(),
          x_label()
        )
      } else if(mainPlot() == "museumHeatmap") {
        snapshot_heatmap(
          museum_type_two_way_summary(),
          metric(),
          year_or_range(),
          period_start(),
          period_end(),
          input$xAxis,
          input$yAxis
        )
      }
    })
    output$mainPlotExplanation <- renderUI({
      explanation_text <- filter(explanations, main_plot==mainPlot())$explanation
      p(explanation_text)
    })
    
    output$museumMapSmall <- renderPlot({
      museum_map_small(
        filtered_museums(),
        main_axis(),
        year_or_range(),
        period_start(),
        period_end()
      )
    }, width=small_chart_size, height=small_chart_size)
    output$museumCountsSmall <- renderPlot({
      snapshot_bar_chart_small(
        museum_type_summary(),
        main_axis(),
        basic_metric(),
        ifelse(
          year_or_range() == "Single year",
          paste("Museums in the UK", period_start()),
          paste0("Museums in the UK ", period_start(), "-", period_end())
        ),
        "Number of Museums"
      )
    }, width=small_chart_size, height=small_chart_size)
    output$museumHeatmapSmall <- renderPlot({
      snapshot_heatmap_small(
        museum_type_two_way_summary(),
        basic_metric(),
        year_or_range(),
        period_start(),
        period_end(),
        input$mainAxis,
        input$secondAxis
      )
    }, width=small_chart_size, height=small_chart_size)

    output$downloadSnapshotTable <- downloadHandler(
      filename = function() {
        paste('museums-data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(filtered_museums(), con)
      },
      contentType = "text/csv"
    )
    
    output$openMuseumsTable <- renderDT({
      filtered_museums()
    })
  })
}
