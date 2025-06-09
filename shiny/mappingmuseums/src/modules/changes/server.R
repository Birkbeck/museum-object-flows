source("src/modules/changes/elements.R")

changesServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$reset, {
      updateSliderInput(session=session, inputId="yearRange", value=c(2000, 2025))
      updateSelectInput(session=session, inputId="mainAxis", selected="All")
      updateSelectInput(session=session, inputId="secondAxis", selected="Country/Region")
      updateRadioButtons(session=session, inputId="countOrPercentage", selected="")
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
    })

    small_chart_size <- 300
    x_labels <- reactive({c(
      "start_total"=paste("Open Museums at start of", period_start()),
      "start_total_pc"=paste("Percentage of museums at start of", period_start()),
      "start_total_pc_x"=paste("Percentage of museums at start of", period_start()),
      "start_total_pc_y"=paste("Percentage of museums at start of", period_start()),
      "end_total"=paste("Open Museums at end of", period_end()),
      "end_total_pc"=paste("Percentage of museums at end of", period_end()),
      "end_total_pc_x"=paste("Percentage of museums at end of", period_end()),
      "end_total_pc_y"=paste("Percentage of museums at end of", period_end()),
      "openings"=paste0("New Museum Openings ", period_start(), "-", period_end()),
      "openings_rate"=paste0("Openings per 100 Existing Museums ", period_start(), "-", period_end()),
      "closures"=paste0("Museum Closures ", period_start(), "-", period_end()),
      "closures_rate"=paste0("Closures per 100 Museums ", period_start(), "-", period_end()),
      "change"=paste0("Change in Museum Numbers ", period_start(), "-", period_end()),
      "change_pc"=paste0("Percentage Change in Museums ", period_start(), "-", period_end())
    )})
    y_labels <- c(
      "all"="All Museums",
      "size"="Museum Size",
      "governance"="Museum Governance",
      "accreditation"="Museum Accreditation",
      "main_subject"="Subject Matter",
      "region"="Country/Region"
    )
    
    period_start <- reactive({
      req(input$yearRange)
      input$yearRange[1]
    })
    period_end <- reactive({
      req(input$yearRange)
      input$yearRange[2]
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
    
    current_main_plot <- reactiveVal("openingsClosures")
    # Update the current plot based on user clicks
    observeEvent(input$openingsVsClosuresScatter, {
      disable("secondAxis")
      current_main_plot("openingsVsClosuresScatter")
    })
    observeEvent(input$timeSeriesLine, {
      disable("secondAxis")
      current_main_plot("timeSeriesLine")
    })
    observeEvent(input$closureRateLine, {
      disable("secondAxis")
      current_main_plot("closureRateLine")
    })
    observeEvent(input$openingRateLine, {
      disable("secondAxis")
      current_main_plot("openingRateLine")
    })
    observeEvent(input$openingClosureRateLine, {
      disable("secondAxis")
      current_main_plot("openingClosureRateLine")
    })
    observeEvent(input$openingsClosures, {
      disable("secondAxis")
      current_main_plot("openingsClosures")
    })
    observeEvent(input$startEnd, {
      disable("secondAxis")
      current_main_plot("startEnd")
    })
    observeEvent(input$openings2Way, {
      enable("secondAxis")
      current_main_plot("openings.2Way")
    })
    observeEvent(input$closures2Way, {
      enable("secondAxis")
      current_main_plot("closures.2Way")
    })
    observeEvent(input$change, {
      disable("secondAxis")
      current_main_plot("change")
    })
    observeEvent(input$change2Way, {
      enable("secondAxis")
      current_main_plot("change.2Way")
    })
    observeEvent(input$openStart2Way, {
      enable("secondAxis")
      current_main_plot("start_total.2Way")
    })
    observeEvent(input$openEnd2Way, {
      enable("secondAxis")
      current_main_plot("end_total.2Way")
    })
    observeEvent(input$openingsMap, {
      disable("secondAxis")
      current_main_plot("openingsMap")
    })
    observeEvent(input$closuresMap, {
      disable("secondAxis")
      current_main_plot("closuresMap")
    })
    
    output$mainPlotOptions <- renderUI({
      if(current_main_plot() == "timeSeriesLine") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show number of museums" = "total",
            "Show change in museum numbers" = "change",
            "Show percentage change in museums" = "percentage_change"
          )
        )
      } else if(current_main_plot() == "openingRateLine") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show number of openings" = "opening_count",
            "Show rate of openings" = "opening_rate"
          )
        )
      } else if(current_main_plot() == "closureRateLine") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show number of closures" = "closure_count",
            "Show rate of closures" = "closure_rate"
          )
        )
      } else if(current_main_plot() == "openingClosureRateLine") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show number of openings/closures" = "count",
            "Show rate of openings/closures" = "rate"
          )
        )
      } else if(current_main_plot() == "openingsClosures") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show number of openings/closures" = "",
            "Show rate of openings/closure" = "_rate"
          )
        )
      } else if(current_main_plot() == "startEnd") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show number of museums" = "",
            "Show percentage of museums" = "_pc"
          )
        )
      } else if(current_main_plot() == "change") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show change in museum numbers" = "",
            "Show percentage change in museum numbers" = "_pc"
          )
        )
      } else if(current_main_plot() == "openings.2Way") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show number of openings" = "",
            "Show openings rate" = "_rate"
          )
        )
      } else if(current_main_plot() == "closures.2Way") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show number of closures" = "",
            "Show closure rate" = "_rate"
          )
        )
      } else if(current_main_plot() == "start_total.2Way") {
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
      } else if(current_main_plot() == "end_total.2Way") {
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
      } else if(current_main_plot() == "change.2Way") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show change in museum numbers" = "",
            "Show percentage change in museum numbers" = "_pc"
          )
        )
      }
    })

    count_or_percentage <- reactive({
      if(!current_main_plot() %in% c(
        "timeSeriesLine",
        "openingRateLine",
        "closureRateLine",
        "openingClosureRateLine",
        "openingsClosures",
        "startEnd",
        "change",
        "openings.2Way",
        "closures.2Way",
        "start_total.2Way",
        "end_total.2Way",
        "change.2Way"
      )) {
        return("")
      } else if (is.na(input$countOrPercentage)) {
        return("")
      }
      return(input$countOrPercentage)
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

    museum_cumulative_counts <- reactive({
      cumulative_counts_by_dimension(
        filtered_museums(),
        main_axis()
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
      if (current_main_plot() == "openingsVsClosuresScatter") {
        openings_vs_closures_scatter(
          museum_type_summary(),
          main_axis()
        )
      } else if (current_main_plot() == "timeSeriesLine") {
        if (count_or_percentage() == "total") {
          title <- "Number of Museums over Time"
          y_title <- "Number of Museums"
        } else if (count_or_percentage() == "change") {
          title <- "Annual Change in Museum Numbers"
          y_title <- "Number of Museums"
        } else {
          title <- "Annual Percentage Change in Museum Numbers"
          y_title <- "Percentage Change"
        }
        time_series_line(
          museum_cumulative_counts(),
          main_axis(),
          count_or_percentage(),
          title,
          y_title,
          period_start(),
          period_end()
        )
      } else if (current_main_plot() == "openingRateLine") {
        if (count_or_percentage() == "opening_count") {
          title <- "Annual Museum Openings"
          y_title <- "Number of New Museums"
        } else {
          title <- "Annual Rate of Museum Openings"
          y_title <- "Openings per 100 Existing Museums"
        }
        time_series_line(
          museum_cumulative_counts(),
          main_axis(),
          count_or_percentage(),
          title,
          y_title,
          period_start(),
          period_end()
        )
      } else if (current_main_plot() == "closureRateLine") {
        if (count_or_percentage() == "closure_count") {
          title <- "Annual Museum Closures"
          y_title <- "Number of Closures"
        } else {
          title <- "Annual Rate of Museum Closures"
          y_title <- "Closures per 100 Museums"
        }
        time_series_line(
          museum_cumulative_counts(),
          main_axis(),
          count_or_percentage(),
          title,
          y_title,
          period_start(),
          period_end()
        )
      } else if (current_main_plot() == "openingClosureRateLine") {
        if (count_or_percentage() == "count") {
          title <- "Annual Museum Openings and Closures"
          y_title <- "Number of Openings/Closures"
          measures <- c("opening_count", "closure_count")
        } else {
          title <- "Annual Rate of Museum Openings and Closures"
          y_title <- "Openings/Closures per 100 Museums"
          measures <- c("opening_rate", "closure_rate")
        }
        time_series_line_double(
          museum_cumulative_counts(),
          main_axis(),
          measures,
          title,
          y_title,
          period_start(),
          period_end()
        )
      } else if (current_main_plot() == "openingsClosures") {
        if (count_or_percentage()=="_rate") {
          measures <- c("openings per 100 museums", "closures per 100 museums")
          x_title <- "Rate of openings/closures"
        } else {
          measures <- c("openings", "closures")
          x_title <- "Number of openings/closures"
        }
        two_measure_bar_chart(
          museum_type_summary(),
          main_axis(),
          measures,
          paste0("Openings vs Closures ", period_start(), "-", period_end()),
          input$mainAxis,
          x_title,
          c(
            "openings"="openings",
            "closures"="closures",
            "openings_rate"="openings per 100 museums",
            "closures_rate"="closures per 100 museums"
          ),
          c(
            "openings"=blue,
            "closures"=red,
            "openings per 100 museums"=blue,
            "closures per 100 museums"=red
          )
        )
      } else if (current_main_plot() == "startEnd") {
        if (count_or_percentage()=="_pc") {
          measures <- c(
            paste("percentage of museums in", period_end()),
            paste("percentage of museums in", period_start())
          )
          x_title <- "Percentage of open museums"
        } else {
          measures <- c(
            paste("open museums in", period_end()),
            paste("open museums in", period_start())
          )
          x_title <- "Number of open museums"
        }
        two_measure_bar_chart(
          museum_type_summary(),
          main_axis(),
          measures,
          paste0("Open Museums in ", period_start(), " vs in ", period_end()),
          input$mainAxis,
          x_title,
          c(
            "start_total"=paste("open museums in", period_start()),
            "end_total"=paste("open museums in", period_end()),
            "start_total_pc"=paste("percentage of museums in", period_start()),
            "end_total_pc"=paste("percentage of museums in", period_end())
          ),
          setNames(
            c(dark_blue, blue, dark_blue, blue),
            c(
              paste("open museums in", period_start()),
              paste("open museums in", period_end()),
              paste("percentage of museums in", period_start()),
              paste("percentage of museums in", period_end())
            )
          )
        )
      } else if (current_main_plot() == "openingsMap") {
        changes_map(
          filtered_museums(),
          main_axis(),
          "openings",
          period_start(),
          period_end()
        )
      } else if (current_main_plot() == "closuresMap") {
        changes_map(
          filtered_museums(),
          main_axis(),
          "closures",
          period_start(),
          period_end()
        )
      } else if (grepl("2Way", current_main_plot(), fixed=TRUE)) {
        measure <- strsplit(current_main_plot(), ".", fixed=TRUE)[[1]][1]
        measure <- paste0(measure, count_or_percentage())
        title <- paste(x_labels()[measure])
        y_label <- input$mainAxis
        x_label <- input$secondAxis
        heatmap(
          museum_type_two_way_summary(),
          main_axis(),
          second_axis(),
          measure,
          title,
          y_label,
          x_label
        )
      } else {
        x_label <- x_labels()[current_main_plot()]
        x_measure <- paste0(current_main_plot(), count_or_percentage())
        y_label <- input$filterField
        title <- paste(x_label, "by", y_label)
        bar_chart(
          museum_type_summary(),
          main_axis(),
          x_measure,
          title,
          y_label,
          x_label
        )
      }
    })
    output$mainPlotExplanation <- renderUI({
      explanation_text <- filter(explanations, main_plot==current_main_plot())$explanation
      text_box(paste("CHANGES-EXPLANATION", explanation_text))
    })
    
    output$openingsVsClosuresScatterSmall <- renderPlot({
      openings_vs_closures_scatter_small(
        museum_type_summary(),
        main_axis()
      )
    }, width=small_chart_size, height=small_chart_size)
    output$timeSeriesSmall <- renderPlot({
      time_series_line_small(
        museum_cumulative_counts(),
        main_axis(),
        "total",
        "Museums over Time",
        "Number of Museums",
        period_start(),
        period_end()
      )
    }, width=small_chart_size, height=small_chart_size)
    output$openingRatesSmall <- renderPlot({
      time_series_line_small(
        museum_cumulative_counts(),
        main_axis(),
        "opening_count",
        "Openings over Time",
        "Number of Openings",
        period_start(),
        period_end()
      )
    }, width=small_chart_size, height=small_chart_size)
    output$closureRatesSmall <- renderPlot({
      time_series_line_small(
        museum_cumulative_counts(),
        main_axis(),
        "closure_count",
        "Closures over Time",
        "Number of Closures",
        period_start(),
        period_end()
      )
    }, width=small_chart_size, height=small_chart_size)
    output$openingClosureRatesSmall <- renderPlot({
      time_series_line_double_small(
        museum_cumulative_counts(),
        main_axis(),
        c("opening_count", "closure_count"),
        "Openings and Closures over Time",
        "Number of Openings/Closures",
        period_start(),
        period_end()
      )
    }, width=small_chart_size, height=small_chart_size)
    output$openingsClosuresSmall <- renderPlot({
      two_measure_bar_chart_small(
        museum_type_summary(),
        main_axis(),
        c("openings", "closures"),
        paste0("Openings vs Closures ", period_start(), "-", period_end()),
        c("openings"="openings", "closures"="closures"),
        c("start_total"=dark_blue, "end_total"=blue, "openings"=blue, "closures"=red)
      )
    }, width=small_chart_size, height=small_chart_size)
    output$startEndSmall <- renderPlot({
      two_measure_bar_chart_small(
        museum_type_summary(),
        main_axis(),
        c(
          paste("open in", period_start()),
          paste("open in", period_end())
        ),
        paste0("Open Museums ", period_start(), " & ", period_end()),
        c(
          "start_total"=paste("open in", period_start()),
          "end_total"=paste("open in", period_end())
        ),
        setNames(
          c(dark_blue, blue),
          c(
            paste("open in", period_start()),
            paste("open in", period_end())
          )
        )
      )
    }, width=small_chart_size, height=small_chart_size)
    output$openingsSmall2Way <- renderPlot({
      heatmap_small(
        museum_type_two_way_summary(),
        main_axis(),
        second_axis(),
        "openings",
        paste0("Openings ", period_start(), "-", period_end())
      )
    }, width=small_chart_size, height=small_chart_size)
    output$closuresSmall2Way <- renderPlot({
      heatmap_small(
        museum_type_two_way_summary(),
        main_axis(),
        second_axis(),
        "closures",
        paste0("Closures ", period_start(), "-", period_end())
      )
    }, width=small_chart_size, height=small_chart_size)
    output$changeSmall <- renderPlot({
      bar_chart_small(
        museum_type_summary(),
        main_axis(),
        "change",
        paste0("Change ", period_start(), "-", period_end()),
        "Change"
      )
    }, width=small_chart_size, height=small_chart_size)
    output$percentageChangeSmall <- renderPlot({
      bar_chart_small(
        museum_type_summary(),
        main_axis(),
        "change_pc",
        paste0("Change (%)", period_start(), "-", period_end()),
        "Change (%)"
      )
    }, width=small_chart_size, height=small_chart_size)
    output$changeSmall2Way <- renderPlot({
      heatmap_small(
        museum_type_two_way_summary(),
        main_axis(),
        second_axis(),
        "change",
        paste0("Change ", period_start(), "-", period_end())
      )
    }, width=small_chart_size, height=small_chart_size)
    output$percentageChangeSmall2Way <- renderPlot({
      heatmap_small(
        museum_type_two_way_summary(),
        main_axis(),
        second_axis(),
        "change_pc",
        paste0("Change (%) ", period_start(), "-", period_end())
      )
    }, width=small_chart_size, height=small_chart_size)
    output$openingsMap <- renderPlot({
      changes_map_small(
        filtered_museums(),
        main_axis(),
        "openings",
        period_start(),
        period_end()
      )
    }, width=small_chart_size, height=small_chart_size)
    output$closuresMap <- renderPlot({
      changes_map_small(
        filtered_museums(),
        main_axis(),
        "closures",
        period_start(),
        period_end()
      )
    }, width=small_chart_size, height=small_chart_size)
    output$openStartSmall2Way <- renderPlot({
      heatmap_small(
        museum_type_two_way_summary(),
        main_axis(),
        second_axis(),
        "start_total",
        paste0("Open Museums ", period_start())
      )
    }, width=small_chart_size, height=small_chart_size)
    output$openEndSmall2Way <- renderPlot({
      heatmap_small(
        museum_type_two_way_summary(),
        main_axis(),
        second_axis(),
        "end_total",
        paste0("Open Museums ", period_end())
      )
    }, width=small_chart_size, height=small_chart_size)

    closures_in_time_period_table <- reactive({
      get_closures_in_time_period(
        filtered_museums(),
        period_start(),
        period_end()
      )
    })

    output$downloadClosuresTable <- downloadHandler(
      filename = function() {
        paste('museum-closures-data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(
          closures_in_time_period_table(),
          con
        )
      },
      contentType = "text/csv"
    )

    output$closuresTable <- renderDT({
      closures_in_time_period_table()
    })

    openings_in_time_period_table <- reactive({
      get_openings_in_time_period(
        filtered_museums(),
        period_start(),
        period_end()
      )
    })

    output$downloadOpeningsTable <- downloadHandler(
      filename = function() {
        paste('museum-openings-data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(
          openings_in_time_period_table(),
          con
        )
      },
      contentType = "text/csv"
    )

    output$openingsTable <- renderDT({
      openings_in_time_period_table()
    })
  })
}
