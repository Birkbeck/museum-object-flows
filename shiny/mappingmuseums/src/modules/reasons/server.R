source("src/modules/reasons/elements.R")

reasonsServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$reset, {
      updateSelectInput(session=session, inputId="reasonLevel", selected="Core categories")
      updateSelectInput(session=session, inputId="museumGrouping", selected="Governance")
      updateRadioButtons(session=session, inputId="countOrPercentage", selected="frequency")
      closure_reasons <- closure_reasons_table()
      reason_choices <- distinct(select(closure_reasons, reason_core))$reason_core
      updatePickerInput(
        session=session,
        inputId="reasonFilter",
        selected=reason_choices
      )
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

    reason_level <- reactive({
      req(input$reasonLevel)
      if (input$reasonLevel == "Core categories") {
        return("reason_core")
      } else if (input$reasonLevel == "Core categories and their sub-categories") {
        return("reason_core_or_child")
      } else {
        return("reason_specific")
      }
    })
    reason_level_name <- reactive({
      req(input$reasonLevel)
      if (input$reasonLevel == "Core categories") {
        return("Reason Core Categories")
      } else if (input$reasonLevel == "Core categories and their sub-categories") {
        return("Reason Core Category/Sub-category")
      } else {
        return("Most Specific Reason")
      }
    })
    reason_filter <- reactive({input$reasonFilter})
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

    closure_reasons <- reactive({
      closure_reasons <- closure_reasons_table()
      choices <- distinct(select(closure_reasons, reason_core))$reason_core
      updatePickerInput(
        inputId="reasonFilter",
        choices=choices,
        selected=choices
      )
      return(closure_reasons)
    })
    closure_reasons_type_counts <- reactive({
      closure_reasons_types_counts_table(
        closure_reasons(),
        museums_including_crown_dependencies,
        size_filter_choices(),
        governance_filter_choices(),
        accreditation_filter_choices(),
        subject_filter_choices(),
        specific_subject_filter_choices(),
        region_filter_choices()
      )
    })
    summary_table <- reactive({
      closure_reasons_summary_table(
        closure_reasons(),
        museums_including_crown_dependencies,
        reason_level(),
        reason_filter(),
        size_filter_choices(),
        governance_filter_choices(),
        accreditation_filter_choices(),
        subject_filter_choices(),
        specific_subject_filter_choices(),
        region_filter_choices()
      )
    })
    two_way_summary_table <- reactive({
      closure_reasons_two_way_summary_table(
        closure_reasons(),
        museums_including_crown_dependencies,
        reason_level(),
        reason_filter(),
        museum_grouping(),
        size_filter_choices(),
        governance_filter_choices(),
        accreditation_filter_choices(),
        subject_filter_choices(),
        specific_subject_filter_choices(),
        region_filter_choices()
      )
    })
    over_time_table <- reactive({
      closure_reasons_over_time_table(
        closure_reasons(),
        museums_including_crown_dependencies,
        reason_level(),
        reason_filter(),
        size_filter_choices(),
        governance_filter_choices(),
        accreditation_filter_choices(),
        subject_filter_choices(),
        specific_subject_filter_choices(),
        region_filter_choices()
      )
    })

    mainPlot <- reactiveVal("reasonsBarChart")
    # Update the current plot based on user clicks
    observeEvent(input$reasonsBarChart, { mainPlot("reasonsBarChart") })
    observeEvent(input$reasonsHeatmap, { mainPlot("reasonsHeatmap") })
    observeEvent(input$reasonsLineChart, { mainPlot("reasonsLineChart") })

    output$mainPlotOptions <- renderUI({
      if(mainPlot() == "reasonsBarChart") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show number of closures" = "frequency",
            "Show percentage of closures" = "percentage"
          )
        )
      } else if(mainPlot() == "reasonsHeatmap") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show number of closures" = "frequency",
            "Show percentage of closures" = "percentage",
            "Show rowwise percentages" = "percentage_y",
            "Show columnwise percentages" = "percentage_x"
          )
        )
      } else if(mainPlot() == "reasonsLineChart") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show number of closures" = "count",
            "Show percentage of closures" = "percentage"
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

    output$mainPlot <- renderUI({
      if (mainPlot() == "reasonsBarChart") {
        ggplotly(
          closure_reasons_bar_chart(
            summary_table(),
            count_or_percentage(),
            reason_level(),
            reason_level_name()
          ),
          height=1200
        ) |>
          renderPlotly()
      } else if (mainPlot() == "reasonsHeatmap") {
        ggplotly(
          closure_reasons_heatmap(
            two_way_summary_table(),
            count_or_percentage(),
            reason_level(),
            reason_level_name(),
            museum_grouping(),
            museum_grouping_name()
          ),
          height=1200
        ) |>
          renderPlotly()
      } else if (mainPlot() == "reasonsLineChart") {
        ggplotly(
          closure_reasons_over_time(
            over_time_table(),
            count_or_percentage(),
            reason_level()
          ),
          height=1200
        ) |>
          renderPlotly()
      }
    })
    output$mainPlotExplanation <- renderUI({
      explanation_text <- filter(explanations, main_plot==mainPlot())$explanation
      p(explanation_text)
    })

    output$reasonsBarChartSmall <- renderPlot({
      closure_reasons_bar_chart_small(summary_table(), reason_level())
    })
    output$reasonsHeatmapSmall <- renderPlot({
      closure_reasons_heatmap_small(
        two_way_summary_table(),
        reason_level(),
        reason_level_name(),
        museum_grouping(),
        museum_grouping_name()
      )
    })
    output$reasonsLineChartSmall <- renderPlot({
      closure_reasons_over_time_small(over_time_table(), reason_level())
    })

    closure_reasons_by_museum_table <- reactive({
      museum_closure_reasons_table(
        closure_reasons(),
        museums_including_crown_dependencies,
        reason_level(),
        reason_filter(),
        size_filter_choices(),
        governance_filter_choices(),
        accreditation_filter_choices(),
        subject_filter_choices(),
        specific_subject_filter_choices(),
        region_filter_choices()
      )
    })

    output$downloadReasonsTable <- downloadHandler(
      filename = function() {
        paste('closure-reasons-data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(
          closure_reasons_by_museum_table(),
          con
        )
      },
      contentType = "text/csv"
    )

    output$closureReasonsTable <- renderDT({
      closure_reasons_by_museum_table()
    }, options=list(pageLength=100))
  })
}
