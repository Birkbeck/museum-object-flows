source("src/modules/reasons/elements.R")

reasonsServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$reset, {
      updateSelectInput(session=session, inputId="reasonLevel", selected="Core categories")
      updateSelectInput(session=session, inputId="museumGrouping", selected="Governance")
      updateRadioButtons(session=session, inputId="countOrPercentage", selected="frequency")
      updatePickerInput(
        session=session, inputId="reasonFilter", selected=reason_core_labels$label
      )
      updatePickerInput(
        session=session, inputId="governanceFilter", selected=governance_broad_labels$label
      )
      updatePickerInput(
        session=session, inputId="sizeFilter", selected=size_labels$label
      )
      updatePickerInput(
        session=session, inputId="subjectFilter", selected=subject_broad_labels$label
      )
      updatePickerInput(
        session=session, inputId="subjectSpecificFilter", selected=subject_labels$label
      )
      updatePickerInput(
        session=session, inputId="regionFilter", selected=region_labels$label
      )
      updatePickerInput(
        session=session, inputId="accreditationFilter", selected=accreditation_labels$label
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

    size_filter_choices <- reactive({ input$sizeFilter })
    governance_filter_choices <- reactive({ input$governanceFilter })
    subject_filter_choices <- reactive({ input$subjectFilter })
    specific_subject_filter_choices <- reactive({ input$subjectSpecificFilter })
    region_filter_choices <- reactive({ input$regionFilter })
    accreditation_filter_choices <- reactive({ input$accreditationFilter })

    observeEvent(subject_filter_choices(), {
      freezeReactiveValue(input, "subjectSpecificFilter")
      specific_subjects <- subject_labels_map |>
        filter(subject_broad %in% subject_filter_choices())
      updatePickerInput(
        session=session,
        inputId="subjectSpecificFilter",
        choices=specific_subjects$subject,
        selected=specific_subjects$subject,
      )
    })

    filtered_reasons <- reactive({
      closure_reasons |>
        filter(
          !is.na(reason_core),
          reason_core %in% reason_filter(),
          size %in% size_filter_choices(),
          governance_broad %in% governance_filter_choices(),
          accreditation %in% accreditation_filter_choices(),
          subject %in% specific_subject_filter_choices(),
          subject_broad %in% subject_filter_choices(),
          region %in% region_filter_choices()
        )
    })
    summary_table <- reactive({
      closure_reasons_summary_table(filtered_reasons(), reason_level())
    })
    two_way_summary_table <- reactive({
      closure_reasons_two_way_summary_table(filtered_reasons(), reason_level(), museum_grouping())
    })
    over_time_table <- reactive({
      closure_reasons_over_time_table(filtered_reasons(), reason_level())
    })

    mainPlot <- reactiveVal("reasonsBarChart")
    # Update the current plot based on user clicks
    observeEvent(input$reasonsBarChart, {
      disable("museumGrouping")
      mainPlot("reasonsBarChart")
    })
    observeEvent(input$reasonsHeatmap, {
      enable("museumGrouping")
      mainPlot("reasonsHeatmap")
    })
    observeEvent(input$reasonsLineChart, {
      disable("museumGrouping")
      mainPlot("reasonsLineChart")
    })

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
            "Show number of closures" = "frequency",
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

    output$errorMessage <- renderUI({
      if (nrow(filtered_reasons()) == 0) {
        p("The filters returned no results. Try less specific filters")
      }
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
      text_box(paste("REASONS-EXPLANATION", explanation_text))
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
      filtered_reasons() |>
        select(
          museum_id,
          museum_name,
          year_opened,
          year_closed,
          reasons_for_closure=super_reasons,
          size,
          governance,
          accreditation,
          subject,
          region
        ) |>
        distinct()
    }, options=list(pageLength=100))
  })
}
