source("src/modules/outcomes/elements.R")

outcomesServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$reset, {
      updateSelectInput(session=session, inputId="outcomeType", selected="Outcome event type")
      updateSelectInput(session=session, inputId="museumGrouping", selected="Governance")
      updatePickerInput(
        session=session,
        inputId="outcomeFilter",
        selected=distinct(
          filter(
            select(closure_outcomes, .data[[outcome_type()]]),
            !is.na(.data[[outcome_type()]])
          )
        )[[outcome_type()]]
      )
      updateRadioButtons(session=session, inputId="countOrPercentage", selected="frequency")
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

    outcome_type_name <- reactive({input$outcomeType})
    outcome_type <- reactive({
      req(input$outcomeType)
      if (input$outcomeType == "Outcome event type") {
        return("outcome_event_type")
      } else if (input$outcomeType == "Outcome recipient type") {
        return("outcome_recipient_type")
      } else if (input$outcomeType == "Outcome recipient count") {
        return("outcome_recipient_count")
      } else if (input$outcomeType == "Outcome largest recipient share") {
        return("largest_share")
      } else {
        return("outcome_destination_type")
      }
    })
    outcome_filter <- reactive({input$outcomeFilter})
    museum_grouping <- reactive({
      req(input$museumGrouping)
      if (input$museumGrouping == "Outcome event type") {
        return("outcome_event_type")
      } else if (input$museumGrouping == "Outcome recipient type") {
        return("outcome_recipient_type")
      } else if (input$museumGrouping == "Outcome recipient count") {
        return("outcome_recipient_count")
      } else if (input$museumGrouping == "Outcome destination type") {
        return("outcome_destination_type")
      } else if (input$museumGrouping == "Outcome largest recipient share") {
        return("largest_share")
      } else if (input$museumGrouping == "Core reason for closure") {
        return("reason_core")
      }
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

    observeEvent(outcome_type(), {
      choices <- distinct(
        filter(
          select(closure_outcomes, .data[[outcome_type()]]),
          !is.na(.data[[outcome_type()]])
        )
      )[[outcome_type()]]
      updatePickerInput(
        inputId="outcomeFilter",
        choices=choices,
        selected=choices
      )
    })

    mainPlot <- reactiveVal("outcomesBarChart")
    # Update the current plot based on user clicks
    observeEvent(input$outcomesBarChart, {
      disable("museumGrouping")
      mainPlot("outcomesBarChart")
    })
    observeEvent(input$outcomesHeatmap, {
      enable("museumGrouping")
      mainPlot("outcomesHeatmap")
    })
    observeEvent(input$outcomesLineChart, {
      disable("museumGrouping")
      mainPlot("outcomesLineChart")
    })

    output$mainPlotOptions <- renderUI({
      if(mainPlot() == "outcomesBarChart") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show number of closures" = "frequency",
            "Show percentage of closures" = "percentage"
          )
        )
      } else if(mainPlot() == "outcomesHeatmap") {
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
      } else if(mainPlot() == "outcomesLineChart") {
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

    output$mainPlot <- renderUI({
      if (mainPlot() == "outcomesBarChart") {
        ggplotly(
          closure_outcomes_bar_chart(
            summary_table(), count_or_percentage(), outcome_type(), outcome_type_name()
          ),
          height=1000
        ) |>
          renderPlotly()
      } else if (mainPlot() == "outcomesHeatmap") {
        ggplotly(
          closure_outcomes_heatmap(
            two_way_summary_table(), count_or_percentage(), outcome_type_name(), museum_grouping_name()
          ),
          height=1000
        ) |>
          renderPlotly()
      } else if (mainPlot() == "outcomesLineChart") {
        ggplotly(
          closure_outcomes_over_time(over_time_table(), count_or_percentage(), outcome_type()),
          height=1000
        ) |>
          renderPlotly()
      }
    })

    filtered_museums <- reactive({
      museums_including_crown_dependencies |>
        filter(
          !is.na(outcome_event_type),
          .data[[outcome_type()]] %in% outcome_filter(),
          size %in% size_filter_choices(),
          governance_broad %in% governance_filter_choices(),
          accreditation %in% accreditation_filter_choices(),
          subject_broad %in% subject_filter_choices(),
          subject %in% specific_subject_filter_choices(),
          region %in% region_filter_choices()
        )
    })
    summary_table <- reactive({
      closure_outcomes_summary_table(filtered_museums(), outcome_type())
    })
    two_way_summary_table <- reactive({
      closure_outcomes_two_way_summary_table(
        filtered_museums(), outcome_type(), museum_grouping()
      )
    })
    over_time_table <- reactive({
      closure_outcomes_over_time_table(filtered_museums(), outcome_type())
    })

    output$outcomesBarChartSmall <- renderPlot({
      closure_outcomes_bar_chart_small(summary_table(), outcome_type())
    })
    output$outcomesHeatmapSmall <- renderPlot({
      closure_outcomes_heatmap_small(
        two_way_summary_table(), outcome_type_name(), museum_grouping_name()
      )
    })
    output$outcomesLineChartSmall <- renderPlot({
      closure_outcomes_over_time_small(over_time_table(), outcome_type())
    })

    output$downloadOutcomesTable <- downloadHandler(
      filename = function() {
        paste('closure-outcomes-data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(
          closure_outcomes_table(),
          con
        )
      },
      contentType = "text/csv"
    )

    output$closureOutcomesTable <- renderDT({
      filtered_museums() |>
        select(
          museum_id,
          museum_name,
          year_opened,
          year_closed,
          reasons_for_closure,
          outcome_event_type,
          outcome_recipient_type,
          outcome_recipient_count,
          outcome_destination_type,
          size,
          governance,
          accreditation,
          subject,
          region
        )
    }, options=list(pageLength=100))
  })
}
