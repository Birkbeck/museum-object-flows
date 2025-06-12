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

    outcome_type_name <- reactive({input$outcomeType})
    outcome_type <- reactive({
      req(input$outcomeType)
      if (input$outcomeType == "Outcome event type") {
        return("outcome_event_type")
      } else if (input$outcomeType == "Outcome recipient type") {
        return("outcome_recipient_type")
      } else if (input$outcomeType == "Outcome recipient count") {
        return("outcome_recipient_count")
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
      } else if (input$museumGrouping == "Core reason for closure") {
        return("closure_reason_top_level")
      }
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

    summary_table <- reactive({
      closure_outcomes_summary_table(
        museums_including_crown_dependencies,
        outcome_type(),
        outcome_filter(),
        size_filter_choices(),
        governance_filter_choices(),
        accreditation_filter_choices(),
        subject_filter_choices(),
        specific_subject_filter_choices(),
        region_filter_choices()
      )
    })
    two_way_summary_table <- reactive({
      closure_outcomes_two_way_summary_table(
        museums_including_crown_dependencies,
        outcome_type(),
        outcome_filter(),
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
      closure_outcomes_over_time_table(
        museums_including_crown_dependencies,
        outcome_type(),
        outcome_filter(),
        size_filter_choices(),
        governance_filter_choices(),
        accreditation_filter_choices(),
        subject_filter_choices(),
        specific_subject_filter_choices(),
        region_filter_choices()
      )
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

    closure_outcomes_table <- reactive({
      museum_closure_outcomes_table(
        museums_including_crown_dependencies,
        outcome_type(),
        outcome_filter(),
        size_filter_choices(),
        governance_filter_choices(),
        accreditation_filter_choices(),
        subject_filter_choices(),
        specific_subject_filter_choices(),
        region_filter_choices()
      )
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
      closure_outcomes_table()
    }, options=list(pageLength=100))
  })
}
