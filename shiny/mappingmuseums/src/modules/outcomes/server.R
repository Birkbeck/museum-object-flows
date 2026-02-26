source("src/modules/outcomes/elements.R")

outcomesServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$reset, {
      updateRadioButtons(session=session, inputId="outcomeType", selected="Collection outcome")
      updateRadioButtons(session=session, inputId="museumGrouping", selected="Governance")
      updateRadioButtons(session=session, inputId="countOrPercentage", selected="frequency")
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
    })

    outcome_type_name <- reactive({input$outcomeType})
    outcome_type <- reactive({
      req(input$outcomeType)
      if (input$outcomeType == "Collection outcome") {
        return("outcome_event_type")
      } else if (input$outcomeType == "Collection recipient") {
        return("outcome_recipient_type")
      } else if (input$outcomeType == "Collection recipient count") {
        return("outcome_recipient_count")
      } else if (input$outcomeType == "Collection recipient share") {
        return("outcome_largest_share")
      } else {
        return("outcome_destination_type")
      }
    })
    museum_grouping <- reactive({
      req(input$museumGrouping)
      if (input$museumGrouping == "Collection outcome") {
        return("outcome_event_type")
      } else if (input$museumGrouping == "Collection recipient") {
        return("outcome_recipient_type")
      } else if (input$museumGrouping == "Collection recipient count") {
        return("outcome_recipient_count")
      } else if (input$museumGrouping == "Collection destination") {
        return("outcome_destination_type")
      } else if (input$museumGrouping == "Collection recipient share") {
        return("outcome_largest_share")
      } else if (input$museumGrouping == "Core reason for closure") {
        return("reason_core")
      }
      filter(field_names(), name==input$museumGrouping)$value[1]
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
      specific_subjects <- subject_labels_map() |>
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
          select(closure_outcomes(), .data[[outcome_type()]]),
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
      shinyjs::hide("museumGroupingFormItem")
      mainPlot("outcomesBarChart")
    })
    observeEvent(input$outcomesHeatmap, {
      shinyjs::show("museumGroupingFormItem")
      mainPlot("outcomesHeatmap")
    })
    observeEvent(input$outcomesLineChart, {
      shinyjs::hide("museumGroupingFormItem")
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
        rowwise <- paste("Show percentages by", tolower(input$outcomeType))
        columnwise <- paste("Show percentages by", tolower(input$museumGrouping))
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = setNames(
            c("frequency", "percentage", "percentage_y", "percentage_x"),
            c(
              "Show number of closures",
              "Show percentage of closures",
              rowwise,
              columnwise
            )
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

    filtered_museums <- debounce(
      reactive({
        museums_including_crown_dependencies() |>
          filter(
            !is.na(outcome_event_type),
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

    output$errorMessage <- renderUI({
      if (nrow(filtered_museums()) == 0) {
        p("The filters returned no results. Try less specific filters")
      }
    })

    output$mainPlot <- renderUI({
      if (mainPlot() == "outcomesBarChart") {
        ggplotly(
          closure_outcomes_bar_chart(
            summary_table(), count_or_percentage(), outcome_type(), outcome_type_name()
          ),
          tooltip=c(count_or_percentage()),
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
          outcome_largest_share,
          outcome_destination_type,
          accreditation,
          governance,
          size,
          subject,
          region
        )
    }, options=list(pageLength=100))
  })
}
