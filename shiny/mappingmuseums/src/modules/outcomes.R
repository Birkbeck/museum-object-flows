outcomesUI <- function(id) {
  fluidPage(
    fluidRow(
      p("What do museums do with their collections when they close? See below for a summary of what initially happens to collections, who initially receives them and how this varies across different types of museum."),
      p("See the table at the bottom of this page for details of each museum and the outcomes of its closure."),
      p("Outcome categories were determined by clustering museums according to the approximate proportion of their collections involved in events of each event type or received by actors of each actor type."),
      p("Firepower! The Royal Artillery Museum has not been included in this analysis."),
    ),
    hr(style=hr_style),
    sidebarLayout(
      sidebarPanel(width=3,
        h3("Filter Outcomes"),

        tagList(
          tags$span(
            tags$strong("Outcome type: "),
            tags$i(
              class = "fa fa-info-circle", # Font Awesome info icon
              style = "color: #007bff; cursor: pointer;", # Style for visibility
              `data-toggle` = "popover", # Bootstrap popover attribute
              `data-placement` = "right", # Position above the icon
              title = "Outcome type",
              `data-content` = "<p>Select how outcomes should be displayed on the diagrams.</p><p><strong>Main event:</strong> Show outcomes of closure in terms of events</p><p><strong>Main actor:</strong> Show outcomes of closure in terms of recipients</p>"
            )
          ),
          tags$script(popover_js),
          selectInput(
            NS(id, "outcomeType"),
            label="",
            choices=c("Main event", "Main recipient"),
            selected="Main event"
          )
        ),

        tagList(
          tags$span(
            tags$strong("Show only outcomes: "),
            tags$i(
              class = "fa fa-info-circle", # Font Awesome info icon
              style = "color: #007bff; cursor: pointer;", # Style for visibility
              `data-toggle` = "popover", # Bootstrap popover attribute
              `data-placement` = "right", # Position above the icon
              title = "Show only outcomes",
              `data-content` = "<p>Select which outcomes should appear in the visualizations. Removing some outcomes could improve the readability of charts.</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "outcomeFilter"),
            label="",
            choices=NULL,
            selected=NULL,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ) 
        ),

        tagList(
          tags$span(
            tags$strong("Group museums by: "),
            tags$i(
              class = "fa fa-info-circle", # Font Awesome info icon
              style = "color: #007bff; cursor: pointer;", # Style for visibility
              `data-toggle` = "popover", # Bootstrap popover attribute
              `data-placement` = "right", # Position above the icon
              title = "Group museums by",
              `data-content` = "<p>For the 2-dimensional heatmap.</p><p>Select which museum attribute to show on the <i>x</i>-axis.</p>"
            )
          ),
          tags$script(popover_js),
          selectInput(
            NS(id, "museumGrouping"),
            label="",
            choices=c(
              field_names$name,
              "Top-level reason for closure",
              "Main event outcome",
              "Main recipient outcome"
            ),
            selected="Governance"
          )
        ),


        h3("Filter Museums"),
        p("Show only the outcomes of closure for certain types of museum."),

        tagList(
          tags$span(
            tags$strong("Museum governance: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Museum governance",
              `data-content` = "<p>The governance structure of the museum</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "governanceFilter"), 
            "",
            choices=governance_labels$tidy_label,
            selected=governance_labels$tidy_label,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ) 
        ),
        
        tagList(
          tags$span(
            tags$strong("Museum size: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Museum size",
              `data-content` = "<p>The size of the museum. Museum sizes are based on approximate annual visitor numbers:</p><p><strong>Small: </strong>0 - 10,000 annual visitors.</p><p><strong>Medium: </strong>10,000 - 50,000 annual visitors</p><p><strong>Large: </strong>50,000 - 1,000,000 annual visitors.</p><p><strong>Huge: </strong>More than 1,000,000 annual visitors.</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "sizeFilter"), 
            "",
            choices=size_labels$tidy_label,
            selected=filter(size_labels, default_filter)$tidy_label,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ) 
        ),
        
        tagList(
          tags$span(
            tags$strong("Museum subject: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Museum subject",
              `data-content` = "<p>The subject matter of the museum.</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "subjectFilter"), 
            "",
            choices=subject_broad_labels$tidy_label,
            selected=filter(subject_broad_labels, default_filter)$tidy_label,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          )  
        ),
        
        tagList(
          tags$span(
            tags$strong("Museum subject (specific): "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Museum subject (specific)",
              `data-content` = "<p>Specific categories of museum subject matter.</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "subjectSpecificFilter"), 
            "",
            choices=NULL,
            selected=NULL,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          )
        ),
        
        tagList(
          tags$span(
            tags$strong("Museum country/region: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Museum country or region",
              `data-content` = "<p>Where in the United Kingdom the museum is located.</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "regionFilter"), 
            "",
            choices=country_region_labels$tidy_label,
            selected=filter(country_region_labels, default_filter)$tidy_label,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          )   
        ),
        
        tagList(
          tags$span(
            tags$strong("Museum accreditation: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Museum accreditation",
              `data-content` = "<p>Whether or not the museum was accredited at the time of closure.</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "accreditationFilter"), 
            "",
            choices=accreditation_labels$tidy_label,
            selected=filter(accreditation_labels, default_filter)$tidy_label,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          )   
        ),
      ),
      mainPanel(
        uiOutput(NS(id, "mainPlot"), height="1200px", width="100%"),
        uiOutput(NS(id, "mainPlotOptions")),
        uiOutput(NS(id, "mainPlotExplanation"))
      )
    ),
    hr(style=hr_style),
    fluidRow(
      p("Click on one of the small charts below to see it enlarged in the main panel above.")
    ),
    fluidRow(
      column(
        3,
        plotOutput(
          NS(id, "outcomesBarChartSmall"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "outcomesBarChart")
        ),
      ),
      column(
        3,
        plotOutput(
          NS(id, "outcomesHeatmapSmall"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "outcomesHeatmap")
        ),
      ),
      column(
        3,
        plotOutput(
          NS(id, "outcomesLineChartSmall"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "outcomesLineChart")
        ),
      ),
    ),
    hr(style=hr_style),
    fluidRow(
      h3("Outcomes of Museum Closure"),
      downloadButton(NS(id, "downloadOutcomesTable"), label="Download table as CSV")
    ),
    fluidRow(
      DTOutput(NS(id, "closureOutcomesTable"))
    )
  )
}

outcomesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    outcome_type_name <- reactive({input$outcomeType})
    outcome_type <- reactive({
      req(input$outcomeType)
      if (input$outcomeType == "Main event") {
        return("outcome_main_event")
      } else {
        return("outcome_main_recipient")
      }
    })
    outcome_filter <- reactive({input$outcomeFilter})
    museum_grouping <- reactive({
      req(input$museumGrouping)
      if (input$museumGrouping == "Main event outcome") {
        return("outcome_main_event")
      } else if (input$museumGrouping == "Main recipient outcome") {
        return("outcome_main_recipient")
      } else if (input$museumGrouping == "Top-level reason for closure") {
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
    observeEvent(input$outcomesBarChart, { mainPlot("outcomesBarChart") })
    observeEvent(input$outcomesHeatmap, { mainPlot("outcomesHeatmap") })
    observeEvent(input$outcomesLineChart, { mainPlot("outcomesLineChart") })
    output$mainPlot <- renderUI({
      if (mainPlot() == "outcomesBarChart") {
        ggplotly(
          closure_outcomes_bar_chart(summary_table(), outcome_type(), outcome_type_name()),
          height=1000
        ) |>
          renderPlotly()
      } else if (mainPlot() == "outcomesHeatmap") {
        ggplotly(
          closure_outcomes_heatmap(
            two_way_summary_table(), outcome_type(), outcome_type_name(), museum_grouping(), museum_grouping_name()
          ),
          height=1000
        ) |>
          renderPlotly()
      } else if (mainPlot() == "outcomesLineChart") {
        ggplotly(
          closure_outcomes_over_time(over_time_table(), outcome_type()),
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
        two_way_summary_table(), outcome_type(), outcome_type_name(), museum_grouping(), museum_grouping_name()
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

closure_outcomes_summary_table <- function(museums_table,
                                           outcome_type,
                                           outcome_filter,
                                           size_filter,
                                           governance_filter,
                                           accreditation_filter,
                                           subject_filter,
                                           specific_subject_filter,
                                           region_filter) {
  museums_table |>
    filter(.data[[outcome_type]] %in% outcome_filter) |>
    filter(size %in% size_filter) |>
    filter(governance %in% governance_filter | governance_main %in% governance_filter) |>
    filter(accreditation %in% accreditation_filter) |>
    filter(main_subject %in% subject_filter) |>
    filter(subject_matter %in% specific_subject_filter) |>
    filter(region %in% region_filter | nation %in% region_filter) |>
    group_by(.data[[outcome_type]]) |>
    summarize(frequency=n()) |>
    ungroup()
}

closure_outcomes_two_way_summary_table <- function(museums_table,
                                                   outcome_type,
                                                   outcome_filter,
                                                   museum_grouping,
                                                   size_filter,
                                                   governance_filter,
                                                   accreditation_filter,
                                                   subject_filter,
                                                   specific_subject_filter,
                                                   region_filter) {
  if(outcome_type==museum_grouping) {
    return(
      closure_outcomes_summary_table(
        museums_table,
        outcome_type,
        outcome_filter,
        size_filter,
        governance_filter,
        accreditation_filter,
        subject_filter,
        specific_subject_filter,
        region_filter
      )
    )
  }
  if(museum_grouping =="closure_reason_top_level") {
    museums_table <- museums_table |>
      left_join(closure_reasons, by="museum_id")
  }
  museums_table |>
    filter(.data[[outcome_type]] %in% outcome_filter) |>
    filter(size %in% size_filter) |>
    filter(governance %in% governance_filter | governance_main %in% governance_filter) |>
    filter(accreditation %in% accreditation_filter) |>
    filter(main_subject %in% subject_filter) |>
    filter(subject_matter %in% specific_subject_filter) |>
    filter(region %in% region_filter | nation %in% region_filter) |>
    group_by(.data[[museum_grouping]], .data[[outcome_type]]) |>
    summarize(frequency=n()) |>
    ungroup()
}

closure_outcomes_over_time_table <- function(museums_table,
                                             outcome_type,
                                             outcome_filter,
                                             size_filter,
                                             governance_filter,
                                             accreditation_filter,
                                             subject_filter,
                                             specific_subject_filter,
                                             region_filter) {
  museums_table |>
    filter(.data[[outcome_type]] %in% outcome_filter) |>
    filter(size %in% size_filter) |>
    filter(governance %in% governance_filter | governance_main %in% governance_filter) |>
    filter(accreditation %in% accreditation_filter) |>
    filter(main_subject %in% subject_filter) |>
    filter(subject_matter %in% specific_subject_filter) |>
    filter(region %in% region_filter | nation %in% region_filter) |>
    filter(!is.na(year_closed_1) & !is.na(year_closed_2)) |>
    filter(year_closed_1 != 9999) |>
    rowwise() |>
    mutate(
      year_closed = mean(c(year_closed_1, year_closed_2)),
      period_of_closure = ifelse(
        year_closed > 1999 & year_closed < 2005,
        "2000-2004",
        ifelse(
          year_closed < 2010,
          "2005-2009",
          ifelse(
            year_closed < 2015,
            "2010-2014",
            ifelse(
              year_closed < 2020,
              "2015-2019",
              "2020-2024"
            )
          )
        )
      ),
      period_of_closure = factor(
        period_of_closure, 
        levels = c("2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2024"),
        ordered = TRUE
      )
    ) |>
    group_by(.data[[outcome_type]], period_of_closure) |>
    summarize(count=n())
}
        
closure_outcomes_bar_chart <- function(summary_table, outcome_type, outcome_type_name) {
  ggplot(summary_table, aes(x=frequency, y=reorder(.data[[outcome_type]], frequency))) +
    geom_col(fill="violet") +
    geom_text(aes(label=frequency), hjust="left", nudge_x=1, size=3) +
    labs(
      title="Outcomes of Museum Closure, 2000-2024",
      y=outcome_type_name,
      x="Number of museum closures with outcome"
    ) +
    standard_bars_theme
}

closure_outcomes_bar_chart_small <- function(summary_table, outcome_type) {
  ggplot(summary_table, aes(x=frequency, y=reorder(.data[[outcome_type]], frequency))) +
    geom_col(fill="violet") +
    geom_text(aes(label=frequency), hjust="left", nudge_x=1, size=3) +
    labs(
      title="Outcomes of Museum Closure, 2000-2024",
      y="",
      x="Number of museums"
    ) +
    theme_minimal()
}

closure_outcomes_heatmap <- function(summary_table, outcome_type, outcome_type_name, museum_grouping, museum_grouping_name) {
  ggplot(
    summary_table,
    aes(
      x=fct_rev(factor(.data[[museum_grouping]], museum_attribute_ordering)),
      y=.data[[outcome_type]],
      fill=frequency
    )
  ) +
    geom_tile(show.legend=FALSE) +
    geom_text(aes(label=frequency)) +
    scale_x_discrete(labels=short_labels) +
    scale_fill_continuous(low="white", high="violet") +
    labs(
      title=paste0("Outcomes of Museum Closure by ", museum_grouping_name, " (Number of Closures)"),
      y=outcome_type_name,
      x=museum_grouping_name
    ) +
    standard_bars_theme +
    theme(
      axis.text.x = element_text(angle=45, hjust=1, vjust=1)
    )
}

closure_outcomes_heatmap_small <- function(summary_table, outcome_type, outcome_type_name, museum_grouping, museum_grouping_name) {
  ggplot(
    summary_table,
    aes(
      x=fct_rev(factor(.data[[museum_grouping]], museum_attribute_ordering)),
      y=.data[[outcome_type]],
      fill=frequency
    )
  ) +
    geom_tile(show.legend=FALSE) +
    geom_text(aes(label=frequency)) +
    scale_x_discrete(labels=short_labels) +
    scale_fill_continuous(low="white", high="violet") +
    labs(
      title=paste0("Outcomes vs ", museum_grouping_name),
      y=outcome_type_name,
      x=museum_grouping_name
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle=45, hjust=1, vjust=1)
    )
}

closure_outcomes_over_time <- function(outcomes_over_time_table, outcome_type) {
  ggplot(
    outcomes_over_time_table, 
    aes(x=period_of_closure, y=count, colour=.data[[outcome_type]])
  ) +
    geom_line(alpha=0.7, aes(group=.data[[outcome_type]])) +
    geom_point() +
    geom_text(
      data=outcomes_over_time_table |> filter(period_of_closure=="2010-2014"),
      position=position_jitter(width=1, height=1, seed=1),
      aes(label=.data[[outcome_type]], colour=.data[[outcome_type]])
    ) +
    guides(
      colour="none"
    ) +
    labs(
      title="Changing Outcomes of Museum Closure Over Time",
      x="Year of Closure",
      y="Number of museum closures with outcome",
      colour="Outcome of closure"
    ) +
    standard_bars_theme
}

closure_outcomes_over_time_small <- function(outcomes_over_time_table, outcome_type) {
  ggplot(
    outcomes_over_time_table, 
    aes(x=period_of_closure, y=count, colour=.data[[outcome_type]])
  ) +
    geom_line(alpha=0.7, aes(group=.data[[outcome_type]])) +
    geom_point() +
    geom_text(
      data=outcomes_over_time_table |> filter(period_of_closure=="2010-2014"),
      position=position_jitter(width=1, height=1, seed=1),
      aes(label=.data[[outcome_type]], colour=.data[[outcome_type]])
    ) +
    guides(
      colour="none"
    ) +
    labs(
      title="Changing Outcomes of Museum Closure Over Time",
      x="Year of Closure",
      y="Number of museum closures with outcome",
      colour="Outcome of closure"
    ) +
    theme_minimal()
}

museum_closure_outcomes_table <- function(museums_including_crown_dependencies,
                                          outcome_type,
                                          outcome_filter,
                                          size_filter,
                                          governance_filter,
                                          accreditation_filter,
                                          subject_filter,
                                          specific_subject_filter,
                                          region_filter) {
  causes <- dispersal_events |>
    select(museum_id=initial_museum_id, reasons_for_closure=super_event_causes) |>
    distinct()
  museums_including_crown_dependencies |>
    filter(!is.na(outcome_main_event)) |>
    filter(.data[[outcome_type]] %in% outcome_filter) |>
    filter(size %in% size_filter) |>
    filter(governance %in% governance_filter | governance_main %in% governance_filter) |>
    filter(accreditation %in% accreditation_filter) |>
    filter(main_subject %in% subject_filter) |>
    filter(subject_matter %in% specific_subject_filter) |>
    filter(region %in% region_filter | nation %in% region_filter) |>
    left_join(causes, by="museum_id") |>
    mutate(
      year_opened = paste(year_opened_1, year_opened_2, sep=":"),
      year_closed = paste(year_closed_1, year_closed_2, sep=":")
    ) |>
    select(
      museum_id,
      museum_name=name_of_museum,
      year_opened,
      year_closed,
      reasons_for_closure,
      outcome_main_event,
      outcome_main_recipient,
      size,
      governance,
      accreditation,
      subject_matter,
      region
    )
}
