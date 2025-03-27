causesUI <- function(id) {
  fluidPage(
    fluidRow(
      p("Why do museums close? We have categorized museum closures in the period 2000-2024 according to a hierarchy of reasons for closure. See the charts below to see reasons given for museum closure and how the prevalence of reasons varies by museum type and over time."),
      p("See the table at the bottom of this page for details of each museum and its reasons for closure.")
    ),
    sidebarLayout(
      sidebarPanel(width=3,
        h3("Filter Reasons"),

        tagList(
          tags$span(
            tags$strong("Reason type level: "),
            tags$i(
              class = "fa fa-info-circle", # Font Awesome info icon
              style = "color: #007bff; cursor: pointer;", # Style for visibility
              `data-toggle` = "popover", # Bootstrap popover attribute
              `data-placement` = "right", # Position above the icon
              title = "Reason type level",
              `data-content` = "<p>Select how reasons for closure should be displayed on the diagrams.</p><p><strong>Top-level:</strong> the most general categories for closure reasons.</p><p><strong>Mid-level:</strong> Categories in between the most general and most specific categories for closure reasons.</p> <p><strong>Low-level:</strong> the most specific categories for closure reasons.</p>"
            )
          ),
          tags$script(popover_js),
          selectInput(
            NS(id, "reasonLevel"),
            label="",
            choices=c("Top-level", "Mid-level", "Low-level"),
            selected="Top-level"
          )
        ),

        tagList(
          tags$span(
            tags$strong("Show only reasons: "),
            tags$i(
              class = "fa fa-info-circle", # Font Awesome info icon
              style = "color: #007bff; cursor: pointer;", # Style for visibility
              `data-toggle` = "popover", # Bootstrap popover attribute
              `data-placement` = "right", # Position above the icon
              title = "Show only reasons",
              `data-content` = "<p>Select which reasons should appear in the visualizations.</p>"
            )
          ),
          tags$script(popover_js),
          pickerInput(
            NS(id, "reasonFilter"),
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
            choices=field_names$name,
            selected="Governance"
          )
        ),

        h3("Filter Museums"),
        p("Show only the reasons for closure of certain types of museum."),

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
        div(uiOutput(NS(id, "mainPlotOptions")))
      ),
      mainPanel(
        div(uiOutput(NS(id, "mainPlot")), style = "height: 1200px; width: 100%;"),
        div(uiOutput(NS(id, "mainPlotExplanation")), style = "margin-top: 20px;"),
        fluidRow(
          p("Click on one of the small charts below to see it enlarged in the main panel above.")
        ),
        fluidRow(
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "reasonsHierarchySmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "reasonsHierarchy")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "reasonsBarChartSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "reasonsBarChart")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "reasonsHeatmapSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "reasonsHeatmap")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "reasonsLineChartSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "reasonsLineChart")
            )
          )
        )
      )
    ),
    fluidRow(
      h3("Reasons for Closure"),
      downloadButton(NS(id, "downloadCausesTable"), label="Download table as CSV")
    ),
    fluidRow(
      DTOutput(NS(id, "closureCausesTable"))
    )
  )
}

causesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reason_level <- reactive({
      req(input$reasonLevel)
      if (input$reasonLevel == "Top-level") {
        return("cause_super_type")
      } else if (input$reasonLevel == "Mid-level") {
        return("cause_type")
      } else {
        return("cause")
      }
    })
    reason_level_name <- reactive({
      req(input$reasonLevel)
      if (input$reasonLevel == "Top-level") {
        return("Top-level Cause Type")
      } else if (input$reasonLevel == "Mid-level") {
        return("Mid-level Cause Type")
      } else {
        return("Low-level Cause Type")
      }
    })
    reason_filter <- reactive({input$reasonFilter})
    museum_grouping <- reactive({
      filter(field_names, name==input$museumGrouping)$value[1]
    })
    museum_grouping_name <- reactive({input$museumGrouping})

    #observeEvent(reason_level(), {
    #  choices <- distinct(select(closure_causes(), cause_super_type))$cause_super_type
    #  updatePickerInput(
    #    inputId="reasonFilter",
    #    choices=choices,
    #    selected=choices
    #  ) 
    #})

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

    closure_causes <- reactive({
      closure_causes <- closure_causes_table()
      choices <- distinct(select(closure_causes, cause_super_type))$cause_super_type
      updatePickerInput(
        inputId="reasonFilter",
        choices=choices,
        selected=choices
      )
      return(closure_causes)
    })
    closure_causes_type_counts <- reactive({
      closure_causes_types_counts_table(
        closure_causes(),
        museums_including_crown_dependencies,
        size_filter_choices(),
        governance_filter_choices(),
        accreditation_filter_choices(),
        subject_filter_choices(),
        specific_subject_filter_choices(),
        region_filter_choices()
      )
    })
    hierarchy_layout <- reactive({
      closure_causes_hierarchy_layout(closure_causes_type_counts(), reason_level(), reason_filter())
    })
    summary_table <- reactive({
      closure_causes_summary_table(
        closure_causes(),
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
      closure_causes_two_way_summary_table(
        closure_causes(),
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
      closure_causes_over_time_table(
        closure_causes(),
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

    mainPlot <- reactiveVal("reasonsHierarchy")
    # Update the current plot based on user clicks
    observeEvent(input$reasonsHierarchy, { mainPlot("reasonsHierarchy") })
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
      if (mainPlot() == "reasonsHierarchy") {
        renderPlot(
          closure_type_hierarchy(hierarchy_layout(), reason_level()),
          height=1200
        )
      } else if (mainPlot() == "reasonsBarChart") {
        ggplotly(
          closure_causes_bar_chart(summary_table(), count_or_percentage(), reason_level(), reason_level_name()),
          height=1200
        ) |>
          renderPlotly()
      } else if (mainPlot() == "reasonsHeatmap") {
        ggplotly(
          closure_causes_heatmap(
            two_way_summary_table(), count_or_percentage(), reason_level(), reason_level_name(), museum_grouping(), museum_grouping_name()
          ),
          height=1200
        ) |>
          renderPlotly()
      } else if (mainPlot() == "reasonsLineChart") {
        ggplotly(
          closure_causes_over_time(over_time_table(), count_or_percentage(), reason_level()),
          height=1200
        ) |>
          renderPlotly()
      }
    })
    output$mainPlotExplanation <- renderUI({
      explanation_text <- filter(explanations, main_plot==mainPlot())$explanation
      p(explanation_text)
    })

    output$reasonsHierarchySmall <- renderPlot({
      closure_type_hierarchy_small(hierarchy_layout())
    })
    output$reasonsBarChartSmall <- renderPlot({
      closure_causes_bar_chart_small(summary_table(), reason_level())
    })
    output$reasonsHeatmapSmall <- renderPlot({
      closure_causes_heatmap_small(
        two_way_summary_table(), reason_level(), reason_level_name(), museum_grouping(), museum_grouping_name()
      )
    })
    output$reasonsLineChartSmall <- renderPlot({
      closure_causes_over_time_small(over_time_table(), reason_level())
    })

    closure_causes_by_museum_table <- reactive({
      museum_closure_causes_table(
        closure_causes(),
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

    output$downloadCausesTable <- downloadHandler(
      filename = function() {
        paste('closure-reasons-data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(
          closure_causes_by_museum_table(),
          con
        )
      },
      contentType = "text/csv"
    )

    output$closureCausesTable <- renderDT({
      closure_causes_by_museum_table()
    }, options=list(pageLength=100))
  })
}

closure_causes_table <- function() {
  dispersal_events |>
    select(
      museum_id=initial_museum_id,
      museum_name=initial_museum_name,
      cause=super_event_cause_types,
      super_causes=super_event_causes
    ) |>
    distinct() |>
    separate_rows(cause, sep = "; ") |>
    separate_wider_delim(
      cause,
      " - ",
      names=c("cause_super_type", "cause_type", "cause"),
      too_few="align_start"
    ) |>
    mutate(
      cause_type = ifelse(
        is.na(cause_type), paste(cause_super_type, "-", "other"), paste(cause_super_type, "-", cause_type)
      ),
      cause = ifelse(
        is.na(cause), paste(cause_type, "-", "other"), paste(cause_type, "-", cause)
      )
    )
}
 
closure_causes_types_counts_table <- function(closure_causes,
                                              museums_table,
                                              size_filter,
                                              governance_filter,
                                              accreditation_filter,
                                              subject_filter,
                                              specific_subject_filter,
                                              region_filter) {
  closure_causes |>
    left_join(museums_table, by="museum_id") |>
    filter(size %in% size_filter) |>
    filter(governance %in% governance_filter | governance_main %in% governance_filter) |>
    filter(accreditation %in% accreditation_filter) |>
    filter(main_subject %in% subject_filter) |>
    filter(subject_matter %in% specific_subject_filter) |>
    filter(region %in% region_filter | nation %in% region_filter) |>
    group_by(cause, cause_type, cause_super_type) |>
    summarise(frequency = n())
}

museum_closure_causes_table <- function(closure_causes,
                                        museums_table,
                                        reason_level,
                                        reason_filter,
                                        size_filter,
                                        governance_filter,
                                        accreditation_filter,
                                        subject_filter,
                                        specific_subject_filter,
                                        region_filter) {
  closure_causes |>
    group_by(museum_id, museum_name, super_causes) |>
    left_join(museums_table, by="museum_id") |>
    mutate(
      year_opened = paste(year_opened_1, year_opened_2, sep=":"),
      year_closed = paste(year_closed_1, year_closed_2, sep=":")
    ) |>
    filter(.data[[reason_level]] %in% reason_filter) |>
    filter(size %in% size_filter) |>
    filter(governance %in% governance_filter | governance_main %in% governance_filter) |>
    filter(accreditation %in% accreditation_filter) |>
    filter(main_subject %in% subject_filter) |>
    filter(subject_matter %in% specific_subject_filter) |>
    filter(region %in% region_filter | nation %in% region_filter) |>
    select(
      museum_id,
      museum_name,
      year_opened,
      year_closed,
      super_causes,
      size,
      governance,
      accreditation,
      subject_matter,
      region
    ) |>
    distinct()
}

closure_causes_summary_table <- function(closure_causes,
                                         museums_table,
                                         reason_level,
                                         reason_filter,
                                         size_filter,
                                         governance_filter,
                                         accreditation_filter,
                                         subject_filter,
                                         specific_subject_filter,
                                         region_filter) {
  number_of_closed_museums <- closure_causes |>
    select(museum_id) |>
    distinct() |>
    nrow()
  causes <- closure_causes |>
    filter(!is.na(cause_super_type)) |>
    left_join(museums_table, by="museum_id") |>
    filter(cause_super_type %in% reason_filter) |>
    filter(size %in% size_filter) |>
    filter(governance %in% governance_filter | governance_main %in% governance_filter) |>
    filter(accreditation %in% accreditation_filter) |>
    filter(main_subject %in% subject_filter) |>
    filter(subject_matter %in% specific_subject_filter) |>
    filter(region %in% region_filter | nation %in% region_filter) |>
    filter(!is.na(cause_super_type)) |>
    group_by(.data[[reason_level]]) |>
    summarize(
      frequency=n(),
      percentage=round(frequency / number_of_closed_museums * 100, 1)
    ) |>
    ungroup()
}

closure_causes_two_way_summary_table <- function(closure_causes,
                                                 museums_table,
                                                 reason_level,
                                                 reason_filter,
                                                 museum_grouping,
                                                 size_filter,
                                                 governance_filter,
                                                 accreditation_filter,
                                                 subject_filter,
                                                 specific_subject_filter,
                                                 region_filter) {
  number_of_closed_museums <- closure_causes |>
    select(museum_id) |>
    distinct() |>
    nrow()
  number_of_closed_museums_by_type <- closure_causes |>
    select(museum_id) |>
    distinct() |>
    left_join(museums_table, by="museum_id") |>
    group_by(.data[[museum_grouping]]) |>
    summarize(number_of_closures=n())
  causes <- closure_causes |>
    filter(!is.na(cause_super_type)) |>
    left_join(museums_table, by="museum_id") |>
    filter(cause_super_type %in% reason_filter) |>
    filter(size %in% size_filter) |>
    filter(governance %in% governance_filter | governance_main %in% governance_filter) |>
    filter(accreditation %in% accreditation_filter) |>
    filter(main_subject %in% subject_filter) |>
    filter(subject_matter %in% specific_subject_filter) |>
    filter(region %in% region_filter | nation %in% region_filter) |>
    group_by(.data[[museum_grouping]], .data[[reason_level]]) |>
    summarize(
      frequency=n(),
      percentage=round(frequency / number_of_closed_museums * 100, 1)
    ) |>
    ungroup() |>
    left_join(number_of_closed_museums_by_type, by=museum_grouping) |>
    group_by(.data[[museum_grouping]]) |>
    mutate(
      percentage_x=round(frequency / number_of_closures * 100, 1)
    ) |>
    ungroup() |>
    group_by(.data[[reason_level]]) |>
    mutate(
      percentage_y=round(frequency / sum(frequency) * 100, 1)
    ) |>
    ungroup()
}

closure_causes_over_time_table <- function(closure_causes,
                                           museums_table,
                                           reason_level,
                                           reason_filter,
                                           size_filter,
                                           governance_filter,
                                           accreditation_filter,
                                           subject_filter,
                                           specific_subject_filter,
                                           region_filter) {
  number_of_closures_by_time_period <- closure_causes |>
    select(museum_id) |>
    distinct() |>
    left_join(museums_table, by="museum_id") |>
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
    group_by(period_of_closure) |>
    summarize(number_of_closures_in_period=n()) |>
    ungroup()
  causes_over_time_table <- closure_causes |>
    left_join(museums_table, by="museum_id") |>
    filter(cause_super_type %in% reason_filter) |>
    filter(size %in% size_filter) |>
    filter(governance %in% governance_filter | governance_main %in% governance_filter) |>
    filter(accreditation %in% accreditation_filter) |>
    filter(main_subject %in% subject_filter) |>
    filter(subject_matter %in% specific_subject_filter) |>
    filter(region %in% region_filter | nation %in% region_filter) |>
    filter(!is.na(year_closed_1) & !is.na(year_closed_2)) |>
    filter(year_closed_1 != 9999) |>
    filter(!is.na(cause_super_type)) |>
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
    left_join(number_of_closures_by_time_period, by="period_of_closure") |>
    group_by(.data[[reason_level]], period_of_closure) |>
    summarize(
      count=n(),
      percentage=round(count / number_of_closures_in_period * 100, 1)
    ) |>
    ungroup() |>
    distinct()
}
        
closure_causes_bar_chart <- function(summary_table, count_or_percentage, reason_level, reason_level_name) {
  if (count_or_percentage == "frequency") {
    x_title <- "Number of museum closures where reason is cited"
  } else {
    x_title <- "Percentage of museum closures where reason is cited"
  }
  if (reason_level == "cause_super_type") {
    use_theme <- standard_bars_theme
  } else {
    use_theme <- theme_minimal()
  }
  ggplot(summary_table, aes(x=.data[[count_or_percentage]], y=reorder(.data[[reason_level]], .data[[count_or_percentage]]))) +
    geom_col(fill="purple", alpha=0.7) +
    geom_text(aes(label=.data[[count_or_percentage]]), hjust="left", nudge_x=1, size=5) +
    labs(
      title="Types of Reasons for Museum Closure, 2000-2024",
      y=reason_level_name,
      x=x_title
    ) +
    use_theme
}

closure_causes_bar_chart_small <- function(summary_table, reason_level) {
  ggplot(summary_table, aes(x=frequency, y=reorder(.data[[reason_level]], frequency))) +
    geom_col(fill="purple") +
    geom_text(aes(label=frequency), hjust="left", nudge_x=1, size=3) +
    labs(
      title="Reasons for Museum Closure, 2000-2024",
      y="",
      x="Number of museums"
    ) +
    scale_y_discrete(labels = function(labels) sapply(labels, shorten_cause_labels)) +
    theme_minimal()+
    theme(
      axis.text.y = element_text(size=6),
    )
}

closure_causes_heatmap <- function(summary_table, count_or_percentage, reason_level, reason_level_name, museum_grouping, museum_grouping_name) {
  if (reason_level == "cause_super_type") {
    use_theme <- standard_bars_theme
  } else {
    use_theme <- theme_minimal()
  }
  ggplot(
    summary_table,
    aes(
      x=fct_rev(factor(.data[[museum_grouping]], museum_attribute_ordering)),
      y=.data[[reason_level]],
      fill=.data[[count_or_percentage]]
    )
  ) +
    geom_tile(show.legend=FALSE) +
    geom_text(aes(label=.data[[count_or_percentage]]), size=5) +
    scale_x_discrete(labels=short_labels) +
    scale_fill_continuous(low="white", high="purple") +
    labs(
      title=paste0("Causes of Museum Closure by ", museum_grouping_name, " (Number of Closures)"),
      y=reason_level_name,
      x=museum_grouping_name
    ) +
    use_theme +
    theme(
      axis.text.x = element_text(angle=45, hjust=1, vjust=1)
    )
}

closure_causes_heatmap_small <- function(summary_table, reason_level, reason_level_name, museum_grouping, museum_grouping_name) {
  ggplot(
    summary_table,
    aes(
      x=fct_rev(factor(.data[[museum_grouping]], museum_attribute_ordering)),
      y=.data[[reason_level]],
      fill=frequency
    )
  ) +
    geom_tile(show.legend=FALSE) +
    scale_x_discrete(labels=short_labels) +
    scale_y_discrete(labels = function(labels) sapply(labels, shorten_cause_labels)) +
    scale_fill_continuous(low="white", high="purple") +
    labs(
      title=paste0("Reasons vs ", museum_grouping_name),
      y=reason_level_name,
      x=museum_grouping_name
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size=6),
      axis.text.x = element_text(angle=45, hjust=1, vjust=1)
    )
}

closure_causes_over_time <- function(causes_over_time_table, count_or_percentage, reason_level) {
  if (count_or_percentage == "count") {
    y_title <- "Number of museum closures where reason is cited"
  } else {
    y_title <- "Percentage of closures where reason is cited"
  }
  ggplot(
    causes_over_time_table, 
    aes(x=period_of_closure, y=.data[[count_or_percentage]], colour=.data[[reason_level]])
  ) +
    geom_line(alpha=0.7, aes(group=.data[[reason_level]])) +
    geom_point() +
    geom_text(
      data=causes_over_time_table |> filter(period_of_closure=="2010-2014"),
      position=position_jitter(width=1, height=1, seed=1),
      aes(label=.data[[reason_level]], colour=.data[[reason_level]])
    ) +
    guides(
      colour="none"
    ) +
    labs(
      title="Changing Reasons for Museum Closure Over Time",
      x="Year of Closure",
      y=y_title,
      colour="Reason for closure"
    ) +
    standard_bars_theme
}

closure_causes_over_time_small <- function(causes_over_time_table, reason_level) {
  ggplot(
    causes_over_time_table, 
    aes(x=period_of_closure, y=count, colour=.data[[reason_level]])
  ) +
    geom_line(alpha=0.7, aes(group=.data[[reason_level]])) +
    geom_point() +
    geom_text(
      data=causes_over_time_table |> filter(period_of_closure=="2010-2014"),
      position=position_jitter(width=1, height=1, seed=1),
      aes(label=sapply(.data[[reason_level]], shorten_cause_labels), colour=.data[[reason_level]])
    ) +
    guides(
      colour="none"
    ) +
    labs(
      title="Changing Reasons for Museum Closure Over Time",
      x="Year of Closure",
      y="Number of museum closures where reason is cited",
      colour="Reason for closure"
    ) +
    theme_minimal()
}

closure_causes_hierarchy_layout <- function(closure_causes, reason_level, reason_filter) {
  closure_causes <- closure_causes |>
    filter(cause_super_type %in% reason_filter)
  causes_1 <- closure_causes |>
    group_by(cause_super_type) |>
    summarize(frequency = sum(frequency)) |>
    ungroup() |>
    mutate(
      from = "cause of closure",
      to = cause_super_type,
      label = paste0(cause_super_type, " (", frequency, ")")
    ) |>
    select(from, to, label)
  causes_2 <- closure_causes |>
    filter(!grepl("- other", cause_type)) |>
    group_by(cause_type, cause_super_type) |>
    summarize(frequency = sum(frequency)) |>
    ungroup() |>
    mutate(
      from = cause_super_type,
      to = cause_type,
      label = sapply(strsplit(cause_type, " - "), function(x) tail(x, n = 1)),
      label = paste0(label, " (", frequency, ")")
    ) |>
    select(from, to, label)
  causes_3 <- closure_causes |>
    filter(!grepl("- other", cause)) |>
    group_by(cause, cause_type, cause_super_type) |>
    summarize(frequency = sum(frequency)) |>
    ungroup() |>
    mutate(
      from = cause_type,
      to = cause,
      label = sapply(strsplit(cause, " - "), function(x) tail(x, n = 1)),
      label = paste0(label, " (", frequency, ")")
    ) |>
    select(from, to, label)
  super_types_with_types <- closure_causes |>
    ungroup() |>
    filter(!grepl("- other", cause_type)) |>
    select(cause_super_type) |>
    distinct()
  types_with_causes <- closure_causes |>
    ungroup() |>
    filter(!grepl("- other", cause)) |>
    select(cause_type) |>
    distinct()
  counter <- 1
  for (i in 1:nrow(super_types_with_types)) {
    new_row_1 <- data.frame(
      from=super_types_with_types$cause_super_type[i],
      to=as.character(counter),
      label=""
    )
    new_row_2 <- data.frame(
      from=super_types_with_types$cause_super_type[i],
      to=paste("z", as.character(counter)),
      label=""
    )
    counter <- counter + 1
    causes_2 <- causes_2 |>
      rbind(new_row_1) |>
      rbind(new_row_2)
  }
  for (i in 1:nrow(types_with_causes)) {
    new_row_1 <- data.frame(
      from=types_with_causes$cause_type[i],
      to=as.character(counter),
      label=""
    )
    new_row_2 <- data.frame(
      from=types_with_causes$cause_type[i],
      to=paste("z", as.character(counter)),
      label=""
    )
    counter <- counter + 1
    causes_3 <- causes_3 |>
      rbind(new_row_1) |>
      rbind(new_row_2)
  }
  if(reason_level == "cause_super_type") {
    causes_tree <- causes_1 |>
      filter(to != "") |>
      filter(!is.na(from)) |>
      arrange(from, to)
  } else if(reason_level == "cause_type") {
    causes_tree <- rbind(causes_1, causes_2) |>
      filter(to != "") |>
      filter(!is.na(from)) |>
      arrange(from, to)
  } else {
    causes_tree <- rbind(causes_1, causes_2) |>
      rbind(causes_3) |>
      filter(to != "") |>
      filter(!is.na(from)) |>
      arrange(from, to)
  }
  cause_instance_labels <- causes_tree |>
    mutate(name=to) |>
    select(name, label)
  graph <- graph_from_data_frame(causes_tree, directed=TRUE)
  V(graph)$distance_to_root <- distances(
    graph, v=V(graph), to=which(V(graph)$name == "cause of closure")
  )
  max_distance <- max(V(graph)$distance_to_root)
  layout <- create_layout(graph, layout="dendrogram", circular=FALSE)
  layout$y <- layout$distance_to_root - max_distance
  layout |> left_join(cause_instance_labels, by="name")
}

closure_type_hierarchy <- function(closure_causes_hierarchy_layout, reason_level) {
  taxonomy_theme <- theme(
    panel.background = element_rect(fill="white"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_text(size="18"),
    legend.position = "bottom",
    legend.title = element_text(size="14"),
    legend.text = element_text(size="12"),
    legend.background = element_rect(fill="white"),
    legend.key = element_rect(fill="white")
  )
  max_distance <- max(closure_causes_hierarchy_layout$distance_to_root)
  ggraph(closure_causes_hierarchy_layout) + 
    geom_edge_diagonal(
      aes(colour=ifelse(label=="", "dummy", "normal")),
      show.legend=FALSE
    ) +
    geom_node_point(
      data=closure_causes_hierarchy_layout |> filter(label!=""),
      alpha=0.7,
      size=2,
      show.legend=FALSE
    ) +
    geom_node_text(
      aes(label=label),
      size=ifelse(reason_level=="cause", 4, 5),
      angle=0,
      vjust="center",
      hjust="left",
      nudge_y=0.02
    ) +
    coord_flip() +
    scale_y_continuous(limits=c(-max_distance, 1)) +
    scale_edge_colour_manual(
      values=c("dummy"="white", "normal"="lightgrey")
    ) +
    labs(
      title="Hierarchy of types of reason for museum closure"
    ) +
    taxonomy_theme
}

closure_type_hierarchy_small <- function(closure_causes_hierarchy_layout) {
  taxonomy_theme <- theme(
    panel.background = element_rect(fill="white"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_text(size="11"),
    legend.position = "bottom",
  )
  max_distance <- max(closure_causes_hierarchy_layout$distance_to_root)
  ggraph(closure_causes_hierarchy_layout) + 
    geom_edge_diagonal(colour="lightgrey") +
    geom_node_point(
      alpha=0.7,
      size=2
    ) +
    coord_flip() +
    scale_y_continuous(limits=c(-max_distance, 1)) +
    labs(
      title="Hierarchy of reasons for closure"
    ) +
    taxonomy_theme
}

shorten_cause_labels <- function(label) {
  split_label <- strsplit(label, " - ")[[1]]
    if (length(split_label) > 1) {
      paste(substr(split_label[1], 1, 7), substr(split_label[length(split_label)], 1, 7), sep = " ... ")
    } else {
      substr(label, 1, 7)
    }
  }

