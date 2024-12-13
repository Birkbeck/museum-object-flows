snapshotUI <- function(id) {
  fluidPage(
    fluidRow(
      p(
        "The distribution of different types of museum across the UK in a single year or during a selected time period."
      ),
      p(
        "If viewing museums in a single year, charts display the number of museums open at the end of that year. If viewing museums during a time period, charts display the number of museums open for at least some of the period."
      ),
      p(
        "The numbers in the charts show the estimated number of museums. Estimates take into account uncertain opening and closure dates."
      ),
      radioButtons(
        NS(id, "yearOrRange"),
        label="Single year or range of years:",
        choices=c("Single year", "Range of years"),
        selected="Single year",
        inline=TRUE
      ),
      uiOutput(NS(id, "yearSlider")),
    ),
    hr(style=hr_style),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          NS(id, "filterField"),
          label="Filter by:",
          choices=c("No filter", field_names$name),
          selected="Governance"
        ),
        uiOutput(NS(id, "subjectChoices")),
        pickerInput(
          NS(id, "choicesField"),
          "Show Only:", 
          choices=NULL,
          selected=NULL,
          options=pickerOptions(
            actionsBox=TRUE, 
            size=10,
            selectedTextFormat="count > 3"
          ), 
          multiple=TRUE
        ),
        p("Use dimension 2 to adjust the second axis of the heatmap charts"),
        selectInput(
          NS(id, "dimension2"),
          label="Dimension 2:",
          choices=field_names$name,
          selected="Country/Region"
        ),
      ),
      mainPanel(
        plotlyOutput(NS(id, "mainPlot"), height="720px", width="720px"),
        uiOutput(NS(id, "mainplotExplanation"))
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
          NS(id, "museumMapSmall"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "museumMap")
        ),
      ),
      column(
        3,
        plotOutput(
          NS(id, "museumCountsSmall"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "museumCounts")
        ),
      ),
      column(
        3,
        plotOutput(
          NS(id, "museumHeatmapSmall"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "museumHeatmap")
        ),
      ),
    ),
    hr(style=hr_style),
    fluidRow(
      h3("Museums Open During Period"),
      DTOutput(NS(id, "openMuseumsTable"))
    )
  )
}

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
        "No filter"="All Museums",
        "size"="Museum Size",
        "governance"="Museum Governance",
        "accreditation"="Museum Accreditation",
        "main_subject"="Subject Matter",
        "region"="Country/Region"
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
    metric <- reactive({
      if(year_or_range() == "Single year") {
        return("end_total")
      }
      return("period_total")
    })
    observeEvent(year_or_range(), {
      freezeReactiveValue(input, "yearRange")
      if (year_or_range() == "Single year") {
        output$yearSlider <- renderUI(
          sliderInput(
            NS(id, "year"),
            label="Year:",
            value=c(2024),
            min=1960,
            max=2024,
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
            value=c(1960, 2024),
            min=1960,
            max=2024,
            step=1,
            sep="",
            ticks=TRUE,
            width="100%"
          )
        )
      }
    }) 

    snapshot_data <- reactive({
      get_open_and_close_data(
        museums,
        filter_field(),
        period_start(),
        period_end()
      )
    })
    filter_field <- reactive({
      if (filter_field_1() == "main_subject" && subject_filter() != "All") {
        return("subject_matter")
      }
      return(filter_field_1())
    })
    filter_field_1 <- reactive({
      req(input$filterField)
      if (input$filterField == "No filter") {
        return("No filter")
      }
      return(
        filter(field_names, name==input$filterField)$value[1]
      )
    })
    filter_field_2 <- reactive({
      req(input$dimension2)
      return(
        filter(field_names, name==input$dimension2)$value[1]
      )
    })
    choices <- reactive({
      req(input$choicesField)
      if (filter_field() != "subject_matter") {
        return(
          filter(
            filter_field_choices,
            label %in% input$choicesField
            & filter_field_1() %in% field
          )$value
        )
      } else {
        return (
          filter(
            subject_filter_field_choices,
            subject_matter %in% input$choicesField
          )$subject_matter
        )
      }
    })
    subject_filter <- reactive({
      req(input$subjectFilterField)
      if (input$subjectFilterField == "All") {
        return("All")
      } else {
        return(
          filter(
            filter_field_choices,
            label == input$subjectFilterField
          )$value
        )
      }
    })
    observeEvent(filter_field_1(), {
      freezeReactiveValue(input, "choicesField")
      choices <- filter_field_choices |> filter(field==filter_field_1())
      selected_choices <- choices |> filter(!value %in% by_default_ignore)
      if (filter_field_1() == "main_subject") {
        output$subjectChoices <- renderUI({
          selectInput(
            NS(id, "subjectFilterField"),
            label="Filter subject:",
            choices=c("All", choices$label),
            selected="All"
          )
        })
      } else {
        output$subjectChoices <- renderUI({})
      }
      updatePickerInput(
        inputId="choicesField",
        choices=choices$label,
        selected=selected_choices$label
      ) 
      filter2 <- field_names |> filter(value != filter_field_1())
      if("Country/Region" %in% filter2$name) {
        selected <- "Country/Region"
      } else {
        selected <- "Governance"
      }
      updateSelectInput(
        inputId="filterField2",
        choices=filter2$name,
        selected=selected
      )
    })
    observeEvent(subject_filter(), {
      if (subject_filter() == "All") {
        choices <- filter_field_choices |> filter(field==filter_field_1())
        updatePickerInput(
          inputId="choicesField",
          choices=choices$label,
          selected=choices$label
        )
      } else {
        choices <- subject_filter_field_choices |> filter(main_subject==subject_filter())
        updatePickerInput(
          inputId="choicesField",
          choices=choices$subject_matter,
          selected=choices$subject_matter
        )
      }
    })
    
    mainPlot <- reactiveVal("museumMap")
    # Update the current plot based on user clicks
    observeEvent(input$museumMap, { mainPlot("museumMap") })
    observeEvent(input$museumCounts, { mainPlot("museumCounts") })
    observeEvent(input$museumHeatmap, { mainPlot("museumHeatmap") })
    
    x_label <- "Number of Museums"
    y_label <- reactive({input$filterField})
    title <- reactive({
      if (filter_field_1() == "No filter") {
        return(x_label)
      } else {
        return(paste(x_label, "by", y_label()))
      }
    })
    
    output$mainPlot <- renderPlotly({
      if (mainPlot() == "museumMap") {
        museum_map(
          museums,
          filter_field(),
          choices(),
          year_or_range(),
          period_start(),
          period_end()
        )
      } else if(mainPlot() == "museumCounts") {
        bar_chart(
          snapshot_data(),
          filter_field(),
          metric(),
          title(),
          y_label(),
          x_label,
          choices()
        )
      } else if(mainPlot() == "museumHeatmap") {
        snapshot_heatmap(
          museums,
          filter_field(),
          filter_field_2(),
          choices(),
          year_or_range(),
          period_start(),
          period_end(),
          input$filterField,
          input$dimension2
        )
      }
    })
    output$mainplotExplanation <- renderUI({
      explanation_text <- filter(explanations, main_plot==mainPlot())$explanation
      p(explanation_text)
    })
    
    output$museumMapSmall <- renderPlot({
      museum_map_small(
        museums,
        filter_field(),
        choices(),
        year_or_range(),
        period_start(),
        period_end()
      )
    }, width=small_chart_size, height=small_chart_size)
    output$museumCountsSmall <- renderPlot({
      bar_chart_small(
        snapshot_data(),
        filter_field(),
        metric(),
        ifelse(
          year_or_range() == "Single year",
          paste("Museums in the UK", input$year[1]),
          paste0("Museums in the UK ", input$yearRange[1], "-", input$yearRange[2])
        ),
        "Number of Museums",
        choices()
      )
    }, width=small_chart_size, height=small_chart_size)
    output$museumHeatmapSmall <- renderPlot({
      if (filter_field_1() != "No filter") {
        snapshot_heatmap_small(
          museums,
          filter_field(),
          filter_field_2(),
          choices(),
          "Range of years",
          period_start(),
          period_end(),
          input$filterField,
          input$dimension2
        )
      }
    }, width=small_chart_size, height=small_chart_size)
    
    output$openMuseumsTable <- renderDT({
      if (filter_field_1() == "No filter") {
        field_must_contain <- c("All")
      } else {
        field_must_contain <- choices()
      }
      if (year_or_range() == "Single year") {
        measure <- "prob_open_at_end"
      } else {
        measure <- "prob_open_for_some_of_period"
      }
      get_open_in_time_period(
        museums |> filter(.data[[filter_field()]] %in% field_must_contain),
        period_start(),
        period_end(),
        measure
      )
    })
  })
}

snapshotApp <- function() {
  ui <- snapshotUI("snapshot")
  server <- function(input, output, session) {
    snapshotServer("snapshot")
  }
  shinyApp(ui, server)
}

museum_map <- function(museums, dimension, show_only_choices, year_or_range, start, end) {
  if (dimension == "No filter") {
    show_only_choices <- c("All")
  }

  if (year_or_range == "Single year") {
    period <- end 
  } else {
    period <- paste0(start, "-", end)
  }
  ggplot(
    museums |>
      filter(.data[[dimension]] %in% show_only_choices) |>
      filter(year_closed_2 > start & year_opened_1 < end),
    aes(label=name_of_museum, x=longitude, y=latitude, colour=.data[[dimension]])
  ) +
    geom_point(size=0.5) +
    scale_colour_manual(values=museum_attribute_colours, labels=tidy_labels) +
    labs(
      title = paste("Museums in the UK", period),
      x = "",
      y = "",
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size=14)
    )
}

museum_map_small <- function(museums, dimension, show_only_choices, year_or_range, start, end) {
  if (dimension == "No filter") {
    show_only_choices <- c("All")
  }

  if (year_or_range == "Single year") {
    period <- end 
  } else {
    period <- paste0(start, "-", end)
  }
  ggplot(
    museums |>
      filter(.data[[dimension]] %in% show_only_choices) |>
      filter(year_closed_2 > start & year_opened_1 < end),
    aes(label=name_of_museum, x=longitude, y=latitude, colour=.data[[dimension]])
  ) +
    geom_point(size=0.5) +
    scale_colour_manual(values=museum_attribute_colours, labels=tidy_labels) +
    labs(
      title=paste("Museums in the UK", period),
      x="",
      y="",
    ) +
    theme_minimal() +
    theme(
      legend.position="Non"
    )
}

snapshot_heatmap <- function(museums, dimension, dimension2, show_only_choices, year_or_range, start, end, x_label, y_label) {
  if (year_or_range == "Single year") {
    period <- end 
    metric <- "end_total"
  } else {
    period <- paste0(start, "-", end)
    metric <- "period_total"
  }
  museums_in_time_period <- museums |>
    group_by(.data[[dimension]], .data[[dimension2]]) |>
    museums_in_time_period(start, end)
  ggplot(
    museums_in_time_period |>
      filter(.data[[dimension]] %in% show_only_choices),
    aes(
      x=fct_rev(factor(.data[[dimension2]], museum_attribute_ordering)),
      y=factor(.data[[dimension]], museum_attribute_ordering),
      fill=.data[[metric]]
    )
  ) +
    geom_tile(show.legend=FALSE) +
    geom_text(aes(label=.data[[metric]])) +
    scale_x_discrete(labels=short_labels) +
    scale_y_discrete(labels=short_labels) +
    scale_fill_continuous(low="white", high="purple") +
    labs(
      title=paste("Museums in the UK", period),
      x=x_label,
      y=y_label,
    ) +
    theme_minimal() +
    theme(
      legend.position="Non",
      axis.text.x=element_text(angle=45, vjust=0.5, hjust=1)
    )
  }


snapshot_heatmap_small <- function(museums, dimension, dimension2, show_only_choices, year_or_range, start, end, x_label, y_label) {
  if (year_or_range == "Single year") {
    period <- end 
    metric <- "end_total"
  } else {
    period <- paste0(start, "-", end)
    metric <- "period_total"
  }
  museums_in_time_period <- museums |>
    group_by(.data[[dimension]], .data[[dimension2]]) |>
    museums_in_time_period(start, end)
  ggplot(
    museums_in_time_period |>
      filter(.data[[dimension]] %in% show_only_choices),
    aes(
      x=fct_rev(factor(.data[[dimension2]], museum_attribute_ordering)),
      y=factor(.data[[dimension]], museum_attribute_ordering),
      fill=.data[[metric]]
    )
  ) +
    geom_tile() +
    geom_text(aes(label=.data[[metric]])) +
    scale_x_discrete(labels=very_short_labels) +
    scale_y_discrete(labels=short_labels) +
    scale_fill_continuous(low="white", high="purple") +
    labs(
      title=paste0(x_label, " vs ", y_label),
      x="",
      y="",
    ) +
    theme_minimal() +
    theme(
      legend.position="Non",
      plot.title=element_text(size=14),
      axis.text.x=element_text(size=11, angle=45, vjust=0.5, hjust=1),
      axis.text.y=element_text(size=11)
    )
  }
