changesUI <- function(id) {
  fluidPage(
    fluidRow(
      p("Changes in the museum sector across the UK during a selected time period (between the start of the first year and the end of the last year). The numbers in the charts below show estimated numbers which take into account uncertain opening and closure dates."),
      p("Click on a smaller chart to view it enlarged in the main panel."),
      sliderInput(
        NS(id, "year_range"),
        label="Time Period:",
        value=c(2000, 2024),
        min=1960,
        max=2024,
        step=1,
        sep="",
        ticks=TRUE,
        width="100%"
      ),
      ),
    hr(style=hr_style),
    fluidRow(
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
          p("Use dimension 2 to adjust the second axis of the heatmap chart s"),
          selectInput(
            NS(id, "filterField2"),
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
      ),
    hr(style=hr_style),
    fluidRow(
      p("Click on one of the small charts below to see it enlarged in the main panel above.")
    ),
    fluidRow(
      column(
        3,
        plotOutput(
          NS(id, "openingsVsClosuresScatterSmall"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "openingsVsClosuresScatter")
        )
      ),
      column(
        3,
        plotOutput(
          NS(id, "timeSeriesSmall"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "timeSeriesLine")
        )
      ),
      column(
        3,
        plotOutput(
          NS(id, "openingRatesSmall"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "openingRateLine")
        )
      ),
      column(
        3,
        plotOutput(
          NS(id, "closureRatesSmall"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "closureRateLine")
        )
      ),
      column(
        3,
        plotOutput(
          NS(id, "openingsMap"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "openingsMap")
        )
      ),
      column(
        3,
        plotOutput(
          NS(id, "closuresMap"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "closuresMap")
        )
      ),
      column(
        3,
        plotOutput(
          NS(id, "openingsSmall2Way"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "openings2Way")
        )
      ),
      column(
        3,
        plotOutput(
          NS(id, "closuresSmall2Way"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "closures2Way")
        )
      ),
      column(
        3,
        plotOutput(
          NS(id, "openingsClosuresSmall"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "openingsClosures")
        )
      ),
      column(
        3,
        plotOutput(
          NS(id, "startEndSmall"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "startEnd")
        )
      ),
      column(
        3,
        plotOutput(
          NS(id, "openStartSmall2Way"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "openStart2Way")
        )
      ),
      column(
        3,
        plotOutput(
          NS(id, "openEndSmall2Way"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "openEnd2Way")
        )
      ),
      column(
        3,
        plotOutput(
          NS(id, "absoluteChangeSmall"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "absoluteChange")
        )
      ),
      column(
        3,
        plotOutput(
          NS(id, "percentageChangeSmall"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "percentageChange")
        )
      ),
      column(
        3,
        plotOutput(
          NS(id, "absoluteChangeSmall2Way"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "absoluteChange2Way")
        )
      ),
      column(
        3,
        plotOutput(
          NS(id, "percentageChangeSmall2Way"),
          width=small_chart_size_px,
          height=small_chart_size_px,
          click=NS(id, "percentageChange2Way")
        )
      ),
    ),
    hr(style=hr_style),
    fluidRow(
      h3("Museum Closures"),
      DTOutput(NS(id, "closuresTable"))
    ),
    fluidRow(
      h3("Museum Openings"),
      DTOutput(NS(id, "openingsTable"))
    )
  )
}

changesServer <- function(id) {
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
    
    openings_and_closures_data <- reactive({
      get_open_and_close_data(
        museums,
        filter_field(),
        input$year_range[1],
        input$year_range[2])
    })
    openings_and_closures_data_2_way <- reactive({
      get_2_way_open_and_close_data(
        museums,
        filter_field(),
        filter_field_2(),
        input$year_range[1],
        input$year_range[2])
    })
    
    currentMainPlot <- reactiveVal("openingsVsClosuresScatter")
    # Update the current plot based on user clicks
    observeEvent(input$openingsVsClosuresScatter, { currentMainPlot("openingsVsClosuresScatter") })
    observeEvent(input$timeSeriesLine, { currentMainPlot("timeSeriesLine") })
    observeEvent(input$closureRateLine, { currentMainPlot("closureRateLine") })
    observeEvent(input$openingRateLine, { currentMainPlot("openingRateLine") })
    observeEvent(input$openingsClosures, { currentMainPlot("openingsClosures") })
    observeEvent(input$startEnd, { currentMainPlot("startEnd") })
    observeEvent(input$openings2Way, { currentMainPlot("openings.2Way") })
    observeEvent(input$closures2Way, { currentMainPlot("closures.2Way") })
    observeEvent(input$absoluteChange, { currentMainPlot("change") })
    observeEvent(input$percentageChange, { currentMainPlot("change_pc") })
    observeEvent(input$absoluteChange2Way, { currentMainPlot("change.2Way") })
    observeEvent(input$percentageChange2Way, { currentMainPlot("change_pc.2Way") })
    observeEvent(input$openStart2Way, { currentMainPlot("start_total.2Way") })
    observeEvent(input$openEnd2Way, { currentMainPlot("end_total.2Way") })
    observeEvent(input$openingsMap, { currentMainPlot("openingsMap") })
    observeEvent(input$closuresMap, { currentMainPlot("closuresMap") })
    
    filter_field <- reactive({
      if (filter_field_1() == "main_subject" && subject_filter() != "All") {
        return("subject_matter")
      }
      return(filter_field_1())
    })     
    filter_field_1 <- reactive({
      req(input$filterField)
      if (input$filterField == "No filter") {
        return ("No filter")
      }
      return(
        filter(field_names, name==input$filterField)$value[1]
      )
    })
    filter_field_2 <- reactive({
      req(input$filterField2)
      return(
        filter(field_names, name==input$filterField2)$value[1]
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
    
    output$mainPlot <- renderPlotly({
      if (currentMainPlot() == "openingsVsClosuresScatter") {
        openings_vs_closures_scatter(
          openings_and_closures_data(),
          filter_field(),
          choices()
        )
      } else if (currentMainPlot() == "timeSeriesLine") {
        time_series_line(
          cumulative_counts_by_dimension(museums, filter_field()),
          filter_field(),
          "total",
          "Number of Museums over Time",
          "Number of Museums",
          choices(),
          input$year_range[1],
          input$year_range[2]
        )
      } else if (currentMainPlot() == "openingRateLine") {
        time_series_line(
          cumulative_counts_by_dimension(museums, filter_field()),
          filter_field(),
          "opening_count",
          "Number of Museum Openings over Time",
          "Number of Openings",
          choices(),
          input$year_range[1],
          input$year_range[2]
        )
      } else if (currentMainPlot() == "closureRateLine") {
        time_series_line(
          cumulative_counts_by_dimension(museums, filter_field()),
          filter_field(),
          "closure_count",
          "Number of Museum Closures over Time",
          "Number of Closures",
          choices(),
          input$year_range[1],
          input$year_range[2]
        )
      } else if (currentMainPlot() == "openingsClosures") {
        two_measure_bar_chart(
          openings_and_closures_data(),
          filter_field(),
          c("openings", "closures"),
          paste0("Openings vs Closures ", input$year_range[1], "-", input$year_range[2]),
          input$filterField,
          "Number of openings/closures",
          c("openings"="openings", "closures"="closures"),
          c("start_total"="#DDDDDD", "end_total"="#555555", "openings"="#6666FF", "closures"="#FF6666"),
          choices()
        )
      } else if (currentMainPlot() == "startEnd") {
        two_measure_bar_chart(
          openings_and_closures_data(),
          filter_field(),
          c(
            paste("open museums in", input$year_range[2]),
            paste("open museums in", input$year_range[1])
          ),
          paste0("Open Museums in ", input$year_range[1], " vs in ", input$year_range[2]),
          input$filterField,
          "Number of open museums",
          c(
            "start_total"=paste("open museums in", input$year_range[1]),
            "end_total"=paste("open museums in", input$year_range[2])
          ),
          setNames(
            c("#DDDDDD", "#555555"),
            c(
              paste("open museums in", input$year_range[1]),
              paste("open museums in", input$year_range[2])
            )
          ),
          choices()
        )
      } else if (currentMainPlot() == "openingsMap") {
        changes_map(
          museums,
          filter_field(),
          "openings",
          choices(),
          input$year_range[1],
          input$year_range[2]
        )
      } else if (currentMainPlot() == "closuresMap") {
        changes_map(
          museums,
          filter_field(),
          "closures",
          choices(),
          input$year_range[1],
          input$year_range[2]
        )
      } else if (grepl("2Way", currentMainPlot(), fixed=TRUE)) {
        measure <- strsplit(currentMainPlot(), ".", fixed=TRUE)[[1]][1]
        title <- paste(x_labels()[measure])
        y_label <- input$filterField
        x_label <- input$filterField2
        heatmap(
          openings_and_closures_data_2_way(),
          filter_field(),
          filter_field_2(),
          measure,
          title,
          y_label,
          x_label,
          choices()
        )
      } else {
        x_label <- x_labels()[currentMainPlot()]
        y_label <- input$filterField
        title <- paste(x_label, "by", y_label)
        bar_chart(
          openings_and_closures_data(),
          filter_field(),
          currentMainPlot(),
          title,
          y_label,
          x_label,
          choices()
        )
      }
    })
    output$mainplotExplanation <- renderUI({
      explanation_text <- filter(explanations, main_plot==currentMainPlot())$explanation
      p(explanation_text)
    })
    
    output$openingsVsClosuresScatterSmall <- renderPlot({
      openings_vs_closures_scatter_small(
        openings_and_closures_data(),
        filter_field(),
        choices())
    }, width=small_chart_size, height=small_chart_size)
    output$timeSeriesSmall <- renderPlot({
      time_series_line_small(
        cumulative_counts_by_dimension(museums, filter_field()),
        filter_field(),
        "total",
        "Museums over Time",
        "Number of Museums",
        choices(),
        input$year_range[1],
        input$year_range[2]
      )
    }, width=small_chart_size, height=small_chart_size)
    output$openingRatesSmall <- renderPlot({
      time_series_line_small(
        cumulative_counts_by_dimension(museums, filter_field()),
        filter_field(),
        "opening_count",
        "Openings over Time",
        "Number of Openings",
        choices(),
        input$year_range[1],
        input$year_range[2]
      )
    }, width=small_chart_size, height=small_chart_size)
    output$closureRatesSmall <- renderPlot({
      time_series_line_small(
        cumulative_counts_by_dimension(museums, filter_field()),
        filter_field(),
        "closure_count",
        "Closures over Time",
        "Number of Closures",
        choices(),
        input$year_range[1],
        input$year_range[2]
      )
    }, width=small_chart_size, height=small_chart_size)
    output$openingsClosuresSmall <- renderPlot({
      two_measure_bar_chart_small(
        openings_and_closures_data(),
        filter_field(),
        c("openings", "closures"),
        paste0("Openings vs Closures ", input$year_range[1], "-", input$year_range[2]),
        c("openings"="openings", "closures"="closures"),
        c("start_total"="#DDDDDD", "end_total"="#555555", "openings"="#6666FF", "closures"="#FF6666"),
        choices()
      )
    }, width=small_chart_size, height=small_chart_size)
    output$startEndSmall <- renderPlot({
      two_measure_bar_chart_small(
        openings_and_closures_data(),
        filter_field(),
        c(
          paste("open in", input$year_range[2]),
          paste("open in", input$year_range[1])
        ),
        paste0("Open Museums ", input$year_range[1], " & ", input$year_range[2]),
        c(
          "start_total"=paste("open in", input$year_range[1]),
          "end_total"=paste("open in", input$year_range[2])
        ),
        setNames(
          c("#DDDDDD", "#555555"),
          c(
            paste("open in", input$year_range[1]),
            paste("open in", input$year_range[2])
          )
        ),
        choices()
      )
    }, width=small_chart_size, height=small_chart_size)
    output$openingsSmall2Way <- renderPlot({
      heatmap_small(
        openings_and_closures_data_2_way(),
        filter_field(),
        filter_field_2(),
        "openings",
        paste0("Openings ", input$year_range[1], "-", input$year_range[2]),
        choices()
      )
    }, width=small_chart_size, height=small_chart_size)
    output$closuresSmall2Way <- renderPlot({
      heatmap_small(
        openings_and_closures_data_2_way(),
        filter_field(),
        filter_field_2(),
        "closures",
        paste0("Closures ", input$year_range[1], "-", input$year_range[2]),
        choices()
      )
    }, width=small_chart_size, height=small_chart_size)
    output$absoluteChangeSmall <- renderPlot({
      bar_chart_small(
        openings_and_closures_data(),
        filter_field(),
        "change",
        paste0("Change ", input$year_range[1], "-", input$year_range[2]),
        "Change",
        choices()
      )
    }, width=small_chart_size, height=small_chart_size)
    output$percentageChangeSmall <- renderPlot({
      bar_chart_small(
        openings_and_closures_data(),
        filter_field(),
        "change_pc",
        paste0("Change (%) ", input$year_range[1], "-", input$year_range[2]),
        "Change (%)",
        choices()
      )
    }, width=small_chart_size, height=small_chart_size)
    output$absoluteChangeSmall2Way <- renderPlot({
      heatmap_small(
        openings_and_closures_data_2_way(),
        filter_field(),
        filter_field_2(),
        "change",
        paste0("Change ", input$year_range[1], "-", input$year_range[2]),
        choices()
      )
    }, width=small_chart_size, height=small_chart_size)
    output$percentageChangeSmall2Way <- renderPlot({
      heatmap_small(
        openings_and_closures_data_2_way(),
        filter_field(),
        filter_field_2(),
        "change_pc",
        paste0("Change (%) ", input$year_range[1], "-", input$year_range[2]),
        choices()
      )
    }, width=small_chart_size, height=small_chart_size)
    output$openingsMap <- renderPlot({
      changes_map_small(
        museums,
        filter_field(),
        "openings",
        choices(),
        input$year_range[1],
        input$year_range[2]
      )
    }, width=small_chart_size, height=small_chart_size)
    output$closuresMap <- renderPlot({
      changes_map_small(
        museums,
        filter_field(),
        "closures",
        choices(),
        input$year_range[1],
        input$year_range[2]
      )
    }, width=small_chart_size, height=small_chart_size)
    output$openStartSmall2Way <- renderPlot({
      heatmap_small(
        openings_and_closures_data_2_way(),
        filter_field(),
        filter_field_2(),
        "start_total",
        paste0("Open Museums ", input$year_range[1]),
        choices()
      )
    }, width=small_chart_size, height=small_chart_size)
    output$openEndSmall2Way <- renderPlot({
      heatmap_small(
        openings_and_closures_data_2_way(),
        filter_field(),
        filter_field_2(),
        "end_total",
        paste0("Open Museums ", input$year_range[2]),
        choices()
      )
    }, width=small_chart_size, height=small_chart_size)
    
    output$closuresTable <- renderDT({
      if (filter_field_1() == "No filter") {
        field_must_contain <- c("All")
      } else {
        field_must_contain <- choices()
      }
      get_closures_in_time_period(
        museums |> filter(.data[[filter_field()]] %in% field_must_contain),
        input$year_range[1],
        input$year_range[2]
      )
    })
    output$openingsTable <- renderDT({
      if (filter_field_1() == "No filter") {
        field_must_contain <- c("All")
      } else {
        field_must_contain <- choices()
      }
      get_openings_in_time_period(
        museums |> filter(.data[[filter_field()]] %in% field_must_contain),
        input$year_range[1],
        input$year_range[2]
      )
    })
  })
}

openings_vs_closures_scatter <- function(data, dimension, show_only_choices) {
  if (dimension == "No filter") {
    show_only_choices <- c("All")
  }
  data <- data |>
    mutate(`total openings and closures`=turnover)
  ggplot(data |> filter(.data[[dimension]] %in% show_only_choices), aes(x=closure_rate, y=opening_rate)) +
    annotate("text", x=3, y=2, size=6, label="Stasis", alpha=0.5) +
    annotate("text", x=3, y=77, size=6, label="Growth", alpha=0.5) +
    annotate("text", x=75, y=2, size=6, label="Decline", alpha=0.5) +
    annotate("text", x=75, y=77, size=6, label="Churn", alpha=0.5) +
    geom_point(aes(colour=.data[[dimension]], size=`total openings and closures`)) +
    geom_abline(colour="grey") +
    geom_text(aes(label=tidy_labels[.data[[dimension]]])) +
    scale_x_continuous(expand=c(0,1), limits=c(0,80)) +
    scale_y_continuous(expand=c(0,1), limits=c(0,80)) +
    scale_size_continuous(limits=c(1, 2000), range=c(2, 40), breaks=c(1, 5, 10, 50, 100, 500, 1000), labels=c("1", "5", "10", "50", "100", "500", "1000")) +
    scale_colour_manual(values=museum_attribute_colours) +
    labs(
      title = "Opening vs Closure Rates",
      x = "Closures per hundred museums",
      y = "Openings per hundred museums"
    ) +
    guides(
      colour="none",
      size=guide_legend(title="Total Openings and Closures")
    ) +
    theme_minimal() +
    theme(
      legend.position = "right"
    )
}

openings_vs_closures_scatter_small <- function(data, dimension, show_only_choices) {
  if (dimension == "No filter") {
    show_only_choices <- c("All")
  }
  
  ggplot(data |> filter(.data[[dimension]] %in% show_only_choices), aes(x=closure_rate, y=opening_rate)) +
    geom_point(aes(colour=.data[[dimension]], size=turnover)) +
    geom_text_repel(aes(label=very_short_labels[.data[[dimension]]])) +
    geom_abline(colour="grey") +
    scale_x_continuous(expand=c(0,1), limits=c(0,80)) +
    scale_y_continuous(expand=c(0,1), limits=c(0,80)) +
    scale_colour_manual(values=museum_attribute_colours) +
    labs(
      title = "Opening vs Closure Rates",
      x = "Closure Rate",
      y = "Opening Rate"
    ) +
    theme_minimal() +
    theme(
      legend.position = "None",
      axis.title = element_text(size=14),
      axis.text = element_text(size=11),
      axis.line.x.bottom = element_line(colour="black"),
      axis.line.y.left = element_line(colour="black")
    )
}

bar_chart <- function(data, dimension, measure, title, y_label, x_label, show_only_choices) {
  if (dimension == "No filter") {
    show_only_choices <- c("All")
  }
  data <- data |> filter(.data[[dimension]] %in% show_only_choices)
  
  if (measure %in% c("closures")) {
    fill_scale <- scale_fill_manual(values=c("TRUE"="#FF6666", "FALSE"="#FF6666"))
  } else {
    fill_scale <- scale_fill_manual(values=c("TRUE"="#6666FF", "FALSE"="#FF6666"))
  }
  
  bar_chart <- ggplot(
    data |> filter(.data[[dimension]] %in% show_only_choices),
    aes(
      x=.data[[measure]],
      y=.data[[dimension]],
      label=.data[[dimension]],
      fill=.data[[measure]] > 0
    )
  ) +
    geom_bar(position="dodge", stat="identity", show.legend=FALSE) +
    scale_y_discrete(labels=tidy_labels) +
    fill_scale +
    guides(fill=FALSE) +
    geom_text(
      aes(label=.data[[measure]]),
      position=position_dodge(),
      size=3
    ) +
    labs(
      title = title,
      y = y_label,
      x = x_label
    ) +
    theme_minimal()

  bar_chart |> ggplotly(tooltip=c("label", "x"))
}

bar_chart_small <- function(data, dimension, measure, title, x_label, show_only_choices) {
  if (dimension == "No filter") {
    show_only_choices <- c("All")
  }
  data <- data |> filter(.data[[dimension]] %in% show_only_choices)
  
  if (measure %in% c("closures")) {
    fill_scale <- scale_fill_manual(values=c("TRUE"="#FF6666", "FALSE"="#FF6666"))
  } else {
    fill_scale <- scale_fill_manual(values=c("TRUE"="#6666FF", "FALSE"="#FF6666"))
  }
  
  if (measure %in% c("openings", "closures")) {
    axis_max <- max(c(max(data$openings), max(data$closures)))
    axis_min <- 0
  } else if (measure %in% c("start_total", "end_total")) {
    axis_max <- max(c(max(data$start_total), max(data$end_total)))
    axis_min <- 0
  } else {
    axis_max <- max(data[[measure]])
    axis_min <- min(data[[measure]])
    if (axis_max < 0) {
      axis_max <- 0
    }
    if (axis_min > 0) {
      axis_min <- 0
    }
  }
  largest_magnitude <- max(axis_max, abs(axis_min))
  if (largest_magnitude > 100) {
    scaling_factor <- 0.01
  } else {
    scaling_factor <- 0.1
  }
  axis_max <- ceiling(axis_max * scaling_factor) / scaling_factor
  axis_min <- floor(axis_min * scaling_factor) / scaling_factor
  if (axis_min < 0) {
    limits <- c(axis_min*1.1, axis_max*1.1)
    breaks <- c(axis_min, 0, axis_max)
  } else {
    limits <- c(0, axis_max*1.1)
    breaks <- c(0, axis_max)
  }
  
  ggplot(
    data,
    aes(
      x=.data[[measure]],
      y=.data[[dimension]],
      fill=.data[[measure]] > 0
    )
  ) +
    geom_bar(position="dodge", stat="identity") +
    geom_vline(xintercept=0, colour="black") +
    scale_x_continuous(limits=limits, expand=c(0.001,0), breaks=breaks) +
    scale_y_discrete(labels=short_labels) +
    fill_scale +
    labs(
      title = title,
      y = "",
      x = "" 
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size=14),
      plot.subtitle = element_text(size=0),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=0),
      axis.text.y = element_text(size=11),
      axis.text.x = element_text(size=11),
      axis.line.x.bottom = element_line(colour="black"),
      legend.position="Non"
    )
}

two_measure_bar_chart <- function(data, dimension, measures, title, y_label, x_label, fill_labels, fill_values, show_only_choices) {
  if (dimension == "No filter") {
    show_only_choices <- c("All")
  }
  colnames(data) <- ifelse(
    colnames(data) %in% names(fill_labels),
    fill_labels[colnames(data)],
    colnames(data)
  )
  data <- data |> filter(.data[[dimension]] %in% show_only_choices) |>
    pivot_longer(
      cols=measures,
      names_to="label",
      values_to="count"
    )
  fill_scale <- scale_fill_manual(values=fill_values)
  bar_chart <- ggplot(
    data,
    aes(
      x=count,
      y=.data[[dimension]],
      label=.data[[dimension]],
      fill=label
    )
  ) +
    geom_bar(position="dodge", stat="identity") +
    scale_y_discrete(labels=tidy_labels) +
    fill_scale +
    guides(fill=guide_legend(reverse=TRUE)) +
    geom_text(
      data=data |> filter(label==measures[1]),
      aes(label=count),
      nudge_y=0.25,
      size=3
    ) +
    geom_text(
      data=data |> filter(label==measures[2]),
      aes(label=count),
      nudge_y=-0.25,
      size=3
    ) +
    labs(
      title = title,
      y = y_label,
      x = x_label,
      fill = ""
    ) +
    theme_minimal() +
    theme(
      legend.position="bottom"
    )
  bar_chart |> ggplotly(tooltip=c("label", "x", "fill"))
}

two_measure_bar_chart_small <- function(data, dimension, measures, title, fill_labels, fill_values, show_only_choices) {
  if (dimension == "No filter") {
    show_only_choices <- c("All")
  }
  colnames(data) <- ifelse(
    colnames(data) %in% names(fill_labels),
    fill_labels[colnames(data)],
    colnames(data)
  )
  data <- data |> filter(.data[[dimension]] %in% show_only_choices) |>
    pivot_longer(
      cols=measures,
      names_to="label",
      values_to="count"
    )
  fill_scale <- scale_fill_manual(values=fill_values)
  ggplot(
    data,
    aes(
      x=count,
      y=.data[[dimension]],
      fill=label
    )
  ) +
    geom_bar(position="dodge", stat="identity") +
    geom_vline(xintercept=0, colour="black") +
    scale_y_discrete(labels=short_labels) +
    fill_scale +
    labs(
      title = title,
      y = "",
      x = "" 
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size=14),
      plot.subtitle = element_text(size=0),
      legend.title = element_text(size=0),
      legend.text = element_text(size=11),
      axis.title.x = element_text(size=0),
      axis.title.y = element_text(size=0),
      axis.text.y = element_text(size=11),
      axis.text.x = element_text(size=11),
      axis.line.x.bottom = element_line(colour="black"),
      legend.position="bottom"
    )
}

changes_map <- function(museums, dimension, measure, show_only_choices, start, end) {
  if (dimension == "No filter") {
    show_only_choices <- c("All")
  }
  if (measure == "openings") {
    data <- museums |>
      filter(.data[[dimension]] %in% show_only_choices) |>
      filter(year_opened_2 > start & year_opened_1 < end)
  } else if (measure == "closures") {
    data <- museums |>
      filter(.data[[dimension]] %in% show_only_choices) |>
      filter(year_closed_2 > start & year_closed_1 < end)
  }
  map <- ggplot(data) +
    geom_polygon(data=regions, aes(x=x, y=y, group=group), linewidth=0.1, label=NA, colour="black", fill=NA) +
    geom_point(aes(label=name_of_museum, x=bng_x, y=bng_y, colour=.data[[dimension]]), size=0.5) +
    scale_colour_manual(values=museum_attribute_colours, labels=tidy_labels) +
    coord_fixed() +
    labs(
      title=paste("Museum", measure, start, "-", end),
      x = "",
      y = "",
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size=14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour="white")
    )

  map |> ggplotly(tooltip=c("label", "colour"))
}

changes_map_small <- function(museums, dimension, measure, show_only_choices, start, end) {
  if (dimension == "No filter") {
    show_only_choices <- c("All")
  }
  if (measure == "openings") {
    data <- museums |>
      filter(.data[[dimension]] %in% show_only_choices) |>
      filter(year_opened_2 > start & year_opened_1 < end)
  } else if (measure == "closures") {
    data <- museums |>
      filter(.data[[dimension]] %in% show_only_choices) |>
      filter(year_closed_2 > start & year_closed_1 < end)
  }
  ggplot(data) +
    geom_polygon(data=regions, aes(x=x, y=y, group=group), linewidth=0.1, label=NA, colour="black", fill=NA) +
    geom_point(aes(label=name_of_museum, x=bng_x, y=bng_y, colour=.data[[dimension]]), size=0.5) +
    scale_colour_manual(values=museum_attribute_colours, labels=tidy_labels) +
    coord_fixed() +
    labs(
      title=paste("Museum", measure, start, "-", end),
      x="",
      y="",
    ) +
    theme_minimal() +
    theme(
      legend.position="Non",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour="white")
    )
}

heatmap <- function(museums, dimension, dimension2, measure, title, y_label, x_label, show_only_choices) {
  data <- museums |>
    filter(.data[[dimension]] %in% show_only_choices) |>
    mutate(
      fill_metric=ifelse(
        is.infinite(.data[[measure]]),
        NA,
        .data[[measure]]
      )
    )
  if (measure %in% c("openings", "start_total", "end_total")) {
    fill_scale <- scale_fill_gradient(low="white", high="blue")
  } else if (measure %in% c("closures")) {
    fill_scale <- scale_fill_gradient(low="white", high="red")
  } else {
    fill_scale <- scale_fill_gradient2(low="red", mid="white", high="blue", midpoint=0)
  }
  heatmap <- ggplot(
    data,
    aes(
      x=.data[[dimension2]],
      y=.data[[dimension]],
      fill=fill_metric
    )
  ) +
    geom_tile(alpha=0.7, show.legend=FALSE) +
    geom_text(aes(label=.data[[measure]]), size=4) +
    scale_x_discrete(labels=short_labels) +
    scale_y_discrete(labels=short_labels) +
    fill_scale +
    labs(
      title=title,
      x=x_label,
      y=y_label
    ) +
    theme_minimal() +
    theme(
      plot.title=element_text(size=14),
      axis.text.y=element_text(size=11),
      axis.text.x=element_text(size=11, angle=45, vjust=1, hjust=1)
    )

  heatmap |> ggplotly(tooltip=c("x", "y", "fill"))
}

heatmap_small <- function(museums, dimension, dimension2, measure, title, show_only_choices) {
  if (measure %in% c("openings", "start_total", "end_total")) {
    fill_scale <- scale_fill_gradient(low="white", high="blue")
  } else if (measure %in% c("closures")) {
    fill_scale <- scale_fill_gradient(low="white", high="red")
  } else {
    fill_scale <- scale_fill_gradient2(low="red", mid="white", high="blue", midpoint=0)
  }
  ggplot(
    museums |>
      filter(.data[[dimension]] %in% show_only_choices),
    aes(
      x=.data[[dimension2]],
      y=.data[[dimension]],
      fill=.data[[measure]]
    )
  ) +
    geom_tile(alpha=0.7) +
    geom_text(aes(label=round(.data[[measure]], 0)), size=4) +
    scale_x_discrete(labels=very_short_labels) +
    scale_y_discrete(labels=short_labels) +
    fill_scale +
    labs(
      title=title,
      x="",
      y="",
      ) +
    theme_minimal() +
    theme(
      legend.position="Non",
      plot.title=element_text(size=14),
      axis.text.y=element_text(size=11),
      axis.text.x=element_text(size=11, angle=45, vjust=1, hjust=1)
    )
}


cumulative_counts_by_dimension <- function(df, dimension) {
  min_year <- min(df$year_opened_1, df$year_closed_1[df$year_closed_1 != 9999])
  max_year <- max(df$year_opened_2, df$year_closed_2[df$year_closed_2 != 9999])
  all_years <- expand.grid(year = seq(min_year, max_year), dimension_value = unique(df[[dimension]]))
  colnames(all_years)[2] <- dimension
  openings <- df |>
    rowwise() |>
    mutate(
      year = list(seq(year_opened_1, year_opened_2)),
      opening_fraction = 1 / length(year)
    ) |>
    unnest(cols = c(year, opening_fraction)) |>
    group_by(year, .data[[dimension]]) |>
    summarize(opening_count = sum(opening_fraction)) |>
    ungroup()
  closures <- df |>
    rowwise() |>
    mutate(
      year = list(seq(year_closed_1, year_closed_2)),
      closure_fraction = 1 / length(year)
    ) |>
    filter(year_closed_1 != 9999) |>
    unnest(cols = c(year, closure_fraction)) |>
    group_by(year, .data[[dimension]]) |>
    summarize(closure_count = sum(closure_fraction)) |>
    ungroup()
  combined <- full_join(all_years, openings, by = c("year", dimension)) |>
    full_join(closures, by = c("year", dimension)) |>
    replace_na(list(opening_count = 0, closure_count = 0)) |>
    arrange(year) |>
    group_by(.data[[dimension]]) |>
    mutate(
      cumulative_opening = cumsum(opening_count),
      cumulative_closure = cumsum(closure_count),
      total = cumulative_opening - cumulative_closure
    )
  return(combined)
}

time_series_line <- function(data, dimension, measure, title, y_label, show_only_choices, start_year, end_year) {
  if (dimension == "No filter") {
    show_only_choices <- c("All")
  }
  ggplot(
    data |> filter(.data[[dimension]] %in% show_only_choices) |> filter(year>=start_year & year <= end_year),
    aes(x=year, y=.data[[measure]], colour=.data[[dimension]])
  ) + 
    geom_line(alpha=0.6 , size=1) +
    geom_text(
      data=data |> filter(.data[[dimension]] %in% show_only_choices) |> filter(year==floor(mean(c(start_year, end_year)))),
      aes(y=.data[[measure]]*1.1, label=tidy_labels[.data[[dimension]]])
    ) +
    scale_y_continuous(limits=function(x){c(0, max(x))}) +
    scale_colour_manual(values=museum_attribute_colours) +
    labs(
      title=title,
      y=y_label,
      x="Year"
    ) +
    guides(
      colour="none"
    ) +
    theme_minimal() +
    theme(
      legend.position = "None"
    )
}

time_series_line_small <- function(data, dimension, measure, title, y_label, show_only_choices, start_year, end_year) {
  if (dimension == "No filter") {
    show_only_choices <- c("All")
  }
  data <- data |> filter(.data[[dimension]] %in% show_only_choices) |> filter(year>=start_year & year <= end_year)
  
  if (measure %in% c("opening_count", "closure_count")) {
    axis_max <- max(c(max(data$opening_count), max(data$closure_count)))
    axis_min <- 0
  } else {
    axis_max <- max(data[[measure]])
    axis_min <- min(data[[measure]])
  }
  largest_magnitude <- max(axis_max, abs(axis_min))
  if (largest_magnitude > 100) {
    scaling_factor <- 0.01
  } else {
    scaling_factor <- 0.1
  }
  axis_max <- ceiling(axis_max * scaling_factor) / scaling_factor
  axis_min <- floor(axis_min * scaling_factor) / scaling_factor
  if (axis_min < 0) {
    limits <- c(axis_min*1.1, axis_max*1.1)
    breaks <- c(axis_min, 0, axis_max)
  } else {
    limits <- c(0, axis_max*1.1)
    breaks <- c(0, axis_max)
  }
  
  ggplot(
    data,
    aes(x=year, y=.data[[measure]], colour=.data[[dimension]])
  ) + 
    geom_line(alpha=0.6 , size=1) +
    geom_text(
      data=data |> filter(.data[[dimension]] %in% show_only_choices) |> filter(year==floor(mean(c(start_year, end_year)))),
      aes(y=.data[[measure]]*1.1, label=tidy_labels[.data[[dimension]]])
    ) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), limits=limits, breaks=breaks) +
    scale_colour_manual(values=museum_attribute_colours) +
    labs(
      title=title,
      y=y_label,
      x="Year"
    ) +
    theme_minimal() +
    theme(
      legend.position = "None",
      plot.title = element_text(size=14),
      axis.title = element_text(size=14),
      axis.text = element_text(size=11),
      axis.line.x.bottom = element_line(colour="black"),
      axis.line.y.left = element_line(colour="black")
    )
}
