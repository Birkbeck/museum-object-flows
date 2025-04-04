library(truncnorm)

lengthUI <- function(id) {
  fluidPage(
    fluidRow(
      p("How long does it take for museums to close? See the charts below to see how length of closure varies by museum type and over time."),
      p("See the table at the bottom of this page for details of each museum and how long its closure took.")
    ),
    sidebarLayout(
      sidebarPanel(width=3,
        h3("Visualisation Options"),

        tagList(
          tags$span(
            tags$strong("Group museums by:"),
            tags$i(
              class = "fa fa-info-circle", # Font Awesome info icon
              style = "color: #007bff; cursor: pointer;", # Style for visibility
              `data-toggle` = "popover", # Bootstrap popover attribute
              `data-placement` = "right", # Position above the icon
              title = "Group museums by",
              `data-content` = "<p>Select which attribute museums should be grouped by.</p>"
            )
          ),
          tags$script(popover_js),
          selectInput(
            NS(id, "museumGrouping"),
            label="",
            choices=c(field_names$name),
            selected="All"
          )
        ),

        h4("Filter Museums"),
        p("Show only the closure length of certain types of museum."),

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

        tagList(
          tags$span(
            tags$strong("Example museum: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Example museum",
              `data-content` = "<p>Select a museum to display its closure timelines.</p>"
            )
          ),
          tags$script(popover_js),
          virtualSelectInput(
            NS(id, "exampleMuseum"),
            "",
            choices=NULL,
            selected=NULL,
            multiple=FALSE,
            disableSelectAll=FALSE,
            search=TRUE
          )
        ),
        
        div(uiOutput(NS(id, "mainPlotOptions")))
      ),
      mainPanel(
        plotlyOutput(NS(id, "mainPlot"), height="720px", width="100%"),
        uiOutput(NS(id, "mainPlotExplanation")),
        fluidRow(
          p("Click on one of the small charts below to see it enlarged in the main panel above.")
        ),
        fluidRow(
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "lengthTileChartSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "lengthTileChart")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "lengthLineChartSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "lengthLineChart")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "lengthScatterSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "lengthScatter")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "exampleTimelinesSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "exampleTimelines")
            )
          )
        )
      )
    ),
    fluidRow(
      h3("Lengths of Closure"),
      downloadButton(NS(id, "downloadLengthsTable"), label="Download table as CSV")
    ),
    fluidRow(
      DTOutput(NS(id, "closureLengthsTable"))
    )
  )
}

lengthServer <- function(id) {
  moduleServer(id, function(input, output, session) {
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
    museum_filters <- reactive({
      list(
        input$sizeFilter,
        input$governanceFilter,
        input$subjectFilter,
        input$subjectSpecificFilter,
        input$regionFilter,
        input$accreditationFilter
      )
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

    initial_museum_ids <- reactive({
      sapply(input$initialMuseum, function(text) sub(".*\\(([^()]*)\\)$", "\\1", text))
    })

    museums_list <- dispersal_events |>
      mutate(
        name=paste0(initial_museum_name, " (", initial_museum_id, ")")
      ) |>
      arrange(name) |>
      select(
        name,
        museum_id=initial_museum_id,
        size=initial_museum_size,
        governance=initial_museum_governance,
        governance_broad=initial_museum_governance_broad,
        subject_matter_broad=initial_museum_subject_matter_broad,
        subject_matter=initial_museum_subject_matter,
        region=initial_museum_region,
        country=initial_museum_country,
        accreditation=initial_museum_accreditation
      ) |>
      distinct()

    observeEvent(museum_filters(), {
      freezeReactiveValue(input, "exampleMuseum")
      filtered_museums_list <- museums_list |>
        filter(
          size %in% size_filter_choices(),
          governance %in% governance_filter_choices() | governance_broad %in% governance_filter_choices(),
          subject_matter_broad %in% subject_filter_choices(),
          subject_matter %in% specific_subject_filter_choices(),
          region %in% region_filter_choices() | country %in% region_filter_choices(),
          accreditation %in% accreditation_filter_choices()
        )
      becm <- "mm.domus.SW043"
      if (becm %in% filtered_museums_list$museum_id) {
        example_museum_name <- filter(filtered_museums_list, museum_id == becm)$name
      } else {
        example_museum_name <- slice_sample(filtered_museums_list, n=1)$name
      }
      updateVirtualSelect(
        session=session,
        inputId="exampleMuseum",
        choices=filtered_museums_list$name,
        selected=example_museum_name
      )
    })

    event_dates_table <- reactive({get_event_dates_table()})
    lengths_table <- reactive({get_lengths_table(event_dates_table())})

    example_museum_id <- reactive({
      filter(
        museums_list,
        name==input$exampleMuseum
      )$museum_id
    })

    output$mainPlotOptions <- renderUI({
      if(currentMainPlot() == "lengthTileChart") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show number of museums" = "count",
            "Show percentage of museums" = "percentage",
            "Show rowwise percentages" = "percentage_y",
            "Show columnwise percentages" = "percentage_x"
          )
        )
      } else if(currentMainPlot() == "lengthLineChart") {
        radioButtons(
          inputId = NS(id, "countOrPercentage"),
          label = "",
          choices = list(
            "Show number of museums" = "count",
            "Show percentage of museums" = "percentage"
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

    currentMainPlot <- reactiveVal("lengthTileChart")
    # Update the current plot based on user clicks
    observeEvent(input$lengthTileChart, { currentMainPlot("lengthTileChart") })
    observeEvent(input$lengthLineChart, { currentMainPlot("lengthLineChart") })
    observeEvent(input$lengthScatter, { currentMainPlot("lengthScatter") })
    observeEvent(input$exampleTimelines, { currentMainPlot("exampleTimelines") })

    output$mainPlot <- renderPlotly({
      if (currentMainPlot() == "lengthTileChart") {
        length_tile_chart(lengths_table(), count_or_percentage(), museum_grouping())
      } else if (currentMainPlot() == "lengthLineChart") {
        length_line_chart(lengths_table(), count_or_percentage(), museum_grouping())
      } else if (currentMainPlot() == "lengthScatter") {
        length_scatter(lengths_table(), museum_grouping())
      } else if (currentMainPlot() == "exampleTimelines") {
        example_timelines(event_dates_table(), example_museum_id())
      }
    })

    output$lengthTileChartSmall <- renderPlot({
      length_tile_chart_small(lengths_table(), museum_grouping())
    })
    output$lengthLineChartSmall <- renderPlot({
      length_line_chart_small(lengths_table(), museum_grouping())
    })
    output$lengthScatterSmall <- renderPlot({
      length_scatter_small(lengths_table(), museum_grouping())
    })
    output$exampleTimelinesSmall <- renderPlot({
      example_timelines_small(event_dates_table(), example_museum_id())
    })

    output$downloadLengthsTable <- downloadHandler(
      filename = function() {
        paste('length-of-closure-data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(
          lengths_table(),
          con
        )
      },
      contentType = "text/csv"
    )

    output$closureLengthsTable <- renderDT({
      lengths_table()
    }, options=list(pageLength=100))

  })
}

get_event_dates_table <- function() {
  closure_super_events <- dispersal_events |>
    mutate(museum_id=initial_museum_id) |>
    left_join(museums_including_crown_dependencies, by="museum_id") |>
    mutate(
      event_date=ifelse(
        year_closed_1==year_closed_2,
        year_closed_1,
        paste(year_closed_1, year_closed_2, sep="-")
      ),
      year1 = year_closed_1,
      year2 = year_closed_2,
      year = (year1 + year2) / 2
    ) |>
    mutate(
      event_level="super",
      event_description="closure"
    ) |>
    select(
      museum=initial_museum_name,
      museum_id=initial_museum_id,
      event_level,
      event_date,
      year1,
      year2,
      year,
      event_description
    ) |>
    distinct()
  closure_events <- dispersal_events |>
    filter(sender_name == initial_museum_name) |>
    mutate(
      event_level="sub",
      event_description=ifelse(
        is.na(collection_description),
        paste(collection_id, event_type, "to", recipient_name),
        paste(collection_description, event_type, "to", recipient_name)
      )
    ) |>
    select(
      museum=initial_museum_name,
      museum_id=initial_museum_id,
      event_level,
      event_date,
      event_description
    ) |>
    mutate(
      year1 = sub("/.*", "", event_date),
      year2 = sub(".*/", "", event_date),
      year1 = sub("-.*", "", year1),
      year2 = sub("-.*", "", year2),
      year1 = sub("\\?", "", year1),
      year2 = sub("\\?", "", year2),
      year1 = as.numeric(year1),
      year2 = as.numeric(year2),
      year2 = ifelse(is.na(year2), year1, year2),
      year = (year1 + year2) / 2,
    ) |>
    filter(year > 1000) |>
    select(
      museum,
      museum_id,
      event_level,
      event_date,
      year1,
      year2,
      year,
      event_description
    ) |>
    rbind(closure_super_events)

  museum_ordering <- closure_events |>
    filter(event_level=="super") |>
    distinct() |>
    arrange(year)
  
  closure_lengths <- closure_events |>
    filter(event_level=="sub") |>
    filter(!is.na(year)) |>
    group_by(museum_id) |>
    summarize(
      earliest = min(year),
      latest = max(year)
    ) |>
    left_join(closure_events |> filter(event_level=="super") |> select(museum_id, year), by="museum_id") |>
    rename(closure_date=year) |>
    mutate(
      time_between_closure_and_earliest = earliest - closure_date,
      latest_including_closure_date = ifelse(closure_date > latest, closure_date, latest),
      length_of_closure = latest_including_closure_date - closure_date
    )

  closure_events <- closure_events |> left_join(closure_lengths, by="museum_id") |>
    filter(
      closure_date < 9999,
      event_level == "super" | year1 >= closure_date
    ) |>
    mutate(
      closure_length_category = case_when(
        is.na(length_of_closure) ~ "unknown",
        length_of_closure < 1 ~ "< 1 year",
        length_of_closure < 2 ~ "< 2 years",
        length_of_closure < 4 ~ "< 4 years",
        length_of_closure < 8 ~ "< 8 years",
        length_of_closure < 16 ~ "< 16 years",
        TRUE ~ "16+ years"
      ),
      closure_length_category = factor(
        closure_length_category,
        c(
          "unknown",
          "< 1 year",
          "< 2 years",
          "< 4 years",
          "< 8 years",
          "< 16 years",
          "16+ years"
        )
      )
    )
}

get_lengths_table <- function(event_dates_table) {
  event_dates_table |>
    filter(event_level=="super") |>
    left_join(museums_including_crown_dependencies, by="museum_id") |>
    select(
      museum_id,
      museum,
      closure_date=event_date,
      earliest,
      latest,
      length_of_closure,
      closure_length_category,
      all,
      size,
      governance,
      governance_main,
      subject_matter,
      main_subject,
      region,
      accreditation
    )
} 

length_tile_chart <- function(lengths_table, count_or_percentage, museum_grouping) {
  heatmap_data <- lengths_table |>
    group_by(closure_length_category, .data[[museum_grouping]]) |>
    summarize(count=n()) |>
    ungroup() |>
    mutate(percentage=round(count / sum(count) * 100, 1)) |>
    group_by(.data[[museum_grouping]]) |>
    mutate(percentage_y=round(count / sum(count) * 100, 1)) |>
    ungroup() |>
    group_by(closure_length_category) |>
    mutate(percentage_x=round(count / sum(count) * 100, 1)) |>
    ungroup()
  heatmap <- ggplot(
    heatmap_data,
    aes(
      x=closure_length_category,
      y=.data[[museum_grouping]],
      fill=.data[[count_or_percentage]]
    )
  ) +
    geom_tile(show.legend=FALSE) +
    geom_text(aes(label=.data[[count_or_percentage]])) +
    scale_y_discrete(labels=short_labels) +
    heatmap_fill_scale +
    labs(
      title = "Length of closure",
      x = "Length of closure (years)",
      y = ""
    ) +
    standard_bars_theme +
    theme(
      axis.text.x=element_text(angle=45, vjust=0.5, hjust=1)
    )
  heatmap |> ggplotly(tooltip=c("x", "y", "fill"))
}

length_tile_chart_small <- function(lengths_table, museum_grouping) {
  heatmap_data <- lengths_table |>
    group_by(closure_length_category, .data[[museum_grouping]]) |>
    summarize(count=n()) |>
    ungroup()
  ggplot(
    heatmap_data,
    aes(
      x=closure_length_category,
      y=.data[[museum_grouping]]
    )
  ) +
    geom_tile(aes(fill=count)) +
    geom_text(aes(label=count)) +
    scale_y_discrete(labels=short_labels) +
    heatmap_fill_scale +
    labs(
      title = "Length of closure",
      x = "Length of closure (years)",
      y = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size=14),
      plot.subtitle = element_text(size=0),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=0),
      axis.text.y = element_text(size=11),
      axis.text.x = element_text(size=11, angle=45, hjust=1),
      axis.line.x.bottom = element_line(colour="black"),
      legend.position="Non"
    )
}

length_line_chart <- function(lengths_table, count_or_percentage, museum_grouping) {
  line_data <- lengths_table |>
    group_by(closure_length_category, .data[[museum_grouping]]) |>
    summarize(count=n()) |>
    ungroup() |>
    group_by(.data[[museum_grouping]]) |>
    mutate(percentage=round(count / sum(count) * 100, 1)) |>
    ungroup()
  ggplot(
    line_data,
    aes(
      x=closure_length_category,
      y=.data[[count_or_percentage]],
      group=.data[[museum_grouping]],
      colour=.data[[museum_grouping]]
    )
  ) +
    geom_point() +
    geom_line() +
    geom_text(
      data=line_data |> filter(closure_length_category=="< 4 years"),
      aes(y=.data[[count_or_percentage]]*1.1, label=tidy_labels[.data[[museum_grouping]]])
    ) +
    scale_colour_manual(values=museum_attribute_colours) +
    guides(colour="none") +
    labs(
      title = "Length of closure",
      x = "Length of closure (years)",
      y = "Number of Museums"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size=14),
      plot.subtitle = element_text(size=0),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=14),
      axis.text.y = element_text(size=11),
      axis.text.x = element_text(size=11, angle=45, hjust=1),
      axis.line.x.bottom = element_line(colour="black"),
      legend.position="Non"
    )
}

length_line_chart_small <- function(lengths_table, museum_grouping) {
  line_data <- lengths_table |>
    group_by(closure_length_category, .data[[museum_grouping]]) |>
    summarize(count=n()) |>
    ungroup()
  ggplot(
    line_data,
    aes(
      x=closure_length_category,
      y=count,
      group=.data[[museum_grouping]],
      colour=.data[[museum_grouping]]
    )
  ) +
    geom_point() +
    geom_line() +
    geom_text(
      data=line_data |> filter(closure_length_category=="< 4 years"),
      aes(y=count*1.1, label=tidy_labels[.data[[museum_grouping]]])
    ) +
    scale_colour_manual(values=museum_attribute_colours) +
    labs(
      title = "Length of closure",
      x = "Length of closure (years)",
      y = "Number of Museums"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size=14),
      plot.subtitle = element_text(size=0),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=14),
      axis.text.y = element_text(size=11),
      axis.text.x = element_text(size=11, angle=45, hjust=1),
      axis.line.x.bottom = element_line(colour="black"),
      legend.position="Non"
    )
}

length_scatter <- function(lengths_table, museum_grouping) {
  ggplot(
    lengths_table,
    aes(
      x=length_of_closure,
      y=.data[[museum_grouping]],
      label=museum
    )
  ) +
    geom_point(position=position_jitter(height=0.3, width=0, seed=1), alpha=0.5) +
    scale_y_discrete(labels=short_labels) +
    labs(
      title = "Length of closure",
      x = "Length of closure (years)",
      y = ""
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

length_scatter_small <- function(lengths_table, museum_grouping) {
  ggplot(
    lengths_table,
    aes(
      x=length_of_closure,
      y=.data[[museum_grouping]]
    )
  ) +
    geom_point(position=position_jitter(height=0.3, width=0, seed=1), alpha=0.5) +
    scale_y_discrete(labels=short_labels) +
    labs(
      title = "Length of closure",
      x = "Length of closure (years)",
      y = ""
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

example_timelines <- function(events_table, example_museum_id) {
  events <- events_table |>
    filter(museum_id==example_museum_id) |>
    mutate(
      event_id = row_number(),
      ymax=event_id * 2,
      ymin=ymax - 0.5,
      year2 = year2 + 1
    )
  ggplot(
    events
  ) +
    geom_rect(
      aes(
        xmin=year1,
        xmax=year2,
        ymin=ymin,
        ymax=ymax,
        fill=event_level,
        label=event_description
      ),
      colour="black"
    ) +
    scale_fill_manual(values=c("sub"=green, "super"=red)) +
    labs(x = "Date", y = "", title = "Timeline of Post-closure events") +
    theme_minimal() +
    theme(
      plot.title = element_text(size=14),
      plot.subtitle = element_text(size=0),
      axis.title.x = element_text(size=14),
      axis.text.x = element_text(size=11),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x.bottom = element_line(colour="black"),
      legend.position="Non"
    )
}

example_timelines_small <- function(events_table, example_museum_id) {
  events <- events_table |>
    filter(museum_id==example_museum_id) |>
    mutate(
      event_id = row_number(),
      ymax=event_id * 2,
      ymin=ymax - 0.5,
      year2 = year2 + 1
    )
  ggplot(
    events
  ) +
    geom_rect(
      aes(
        xmin=year1,
        xmax=year2,
        ymin=ymin,
        ymax=ymax,
        fill=event_level
      ),
      colour="black"
    ) +
    scale_fill_manual(values=c("sub"=green, "super"=red)) +
    labs(x = "Date", y = "", title = "Timeline of Post-closure events") +
    theme_minimal() +
    theme(
      plot.title = element_text(size=14),
      plot.subtitle = element_text(size=0),
      axis.title.x = element_text(size=14),
      axis.text.x = element_text(size=11),
      axis.title.y = element_text(size=0),
      axis.text.y = element_text(size=0),
      axis.line.x.bottom = element_line(colour="black"),
      legend.position="Non"
    )
}
