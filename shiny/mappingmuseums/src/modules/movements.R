movementsUI <- function(id) {
  fluidPage(
    fluidRow(
      p("Where do the collections of closed museums go? The diagrams below detail the movements of objects as they leave closed museums.")
    ),
    fluidRow(
      sidebarLayout(
        sidebarPanel(width=3, dispersalFiltersUI(NS(id, "dispersalFilters"))),
        mainPanel(
          plotOutput(NS(id, "movementsMap"), width="80%", height="850px"),
          tagList(
            tags$span(
              tags$strong("Display: "),
              tags$i(
                class = "fa fa-info-circle",
                style = "color: #007bff; cursor: pointer;",
                `data-toggle` = "popover",
                `data-placement` = "right",
                title = "Display steps or first and last actors",
                `data-content` = "<p><strong>Steps in path:</strong> View intermediate actors in the sequences of ownership and/or custody changes</p><p><strong>First and last actors:</strong> View only the initial museum and the last known actor in the sequence.</p>"
              )
            ),
            tags$script(popover_js),
            radioButtons(
              NS(id, "stepsOrFirstLast"),
              label="",
              choices=c("Steps in path", "First and last actors"),
              selected="Steps in path",
              inline=TRUE
            ),
          ),
          tagList(
            tags$span(
              tags$strong("Steps in path: "),
              tags$i(
                class = "fa fa-info-circle",
                style = "color: #007bff; cursor: pointer;",
                `data-toggle` = "popover",
                `data-placement` = "right",
                title = "Steps in path",
                `data-content` = "<p>Select the start and end point of sequences. Step 1 shows the initial museums where collections originated.</p><p>Use the slider to increase the number of steps away from the museum shown on the diagram.</p>"
              )
            ),
            tags$script(popover_js),
            sliderInput(
              NS(id, "stagesInOwnershipPath"),
              label="",
              value=c(1,2),
              min=1,
              max=7,
              step=1,
              ticks=FALSE,
              width="50%"
            )
          ),
          #img(src='actor-sector-key.png', align="left", width="150px"),
          plotlyOutput(NS(id, "movementsScatter"), height="600px", width="100%"),
          radioButtons(
            NS(id, "pointsOrBoxplot"),
            label="",
            choices=c("Show collections as points", "Summarize collection movements with boxplots"),
            selected="Show collections as points",
            inline=TRUE
          ),
          selectInput(
            NS(id, "scatterGroup"),
            label="Group collections by origin museum:",
            choices=c("All museums", field_names$name),
            selected="Governance"
          )
        )
      ),
    ),
    fluidRow(
      h3("Sequences Data"),
      pickerInput(
        NS(id, "tableSelect"),
        label="show columns:",
        choices=sequences_table_choices,
        selected=sequences_table_choices,
        options = pickerOptions(
          actionsBox = TRUE, 
          size = 10,
          selectedTextFormat = "count > 3"
        ), 
        multiple = TRUE
      ), 
      downloadButton(NS(id, "downloadSequencesTable"), label="Download table as CSV")
    ),
    fluidRow(
      DTOutput(NS(id, "movementsTable"))
    )
  )
}

movementsServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    steps_or_first_last <- reactive({input$stepsOrFirstLast})

    observeEvent(steps_or_first_last(), {
      if (input$stepsOrFirstLast == "First and last actors") {
        shinyjs::disable("stagesInOwnershipPath")
      } else {
        shinyjs::enable("stagesInOwnershipPath")
      }
    })

    filtered_sequences <- dispersalFiltersServer("dispersalFilters", steps_or_first_last)

    selected_columns <- reactive({input$tableSelect})

    ownershipChangesStart <- reactiveVal(1)
    ownershipChangesEnd <- reactiveVal(2)
    debouncedStagesInOwnershipPath <- debounce(
      reactive(input$stagesInOwnershipPath),
      millis=300
    )
    observeEvent(debouncedStagesInOwnershipPath(), {
      ownershipChangesStart(debouncedStagesInOwnershipPath()[1])
      ownershipChangesEnd(debouncedStagesInOwnershipPath()[2])
    })

    output$movementsMap <- renderPlot({
      generate_movements_map(
        filtered_sequences(),
        ownershipChangesStart(),
        ownershipChangesEnd(),
        input$`dispersalFilters-grouping`,
        input$`dispersalFilters-showTransactionCounts`,
        steps_or_first_last()
      )
    })

    show_boxplot <- reactive({
      input$pointsOrBoxplot == "Summarize collection movements with boxplots"
    })
    scatter_grouping <- reactive({
      if (input$scatterGroup == "All museums") {
        return("all")
      } else if (input$scatterGroup == "Size") {
        return("initial_museum_size")
      } else if (input$scatterGroup == "Governance") {
        return("initial_museum_governance_broad")
      } else if (input$scatterGroup == "Accreditation") {
        return("initial_museum_accreditation")
      } else if (input$scatterGroup == "Subject Matter") {
        return("initial_museum_subject_matter_broad")
      } else if (input$scatterGroup == "Country/Region") {
        return("initial_museum_region")
      }
    })
    output$movementsScatter <- renderPlotly({
      generate_movements_scatter(
        filtered_sequences(),
        ownershipChangesStart(),
        ownershipChangesEnd(),
        scatter_grouping(),
        input$scatterGroup,
        input$`dispersalFilters-showTransactionCounts`,
        steps_or_first_last(),
        show_boxplot()
      )
    })

    output$downloadSequencesTable <- downloadHandler(
      filename = function() {
        paste('dispersal-sequences-data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(
          filtered_sequences() |> select(all_of(selected_columns())),
          con
        )
      },
      contentType = "text/csv"
    )

    output$movementsTable <- renderDT({
      pathway_table(filtered_sequences(), selected_columns())
    }, options=list(pageLength=100))

  })
}

generate_movements_map <- function(sequences,
                                   start_position,
                                   end_position,
                                   grouping_dimension,
                                   show_transaction_counts,
                                   steps_or_first_last) {
  grouping_dimension <- list(
    "Actor Sector"="sector",
    "Actor Type (Core Categories)"="core_type",
    "Actor Type (Most General)"="general_type",
    "Actor Type (Most Specific)"="type"
  )[grouping_dimension]
  sender_grouping_dimension <- paste0("sender_", grouping_dimension)
  recipient_grouping_dimension <- paste0("recipient_", grouping_dimension)

  from_nodes <- sequences |>
    filter(sender_position >= start_position & sender_position <= end_position) |>
    select(
      id=origin_id,
      name=sender_name,
      governance_broad=sender_governance_broad,
      grouping_dimension=.data[[sender_grouping_dimension]],
      x=origin_x,
      y=origin_y
    )
  to_nodes <- sequences |>
    filter(recipient_position >= start_position & recipient_position <= end_position) |>
    select(
      id=destination_id,
      name=recipient_name,
      governance_broad=recipient_governance_broad,
      grouping_dimension=.data[[recipient_grouping_dimension]],
      x=destination_x,
      y=destination_y
    )
  nodes <- rbind(from_nodes, to_nodes) |>
    distinct() |>
    filter(
      !is.na(x),
      !is.na(y),
      y < 2e6
    )

  edges <- sequences |>
    filter(
      sender_position >= start_position,
      recipient_position <= end_position,
      !is.na(origin_x),
      !is.na(destination_x)
    )
  count_edges <- sequences |>
    select(
      from=origin_id,
      to=destination_id,
      sender_sector
    ) |>
    group_by(from, to, sender_sector) |>
    summarize(count=n()) |>
    ungroup() |>
    mutate(label=count)
  size_edges <- sequences |>
    mutate(
      collection_estimated_size = ifelse(is.na(collection_estimated_size), 1, collection_estimated_size)
    ) |>
    select(
      from=origin_id,
      to=destination_id,
      sender_sector,
      collection_estimated_size
    ) |>
    group_by(from, to, sender_sector) |>
    summarize(count=sum(collection_estimated_size)) |>
    ungroup() |>
    mutate(label="")
  max_size_edges <- sequences |>
    mutate(
      collection_estimated_size = ifelse(is.na(collection_estimated_size_max), 1, collection_estimated_size_max)
    ) |>
    select(
      from=origin_id,
      to=destination_id,
      sender_sector,
      collection_estimated_size
    ) |>
    group_by(from, to, sender_sector) |>
    summarize(count=sum(collection_estimated_size)) |>
    ungroup() |>
    mutate(label="")
  min_size_edges <- sequences |>
    mutate(
      collection_estimated_size = ifelse(is.na(collection_estimated_size_min), 1, collection_estimated_size_min)
    ) |>
    select(
      from=origin_id,
      to=destination_id,
      sender_sector,
      collection_estimated_size
    ) |>
    group_by(from, to, sender_sector) |>
    summarize(count=sum(collection_estimated_size)) |>
    ungroup() |>
    mutate(label="")
  edges <- rbind(
    count_edges,
    min_size_edges |> mutate(count = count * 0.25),
    min_size_edges |> mutate(count = count * 0.5),
    min_size_edges |> mutate(count = count * 0.75),
    min_size_edges,
    size_edges |> mutate(count = count * 0.25),
    size_edges |> mutate(count = count * 0.5),
    size_edges |> mutate(count = count * 0.75),
    size_edges,
    max_size_edges |> mutate(count = count * 0.25),
    max_size_edges |> mutate(count = count * 0.5),
    max_size_edges |> mutate(count = count * 0.75),
    max_size_edges
  )

  start_positions <- nodes |>
    select(from=id, x, y)
  end_positions <- nodes |>
    select(to=id, xend=x, yend=y)
  edges <- edges |>
    left_join(start_positions, by="from") |>
    left_join(end_positions, by="to") |>
    mutate(
      label_position_x=(x + xend) / 2,
      label_position_y=(y + yend) / 2
    )

  movements_plot <- ggplot(nodes, aes(x=x, y=y)) +
    geom_polygon(data=regions, aes(x=x, y=y, group=group), linewidth=0.1, label=NA, colour="black", fill=NA) +
    geom_segment(
      data=edges,
      aes(
        x=x,
        y=y,
        xend=xend,
        yend=yend,
        linewidth=count,
        colour=sender_sector
      ),
      alpha=0.1,
      arrow=arrow(ends="last", length=unit(0.1, "inches"))
    ) +
    geom_point(
      aes(
        fill=grouping_dimension_and_governance_to_sector(governance_broad, grouping_dimension)
      ),
      size=2,
      pch=21,
      colour="black",
      alpha=0.9
    ) +
    coord_fixed() +
    scale_size_continuous(range=c(5, 20)) +
    scale_linewidth(range=c(0.5,5)) +
    public_private_fill_scale +
    public_private_colour_scale +
    labs(
      title="Pathways Taken by Museum Collections"
    ) +
    network_theme +
    theme(
      axis.title=element_text(size=0),
      axis.text=element_text(size=0),
      legend.position="None"
    )
  movements_plot
}

calculate_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert degrees to radians
  radians <- function(degrees) {
    degrees * pi / 180
  }
  earth_radius <- 6371
  dlat <- radians(lat2 - lat1)
  dlon <- radians(lon2 - lon1)
  lat1 <- radians(lat1)
  lat2 <- radians(lat2)
  # Haversine formula
  a <- sin(dlat / 2) * sin(dlat / 2) +
    cos(lat1) * cos(lat2) * sin(dlon / 2) * sin(dlon / 2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  # Distance in kilometers
  distance <- earth_radius * c
  return(distance)
}

generate_movements_scatter <- function(sequences,
                                       start_position,
                                       end_position,
                                       grouping_dimension,
                                       grouping_title,
                                       show_transaction_counts,
                                       steps_or_first_last,
                                       show_boxplot) {
  jumps <- sequences |>
    filter(
      sender_position >= start_position,
      recipient_position <= end_position,
      !is.na(origin_x),
      !is.na(destination_x)
    ) |>
    mutate(
      all="All",
      label=paste(
        ifelse(is.na(collection_description), "", collection_description),
        "from:",
        sender_name,
        "to:",
        recipient_name
      ),
      distance=calculate_distance(
        origin_latitude,
        origin_longitude,
        destination_latitude,
        destination_longitude
      )
    ) |>
    arrange(.data[[grouping_dimension]])

  max_distance <- 850

  p <- ggplot(
    jumps,
    aes(y=distance, x=factor(.data[[grouping_dimension]], museum_attribute_ordering), label=label)
  )

  if (show_boxplot) {
    p <- p +
      geom_boxplot(aes(fill=.data[[grouping_dimension]]), alpha=0.5)
  } else {
    p <- p + 
      geom_jitter(
        #position=position_jitter(width=0, height=1, seed=1),
        alpha=0.1
      )
  }

  p <- p +
    scale_x_discrete(labels=tidy_labels) +
    scale_y_continuous(limits=c(0, max_distance)) +
    scale_fill_manual(values=museum_attribute_colours) +
    coord_flip() +
    labs(
      title = "Distances travelled by collections",
      y = "Distance (km)",
      x = paste0("Origin museum (", grouping_title, ")")
    ) +
    theme_classic()

  ggplotly(p, tooltip=c("label", "y")) |>
    layout(showlegend=FALSE)
}
