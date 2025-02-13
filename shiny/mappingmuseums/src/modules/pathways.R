pathwaysUI <- function(id) {
  fluidPage(
    fluidRow(
      p("The transfer of museum collections after closure. The diagram below summarizes categorizes the pathways that collections follow when leaving a closed museum."),
      p("Nodes are labelled with the number of actors belonging to the category at that time step. The width of lines is proportional to the possible number of objects involved in transfers."),
      p("Use the options to alter the way that museums and other actors are grouped together and to filter transactions according to type or collection source/destination."),
      p("Find out more about actor and event types in the actor and event types tab."),
      p("The table below the diagram provides details of the depicted transfers."),
      ),
    fluidRow(
      sidebarLayout(
        sidebarPanel(width=3, dispersalFiltersUI(NS(id, "dispersalFilters"))),
        mainPanel(
          plotlyOutput(NS(id, "pathwaysNetwork"), width="80%", height="850px"),
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
              value=2,
              min=1,
              max=7,
              step=1,
              ticks=FALSE,
              width="50%"
            )
          ),
          img(src='actor-sector-key.png', align="left", width="150px")
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
      DTOutput(NS(id, "pathwaysTable"))
    )
  )
}

pathwaysServer <- function(id) {
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
      ownershipChangesEnd(debouncedStagesInOwnershipPath())
    })

    museum_grouping <- reactive({
      if (input$`dispersalFilters-groupingMuseums` == "All museums") {
        return("all")
      } else if (input$`dispersalFilters-groupingMuseums` == "Size") {
        return("size")
      } else if (input$`dispersalFilters-groupingMuseums` == "Governance") {
        return("governance_broad")
      } else if (input$`dispersalFilters-groupingMuseums` == "Accreditation") {
        return("accreditation")
      } else if (input$`dispersalFilters-groupingMuseums` == "Subject Matter") {
        return("subject_matter_broad")
      } else if (input$`dispersalFilters-groupingMuseums` == "Country/Region") {
        return("region")
      }
    })

    output$pathwaysNetwork <- renderPlotly({
      generate_pathway_dendrogram(
        filtered_sequences(),
        ownershipChangesStart(),
        ownershipChangesEnd(),
        input$`dispersalFilters-grouping`,
        museum_grouping(),
        input$`dispersalFilters-showTransactionCounts`,
        steps_or_first_last()
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

    output$pathwaysTable <- renderDT({
      pathway_table(filtered_sequences(), selected_columns())
    }, options=list(pageLength=100))

  })
}

pathway_table <- function(sequences, selected_columns) {
  sequences |>
    select(all_of(selected_columns))
}

generate_pathway_dendrogram <- function(sequences,
                                        start_position,
                                        end_position,
                                        grouping_dimension,
                                        museum_grouping_dimension,
                                        show_transaction_counts,
                                        steps_or_first_last) {

  grouping_dimension_name <- list(
    "Actor Sector"="sector",
    "Actor Type (Core Categories)"="core_type",
    "Actor Type (Most General)"="general_type",
    "Actor Type (Most Specific)"="type"
  )[grouping_dimension]
  sender_grouping_dimension <- paste0("sender_", grouping_dimension_name)
  recipient_grouping_dimension <- paste0("recipient_", grouping_dimension_name)

  sender_museum_grouping_dimension <- paste0("sender_", museum_grouping_dimension)
  recipient_museum_grouping_dimension <- paste0("recipient_", museum_grouping_dimension)

 dendrogram_data <-  sequences |>
   mutate(initial_museum_all="all", sender_all="all", recipient_all="all") |>
   filter(recipient_position <= end_position) |>
   select(
     event_id,
     event_stage_in_path,
     previous_shown_event,
     from,
     to,
     sender_id,
     recipient_id,
     .data[[sender_museum_grouping_dimension]],
     .data[[recipient_museum_grouping_dimension]],
     .data[[sender_grouping_dimension]],
     .data[[recipient_grouping_dimension]],
     sender_position,
     recipient_position,
     sender_quantity,
     recipient_quantity,
     collection_estimated_size,
     collection_estimated_size_max,
     collection_estimated_size_min
   )

  build_chains <- function(data) {
    # give stages in each type of path a unique id
    data <- data |>
      mutate(
        from_label=from,
        to_label=to,
        full_from=from,
        full_to=paste(from, "->", to)
      ) |>
      arrange(event_stage_in_path)
    for (i in 1:nrow(data)) {
      prev_event <- data$previous_shown_event[i]
      if (!is.na(prev_event)) {
        prev_index <- which(data$event_id == prev_event)
        data$full_from[i] <- paste(data$full_from[prev_index], "->", data$from[i])
        data$full_to[i] <- paste(data$full_to[prev_index], "->", data$to[i])
      }
    }
    data |>
      select(-from, -to) |>
      rename(from = full_from, to = full_to)
  }

  if (steps_or_first_last == "Steps in path") {
    dendrogram_data <- build_chains(dendrogram_data)
  } else {
    dendrogram_data <- dendrogram_data |>
      mutate(
        to = paste(from, "->", to)
      )
  }

  initial_senders <- dendrogram_data |>
    filter(event_stage_in_path == 1) |>
    select(.data[[sender_museum_grouping_dimension]]) |>
    distinct()
  if(nrow(initial_senders) > 1) {
    dummy_rows <- dendrogram_data |>
      filter(event_stage_in_path == 1) |>
      mutate(
        event_id = "",
        event_stage_in_path = 0,
        previous_shown_event = "",
        to = from,
        from = "",
        recipient_id = sender_id,
        sender_id = "",
        !!sym(recipient_museum_grouping_dimension) := .data[[sender_museum_grouping_dimension]],
        !!sym(sender_museum_grouping_dimension) := "dummy",
        !!sym(recipient_grouping_dimension) := .data[[sender_grouping_dimension]],
        !!sym(sender_grouping_dimension) := "",
        recipient_position = sender_position,
        sender_position = 0,
        recipient_quantity = sender_quantity,
        sender_quantity = "1",
      )
    dendrogram_data <- bind_rows(dendrogram_data, dummy_rows)
  }

  from_nodes_counts <- dendrogram_data |>
    mutate(
      id=from,
      museum_grouping_dimension=.data[[sender_museum_grouping_dimension]],
      grouping_dimension=.data[[sender_grouping_dimension]],
      position=sender_position
    ) |>
    select(sender_id, sender_quantity, id, museum_grouping_dimension, grouping_dimension, position) |>
    distinct() |>
    mutate(
      sender_count=ifelse(sender_quantity=="many",2,as.numeric(sender_quantity))
    ) |>
    group_by(id, museum_grouping_dimension, grouping_dimension, position) |>
    summarize(
      from_count=sum(sender_count),
      from_count_suffix=ifelse("many" %in% sender_quantity, "+", ""),
      from_count_label=paste0(from_count, from_count_suffix)
    ) |>
    ungroup()

  to_nodes_counts <- dendrogram_data |>
    mutate(
      id=to,
      museum_grouping_dimension=.data[[recipient_museum_grouping_dimension]],
      grouping_dimension=.data[[recipient_grouping_dimension]],
      position=recipient_position,
    ) |>
    select(recipient_id, recipient_quantity, id, museum_grouping_dimension, grouping_dimension, position) |>
    distinct() |>
    mutate(
      recipient_count=ifelse(recipient_quantity=="many",2,as.numeric(recipient_quantity))
    ) |>
    group_by(id, museum_grouping_dimension, grouping_dimension, position) |>
    summarize(
      to_count=sum(recipient_count),
      to_count_suffix=ifelse("many" %in% recipient_quantity, "+", ""),
      to_count_label=paste0(to_count, to_count_suffix)
    ) |>
    ungroup()

  node_counts <- from_nodes_counts |>
    full_join(to_nodes_counts, by=c("id", "museum_grouping_dimension", "grouping_dimension", "position")) |>
    mutate(
      count = ifelse(
        is.na(to_count),
        from_count,
        ifelse(
          is.na(from_count), 
          to_count,
          ifelse(
            from_count > to_count,
            from_count,
            to_count
          )
        )
      ),
      count_label = ifelse(
        is.na(to_count),
        from_count_label,
        ifelse(
          is.na(from_count), 
          to_count_label,
          ifelse(
            from_count > to_count,
            from_count_label,
            to_count_label
          )
        )
      )
    ) |>
    filter(position >= start_position & position <= end_position)

  if (grouping_dimension_name == "sector") {
    node_counts <- node_counts |>
      mutate(
        name=paste(museum_grouping_dimension, grouping_dimension, sep="@"),
        label = ifelse(
          !is.na(museum_grouping_dimension), 
          paste(gsub("_", " ", museum_grouping_dimension), "museum"),
          paste(grouping_dimension, "sector")
        )
      )
  } else {
    node_counts <- node_counts |>
      mutate(
        name=paste(museum_grouping_dimension, grouping_dimension, sep="@"),
        label = ifelse(
          !is.na(museum_grouping_dimension), 
          paste(gsub("_", " ", museum_grouping_dimension), "museum"),
          grouping_dimension
        )
      )
  }

  count_edges <- dendrogram_data |>
    select(from, to, .data[[sender_grouping_dimension]], .data[[sender_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], .data[[recipient_museum_grouping_dimension]]) |>
    group_by(from, to, .data[[sender_grouping_dimension]], .data[[sender_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], .data[[recipient_museum_grouping_dimension]]) |>
    summarize(count = n()) |>
    ungroup() |>
    mutate(
      label=count
    )
  size_edges <- dendrogram_data |>
    mutate(
      collection_estimated_size = ifelse(is.na(collection_estimated_size), 1, collection_estimated_size)
    ) |>
    select(from, to, .data[[sender_grouping_dimension]], .data[[sender_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], .data[[recipient_museum_grouping_dimension]], collection_estimated_size) |>
    group_by(from, to, .data[[sender_grouping_dimension]], .data[[sender_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], .data[[recipient_museum_grouping_dimension]]) |>
    summarize(count = sum(collection_estimated_size)) |>
    ungroup() |>
    mutate(
      label=""
    )
  max_size_edges <- dendrogram_data |>
    mutate(
      collection_estimated_size = ifelse(is.na(collection_estimated_size_max), 1, collection_estimated_size_max)
    ) |>
    select(from, to, .data[[sender_grouping_dimension]], .data[[sender_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], .data[[recipient_museum_grouping_dimension]], collection_estimated_size) |>
    group_by(from, to, .data[[sender_grouping_dimension]], .data[[sender_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], .data[[recipient_museum_grouping_dimension]]) |>
    summarize(count = sum(collection_estimated_size)) |>
    ungroup() |>
    mutate(
      label=""
    )
  min_size_edges <- dendrogram_data |>
    mutate(
      collection_estimated_size = ifelse(is.na(collection_estimated_size_min), 1, collection_estimated_size_min)
    ) |>
    select(from, to, .data[[sender_grouping_dimension]], .data[[sender_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], .data[[recipient_museum_grouping_dimension]], collection_estimated_size) |>
    group_by(from, to, .data[[sender_grouping_dimension]], .data[[sender_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], .data[[recipient_museum_grouping_dimension]]) |>
    summarize(count = sum(collection_estimated_size)) |>
    ungroup() |>
    mutate(
      label=""
    )
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

  dendrogram_graph <- graph_from_data_frame(count_edges)
  dendrogram_layout <- create_layout(dendrogram_graph, layout="dendrogram", circular=TRUE) |>
    mutate(id=name) |>
    select(id, x, y)

  node_counts <- node_counts |> left_join(dendrogram_layout, by="id")
  start_positions <- node_counts |>
    select(from=id, x, y)
  end_positions <- node_counts |>
    select(to=id, xend=x, yend=y)
  edges <- edges |>
    left_join(start_positions, by="from") |>
    left_join(end_positions, by="to") |>
    mutate(
      label_position_x=(x + xend) / 2,
      label_position_y=(y + yend) / 2
    )

  theta <- seq(pi/8, 2*pi, length.out=16)
  xo <- diff(range(node_counts$x)) / 500
  yo <- diff(range(node_counts$y)) / 500
  label_shadows <- node_counts |>
    mutate(y=y-0.06) |>
    crossing(theta=theta) |>
    mutate(
      x_offset = x + cos(theta) * xo,
      y_offset = y + sin(theta) * yo
    )

  transaction_pathways_plot <- ggplot(node_counts, aes(x=x, y=y)) +
    geom_segment(
      data=edges,
      aes(
        x=x,
        y=y,
        xend=xend,
        yend=yend,
        linewidth=count,
        colour=grouping_dimension_and_governance_to_sector(.data[[sender_museum_grouping_dimension]], .data[[sender_grouping_dimension]])
      ),
      alpha=0.1,
      arrow=arrow(ends="last", length=unit(0.2, "inches"))
    ) +
    geom_point(
      aes(
        label=label,
        fill=grouping_dimension_and_governance_to_sector(museum_grouping_dimension, grouping_dimension),
        size=count
      ),
      pch=21,
      colour="black",
      alpha=0.7
    ) +
    geom_text(
      aes(label=count_label)
    ) +
    geom_text(
      data=label_shadows,
      aes(x=x_offset, y=y_offset,label=label),
      size=5,
      colour="white"
    ) +
    geom_text(
      data=node_counts,
      aes(y=y-0.06,label=label),
      size=5,
    ) +
    scale_x_continuous(expand=c(0.1, 0)) +
    scale_y_continuous(expand=c(0.1, 0)) +
    scale_size_continuous(range=c(5, 20)) +
    scale_linewidth(range=c(1,10)) +
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

  if (show_transaction_counts) {
    transaction_pathways_plot <- transaction_pathways_plot +
      geom_text(
        data=edges,
        aes(
          x=label_position_x,
          y=label_position_y,
          label=label,
          size=4
        )
      )
  }
  
  transaction_pathways_plot |>
    ggplotly(tooltip=c("label", "count")) |>
    layout(
      showlegend=FALSE,
      xaxis=list(title="", zeroline=FALSE, showticklabels=FALSE),
      yaxis=list(title="", zeroline=FALSE, showticklabels=FALSE)
    ) 

}
