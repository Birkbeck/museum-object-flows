sequences_table_choices <- c(
  "event_stage_in_path",
  "collection_id",
  "collection_status",
  "collection_description",
  "collection_types",
  "collection_size",
  "initial_museum_governance",
  "initial_museum_size",
  "initial_museum_subject_matter",
  "initial_museum_region",
  "sender_name",
  "event_type",
  "recipient_name",
  "event_is_change_of_custody",
  "event_is_change_of_ownership",
  "sender_position",
  "sender_type",
  "sender_sector",
  "sender_governance",
  "sender_town",
  "recipient_position",
  "recipient_type",
  "recipient_sector",
  "recipient_governance",
  "recipient_town"
)

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
          plotOutput(NS(id, "pathwaysNetwork"), width="80%", height="850px"),
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
              value=7,
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

    output$pathwaysNetwork <- renderPlot({
      generate_pathway_dendrogram(
        filtered_sequences(),
        ownershipChangesStart(),
        ownershipChangesEnd(),
        input$`dispersalFilters-grouping`,
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

 dendrogram_data <-  sequences |>
   filter(recipient_position <= end_position)

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
    data |> select(-from, -to) %>% rename(from = full_from, to = full_to)
  }

  if (steps_or_first_last == "Steps in path") {
    dendrogram_data <- build_chains(dendrogram_data)
  }

  from_nodes_counts <- dendrogram_data |>
    mutate(
      id=from,
      governance_broad=sender_governance_broad,
      grouping_dimension=.data[[sender_grouping_dimension]],
      position=sender_position
    ) |>
    select(sender_name, id, governance_broad, grouping_dimension, position) |>
    group_by(id, governance_broad, grouping_dimension, position) |>
    summarize(from_count = n_distinct(sender_name)) |>
    ungroup()

  to_nodes_counts <- dendrogram_data |>
    mutate(
      id=to,
      governance_broad=recipient_governance_broad,
      grouping_dimension=.data[[recipient_grouping_dimension]],
      position=recipient_position,
    ) |>
    select(recipient_name, id, governance_broad, grouping_dimension, position) |>
    group_by(id, governance_broad, grouping_dimension, position) |>
    summarize(to_count = n_distinct(recipient_name)) |>
    ungroup()

  node_counts <- from_nodes_counts |>
    full_join(to_nodes_counts, by=c("id", "governance_broad", "grouping_dimension", "position")) |>
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
      )
    ) |>
    mutate(
      name=paste(governance_broad, grouping_dimension, sep="@"),
      label = ifelse(
        !is.na(governance_broad), 
        paste(gsub("_", " ", governance_broad), "Museum"),
        grouping_dimension
      )
    )  |>
    filter(position >= start_position & position <= end_position)

  count_edges <- dendrogram_data |>
    select(from, to, .data[[sender_grouping_dimension]], sender_governance_broad, .data[[recipient_grouping_dimension]], recipient_governance_broad) |>
    group_by(from, to, .data[[sender_grouping_dimension]], sender_governance_broad, .data[[recipient_grouping_dimension]], recipient_governance_broad) |>
    summarize(count = n()) |>
    ungroup() |>
    mutate(
      label=count
    )
  size_edges <- dendrogram_data |>
    mutate(
      collection_estimated_size = ifelse(is.na(collection_estimated_size), 1, collection_estimated_size)
    ) |>
    select(from, to, .data[[sender_grouping_dimension]], sender_governance_broad, .data[[recipient_grouping_dimension]], recipient_governance_broad, collection_estimated_size) |>
    group_by(from, to, .data[[sender_grouping_dimension]], sender_governance_broad, .data[[recipient_grouping_dimension]], recipient_governance_broad) |>
    summarize(count = sum(collection_estimated_size)) |>
    ungroup() |>
    mutate(
      label=""
    )
  max_size_edges <- dendrogram_data |>
    mutate(
      collection_estimated_size = ifelse(is.na(collection_estimated_size_max), 1, collection_estimated_size_max)
    ) |>
    select(from, to, .data[[sender_grouping_dimension]], sender_governance_broad, .data[[recipient_grouping_dimension]], recipient_governance_broad, collection_estimated_size) |>
    group_by(from, to, .data[[sender_grouping_dimension]], sender_governance_broad, .data[[recipient_grouping_dimension]], recipient_governance_broad) |>
    summarize(count = sum(collection_estimated_size)) |>
    ungroup() |>
    mutate(
      label=""
    )
  min_size_edges <- dendrogram_data |>
    mutate(
      collection_estimated_size = ifelse(is.na(collection_estimated_size_min), 1, collection_estimated_size_min)
    ) |>
    select(from, to, .data[[sender_grouping_dimension]], sender_governance_broad, .data[[recipient_grouping_dimension]], recipient_governance_broad, collection_estimated_size) |>
    group_by(from, to, .data[[sender_grouping_dimension]], sender_governance_broad, .data[[recipient_grouping_dimension]], recipient_governance_broad) |>
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

  transaction_pathways_plot <- ggplot(node_counts, aes(x=x, y=y)) +
    geom_segment(
      data=edges,
      aes(
        x=x,
        y=y,
        xend=xend,
        yend=yend,
        linewidth=count,
        colour=grouping_dimension_and_governance_to_sector(sender_governance_broad, .data[[sender_grouping_dimension]])
      ),
      alpha=0.1,
      arrow=arrow(ends="last", length=unit(0.2, "inches"))
    ) +
    geom_point(
      aes(
        fill=grouping_dimension_and_governance_to_sector(governance_broad, grouping_dimension),
        size=count
      ),
      pch=21,
      colour="black",
      alpha=0.9
    ) +
    geom_text(
      aes(label=count)
    ) +
    geom_text(
      aes(label=label),
      hjust="left",
      size=5,
      nudge_x=0.03
    ) +
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
  
  transaction_pathways_plot

}
