sequencesUI <- function(id) {
  fluidPage(
    fluidRow(
      p("The transfer of museum collections after closure. The diagram below summarizes stepwise transfers of ownership and/or custody of museum collections."),
      p("Nodes are labelled with the number of actors belonging to the category at that time step. The width of lines is proportional to the possible number of objects involved in transfers."),
      p("By default, the chart shows actors grouped by governance/sector and only pathways starting in local authority museums. Use the options to alter the way that museums and other actors are grouped together and to filter transactions according to type or collection source/destination."),
      p("Find out more about actor and event types in the actor and event types tab."),
      p("The table below the diagram provides details of the depicted transfers."),
      ),
    fluidRow(
      sidebarLayout(
        sidebarPanel(width=3, dispersalFiltersUI(NS(id, "dispersalFilters"))),
        mainPanel(
          plotOutput(NS(id, "ownershipNetwork"), width="80%", height="850px"),
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
      DTOutput(NS(id, "ownershipNetworkTable"))
    )
  )
}

sequencesServer <- function(id) {
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

    output$ownershipNetwork <- renderPlot({
      sequence_network(
        filtered_sequences(),
        ownershipChangesStart(),
        ownershipChangesEnd(),
        input$`dispersalFilters-grouping`,
        input$`dispersalFilters-showTransactionCounts`
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

    output$ownershipNetworkTable <- renderDT({
      sequence_table(filtered_sequences(), selected_columns())
    }, options=list(pageLength=100))
  })
}

sequence_table <- function(sequences, selected_columns) {
  sequences |>
    select(all_of(selected_columns))
}

sequence_network <- function(sequences,
                             start_position,
                             end_position,
                             grouping_dimension,
                             show_transaction_counts) {

  grouping_dimension <- list(
    "Actor Sector"="sector",
    "Actor Type (Core Categories)"="core_type",
    "Actor Type (Most General)"="general_type",
    "Actor Type (Most Specific)"="type"
  )[grouping_dimension]
  sender_grouping_dimension <- paste0("sender_", grouping_dimension)
  recipient_grouping_dimension <- paste0("recipient_", grouping_dimension)

  from_nodes_counts <- sequences |>
    mutate(
      id=from,
      governance_broad=sender_governance_broad,
      grouping_dimension=.data[[sender_grouping_dimension]],
      position=sender_position
    ) |>
    select(sender_id, sender_quantity, id, governance_broad, grouping_dimension, position, sender_sector) |>
    distinct() |>
    mutate(
      sender_count=ifelse(sender_quantity=="many",2,as.numeric(sender_quantity))
    ) |>
    group_by(id, governance_broad, grouping_dimension, position) |>
    summarize(
      from_count=sum(sender_count),
      from_count_suffix=ifelse("many" %in% sender_quantity, "+", ""),
      from_count_label=paste0(from_count, from_count_suffix),
      public_instances=sum(ifelse(sender_sector=="public", sender_count, 0)),
      university_instances=sum(ifelse(sender_sector=="university", sender_count, 0)),
      third_instances=sum(ifelse(sender_sector=="third", sender_count, 0)),
      private_instances=sum(ifelse(sender_sector=="private", sender_count, 0)),
      hybrid_instances=sum(ifelse(sender_sector=="hybrid", sender_count, 0)),
      public_proportion = public_instances / from_count,
      university_proportion = university_instances / from_count,
      third_proportion = third_instances / from_count,
      private_proportion = private_instances / from_count,
      hybrid_proportion = hybrid_instances / from_count,
      from_sector = case_when(
        public_proportion == 1 ~ "public",
        university_proportion == 1 ~ "university",
        third_proportion == 1 ~ "third",
        private_proportion == 1 ~ "private",
        hybrid_proportion == 1 ~ "hybrid",
        public_proportion >= 0.5 ~ "mostly public",
        university_proportion >= 0.5 ~ "mostly university",
        third_proportion >= 0.5 ~ "mostly third",
        private_proportion >= 0.5 ~ "mostly private",
        hybrid_proportion >= 0.5 ~ "mostly hybrid",
        TRUE ~ "unknown"
      )
    ) |>
    ungroup()

  to_nodes_counts <- sequences |>
    mutate(
      id=to,
      governance_broad=recipient_governance_broad,
      grouping_dimension=.data[[recipient_grouping_dimension]],
      position=recipient_position,
    ) |>
    select(recipient_id, recipient_quantity, id, governance_broad, grouping_dimension, position, recipient_sector) |>
    distinct() |>
    mutate(
      recipient_count=ifelse(recipient_quantity=="many",2,as.numeric(recipient_quantity))
    ) |>
    group_by(id, governance_broad, grouping_dimension, position) |>
    summarize(
      to_count=sum(recipient_count),
      to_count_suffix=ifelse("many" %in% recipient_quantity, "+", ""),
      to_count_label=paste0(to_count, to_count_suffix),
      public_instances=sum(ifelse(recipient_sector=="public", recipient_count, 0)),
      university_instances=sum(ifelse(recipient_sector=="university", recipient_count, 0)),
      third_instances=sum(ifelse(recipient_sector=="third", recipient_count, 0)),
      private_instances=sum(ifelse(recipient_sector=="private", recipient_count, 0)),
      hybrid_instances=sum(ifelse(recipient_sector=="hybrid", recipient_count, 0)),
      public_proportion = public_instances / to_count,
      university_proportion = university_instances / to_count,
      third_proportion = third_instances / to_count,
      private_proportion = private_instances / to_count,
      hybrid_proportion = hybrid_instances / to_count,
      to_sector = case_when(
        public_proportion == 1 ~ "public",
        university_proportion == 1 ~ "university",
        third_proportion == 1 ~ "third",
        private_proportion == 1 ~ "private",
        hybrid_proportion == 1 ~ "hybrid",
        public_proportion >= 0.5 ~ "mostly public",
        university_proportion >= 0.5 ~ "mostly university",
        third_proportion >= 0.5 ~ "mostly third",
        private_proportion >= 0.5 ~ "mostly private",
        hybrid_proportion >= 0.5 ~ "mostly hybrid",
        TRUE ~ "unknown"
      )
    ) |>
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
      ),
      name = paste(governance_broad, grouping_dimension, sep="@"),
      sector_label = ifelse(
        is.na(to_count),
        from_sector,
        ifelse(
          is.na(from_count), 
          to_sector,
          ifelse(
            from_count > to_count,
            from_sector,
            to_sector
          )
        )
      )
    ) |>
    filter(position >= start_position & position <= end_position)

  name_mapping <- node_counts |>
    select(name) |>
    distinct() |>
    mutate(
      name_numeric = as.numeric(factor(name, sector_type_ordering[sector_type_ordering %in% node_counts$name]))
    )
  
  if (grouping_dimension == "sector") {
    name_mapping <- name_mapping |> 
      mutate(
        governance = sapply(strsplit(name, "@"), `[`, 1),
        sector = sapply(strsplit(name, "@"), `[`, 2),
        label = ifelse(
          governance != "NA", 
          paste(governance, "museum"), 
          paste("Other", sector, "sector")
        )
      ) |> 
      select(-governance, -sector)
  } else {
    name_mapping <- name_mapping |> 
      mutate(
        governance = sapply(strsplit(name, "@"), `[`, 1),
        actor_type = sapply(strsplit(name, "@"), `[`, 2),
        label = ifelse(
          governance != "NA", 
          paste(governance, "museum"), 
          actor_type
        )
      ) |> 
      select(-governance, -actor_type)
  }
  
  node_counts <- node_counts |>
    left_join(name_mapping, by = "name")
  
  count_edges <- sequences |>
    select(from, to, .data[[sender_grouping_dimension]], sender_governance_broad, .data[[recipient_grouping_dimension]], recipient_governance_broad) |>
    group_by(from, to, .data[[sender_grouping_dimension]], sender_governance_broad, .data[[recipient_grouping_dimension]], recipient_governance_broad) |>
    summarize(count = n()) |>
    ungroup() |>
    mutate(
      label=count
    )

  size_edges <- sequences |>
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
  
  max_size_edges <- sequences |>
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
  
  min_size_edges <- sequences |>
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
  ) |>
    mutate(
      from_name=paste(sender_governance_broad, .data[[sender_grouping_dimension]], sep="@"),
      to_name=paste(recipient_governance_broad, .data[[recipient_grouping_dimension]], sep="@"),
      label=ifelse(!is.na(label), as.character(label), ""),
      from_position=as.numeric(sapply(str_split(from, "@"), function(x) if(length(x) > 2) x[3] else NA)),
      to_position=as.numeric(sapply(str_split(to, "@"), function(x) if(length(x) > 2) x[3] else NA)),
    ) |>
    filter(from_position >= start_position & to_position <= end_position) |>
    left_join(name_mapping |> select(-label), by = c("from_name" = "name")) |>
    rename(from_name_numeric = name_numeric) |>
    left_join(name_mapping |> select(-label), by = c("to_name" = "name")) |>
    rename(to_name_numeric = name_numeric) |>
    mutate(
      # Calculate direction vector
      dx = to_name_numeric - from_name_numeric,
      dy = to_position - from_position,
      
      # Normalize the direction vector to unit length
      length = sqrt(dx^2 + dy^2),
      ux = dx / length,
      uy = dy / length,
      
      # Calculate perpendicular vectors for control points
      perp_x = -uy,  # Perpendicular vector x component
      perp_y = ux,   # Perpendicular vector y component
      
      # Control points
      control1_x = from_name_numeric + (dx / 3) + (perp_x * 0.5),  # Shift control point 1 perpendicular to the line
      control1_y = from_position + (dy / 3) + (perp_y * 0.5),
      
      control2_x = from_name_numeric + 2 * (dx / 3) - (perp_x * 0.5),  # Shift control point 2 in the opposite perpendicular direction
      control2_y = from_position + 2 * (dy / 3) - (perp_y * 0.5)
    ) |>
    rowwise() |>
    mutate(
      random_offset = runif(n(), min=-0.1, max=0.1),
      gradient = (to_position - from_position) / (to_name_numeric - from_name_numeric),
      label_position_x = mean(c(from_name_numeric, to_name_numeric)),
      label_position_x = ifelse(
          gradient == Inf,
          label_position_x,
          label_position_x + random_offset * (1 / gradient)
      ),
      label_position_y = mean(c(from_position, to_position)) + random_offset,
    ) |>
    left_join(node_counts |> select(from=id, from_sector_label=sector_label), by="from")


  edges_bezier <- edges %>%
    select(from_name_numeric, from_position, control1_x, control1_y, control2_x, control2_y, to_name_numeric, to_position, count, .data[[sender_grouping_dimension]], label) %>%
    pivot_longer(cols = c(from_name_numeric, control1_x, control2_x, to_name_numeric), names_to = "point", values_to = "x") %>%
    pivot_longer(cols = c(from_position, control1_y, control2_y, to_position), names_to = "point_y", values_to = "y") %>%
    filter(
      (point == "from_name_numeric" & point_y == "from_position") |
        (point == "control1_x" & point_y == "control1_y") |
        (point == "control2_x" & point_y == "control2_y") |
        (point == "to_name_numeric" & point_y == "to_position")
    ) %>%
    arrange(.data[[sender_grouping_dimension]], count, label) %>%
    group_by(interaction(count, .data[[sender_grouping_dimension]], label)) %>%
    filter(n() == 4) 
  
  transaction_sequence_plot <- ggplot(node_counts, aes(x=name_numeric, y=position)) +
    geom_segment(
      data=edges,
      aes(
        x=from_name_numeric,
        y=from_position,
        xend=to_name_numeric,
        yend=to_position,
        linewidth=count,
        colour=from_sector_label,
      ),
      alpha=0.1
    ) +
    # geom_bezier(
    #   data=edges_bezier,
    #   aes(x=x, y=y, group=interaction(count, sender_core_type, label), linewidth=count, colour=sender_core_type),
    #   alpha=0.1
    # ) +
    geom_point(
      aes(
        fill=sector_label,
        size=count
      ),
      pch=21,
      colour="black",
      alpha=0.7
    ) +
    geom_text(aes(label=count_label), size=5) +
    coord_flip() +
    scale_x_continuous(breaks=name_mapping$name_numeric, labels=str_replace_all(name_mapping$label, "_", " ")) +
    scale_y_continuous(breaks=start_position:end_position) +
    scale_size_continuous(range=c(5, 20)) +
    scale_linewidth(range=c(1,10)) +
    public_private_fill_scale +
    public_private_colour_scale +
    labs(
      title="Sequential Transactions of Museum Collections",
      x="Actor",
      y="Stage in path"
    ) +
    guides(
      fill=guide_legend(override.aes = list(size = 12)),
      colour="none",
      size="none",
      linewidth="none"
    ) +
    network_theme +
    theme(
      legend.position="None"
    )
  
  if (show_transaction_counts) {
    transaction_sequence_plot <- transaction_sequence_plot +
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
  
  transaction_sequence_plot
}

