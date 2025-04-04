glossaryUI <- function(id) {
  fluidPage(
    p("Below is a glossary of the terms used in visualizations."),

    h3("Museum Attributes"),
    p("In the visualizations, museums can be grouped according to their attributes: size, governance, subject matter, and accreditation."),
    h4("Size"),
    p(""),
    h4("Governance"),
    p(""),
    h4("Subject Matter"),
    p(""),
    h4("Accreditation"),
    p(""),

    h3("Actor Types"),
    p("The types of actor involved in transactions with museums are shown below. The leaves of the tree include highly specific categories. These are grouped into core categories which are used in some of the visual summaries in the following tabs."),
    plotOutput(NS(id, "actorTypes"), width="80%", height="1500px"),

    h3("Event Types"),
    p("The types of event involving museum collections are shown below."),
    p("Events can be changes of ownership, changes of custody, both, or neither. Alternatively they can mark the end of a collection's or object's existence. The taxonomy colours each event type according to the default way in which it should be understood. Individual events can override this default. For example, some sales do not result in a change of custody."),
    plotOutput(NS(id, "eventTypes"), width="80%", height="900px"),
    DTOutput(NS(id, "eventTypesGlossary")),

  )
}

glossaryServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$actorTypes <- renderPlot({
      actor_types_taxonomy()
    })

    output$eventTypes <- renderPlot({
      event_types_taxonomy()
    })

    output$eventTypesGlossary <- renderDT({
      event_types |>
        select(
          `Type name`=type_name,
          `Sub-type of`=sub_type_of,
          `Definition`=definition,
          `Core category?`=is_core_category,
          `Usually a change of ownership?`=change_of_ownership,
          `Usually a change of custody?`=change_of_custody,
          `Usually an end of existence?`=end_of_existence
        )
    })

  })
}

actor_types_taxonomy <- function() {
  # add dummy types to use as spaces between groups
  actor_types <- actor_types |>
    mutate(definition = "")

  counter <- 1
  types_with_sub_types <- actor_types |>
    filter(!is.na(sub_type_of)) |>
    select(type_name=sub_type_of) |>
    distinct()
  for (i in 1:nrow(types_with_sub_types)) {
    new_row_1 <- data.frame(
      type_name = as.character(counter),
      sub_type_of = types_with_sub_types$type_name[i],
      is_core_category = FALSE,
      public_instances=0,
      university_instances=0,
      third_instances=0,
      private_instances=0,
      hybrid_instances=0,
      unknown_instances=0,
      total_instances=0,
      definition = "dummy"
    )
    new_row_2 <- data.frame(
      type_name = paste("z", as.character(counter)),
      sub_type_of = types_with_sub_types$type_name[i],
      is_core_category = FALSE,
      public_instances=0,
      university_instances=0,
      third_instances=0,
      private_instances=0,
      hybrid_instances=0,
      unknown_instances=0,
      total_instances=0,
      definition = "dummy"
    )
    counter <- counter + 1
    actor_types <- actor_types |>
      rbind(new_row_1) |>
      rbind(new_row_2)
  }

  actor_types <- actor_types |>
    mutate(
      public_proportion = public_instances / total_instances,
      university_proportion = university_instances / total_instances,
      third_proportion = third_instances / total_instances,
      private_proportion = private_instances / total_instances,
      hybrid_proportion = hybrid_instances / total_instances,
      sector = ifelse(
        public_proportion >= 0.5,
        "Mostly public sector",
        ifelse(
          university_proportion >= 0.5,
          "Mostly university sector",
          ifelse(
            third_proportion >= 0.5,
            "Mostly third sector",
            ifelse(
              private_proportion >= 0.5,
              "Mostly private sector",
              "Mixed/Unknown"
            )
          )
        )
      )
    )
 
  core_actor_types <- actor_types |>
    filter(is_core_category == "TRUE") |>
    select(type_name)
  dummy_actor_types <- actor_types |>
    filter(definition == "dummy") |>
    select(type_name)
  
  actor_edges <- actor_types |>
    arrange(sub_type_of, type_name) |>
    filter(sub_type_of != "") |>
    select(
      from=sub_type_of,
      to=type_name
    ) |>
    mutate(is_to_dummy = to %in% dummy_actor_types$type_name)

  graph <- graph_from_data_frame(actor_edges, directed=TRUE)
  V(graph)$distance_to_root <- distances(graph, v=V(graph), to=which(V(graph)$name == "actor"))
  max_distance <- max(V(graph)$distance_to_root)
  layout <- create_layout(graph, layout="dendrogram", circular=FALSE) |>
    left_join(actor_types |> select(name=type_name, sector, is_core_category), by="name")
  layout$y <- layout$distance_to_root - max_distance
  layout$is_core_category <- layout$name %in% core_actor_types$type_name
  layout$is_dummy <- layout$name %in% dummy_actor_types$type_name
  
  ggraph(layout) + 
    geom_edge_diagonal(
      aes(colour = ifelse(is_to_dummy, "dummy", "normal")),
      show.legend=FALSE
    ) +
    geom_node_point(
      data=layout |> filter(!is_dummy),
      aes(
        fill=sector,
        colour=is_core_category
      ),
      shape=21,
      size=4,
      stroke=2
    ) +
    geom_node_text(
      data=layout |> filter(!is_dummy),
      aes(label=name),
      size=5,
      angle=0,
      vjust="center",
      hjust="left",
      nudge_y=0.05
    ) +
    coord_flip() +
    scale_y_continuous(limits=c(-max_distance, 1)) +
    scale_fill_manual(
      values=c(
        "Mostly public sector"="#F4A4C1",
        "Mostly university sector"="#99FFE3",
        "Mostly third sector"="#99DAFF",
        "Mostly private sector"="#FFE099",
        "Mixed/Unknown"="lightgrey"
      ), 
      name="",
      na.value="black"
    ) +
    scale_colour_manual(
      values=c("TRUE"="black", "FALSE"="lightgrey"),
      labels=c("TRUE"="core categories", "FALSE"="non-core categories"),
      name=""
    ) +
    scale_edge_colour_manual(
      values=c("dummy"="white", "normal"="lightgrey")
    ) +
    labs(
      title="Taxonomy of actors involved in dispersal of museum collections"
    ) +
    taxonomy_theme
}

event_types_taxonomy <- function() {
  # add dummy types to use as spaces between groups
  counter <- 1
  types_with_sub_types <- event_types |>
    filter(!is.na(sub_type_of)) |>
    select(type_name=sub_type_of) |>
    distinct()
  for (i in 1:nrow(types_with_sub_types)) {
    new_row_1 <- data.frame(
      type_name = as.character(counter),
      sub_type_of = types_with_sub_types$type_name[i],
      core_type = NA,
      is_core_category = FALSE,
      change_of_ownership = FALSE,
      change_of_custody = FALSE,
      end_of_existence = FALSE,
      definition = "dummy",
      total_instances = NA
    )
    new_row_2 <- data.frame(
      type_name = paste("z", as.character(counter)),
      sub_type_of = types_with_sub_types$type_name[i],
      core_type = NA,
      is_core_category = FALSE,
      change_of_ownership = FALSE,
      change_of_custody = FALSE,
      end_of_existence = FALSE,
      definition = "dummy",
      total_instances = NA
    )
    counter <- counter + 1
    event_types <- event_types |>
      rbind(new_row_1) |>
      rbind(new_row_2)
  }
 
  ownership_transfers <- event_types |>
    filter(change_of_ownership == "TRUE") |>
    select(type_name)
  custody_transfers <- event_types |>
   filter(change_of_custody == "TRUE") |>
   select(type_name)
  ends_of_existence <- event_types |>
   filter(end_of_existence == "TRUE") |>
   select(type_name)
  core_types <- event_types |>
    filter(is_core_category == "TRUE") |>
    select(type_name)
  dummy_types <- event_types |>
    filter(definition=="dummy") |>
    select(type_name)
 
  event_edges <- event_types |>
    arrange(sub_type_of, type_name) |>
    filter(sub_type_of != "") |>
    select(
      from=sub_type_of,
      to=type_name
    ) |>
    mutate(is_to_dummy = to %in% dummy_types$type_name)
  
  graph <- graph_from_data_frame(event_edges)
  V(graph)$distance_to_root <- distances(graph, v=V(graph), to=which(V(graph)$name == "event"))
  max_distance <- max(V(graph)$distance_to_root)
  parent_nodes <- sapply(V(graph), function(v) {
    parents <- neighbors(graph, v, mode = "in")
    if (length(parents) == 0) {
      return(NA) # Root node has no parent
    } else {
      return(V(graph)$name[parents[1]]) # Assuming one parent for a tree structure
    }
  })

  layout <- create_layout(graph, layout="dendrogram", circular=FALSE)
  layout$is_core_category <- layout$name %in% core_types$type_name
  # arrange nodes according to distance from root and move all core categories to the same y
  layout$y <- ifelse(layout$is_core_category, -2, layout$distance_to_root - max_distance)
  layout$parent <- parent_nodes[layout$name]
  layout$parent_y <- sapply(1:nrow(layout), function(i) {
    parent_name <- layout$parent[i]
    if (is.na(parent_name)) {
      return(NA) # Root node has no parent, so no parent y-coordinate
    } else {
      return(layout$y[layout$name == parent_name]) # Get the y-coordinate of the parent node
    }
  })
  layout$y <- ifelse(
    !is.na(layout$parent_y) & layout$y == layout$parent_y,
    layout$y + 1,
    layout$y
  )
  # add transfer types to nodes
  layout$transfer_type <- ifelse(
    layout$name %in% ownership_transfers$type_name & layout$name %in% custody_transfers$type_name,
    "Change of ownership and custody",
    ifelse(
      layout$name %in% ownership_transfers$type_name,
      "Change of ownership",
      ifelse(
        layout$name %in% custody_transfers$type_name,
        "Change of custody",
        ifelse(
          layout$name %in% ends_of_existence$type_name,
          "End of existence",
          "Event without recipient"
        )
      )
    )
  )
  layout$is_dummy <- layout$name %in% dummy_types$type_name

  ggraph(layout) + 
    geom_edge_diagonal(
      aes(colour = ifelse(is_to_dummy, "dummy", "normal")),
      show.legend=FALSE
    ) +
    geom_node_point(
      data=layout |> filter(!is_dummy),
      aes(fill=transfer_type, colour=is_core_category),
      shape=21,
      size=4,
      stroke=2
    ) +
    geom_node_text(
      data=layout |> filter(!is_dummy),
      aes(label=name),
      size=5,
      angle=0,
      vjust="center",
      hjust="left",
      nudge_y=0.05
    ) +
    coord_flip() +
    scale_y_continuous(limits=c(-max_distance, 1)) +
    scale_fill_manual(
      values=c(
        "Change of ownership and custody"="#785EF0",
        "Change of ownership"="#648FFF",
        "Change of custody"="#FE6100",
        "End of existence"="#000000",
        "Event without recipient"="lightgrey"
      ), 
      name="",
      na.value="black"
    ) +
    scale_colour_manual(
      values=c("TRUE"="black", "FALSE"="lightgrey"),
      labels=c("TRUE"="core categories", "FALSE"="non-core categories"),
      name=""
    ) +
    scale_edge_colour_manual(
      values=c("dummy"="white", "normal"="lightgrey")
    ) +
    labs(
      title="Taxonomy of events involved in dispersal of museum collections"
    ) +
    taxonomy_theme
}
