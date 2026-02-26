governance_taxonomy <- function(governance_types) {
  governance_edges <- governance_types |>
    mutate(sub_type_of=ifelse(is.na(sub_type_of), "governance", sub_type_of)) |>
    arrange(sub_type_of, type_name) |>
    select(
      from=sub_type_of,
      to=type_name
    )

  graph <- graph_from_data_frame(governance_edges, directed=TRUE)
  V(graph)$distance_to_root <- distances(graph, v=V(graph), to=which(V(graph)$name == "governance"))
  max_distance <- max(V(graph)$distance_to_root)
  layout <- create_layout(graph, layout="dendrogram", circular=FALSE) |>
    left_join(governance_types |> select(name=type_name, is_broad), by="name")
  layout$y <- layout$distance_to_root - max_distance
  layout$x <- -layout$x

  ggraph(layout) + 
    geom_edge_diagonal(
      colour="lightgrey",
      show.legend=FALSE
    ) +
    geom_node_point(
      data=layout,
      aes(fill=name, colour=is_broad),
      shape=21,
      size=2,
      stroke=1
    ) +
    geom_node_text(
      data = layout,
      aes(label=name),
      size=3,
      angle=0,
      vjust="center",
      hjust="left",
      nudge_y=0.02
    ) +
    coord_flip() +
    scale_y_continuous(limits=c(-2,1)) +
    scale_fill_manual(values=governance_colours, guide="none") +
    scale_colour_manual(
      values=c("TRUE"="black", "FALSE"="lightgrey"),
      labels=c("TRUE"="'broad' governance categories", "FALSE"="sub-categories"),
      name="",
      guide=guide_legend(reverse=TRUE),
      na.translate=FALSE
    ) +
    taxonomy_theme
}

size_taxonomy <- function(size_types) {
  size_edges <- size_types |>
    mutate(sub_type_of=ifelse(is.na(sub_type_of), "size", sub_type_of)) |>
    arrange(sub_type_of, type_name) |>
    select(
      from=sub_type_of,
      to=type_name
    )

  graph <- graph_from_data_frame(size_edges, directed=TRUE)
  V(graph)$distance_to_root <- distances(graph, v=V(graph), to=which(V(graph)$name == "size"))
  max_distance <- max(V(graph)$distance_to_root)
  layout <- create_layout(graph, layout="dendrogram", circular=FALSE) |>
    left_join(size_types |> select(name=type_name, definition), by="name") |>
    mutate(is_broad=ifelse(name == "size", NA, TRUE))
  layout$y <- layout$distance_to_root - max_distance
  layout$x <- -layout$x

  ggraph(layout) + 
    geom_edge_diagonal(
      colour="lightgrey",
      show.legend=FALSE
    ) +
    geom_node_point(
      data=layout,
      aes(fill=name, colour=is_broad),
      shape=21,
      size=2,
      stroke=1
    ) +
    geom_node_text(
      data = layout,
      aes(
        label=ifelse(
          name == "size",
          "size",
          paste0(name, " (", definition, ")")
        )
      ),
      size=3,
      angle=0,
      vjust="center",
      hjust="left",
      nudge_y=0.02
    ) +
    coord_flip() +
    scale_y_continuous(limits=c(-1,1)) +
    scale_fill_manual(values=size_colours, guide="none") +
    scale_colour_manual(
      values=c("TRUE"="black", "FALSE"="lightgrey"),
      name="",
      guide="none",
      na.translate=FALSE
    ) +
    taxonomy_theme
}

subject_taxonomy <- function(subject_types) {
  subject_types <- subject_types |>
    mutate(
      label=ifelse(
        str_detect(type_name, ": "),
        # get name after ": "
        str_replace(type_name, ".*?:\\s*", ""),
        type_name
      ),
      is_dummy=FALSE
    )
  dummy_types <- subject_types |>
    filter(is_broad) |>
    mutate(
      sub_type_of=type_name,
      type_name=as.character(row_number()),
      label=type_name,
      is_broad=FALSE,
      is_dummy=TRUE,
    ) |>
    rbind(
      subject_types |>
        filter(is_broad) |>
        mutate(
          sub_type_of=type_name,
          type_name=paste("z", row_number()),
          label=type_name,
          is_broad=FALSE,
          is_dummy=TRUE,
        )
    )
  subject_types <- rbind(subject_types, dummy_types)
  subject_edges <- subject_types |>
    mutate(
      sub_type_of=ifelse(is.na(sub_type_of), "subject", sub_type_of)
    ) |>
    arrange(sub_type_of, type_name) |>
    select(
      from=sub_type_of,
      to=type_name
    ) |>
    mutate(is_to_dummy = to %in% dummy_types$type_name)

  graph <- graph_from_data_frame(subject_edges, directed=TRUE)
  V(graph)$distance_to_root <- distances(graph, v=V(graph), to=which(V(graph)$name == "subject"))
  max_distance <- max(V(graph)$distance_to_root)
  layout <- create_layout(graph, layout="dendrogram", circular=FALSE) |>
    left_join(subject_types |> select(name=type_name, is_broad, label), by="name")
  layout$is_dummy <- layout$name %in% dummy_types$type_name
  layout$y <- layout$distance_to_root - max_distance
  layout$x <- -layout$x

  ggraph(layout) + 
    geom_edge_diagonal(
      aes(colour = ifelse(is_to_dummy, "dummy", "normal")),
      show.legend=FALSE
    ) +
    geom_node_point(
      data=layout |> filter(!is_dummy),
      aes(fill=name, colour=is_broad),
      shape=21,
      size=2,
      stroke=1
    ) +
    geom_node_text(
      data = layout |> filter(!is_dummy),
      aes(label=label),
      size=3,
      angle=0,
      vjust="center",
      hjust="left",
      nudge_y=0.02
    ) +
    coord_flip() +
    scale_y_continuous(limits=c(-2,1)) +
    scale_fill_manual(values=subject_colours, guide="none", na.value="white") +
    scale_colour_manual(
      values=c("TRUE"="black", "FALSE"="lightgrey"),
      labels=c("TRUE"="'broad' subject categories", "FALSE"="sub-categories"),
      name="",
      guide=guide_legend(reverse=TRUE),
      na.translate=FALSE
    ) +
    scale_edge_colour_manual(
      values=c("dummy"="white", "normal"="lightgrey")
    ) +
    taxonomy_theme
}

actors_taxonomy <- function() {
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
      size=2,
      stroke=1
    ) +
    geom_node_text(
      data=layout |> filter(!is_dummy),
      aes(label=name),
      size=3,
      angle=0,
      vjust="center",
      hjust="left",
      nudge_y=0.02
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
      na.value="black",
    ) +
    scale_colour_manual(
      values=c("TRUE"="black", "FALSE"="lightgrey"),
      labels=c("TRUE"="core categories", "FALSE"="sub-categories"),
      name=""
    ) +
    scale_edge_colour_manual(
      values=c("dummy"="white", "normal"="lightgrey")
    ) +
    guides(
      fill=guide_legend(order=1, override.aes=list(colour=NA, stroke=0)),
      colour=guide_legend(order=2, override.aes=list(fill=NA))
    ) +
    taxonomy_theme
}

events_taxonomy <- function() {
  # add dummy types to use as spaces between groups
  event_types <- event_types |> select(-contributes_to_length_calculation)
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
      size=2,
      stroke=1
    ) +
    geom_node_text(
      data=layout |> filter(!is_dummy),
      aes(label=name),
      size=3,
      angle=0,
      vjust="center",
      hjust="left",
      nudge_y=0.02
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
      labels=c("TRUE"="core categories", "FALSE"="sub-categories"),
      name=""
    ) +
    scale_edge_colour_manual(
      values=c("dummy"="white", "normal"="lightgrey")
    ) +
    guides(
      fill=guide_legend(order=1, override.aes=list(colour=NA, stroke=0)),
      colour=guide_legend(order=2, override.aes=list(fill=NA))
    ) +
    taxonomy_theme
}

reasons_taxonomy <- function() {
    closure_reasons_raw <- super_events |>
      separate_rows(reason, sep = "; ") |>
      separate_wider_delim(
        reason,
        " - ",
        names=c("core_reason", "sub_core_reason", "specific_reason"),
        too_few="align_start"
      ) |>
      select(specific_reason, sub_core_reason, core_reason) |>
      distinct()
    
    core_reasons <- closure_reasons_raw |>
      select(type_name=core_reason) |>
      distinct() |>
      mutate(sub_type_of="reason", is_core_category=TRUE) |>
      select(type_name, sub_type_of, is_core_category)
    sub_core_reasons <- closure_reasons_raw |>
      select(type_name=sub_core_reason, sub_type_of=core_reason) |>
      distinct() |>
      mutate(is_core_category=FALSE) |>
      select(type_name, sub_type_of, is_core_category)
    specific_reasons <- closure_reasons_raw |>
      select(type_name=specific_reason, sub_type_of=sub_core_reason) |>
      distinct() |>
      mutate(is_core_category=FALSE) |>
      select(type_name, sub_type_of, is_core_category)
    
    closure_reasons <- data.frame(type_name=c("reason"), sub_type_of=c(NA), is_core_category=c(FALSE)) |>
      rbind(core_reasons) |>
      rbind(sub_core_reasons) |>
      rbind(specific_reasons) |>
      filter(!is.na(type_name)) |>
      #filter(!type_name %in% c("disagreement", "theft")) |>
      add_dummies()
    
    reasons_layout <- get_taxonomy_layout(closure_reasons, "reason")
    reasons_taxonomy <- get_taxonomy(reasons_layout)
    reasons_taxonomy
}

add_dummies <- function(table) {
  table <- table |> mutate(is_dummy=FALSE)
  counter <- 1
  types_with_sub_types <- table |>
    filter(!is.na(sub_type_of)) |>
    select(type_name=sub_type_of) |>
    distinct()
  for (i in 1:nrow(types_with_sub_types)) {
    new_row_1 <- data.frame(
      type_name = as.character(counter),
      sub_type_of = types_with_sub_types$type_name[i],
      is_core_category = FALSE,
      is_dummy = TRUE
    )
    new_row_2 <- data.frame(
      type_name = paste("z", as.character(counter)),
      sub_type_of = types_with_sub_types$type_name[i],
      is_core_category = FALSE,
      is_dummy = TRUE
    )
    counter <- counter + 1
    table <- table |>
      rbind(new_row_1) |>
      rbind(new_row_2)
  }
  table
}

get_taxonomy_layout <- function(types, root_name) {
  core_types <- types |>
    filter(is_core_category) |>
    select(type_name)
  dummy_types <- types |>
    filter(is_dummy) |>
    select(type_name)
  edges <- types |>
    arrange(sub_type_of, type_name) |>
    filter(!is.na(sub_type_of), sub_type_of != "") |>
    select(
      from=sub_type_of,
      to=type_name
    ) |>
    mutate(is_to_dummy = to %in% dummy_types$type_name)
  graph <- graph_from_data_frame(edges, directed=TRUE)
  V(graph)$distance_to_root <- distances(graph, v=V(graph), to=which(V(graph)$name == root_name))
  max_distance <- max(V(graph)$distance_to_root)
  layout <- create_layout(graph, layout="dendrogram", circular=FALSE) |>
    left_join(types |> select(name=type_name, is_core_category), by="name")
  layout$y <- layout$distance_to_root - max_distance
  layout$is_core_category <- layout$name %in% core_types$type_name
  layout$is_dummy <- layout$name %in% dummy_types$type_name
  layout
}

get_taxonomy <- function(layout) {
  min_y <- min(layout$y)
  ggraph(layout) + 
    geom_edge_diagonal(
      aes(colour = ifelse(is_to_dummy, "dummy", "normal")),
      show.legend=FALSE
    ) +
    geom_node_point(
      data=layout |> filter(!is_dummy),
      aes(colour=is_core_category),
      shape=21,
      size=2,
      stroke=1,
      fill="white"
    ) +
    geom_node_text(
      data=layout |> filter(!is_dummy),
      aes(label=name),
      size=3,
      angle=0,
      vjust="center",
      hjust="left",
      nudge_y=0.02
    ) +
    coord_flip() +
    scale_y_continuous(limits=c(min_y, 1)) +
    scale_colour_manual(
      values=c("TRUE"="black", "FALSE"="lightgrey"),
      labels=c("TRUE"="core categories", "FALSE"="sub-categories"),
      name=""
    ) +
    scale_edge_colour_manual(
      values=c("dummy"="white", "normal"="lightgrey")
    ) +
    guides(
      colour=guide_legend(order=2, override.aes=list(fill=NA))
    ) +
    taxonomy_theme
}
