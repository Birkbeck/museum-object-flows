sequences_table_choices <- c(
  "event_stage_in_path",
  "event_date",
  "event_date_from",
  "event_date_to",
  "event_type",
  "event_core_type",
  "event_type_uncertainty",
  "event_is_change_of_custody",
  "event_is_change_of_ownership",
  "distance",
  "distance_category",
  "collection_id",
  "collection_description",
  "collection_status",
  "collection_types",
  "collection_size",
  "initial_museum_governance",
  "initial_museum_size",
  "initial_museum_subject",
  "initial_museum_subject_broad",
  "initial_museum_region",
  "initial_museum_town",
  "sender_name",
  "recipient_name",
  "sender_quantity",
  "sender_type",
  "sender_sector",
  "sender_governance",
  "sender_size",
  "sender_subject_broad",
  "sender_accreditation",
  "sender_region",
  "sender_town",
  "recipient_quantity",
  "recipient_type",
  "recipient_sector",
  "recipient_governance",
  "recipient_size",
  "recipient_subject_broad",
  "recipient_accreditation",
  "recipient_region",
  "recipient_town"
)

sequences_table_selected <- c(
  "event_stage_in_path",
  "event_date",
  "event_date_from",
  "event_date_to",
  "event_type",
  "distance_category",
  "collection_description",
  "collection_status",
  "collection_types",
  "collection_size",
  "initial_museum_governance",
  "initial_museum_size",
  "initial_museum_subject",
  "initial_museum_town",
  "recipient_name",
  "recipient_quantity",
  "recipient_type",
  "recipient_sector",
  "recipient_town"
)

grouping_dimension_map <- list(
  "Actor sector"="sector",
  "Actor type (core categories)"="core_type",
  "Actor type (most specific)"="type",
  "Actor region/country"="region"
) 

get_dispersal_initial_museums <- function(dispersal_events,
                                         include_firepower,
                                         size_filter,
                                         governance_filter,
                                         subject_filter,
                                         specific_subject_filter,
                                         region_filter,
                                         accreditation_filter) {
  dispersal_events |>
    filter(
      include_firepower | initial_museum_id != "mm.domus.SE513",
      initial_museum_size %in% size_filter,
      initial_museum_governance_broad %in% governance_filter,
      initial_museum_subject_broad %in% subject_filter,
      initial_museum_subject %in% specific_subject_filter,
      initial_museum_region %in% region_filter,
      initial_museum_accreditation %in% accreditation_filter
    ) |>
    mutate(
      museum_id=initial_museum_id,
      name=paste0(initial_museum_name, " (", initial_museum_id, ")")
    ) |>
    arrange(name) |>
    select(name, museum_id, initial_museum_size) |>
    distinct()
}

get_actor_choices <- function(grouping_dimension, museum_grouping_dimension) {
  grouping_dimension <- grouping_dimension_map[grouping_dimension]
  recipient_grouping_dimension <- paste0("recipient_", grouping_dimension)
  recipient_museum_grouping_dimension <- paste0("recipient_", museum_grouping_dimension)
  if (recipient_museum_grouping_dimension == recipient_grouping_dimension) {
    grouped_events <- dispersal_events |>
      group_by(.data[[recipient_grouping_dimension]])
  } else {
    grouped_events <- dispersal_events |>
      group_by(.data[[recipient_museum_grouping_dimension]], .data[[recipient_grouping_dimension]])
  }
  choices_table <- grouped_events |>
    summarize(
      to=paste(.data[[recipient_museum_grouping_dimension]], .data[[recipient_grouping_dimension]], sep="@"),
      label=ifelse(
        !is.na(.data[[recipient_museum_grouping_dimension]]),
        paste(.data[[recipient_museum_grouping_dimension]], "museum"),
        .data[[recipient_grouping_dimension]]
      )
    ) |>
    ungroup() |>
    select(to, label) |>
    distinct()
  choices_table
}

grouping_dimension_and_governance_to_sector <- function(governance, grouping_dimension) {
  ifelse(
    governance %in% names(governance_to_sector),
    governance_to_sector[governance],
    grouping_dimension
  )
}

get_filtered_sequences <- function(events_data,
                                   show_transaction_types,
                                   grouping_dimension,
                                   museum_grouping_dimension,
                                   event_type_filter,
                                   event_type_uncertainty_filter,
                                   collection_status_filter,
                                   initial_museum_ids,
                                   show_ending_points,
                                   show_passes_through,
                                   steps_or_first_last) {
  sequences <- events_data |>
    mutate(
      sender_group=.data[[paste0("sender_", grouping_dimension)]],
      sender_museum_group=.data[[paste0("sender_", museum_grouping_dimension)]],
      recipient_group=.data[[paste0("recipient_", grouping_dimension)]],
      recipient_museum_group=.data[[paste0("recipient_", museum_grouping_dimension)]]
    ) |>
    filter(initial_museum_id %in% initial_museum_ids) |>
    find_events_to_show(show_transaction_types) |>
    find_previous_events() |>
    filter(show_event) |>
    assign_stages_in_path() |>
    add_sender_details(
      grouping_dimension, museum_grouping_dimension
    ) |>
    filter_by_final_recipient(
      show_ending_points, grouping_dimension, museum_grouping_dimension
    ) |>
    filter_by_intermediary_recipient(
      show_passes_through, grouping_dimension, museum_grouping_dimension
    ) |>
    filter(
      collection_status %in% collection_status_filter,
      event_core_type %in% c(event_type_filter),
      event_type_uncertainty %in% c(event_type_uncertainty_filter),
    )
  if (steps_or_first_last == "First and last actors") {
    sequences <- remove_sequence_middle(
      sequences, grouping_dimension, museum_grouping_dimension
    )
  }
  sequences
}

find_events_to_show <- function(events_data, show_transaction_types) {
  show_ownership_changes <- "Change of ownership" %in% show_transaction_types
  show_custody_changes <- "Change of custody" %in% show_transaction_types
  show_ends_of_existence <- "End of existence" %in% show_transaction_types
  events_data |>
    mutate(
      show_event=show_ownership_changes & event_is_change_of_ownership |
        show_custody_changes & event_is_change_of_custody |
        show_ends_of_existence & event_is_end_of_existence
    )
}

find_previous_events <- function(events_data) {
  events_data |>
    rowwise() |>
    mutate(
      previous_shown_event= {
        prev_event <- previous_event_id
        while (!is.na(prev_event)) {
          prev_row <- events_data |>
            filter(event_id==prev_event)
          if (nrow(prev_row) == 0) {
            prev_event <- NA
            break
          }
          if (prev_row$show_event) {
            prev_event <- prev_row$event_id
            break
          } else {
            prev_event <- prev_row$previous_event_id
          }
        }
        prev_event
      }
    ) |>
    ungroup()
}

assign_stages_in_path <- function(events_data) {
  events_data |>
    group_by(original_collection_id) |>
    arrange(event_stage_in_path) |>
    mutate(
      event_stage_in_path={
        event_id_to_event_stage_map <- list()
        result <- integer(n())
        for (i in seq_len(n())) {
          current_id <- as.character(event_id[i])
          previous_id <- previous_shown_event[i]
          this_is_the_first_event_involving_the_collection <- is.na(previous_id)
          if (this_is_the_first_event_involving_the_collection) {
            stage <- 1L
          } else {
            stage <- event_id_to_event_stage_map[[as.character(previous_id)]] + 1L
          }
          result[i] <- stage
          event_id_to_event_stage_map[[current_id]] <- stage
        }
        result
      },
      sender_position=event_stage_in_path,
      recipient_position=event_stage_in_path+1L,
    ) |>
    ungroup()
}

add_sender_details <- function(events_data, grouping_dimension, museum_grouping_dimension) {
  # It is necessary to re-infer the sender of events
  # This is because the database records the sender as the recipient of the previous event
  # But if there is a sequence of events which are mixtures of changes of custody and ownership,
  # Then the sender of custody and sender of ownership are not necessarily the same
  # If a filter has removed certain types of transaction, the relevant sender must be found
  events_data |>
    left_join(
      events_data |>
        select(
          previous_shown_event=event_id,
          from_name=recipient_name,
          from_quantity=recipient_quantity,
          from_type=recipient_type,
          from_core_type=recipient_core_type,
          from_general_type=recipient_general_type,
          from_sector=recipient_sector,
          # TODO: all museum attributes need to be updated
          from_size=recipient_size,
          from_governance=recipient_governance,
          from_governance_broad=recipient_governance_broad,
          from_accreditation=recipient_accreditation,
          from_subject_broad=recipient_subject_broad,
          from_country=recipient_country,
          from_region=recipient_region,
          from_town=recipient_town
        ),
      by=c("previous_shown_event")
    ) |>
    mutate(
      sender_name=ifelse(
        event_stage_in_path==1, initial_museum_name, from_name
      ),
      sender_quantity=ifelse(
        event_stage_in_path==1, 1, from_quantity
      ),
      sender_type=ifelse(
        event_stage_in_path==1, initial_museum_type, from_type
      ),
      sender_core_type=ifelse(
        event_stage_in_path==1, initial_museum_core_type, from_core_type
      ),
      sender_general_type=ifelse(
        event_stage_in_path==1, initial_museum_general_type, from_general_type
      ),
      sender_sector=ifelse(
        event_stage_in_path==1, initial_museum_sector, from_sector
      ),
      sender_size=ifelse(
        event_stage_in_path==1, initial_museum_size, from_size
      ),
      sender_governance=ifelse(
        event_stage_in_path==1, initial_museum_governance, from_governance
      ),
      sender_governance_broad=ifelse(
        event_stage_in_path==1, initial_museum_governance_broad, from_governance_broad
      ),
      sender_accreditation=ifelse(
        event_stage_in_path==1, initial_museum_accreditation, from_accreditation
      ),
      sender_subject_broad=ifelse(
        event_stage_in_path==1, initial_museum_subject_broad, from_subject_broad
      ),
      sender_country=ifelse(
        event_stage_in_path==1, initial_museum_country, from_country
      ),
      sender_region=ifelse(
        event_stage_in_path==1, initial_museum_region, from_region
      ),
      sender_town=ifelse(
        event_stage_in_path==1, initial_museum_town, from_town
      ),
      from=ifelse(
        event_stage_in_path==1,
        paste(
          .data[[paste0("initial_museum_", museum_grouping_dimension)]],
          .data[[paste0("initial_museum_", grouping_dimension)]],
          1,
          sep="@"
        ),
        paste(
          .data[[paste0("sender_", museum_grouping_dimension)]],
          .data[[paste0("sender_", grouping_dimension)]],
          sender_position,
          sep="@"
        )
      ),
      to=paste(
        .data[[paste0("recipient_", museum_grouping_dimension)]],
        .data[[paste0("recipient_", grouping_dimension)]],
        recipient_position,
        sep="@"
      ),
    )
}

filter_by_final_recipient <- function(events_data,
                                      show_ending_points,
                                      grouping_dimension,
                                      museum_grouping_dimension) {
  events_final_recipients <- events_data |>
    group_by(event_id, ancestor_events) |>
    filter(recipient_position==max(recipient_position)) |>
    summarize(
      final_recipient=paste(
        .data[[paste0("recipient_", museum_grouping_dimension)]],
        .data[[paste0("recipient_", grouping_dimension)]],
        sep="@"
      )
    ) |>
    ungroup()
  ancestor_final_recipients <- events_final_recipients |>
    mutate(
      ancestor_events = str_remove_all(ancestor_events, "\\[|\\]"),
      ancestor_events = strsplit(as.character(ancestor_events), ", ")
    ) |>
    unnest(ancestor_events) |>
    mutate(
      ancestor_events = str_remove_all(ancestor_events, "^'|'$")
    ) |>
    select(event_id=ancestor_events, final_recipient)
  events_final_recipients <- rbind(
    events_final_recipients |> select(event_id, final_recipient),
    ancestor_final_recipients
  )
  events_with_allowed_final_recipients <- events_final_recipients |>
    filter(final_recipient %in% show_ending_points)
  events_data |>
    filter(
      event_id %in% events_with_allowed_final_recipients$event_id
    )
}

filter_by_intermediary_recipient <- function(events_data,
                                             show_passes_through,
                                             grouping_dimension,
                                             museum_grouping_dimension) {
  events_recipients <- events_data |>
    mutate(
      recipient=paste(
        .data[[paste0("recipient_", museum_grouping_dimension)]],
        .data[[paste0("recipient_", grouping_dimension)]],
        sep="@"
      )
    )
  ancestor_recipients <- events_recipients |>
    mutate(
      ancestor_events = str_remove_all(ancestor_events, "\\[|\\]"),
      ancestor_events = strsplit(as.character(ancestor_events), ", ")
    ) |>
    unnest(ancestor_events) |>
    mutate(
      ancestor_events = str_remove_all(ancestor_events, "^'|'$")
    ) |>
    select(event_id=ancestor_events, recipient)
  events_recipients <- rbind(
    events_recipients |> select(event_id, recipient),
    ancestor_recipients
  )
  events_with_allowed_intermediary_recipients <- events_recipients |>
    filter(recipient %in% show_passes_through)
  events_data |>
    filter(
      event_id %in% events_with_allowed_intermediary_recipients$event_id
    )
}

remove_sequence_middle <- function(events_data, grouping_dimension, museum_grouping_dimension) {
  events_data |>
    group_by(collection_id) |>
    filter(recipient_position==max(recipient_position)) |>
    mutate(
      sender_id=initial_museum_id,
      sender_name=initial_museum_name,
      sender_type=initial_museum_type,
      sender_core_type=initial_museum_core_type,
      sender_general_type=initial_museum_general_type,
      sender_sector=initial_museum_sector,
      sender_size=initial_museum_size,
      sender_governance=initial_museum_governance,
      sender_governance_broad=initial_museum_governance_broad,
      sender_subject_matter=initial_museum_subject_matter,
      sender_subject_broad=initial_museum_subject_broad,
      sender_accreditation=initial_museum_accreditation,
      sender_town=initial_museum_town,
      sender_position=1,
      sender_quantity="1",
      recipient_position=2,
      from=paste(
        .data[[paste0("initial_museum_", museum_grouping_dimension)]],
        .data[[paste0("initial_museum_", grouping_dimension)]],
        sender_position,
        sep="@"
      ),
      to=paste(
        .data[[paste0("recipient_", museum_grouping_dimension)]],
        .data[[paste0("recipient_", grouping_dimension)]],
        recipient_position,
        sep="@"
      )
    ) |>
    ungroup()
}

pathway_table <- function(sequences, selected_columns) {
  sequences |>
    select(all_of(selected_columns))
}

get_pathways_layout <- function(sequences,
                                start_position,
                                end_position,
                                grouping_dimension,
                                museum_grouping_dimension,
                                steps_or_first_last) {

 dendrogram_data <- sequences |>
   # TODO: move filtering to datatable creation
   filter(recipient_position <= end_position) |>
   give_individual_names_to_nodes_in_different_paths(steps_or_first_last) |>
   add_central_node_if_there_are_many_starting_points()

  from_nodes <- get_nodes(
    dendrogram_data, "from", "sender", "sender_group", "sender_museum_group"
  )
  to_nodes <- get_nodes(
    dendrogram_data, "to", "recipient", "recipient_group", "recipient_museum_group"
  )
  nodes <- merge_from_and_to_nodes(from_nodes, to_nodes) |>
    # TODO: move filtering to datatable creation
    filter(position >= start_position & position <= end_position)

  if (grouping_dimension == "sector") {
    nodes <- nodes |>
      mutate(
        name=paste(museum_group, actor_group, sep="@"),
        label = ifelse(
          !is.na(museum_group), 
          paste(gsub("_", " ", museum_group), "museum"),
          paste(actor_group, "sector")
        )
      )
  } else if (museum_grouping_dimension == "region" && grouping_dimension == "region") {
     nodes <- nodes |> 
      mutate(
        name=paste(museum_group, actor_group, sep="@"),
        label=actor_group
      )
  } else if (grouping_dimension == "region") {
     nodes <- nodes |>
      mutate(
        name=paste(museum_group, actor_group, sep="@"),
        label = ifelse(
          !is.na(museum_group),
          paste(actor_group, museum_group, "museum"),
          paste(actor_group, "other")
        )
      )
  } else {
    nodes <- nodes |>
      mutate(
        name=paste(museum_group, actor_group, sep="@"),
        label = ifelse(
          !is.na(museum_group), 
          paste(gsub("_", " ", museum_group), "museum"),
          actor_group
        )
      )
  }

  edges <- get_edges(dendrogram_data, nodes)
  write_csv(edges, "edges.csv")
  count_edges <- edges |> filter(label != "")

  dendrogram_graph <- graph_from_data_frame(count_edges)
  dendrogram_layout <- create_layout(dendrogram_graph, layout="dendrogram", circular=TRUE) |>
    mutate(id=name) |>
    select(id, x, y)

  nodes <- nodes |> left_join(dendrogram_layout, by="id")
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

  list("nodes"=nodes, "edges"=edges)
}

get_sequences_layout <- function(sequences,
                                 start_position,
                                 end_position,
                                 grouping_dimension,
                                 museum_grouping_dimension) {

  from_nodes <- get_nodes(
    sequences, "from", "sender", "sender_group", "sender_museum_group"
  )
  to_nodes <- get_nodes(
    sequences, "to", "recipient", "recipient_group", "recipient_museum_group"
  )
  nodes <- merge_from_and_to_nodes(from_nodes, to_nodes) |>
    mutate(name=paste(museum_group, actor_group, sep="@")) |>
    # TODO: move filtering to datatable creation
    filter(position >= start_position & position <= end_position)

  name_mapping <- nodes |>
    select(name) |>
    distinct() |>
    mutate(
      name_numeric = row_number()
    )
  
  if (grouping_dimension == "sector") {
    name_mapping <- name_mapping |> 
      mutate(
        museum_type = sapply(strsplit(name, "@"), `[`, 1),
        sector = sapply(strsplit(name, "@"), `[`, 2),
        label = ifelse(
          museum_type != "NA", 
          paste(museum_type, "museum"), 
          paste("Other", sector, "sector")
        )
      ) |> 
      select(-museum_type, -sector)
  } else if (museum_grouping_dimension == "region" && grouping_dimension == "region") {
    name_mapping <- name_mapping |> 
      mutate(
        label = sapply(strsplit(name, "@"), `[`, 1)
      )
  } else if (grouping_dimension == "region") {
    name_mapping <- name_mapping |>
      mutate(
        museum_type = sapply(strsplit(name, "@"), `[`, 1),
        region = sapply(strsplit(name, "@"), `[`, 2),
        label = ifelse(
          museum_type != "NA",
          paste(region, museum_type, "museum"),
          paste(region, "other")
        )
      ) |>
      select(-museum_type, -region)
  } else {
    name_mapping <- name_mapping |> 
      mutate(
        museum_type = sapply(strsplit(name, "@"), `[`, 1),
        actor_type = sapply(strsplit(name, "@"), `[`, 2),
        label = ifelse(
          museum_type != "NA", 
          paste(museum_type, "museum"), 
          actor_type
        )
      ) |> 
      select(-museum_type, -actor_type)
  }
  
  nodes <- nodes |>
    left_join(name_mapping, by = "name")

  edges <- get_edges(sequences, nodes) |>
    mutate(
      from_name=paste(sender_museum_group, sender_group, sep="@"),
      to_name=paste(recipient_museum_group, recipient_group, sep="@"),
      label=ifelse(!is.na(label), as.character(label), ""),
      from_position=as.numeric(sapply(str_split(from, "@"), function(x) if(length(x) > 2) x[3] else NA)),
      to_position=as.numeric(sapply(str_split(to, "@"), function(x) if(length(x) > 2) x[3] else NA)),
    ) |>
    filter(from_position >= start_position & to_position <= end_position) |>
    left_join(name_mapping |> select(-label), by = c("from_name" = "name")) |>
    rename(from_name_numeric = name_numeric) |>
    left_join(name_mapping |> select(-label), by = c("to_name" = "name")) |>
    rename(to_name_numeric = name_numeric) |>
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
    )

  write_csv(edges, "edges-sequences.csv")

  list("nodes"=nodes, "edges"=edges, "name_mapping"=name_mapping)
}

give_individual_names_to_nodes_in_different_paths <- function(sequences, steps_or_first_last) {
  build_chains <- function(data) {
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
    sequences <- build_chains(sequences)
  } else {
    sequences <- sequences |>
      mutate(
        to = paste(from, "->", to)
      )
  }
  sequences
}

add_central_node_if_there_are_many_starting_points <- function(sequences) {
  # because a dendrogram layout is being used, a single root node is needed so that paths form a tree.
  # if there are many starting points, a dummy root node is created as an invisible starting point.
  stage_1_events <- sequences |>
    filter(event_stage_in_path == 1)
  initial_senders <- stage_1_events |>
    select(sender_museum_group, sender_group) |>
    distinct()
  if(nrow(initial_senders) > 1) {
    dummy_rows <- stage_1_events |>
      mutate(
        event_id = "",
        event_stage_in_path = 0,
        previous_shown_event = "",
        to = from,
        from = "",
        recipient_id = sender_id,
        sender_id = "",
        recipient_sector = sender_sector,
        recipient_museum_group = sender_museum_group,
        sender_museum_group = "dummy",
        recipient_group = sender_group,
        sender_group = "",
        recipient_position = sender_position,
        sender_position = 0,
        recipient_quantity = sender_quantity,
        sender_quantity = "1"
      )
    sequences <- bind_rows(sequences, dummy_rows)
  }
  sequences
}

get_nodes <- function(sequences, endpoint, actor_role, grouping_dimension, museum_grouping_dimension) {
  endpoint_count <- paste0(endpoint, "_count")
  endpoint_count_suffix <- paste0(endpoint, "_count_suffix")
  endpoint_count_label <- paste0(endpoint, "_count_label")
  sequences |>
    mutate(
      id=.data[[endpoint]],
      actor_id=.data[[paste0(actor_role, "_id")]],
      quantity=.data[[paste0(actor_role, "_quantity")]],
      count=ifelse(quantity=="many", 2, as.numeric(quantity)),
      sector=.data[[paste0(actor_role, "_sector")]],
      museum_group=.data[[museum_grouping_dimension]],
      actor_group=.data[[grouping_dimension]],
      position=.data[[paste0(actor_role, "_position")]]
    ) |>
    select(id, actor_id, quantity, count, sector, museum_group, actor_group, position) |>
    distinct() |>
    group_by(id, museum_group, actor_group, position) |>
    summarize(
      !!sym(endpoint_count):=sum(count),
      !!sym(endpoint_count_suffix):=ifelse("many" %in% quantity, "+", ""),
      !!sym(endpoint_count_label):=paste0(
        .data[[endpoint_count]], .data[[endpoint_count_suffix]]
      ),
      public_instances=sum(ifelse(sector=="public", count, 0)),
      university_instances=sum(ifelse(sector=="university", count, 0)),
      third_instances=sum(ifelse(sector=="third", count, 0)),
      private_instances=sum(ifelse(sector=="private", count, 0)),
      hybrid_instances=sum(ifelse(sector=="hybrid", count, 0)),
      public_proportion = public_instances / .data[[endpoint_count]],
      university_proportion = university_instances / .data[[endpoint_count]],
      third_proportion = third_instances / .data[[endpoint_count]],
      private_proportion = private_instances / .data[[endpoint_count]],
      hybrid_proportion = hybrid_instances / .data[[endpoint_count]],
      !!sym(paste0(endpoint, "_sector")):=case_when(
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
}

merge_from_and_to_nodes <- function(from_nodes, to_nodes) {
  from_nodes |>
    full_join(to_nodes, by=c("id", "museum_group", "actor_group", "position")) |>
    mutate(
      count=case_when(
        is.na(to_count) ~ from_count,
        is.na(from_count) ~ to_count,
        from_count > to_count ~ from_count,
        TRUE ~ to_count
      ),
      count_label=case_when(
        is.na(to_count) ~ from_count_label,
        is.na(from_count) ~ to_count_label,
        from_count > to_count ~ from_count_label,
        TRUE ~ to_count_label
      ),
      sector_label=case_when(
        is.na(to_count) ~ from_sector,
        is.na(from_count) ~ to_sector,
        from_count > to_count ~ from_sector,
        TRUE ~ to_sector
      )
    )
}

get_edges <- function(sequences, nodes) {
  museum_sizes = c(
    "huge"=3e6,
    "large"=5e5,
    "medium"=3e4,
    "small"=5e3,
    "unknown"=5e3
  )
  count_edges <- sequences |>
    mutate(
      size_mid = ifelse(is.na(collection_estimated_size), 1, collection_estimated_size),
      size_max = ifelse(is.na(collection_estimated_size_max), 1, collection_estimated_size_max),
      size_min = ifelse(is.na(collection_estimated_size_min), 1, collection_estimated_size_min)
    ) |>
    group_by(
      from,
      to,
      sender_group,
      sender_museum_group,
      recipient_group,
      recipient_museum_group,
    ) |>
    summarize(count = n()) |>
    ungroup() |>
    mutate(label=count)
  edges <- sequences |>
    mutate(
      size_mid = ifelse(is.na(collection_estimated_size), 1, collection_estimated_size),
      size_max = ifelse(is.na(collection_estimated_size_max), 1, collection_estimated_size_max),
      size_min = ifelse(is.na(collection_estimated_size_min), 1, collection_estimated_size_min)
    ) |>
    group_by(
      sender_id,
      from,
      to,
      sender_group,
      sender_museum_group,
      recipient_group,
      recipient_museum_group
    ) |>
    summarize(
      # this initial summing of transaction sizes is from each individual sender to each recipient type
      # the sums must not add up to more than 100% of the initial museum.
      transaction_total_size_mid = min(sum(size_mid), museum_sizes[initial_museum_size]),
      transaction_total_size_max = min(sum(size_max), museum_sizes[initial_museum_size]),
      transaction_total_size_min = min(sum(size_min), museum_sizes[initial_museum_size])
    ) |>
    group_by(
      from,
      to,
      sender_group,
      sender_museum_group,
      recipient_group,
      recipient_museum_group
    ) |>
    summarize(
      # the totals can now be summed for edges from each sender type to each recipient type
      transaction_total_size_mid = sum(transaction_total_size_mid),
      transaction_total_size_max = sum(transaction_total_size_max),
      transaction_total_size_min = sum(transaction_total_size_min),
      # extra counts are added in order to give edges a blurry line
      transaction_total_size_min_1_4 = transaction_total_size_min * 0.25,
      transaction_total_size_min_1_2 = transaction_total_size_min * 0.5,
      transaction_total_size_min_3_4 = transaction_total_size_min * 0.75,
      transaction_total_size_mid_1_4 = transaction_total_size_mid * 0.25,
      transaction_total_size_mid_1_2 = transaction_total_size_mid * 0.5,
      transaction_total_size_mid_3_4 = transaction_total_size_mid * 0.75,
      transaction_total_size_max_1_4 = transaction_total_size_max * 0.25,
      transaction_total_size_max_1_2 = transaction_total_size_max * 0.5,
      transaction_total_size_max_3_4 = transaction_total_size_max * 0.75
    ) |>
    ungroup() |>
    pivot_longer(
      # each transaction size measure is moved onto a separate row
      # so that a separate edge with different thickness can be drawn on the plot
      cols=starts_with("transaction_"),
      values_to="count"
    ) |>
    select(-name) |>
    mutate(label="") |>
    rbind(count_edges) |>
    left_join(nodes |> select(from=id, from_sector_label=sector_label), by="from")
  edges
}

pathway_dendrogram <- function(layout, show_transaction_counts) {
  node_counts <- layout$nodes
  edges <- layout$edges
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
        colour=from_sector_label,
      ),
      alpha=0.1,
      arrow=arrow(ends="last", length=unit(0.2, "inches"))
    ) +
    geom_point(
      aes(
        label=label,
        fill=sector_label,
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
      title="Pathways Taken by Collections"
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

pathway_dendrogram_small <- function(layout) {
  node_counts <- layout$nodes
  edges <- layout$edges
  ggplot(node_counts, aes(x=x, y=y)) +
    geom_segment(
      data=edges,
      aes(
        x=x,
        y=y,
        xend=xend,
        yend=yend,
        linewidth=count,
        colour=from_sector_label,
      ),
      alpha=0.1,
      arrow=arrow(ends="last", length=unit(0.2, "inches"))
    ) +
    geom_point(
      aes(
        label=label,
        fill=sector_label,
        size=count
      ),
      pch=21,
      colour="black",
      alpha=0.7
    ) +
    geom_text(
      aes(label=count_label)
    ) +
    scale_x_continuous(expand=c(0.1, 0)) +
    scale_y_continuous(expand=c(0.1, 0)) +
    scale_size_continuous(range=c(5, 20)) +
    scale_linewidth(range=c(1,10)) +
    public_private_fill_scale +
    public_private_colour_scale +
    labs(
      title="Pathways Taken by Collections"
    ) +
    network_theme +
    theme(
      plot.title = element_text(size=14),
      legend.position="None",
      axis.title = element_blank(),
      axis.text = element_blank()
    )
}

sequence_network <- function(layout, start_position, end_position, show_transaction_counts) {
  node_counts <- layout$nodes
  edges <- layout$edges
  name_mapping <- layout$name_mapping
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
    scale_x_continuous(
      name="Actor",
      breaks=name_mapping$name_numeric,
      labels=str_replace_all(name_mapping$label, "_", " "),
      sec.axis=dup_axis(name="Actor")
    ) +
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

  transaction_sequence_plot |>
    ggplotly(tooltip=c("label", "count")) |>
    layout(
      showlegend=FALSE
    )
}
  
sequence_network_small <- function(layout, start_position, end_position) {
  nodes <- layout$nodes
  edges <- layout$edges
  ggplot(nodes, aes(x=name_numeric, y=position)) +
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
    scale_size_continuous(range=c(5, 20)) +
    scale_linewidth(range=c(1,10)) +
    public_private_fill_scale +
    public_private_colour_scale +
    labs(
      title="Sequential Transactions of Collections",
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
      plot.title = element_text(size=14),
      legend.position="None",
      axis.title = element_text(size=14),
      axis.text = element_blank()
    )
}

get_map_layout <- function(sequences,
                                   start_position,
                                   end_position,
                                   grouping_dimension,
                                   show_transaction_counts,
                                   steps_or_first_last) {
  grouping_dimension <- grouping_dimension_map[grouping_dimension]
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

  list("nodes"=nodes, "edges"=edges)
}

movements_map <- function(layout) {
  nodes <- layout$nodes
  edges <- layout$edges
  transaction_map_plot <- ggplot(nodes, aes(x=x, y=y)) +
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
      data=edges,
      aes(x=xend, y=yend, colour=sender_sector),
      pch=17,
      size=3
    ) +
    geom_point(
      aes(
        label=name,
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
      title="Pathways Taken by Collections"
    ) +
    standard_bars_theme +
    theme(
      plot.title = element_text(size=14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour="white"),
      axis.title = element_text(colour="white"),
      legend.position = "non"
    )

  transaction_map_plot |>
    ggplotly(tooltip=c("label")) |>
    layout(showlegend=FALSE)
}

movements_map_small <- function(layout) {
  nodes <- layout$nodes
  edges <- layout$edges
  ggplot(nodes, aes(x=x, y=y)) +
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
      title="Pathways Taken by Collections"
    ) +
    standard_bars_theme +
    theme(
      plot.title = element_text(size=14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour="white"),
      axis.title = element_text(colour="white"),
      legend.position = "non"
    )
}

get_movements_distances <- function(sequences,
                                    start_position,
                                    end_position,
                                    grouping_dimension,
                                    grouping_title,
                                    show_transaction_counts,
                                    steps_or_first_last,
                                    show_boxplot) {
  data <- sequences |>
    filter(
      sender_position >= start_position,
      recipient_position <= end_position
    ) |>
    mutate(
      all="all",
      label=paste(
        ifelse(is.na(collection_description), "", collection_description),
        "from:",
        sender_name,
        "to:",
        recipient_name
      )
    )
  data_2_way <- data |>
    group_by(.data[[grouping_dimension]], distance_category) |>
    summarize(
      count = n()
    ) |>
    ungroup() |>
    group_by(.data[[grouping_dimension]]) |>
    mutate(
      percentage_y = round(count / sum(count) * 100, 1)
    ) |>
    ungroup() |>
    group_by(distance_category) |>
    mutate(
      percentage_x = round(count / sum(count) * 100, 1)
    ) |>
    ungroup() |>
    mutate(
      percentage = round(count / sum(count) * 100, 1)
    )
  data_museum_totals <- data |>
    group_by(.data[[grouping_dimension]]) |>
    summarize(
      count = n()
    ) |>
    mutate(
      percentage_x = round(count / sum(count) * 100, 1)
    ) |>
    ungroup() |>
    mutate(
      distance_category = "all",
      percentage = round(count / sum(count) * 100, 1),
      percentage_y = 100
    )
  data_distance_totals <- data |>
    group_by(distance_category) |>
    summarize(
      count = n()
    ) |>
    mutate(
      percentage_y = round(count / sum(count) * 100, 1)
    ) |>
    ungroup() |>
    mutate(
      !!sym(grouping_dimension) := "all",
      percentage = round(count / sum(count) * 100, 1),
      percentage_x = 100
    )
  data_all_totals <- data |>
    summarize(
      count = n()
    ) |>
    mutate(
      !!sym(grouping_dimension) := "all",
      distance_category = "all",
      percentage = 100,
      percentage_x = 100,
      percentage_y = 100
    )
  data_2_way |>
    rbind(data_museum_totals) |>
    rbind(data_distance_totals) |>
    rbind(data_all_totals)
}

movements_heatmap <- function(jumps, grouping_dimension, grouping_title, count_or_percentage) {
  ggplot(
    jumps,
    aes(
      x=distance_category,
      y=factor(.data[[grouping_dimension]], museum_attribute_ordering)
    )
  ) +
    geom_tile(aes(fill=.data[[count_or_percentage]]), show.legend=FALSE) +
    geom_text(aes(label=.data[[count_or_percentage]])) +
    geom_hline(aes(yintercept=1.5), colour="black") +
    geom_vline(aes(xintercept=1.5), colour="black") +
    scale_y_discrete(labels=tidy_labels) +
    heatmap_fill_scale +
    labs(
      title = "Distances travelled by collections\n<sup>(Number of collections/objects)</sup>",
      x = "Distance travelled (km)",
      y = paste0("Origin museum (", grouping_title, ")")
    ) +
    standard_bars_theme +
    theme(
      axis.text.x = element_text(angle=45, hjust=1)
    )
}

movements_heatmap_small <- function(jumps, grouping_dimension, grouping_title) {
  ggplot(
    jumps,
    aes(
      x=distance_category,
      y=factor(.data[[grouping_dimension]], museum_attribute_ordering)
    )
  ) +
    geom_tile(aes(fill=count)) +
    geom_text(aes(label=count)) +
    geom_hline(aes(yintercept=1.5), colour="black") +
    geom_vline(aes(xintercept=1.5), colour="black") +
    scale_y_discrete(labels=tidy_labels) +
    heatmap_fill_scale +
    labs(
      title = "Distances travelled by collections",
      x = "Distance (km)",
      y = paste0("Origin museum (", grouping_title, ")")
    ) +
    standard_bars_theme +
    theme(
      plot.title = element_text(size=14),
      axis.title.x = element_text(size=14),
      axis.text.x = element_text(size=11, angle=45, hjust=1),
      axis.title.y = element_text(size=0),
      axis.text.y = element_text(size=11),
      legend.position = "non"
    )
}
