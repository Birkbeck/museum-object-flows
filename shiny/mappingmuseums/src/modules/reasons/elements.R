closure_reasons_table <- function() {
  dispersal_events |>
    select(
      museum_id=initial_museum_id,
      museum_name=initial_museum_name,
      reason=super_event_cause_types,
      super_reasons=super_event_causes
    ) |>
    distinct() |>
    separate_rows(reason, sep = "; ") |>
    separate_wider_delim(
      reason,
      " - ",
      names=c("reason_core", "reason_core_or_child", "reason_specific"),
      too_few="align_start"
    ) |>
    mutate(
      reason_core_or_child=ifelse(
        is.na(reason_core_or_child),
        reason_core,
        paste(reason_core, "-", reason_core_or_child)
      ),
      reason_specific=ifelse(
        is.na(reason_specific),
        reason_core_or_child,
        paste(reason_core_or_child, "-", reason_specific)
      )
    )
}
 
closure_reason_types_counts_table <- function(closure_reasons,
                                              museums_table,
                                              size_filter,
                                              governance_filter,
                                              accreditation_filter,
                                              subject_filter,
                                              specific_subject_filter,
                                              region_filter) {
  closure_reasons |>
    left_join(museums_table, by="museum_id") |>
    filter(size %in% size_filter) |>
    filter(governance %in% governance_filter | governance_main %in% governance_filter) |>
    filter(accreditation %in% accreditation_filter) |>
    filter(main_subject %in% subject_filter) |>
    filter(subject_matter %in% specific_subject_filter) |>
    filter(region %in% region_filter | nation %in% region_filter) |>
    group_by(reason_core, reason_core_or_child, reason_specific) |>
    summarise(frequency = n())
}

museum_closure_reasons_table <- function(closure_reasons,
                                         museums_table,
                                         reason_level,
                                         reason_filter,
                                         size_filter,
                                         governance_filter,
                                         accreditation_filter,
                                         subject_filter,
                                         specific_subject_filter,
                                         region_filter) {
  closure_reasons |>
    group_by(museum_id, museum_name, super_reasons) |>
    left_join(museums_table, by="museum_id") |>
    mutate(
      year_opened = paste(year_opened_1, year_opened_2, sep=":"),
      year_closed = paste(year_closed_1, year_closed_2, sep=":")
    ) |>
    filter(.data[[reason_level]] %in% reason_filter) |>
    filter(size %in% size_filter) |>
    filter(governance %in% governance_filter | governance_main %in% governance_filter) |>
    filter(accreditation %in% accreditation_filter) |>
    filter(main_subject %in% subject_filter) |>
    filter(subject_matter %in% specific_subject_filter) |>
    filter(region %in% region_filter | nation %in% region_filter) |>
    select(
      museum_id,
      museum_name,
      year_opened,
      year_closed,
      super_reasons,
      size,
      governance,
      accreditation,
      subject_matter,
      region
    ) |>
    distinct()
}

closure_reasons_summary_table <- function(closure_reasons,
                                          museums_table,
                                          reason_level,
                                          reason_filter,
                                          size_filter,
                                          governance_filter,
                                          accreditation_filter,
                                          subject_filter,
                                          specific_subject_filter,
                                          region_filter) {
  closure_reasons <- closure_reasons |>
    left_join(museums_table, by="museum_id") |>
    filter(size %in% size_filter) |>
    filter(governance %in% governance_filter | governance_main %in% governance_filter) |>
    filter(accreditation %in% accreditation_filter) |>
    filter(main_subject %in% subject_filter) |>
    filter(subject_matter %in% specific_subject_filter) |>
    filter(region %in% region_filter | nation %in% region_filter)
  number_of_closed_museums <- closure_reasons |>
    select(museum_id) |>
    distinct() |>
    nrow()
  closure_reasons |>
    filter(!is.na(reason_core)) |>
    filter(reason_core %in% reason_filter) |>
    filter(!is.na(reason_core)) |>
    group_by(.data[[reason_level]]) |>
    summarize(
      frequency=n(),
      percentage=round(frequency / number_of_closed_museums * 100, 1)
    ) |>
    ungroup()
}

closure_reasons_two_way_summary_table <- function(closure_reasons,
                                                  museums_table,
                                                  reason_level,
                                                  reason_filter,
                                                  museum_grouping,
                                                  size_filter,
                                                  governance_filter,
                                                  accreditation_filter,
                                                  subject_filter,
                                                  specific_subject_filter,
                                                  region_filter) {
  closure_reasons <- closure_reasons |>
    left_join(museums_table, by="museum_id") |>
    filter(size %in% size_filter) |>
    filter(governance %in% governance_filter | governance_main %in% governance_filter) |>
    filter(accreditation %in% accreditation_filter) |>
    filter(main_subject %in% subject_filter) |>
    filter(subject_matter %in% specific_subject_filter) |>
    filter(region %in% region_filter | nation %in% region_filter)
  number_of_closed_museums <- closure_reasons |>
    select(museum_id) |>
    distinct() |>
    nrow()
  number_of_closed_museums_by_type <- closure_reasons |>
    select(museum_id, .data[[museum_grouping]]) |>
    distinct() |>
    group_by(.data[[museum_grouping]]) |>
    summarize(number_of_closures=n())
  closure_reasons |>
    filter(!is.na(reason_core)) |>
    filter(reason_core %in% reason_filter) |>
    group_by(.data[[museum_grouping]], .data[[reason_level]]) |>
    summarize(
      frequency=n(),
      percentage=round(frequency / number_of_closed_museums * 100, 1)
    ) |>
    ungroup() |>
    left_join(number_of_closed_museums_by_type, by=museum_grouping) |>
    group_by(.data[[museum_grouping]]) |>
    mutate(
      percentage_x=round(frequency / number_of_closures * 100, 1)
    ) |>
    ungroup() |>
    group_by(.data[[reason_level]]) |>
    mutate(
      percentage_y=round(frequency / sum(frequency) * 100, 1)
    ) |>
    ungroup()
}

closure_reasons_over_time_table <- function(closure_reasons,
                                            museums_table,
                                            reason_level,
                                            reason_filter,
                                            size_filter,
                                            governance_filter,
                                            accreditation_filter,
                                            subject_filter,
                                            specific_subject_filter,
                                            region_filter) {
  number_of_closures_by_time_period <- closure_reasons |>
    select(museum_id) |>
    distinct() |>
    left_join(museums_table, by="museum_id") |>
    rowwise() |>
    mutate(
      year_closed = mean(c(year_closed_1, year_closed_2)),
      period_of_closure = ifelse(
        year_closed > 1999 & year_closed < 2005,
        "2000-2004",
        ifelse(
          year_closed < 2010,
          "2005-2009",
          ifelse(
            year_closed < 2015,
            "2010-2014",
            ifelse(
              year_closed < 2020,
              "2015-2019",
              "2020-2024"
            )
          )
        )
      ),
      period_of_closure = factor(
        period_of_closure, 
        levels = c("2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2024"),
        ordered = TRUE
      )
    ) |>
    group_by(period_of_closure) |>
    summarize(number_of_closures_in_period=n()) |>
    ungroup()
  closure_reasons |>
    left_join(museums_table, by="museum_id") |>
    filter(reason_core %in% reason_filter) |>
    filter(size %in% size_filter) |>
    filter(governance %in% governance_filter | governance_main %in% governance_filter) |>
    filter(accreditation %in% accreditation_filter) |>
    filter(main_subject %in% subject_filter) |>
    filter(subject_matter %in% specific_subject_filter) |>
    filter(region %in% region_filter | nation %in% region_filter) |>
    filter(!is.na(year_closed_1) & !is.na(year_closed_2)) |>
    filter(year_closed_1 != 9999) |>
    filter(!is.na(reason_core)) |>
    rowwise() |>
    mutate(
      year_closed = mean(c(year_closed_1, year_closed_2)),
      period_of_closure = ifelse(
        year_closed > 1999 & year_closed < 2005,
        "2000-2004",
        ifelse(
          year_closed < 2010,
          "2005-2009",
          ifelse(
            year_closed < 2015,
            "2010-2014",
            ifelse(
              year_closed < 2020,
              "2015-2019",
              "2020-2024"
            )
          )
        )
      ),
      period_of_closure = factor(
        period_of_closure, 
        levels = c("2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2024"),
        ordered = TRUE
      )
    ) |>
    left_join(number_of_closures_by_time_period, by="period_of_closure") |>
    group_by(.data[[reason_level]], period_of_closure) |>
    summarize(
      count=n(),
      percentage=round(count / number_of_closures_in_period * 100, 1)
    ) |>
    ungroup() |>
    distinct()
}
        
closure_reasons_bar_chart <- function(summary_table,
                                      count_or_percentage,
                                      reason_level,
                                      reason_level_name) {
  if (count_or_percentage == "frequency") {
    x_title <- "Number of museum closures where reason is cited"
  } else {
    x_title <- "Percentage of museum closures where reason is cited"
  }
  if (reason_level == "reason_core") {
    use_theme <- standard_bars_theme
  } else {
    use_theme <- theme_minimal()
  }
  ggplot(summary_table, aes(x=.data[[count_or_percentage]], y=reorder(.data[[reason_level]], .data[[count_or_percentage]]))) +
    geom_col(fill=purple) +
    geom_text(aes(label=.data[[count_or_percentage]]), hjust="left", nudge_x=1, size=5) +
    labs(
      title="Reasons for Museum Closure, 2000-2024",
      y=reason_level_name,
      x=x_title
    ) +
    use_theme
}

closure_reasons_bar_chart_small <- function(summary_table, reason_level) {
  ggplot(summary_table, aes(x=frequency, y=reorder(.data[[reason_level]], frequency))) +
    geom_col(fill=purple) +
    geom_text(aes(label=frequency), hjust="left", nudge_x=1, size=3) +
    labs(
      title="Reasons for Museum Closure, 2000-2024",
      y="",
      x="Number of museums"
    ) +
    scale_y_discrete(labels = function(labels) sapply(labels, shorten_cause_labels)) +
    theme_minimal()+
    theme(
      axis.text.y = element_text(size=6),
    )
}

closure_reasons_heatmap <- function(summary_table,
                                    count_or_percentage,
                                    reason_level,
                                    reason_level_name,
                                    museum_grouping,
                                    museum_grouping_name) {
  if (reason_level == "reason_core") {
    use_theme <- standard_bars_theme
  } else {
    use_theme <- theme_minimal()
  }
  ggplot(
    summary_table,
    aes(
      x=fct_rev(factor(.data[[museum_grouping]], museum_attribute_ordering)),
      y=.data[[reason_level]],
      fill=.data[[count_or_percentage]]
    )
  ) +
    geom_tile(show.legend=FALSE) +
    geom_text(aes(label=.data[[count_or_percentage]]), size=5) +
    scale_x_discrete(labels=short_labels) +
    heatmap_fill_scale +
    labs(
      title=paste0("Reasons for Museum Closure by ", museum_grouping_name, " (Number of Closures)"),
      y=reason_level_name,
      x=museum_grouping_name
    ) +
    use_theme +
    theme(
      axis.text.x = element_text(angle=45, hjust=1, vjust=1)
    )
}

closure_reasons_heatmap_small <- function(summary_table,
                                          reason_level,
                                          reason_level_name,
                                          museum_grouping,
                                          museum_grouping_name) {
  ggplot(
    summary_table,
    aes(
      x=fct_rev(factor(.data[[museum_grouping]], museum_attribute_ordering)),
      y=.data[[reason_level]],
      fill=frequency
    )
  ) +
    geom_tile(show.legend=FALSE) +
    scale_x_discrete(labels=short_labels) +
    scale_y_discrete(labels = function(labels) sapply(labels, shorten_cause_labels)) +
    scale_fill_continuous(low="white", high=purple) +
    labs(
      title=paste0("Reasons vs ", museum_grouping_name),
      y=reason_level_name,
      x=museum_grouping_name
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size=6),
      axis.text.x = element_text(angle=45, hjust=1, vjust=1)
    )
}

closure_reasons_over_time <- function(reasons_over_time_table,
                                      count_or_percentage,
                                      reason_level) {
  if (count_or_percentage == "count") {
    y_title <- "Number of museum closures where reason is cited"
  } else {
    y_title <- "Percentage of closures where reason is cited"
  }
  ggplot(
    reasons_over_time_table, 
    aes(x=period_of_closure, y=.data[[count_or_percentage]], colour=.data[[reason_level]])
  ) +
    geom_line(alpha=0.7, aes(group=.data[[reason_level]])) +
    geom_point() +
    geom_text(
      data=reasons_over_time_table |> filter(period_of_closure=="2010-2014"),
      position=position_jitter(width=1, height=1, seed=1),
      aes(label=.data[[reason_level]], colour=.data[[reason_level]])
    ) +
    guides(
      colour="none"
    ) +
    labs(
      title="Changing Reasons for Museum Closure Over Time",
      x="Year of Closure",
      y=y_title,
      colour="Reason for closure"
    ) +
    standard_bars_theme
}

closure_reasons_over_time_small <- function(reasons_over_time_table, reason_level) {
  ggplot(
    reasons_over_time_table, 
    aes(x=period_of_closure, y=count, colour=.data[[reason_level]])
  ) +
    geom_line(alpha=0.7, aes(group=.data[[reason_level]])) +
    geom_point() +
    geom_text(
      data=reasons_over_time_table |> filter(period_of_closure=="2010-2014"),
      position=position_jitter(width=1, height=1, seed=1),
      aes(label=sapply(.data[[reason_level]], shorten_cause_labels), colour=.data[[reason_level]])
    ) +
    guides(
      colour="none"
    ) +
    labs(
      title="Changing Reasons for Museum Closure Over Time",
      x="Year of Closure",
      y="Number of museum closures where reason is cited",
      colour="Reason for closure"
    ) +
    theme_minimal()
}

closure_causes_hierarchy_layout <- function(closure_causes, reason_level, reason_filter) {
  closure_causes <- closure_causes |>
    filter(cause_super_type %in% reason_filter)
  causes_1 <- closure_causes |>
    group_by(cause_super_type) |>
    summarize(frequency = sum(frequency)) |>
    ungroup() |>
    mutate(
      from = "cause of closure",
      to = cause_super_type,
      label = paste0(cause_super_type, " (", frequency, ")")
    ) |>
    select(from, to, label)
  causes_2 <- closure_causes |>
    filter(!grepl("- other", cause_type)) |>
    group_by(cause_type, cause_super_type) |>
    summarize(frequency = sum(frequency)) |>
    ungroup() |>
    mutate(
      from = cause_super_type,
      to = cause_type,
      label = sapply(strsplit(cause_type, " - "), function(x) tail(x, n = 1)),
      label = paste0(label, " (", frequency, ")")
    ) |>
    select(from, to, label)
  causes_3 <- closure_causes |>
    filter(!grepl("- other", cause)) |>
    group_by(cause, cause_type, cause_super_type) |>
    summarize(frequency = sum(frequency)) |>
    ungroup() |>
    mutate(
      from = cause_type,
      to = cause,
      label = sapply(strsplit(cause, " - "), function(x) tail(x, n = 1)),
      label = paste0(label, " (", frequency, ")")
    ) |>
    select(from, to, label)
  super_types_with_types <- closure_causes |>
    ungroup() |>
    filter(!grepl("- other", cause_type)) |>
    select(cause_super_type) |>
    distinct()
  types_with_causes <- closure_causes |>
    ungroup() |>
    filter(!grepl("- other", cause)) |>
    select(cause_type) |>
    distinct()
  counter <- 1
  for (i in 1:nrow(super_types_with_types)) {
    new_row_1 <- data.frame(
      from=super_types_with_types$cause_super_type[i],
      to=as.character(counter),
      label=""
    )
    new_row_2 <- data.frame(
      from=super_types_with_types$cause_super_type[i],
      to=paste("z", as.character(counter)),
      label=""
    )
    counter <- counter + 1
    causes_2 <- causes_2 |>
      rbind(new_row_1) |>
      rbind(new_row_2)
  }
  for (i in 1:nrow(types_with_causes)) {
    new_row_1 <- data.frame(
      from=types_with_causes$cause_type[i],
      to=as.character(counter),
      label=""
    )
    new_row_2 <- data.frame(
      from=types_with_causes$cause_type[i],
      to=paste("z", as.character(counter)),
      label=""
    )
    counter <- counter + 1
    causes_3 <- causes_3 |>
      rbind(new_row_1) |>
      rbind(new_row_2)
  }
  if(reason_level == "cause_super_type") {
    causes_tree <- causes_1 |>
      filter(to != "") |>
      filter(!is.na(from)) |>
      arrange(from, to)
  } else if(reason_level == "cause_type") {
    causes_tree <- rbind(causes_1, causes_2) |>
      filter(to != "") |>
      filter(!is.na(from)) |>
      arrange(from, to)
  } else {
    causes_tree <- rbind(causes_1, causes_2) |>
      rbind(causes_3) |>
      filter(to != "") |>
      filter(!is.na(from)) |>
      arrange(from, to)
  }
  cause_instance_labels <- causes_tree |>
    mutate(name=to) |>
    select(name, label)
  graph <- graph_from_data_frame(causes_tree, directed=TRUE)
  V(graph)$distance_to_root <- distances(
    graph, v=V(graph), to=which(V(graph)$name == "cause of closure")
  )
  max_distance <- max(V(graph)$distance_to_root)
  layout <- create_layout(graph, layout="dendrogram", circular=FALSE)
  layout$y <- layout$distance_to_root - max_distance
  layout |> left_join(cause_instance_labels, by="name")
}

closure_type_hierarchy <- function(closure_causes_hierarchy_layout, reason_level) {
  taxonomy_theme <- theme(
    panel.background = element_rect(fill="white"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_text(size="18"),
    legend.position = "bottom",
    legend.title = element_text(size="14"),
    legend.text = element_text(size="12"),
    legend.background = element_rect(fill="white"),
    legend.key = element_rect(fill="white")
  )
  max_distance <- max(closure_causes_hierarchy_layout$distance_to_root)
  ggraph(closure_causes_hierarchy_layout) + 
    geom_edge_diagonal(
      aes(colour=ifelse(label=="", "dummy", "normal")),
      show.legend=FALSE
    ) +
    geom_node_point(
      data=closure_causes_hierarchy_layout |> filter(label!=""),
      alpha=0.7,
      size=2,
      show.legend=FALSE
    ) +
    geom_node_text(
      aes(label=label),
      size=ifelse(reason_level=="cause", 4, 5),
      angle=0,
      vjust="center",
      hjust="left",
      nudge_y=0.02
    ) +
    coord_flip() +
    scale_y_continuous(limits=c(-max_distance, 1)) +
    scale_edge_colour_manual(
      values=c("dummy"="white", "normal"="lightgrey")
    ) +
    labs(
      title="Hierarchy of reasons for museum closure"
    ) +
    taxonomy_theme
}

closure_type_hierarchy_small <- function(closure_causes_hierarchy_layout) {
  taxonomy_theme <- theme(
    panel.background = element_rect(fill="white"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_text(size="11"),
    legend.position = "bottom",
  )
  max_distance <- max(closure_causes_hierarchy_layout$distance_to_root)
  ggraph(closure_causes_hierarchy_layout) + 
    geom_edge_diagonal(colour="lightgrey") +
    geom_node_point(
      alpha=0.7,
      size=2
    ) +
    coord_flip() +
    scale_y_continuous(limits=c(-max_distance, 1)) +
    labs(
      title="Hierarchy of reasons for closure"
    ) +
    taxonomy_theme
}

shorten_cause_labels <- function(label) {
  split_label <- strsplit(label, " - ")[[1]]
    if (length(split_label) > 1) {
      paste(substr(split_label[1], 1, 7), substr(split_label[length(split_label)], 1, 7), sep = " ... ")
    } else {
      substr(label, 1, 7)
    }
  }

