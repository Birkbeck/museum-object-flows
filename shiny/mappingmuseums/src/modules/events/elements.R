events_table_choices <- c(
  "event_stage_in_path",
  "event_date",
  "event_date_from",
  "event_date_to",
  "event_type",
  "event_core_type",
  "event_type_uncertainty",
  "event_is_change_of_custody",
  "event_is_change_of_ownership",
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

events_table_selected <- c(
  "event_stage_in_path",
  "event_date",
  "event_date_from",
  "event_date_to",
  "event_type",
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

filter_events <- function(events,
                          event_grouping,
                          sender_grouping,
                          recipient_grouping,
                          museum_grouping,
                          only_show_last_event,
                          stages_in_path,
                          event_filter,
                          sender_filter,
                          recipient_filter,
                          collection_type_filter,
                          collection_status_filter,
                          size_filter,
                          governance_filter,
                          subject_broad_filter,
                          subject_specific_filter,
                          region_filter,
                          accreditation_filter) {
  event_collection_types <- events |>
    mutate(
      collection_type = str_remove_all(collection_types, "\\[|\\]|'") |>
        str_split(",\\s*")
    ) |>
    unnest(collection_type) |>
    select(event_id, collection_type) |>
    filter(collection_type %in% collection_type_filter)
  if (only_show_last_event) {
    events <- events |>
      group_by(collection_id) |>
      filter(event_stage_in_path == max(event_stage_in_path)) |>
      ungroup()
  } else {
    events <- events |>
      filter(event_stage_in_path %in% stages_in_path)
  }
  events <- events |>
    filter(
      .data[[event_grouping]] %in% event_filter,
      event_id %in% event_collection_types$event_id,
      collection_status %in% collection_status_filter,
      initial_museum_size %in% size_filter, 
      initial_museum_governance_broad %in% governance_filter,
      initial_museum_subject_broad %in% subject_broad_filter,
      initial_museum_subject %in% subject_specific_filter,
      initial_museum_region %in% region_filter,
      initial_museum_accreditation %in% accreditation_filter
    )
  if (museum_grouping != "_all") {
    sender_museum_type <- paste0("sender", museum_grouping)
    recipient_museum_type <- paste0("recipient", museum_grouping)
    events <- events |>
      mutate(
        !!sym(sender_grouping):=ifelse(
          is.na(.data[[sender_museum_type]]),
          .data[[sender_grouping]],
          paste0("museum (", .data[[sender_museum_type]], ")")
        ),
        !!sym(recipient_grouping):=ifelse(
          is.na(.data[[recipient_museum_type]]),
          .data[[recipient_grouping]],
          paste0("museum (", .data[[recipient_museum_type]], ")")
        )
      )
  }
  events |>
    filter(
      .data[[sender_grouping]] %in% sender_filter,
      .data[[recipient_grouping]] %in% recipient_filter
    )
}

summarize_events <- function(events, dimension_1, dimension_2) {
  if ("collection_type" %in% c(dimension_1, dimension_2)) {
    events <- events |>
      mutate(
        collection_type = str_remove_all(collection_types, "\\[|\\]|'") |>
          str_split(",\\s*")
      ) |>
      unnest(collection_type)
  }
  events <- events |>
    mutate(
      dimension_1=.data[[dimension_1]],
      dimension_2=.data[[dimension_2]]
    )
  events_2_way <- events |>
    group_by(dimension_1, dimension_2) |>
    summarize(count=n_distinct(event_id)) |>
    ungroup() |>
    mutate(
      percentage=round(count / sum(count) * 100, 1)
    ) |>
    group_by(dimension_2) |>
    mutate(
      percentage_rowwise=round(count / sum(count) * 100, 1)
    ) |>
    ungroup() |>
    group_by(dimension_1) |>
    mutate(
      percentage_columnwise=round(count / sum(count) * 100, 1)
    )
  dimension_1_totals <- events |>
    group_by(dimension_1) |>
    summarize(count=n_distinct(event_id)) |>
    ungroup() |>
    mutate(
      dimension_2 = "All",
      percentage=round(count / sum(count) * 100, 1),
      percentage_rowwise=round(count / sum(count) * 100, 1),
      percentage_columnwise=100
    )
  dimension_2_totals <- events |>
    group_by(dimension_2) |>
    summarize(count=n_distinct(event_id)) |>
    ungroup() |>
    mutate(
      dimension_1 = "All",
      percentage=round(count / sum(count) * 100, 1),
      percentage_rowwise=100,
      percentage_columnwise=round(count / sum(count) * 100, 1)
    )
  all_totals <- events |>
    summarize(count=n_distinct(event_id)) |>
    mutate(
      dimension_1 = "All",
      dimension_2 = "All",
      percentage=100,
      percentage_rowwise=100,
      percentage_columnwise=100
    )
  events_2_way |>
    rbind(dimension_1_totals) |>
    rbind(dimension_2_totals) |>
    rbind(all_totals)
}

event_heatmap <- function(table, x_label, y_label, count_or_percentage) {
  plot <- ggplot(
    table,
    aes(
      x=dimension_1,
      y=dimension_2,
      fill=.data[[count_or_percentage]]
    )
  ) +
    geom_tile(show.legend=FALSE) +
    geom_text(aes(label=.data[[count_or_percentage]]), size=5) +
    heatmap_fill_scale +
    labs(
      title="",
      x=x_label,
      y=y_label
    ) +
    standard_bars_theme +
    theme(
      axis.text.x=element_text(angle=45, hjust=1, vjust=1)
    )

  if (count_or_percentage == "percentage_rowwise") {
    y_lines <- data.frame(
      y=seq_along(
        unique(select(table, dimension_1))$dimension_1
      )
    ) |>
      mutate(y=y+0.5)
    plot <- plot + geom_hline(data=y_lines, aes(yintercept=y), colour="white")
  } else if (count_or_percentage == "percentage_columnwise") {
    x_lines <- data.frame(
      x=seq_along(
        unique(select(table, dimension_2))$dimension_2
      )
    ) |>
      mutate(x=x+0.5)
    plot <- plot + geom_vline(data=x_lines, aes(xintercept=x), colour="white")
  }

  plot <- plot +
    geom_hline(yintercept=1.5) +
    geom_vline(xintercept=1.5)
}
