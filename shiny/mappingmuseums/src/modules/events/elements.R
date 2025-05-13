filter_events <- function(events,
                          event_grouping,
                          sender_grouping,
                          recipient_grouping,
                          only_show_last_event,
                          stages_in_path,
                          event_filter,
                          sender_filter,
                          recipient_filter,
                          collection_status_filter,
                          size_filter,
                          governance_filter,
                          subject_broad_filter,
                          subject_specific_filter,
                          region_filter,
                          accreditation_filter) {
  if (only_show_last_event) {
    events <- events |>
      group_by(collection_id) |>
      filter(event_stage_in_path == max(event_stage_in_path)) |>
      ungroup()
  } else {
    events <- events |>
      filter(event_stage_in_path %in% stages_in_path)
  }
  events |>
    filter(
      .data[[event_grouping]] %in% event_filter,
      .data[[sender_grouping]] %in% sender_filter,
      .data[[recipient_grouping]] %in% recipient_filter,
      collection_status %in% collection_status_filter,
      initial_museum_size %in% size_filter, 
      initial_museum_governance %in% governance_filter
      | initial_museum_governance_broad %in% governance_filter,
      initial_museum_subject_matter_broad %in% subject_broad_filter,
      initial_museum_subject_matter %in% subject_specific_filter,
      initial_museum_region %in% region_filter
      | initial_museum_country %in% region_filter,
      initial_museum_accreditation %in% accreditation_filter
    )
}

summarize_events <- function(events, dimension_1, dimension_2) {
  events <- events |>
    mutate(
      dimension_1=.data[[dimension_1]],
      dimension_2=.data[[dimension_2]]
    )
  events_2_way <- events |>
    group_by(dimension_1, dimension_2) |>
    summarize(count=n()) |>
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
    summarize(count=n()) |>
    ungroup() |>
    mutate(
      dimension_2 = "All",
      percentage=round(count / sum(count) * 100, 1),
      percentage_rowwise=round(count / sum(count) * 100, 1),
      percentage_columnwise=100
    )
  dimension_2_totals <- events |>
    group_by(dimension_2) |>
    summarize(count=n()) |>
    ungroup() |>
    mutate(
      dimension_1 = "All",
      percentage=round(count / sum(count) * 100, 1),
      percentage_rowwise=100,
      percentage_columnwise=round(count / sum(count) * 100, 1)
    )
  all_totals <- events |>
    summarize(count=n()) |>
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
