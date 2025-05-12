summarize_events <- function(events, dimension_1, dimension_2) {
  events |>
    mutate(
      dimension_1=.data[[dimension_1]],
      dimension_2=.data[[dimension_2]]
    ) |>
    group_by(dimension_1, dimension_2) |>
    summarize(
      count=n()
    ) |>
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
}

event_heatmap <- function(table, x_label, y_label, count_or_percentage) {
  ggplot(
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
}
