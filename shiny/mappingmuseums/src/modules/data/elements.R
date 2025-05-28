events_per_museum <- function() {
  summary <- dispersal_events |>
    group_by(initial_museum_id, initial_museum_name) |>
    summarize(
      number_of_events = n(),
      number_of_collections = n_distinct(collection_id)
    )
  ggplot(summary, aes(x=number_of_events, y=number_of_collections, label=initial_museum_name)) +
    geom_point(position=position_jitter(width=0.5, height=0.5, seed=1)) +
    labs(
      title="Quantity of Data Collected for Each Museum",
      x="Number of events recorded",
      y="Number of collections/objects recorded"
    ) +
    standard_bars_theme
}

collection_granularity_bars <- function() {
  summary <- dispersal_events |>
    select(collection_id, collection_size) |>
    distinct() |>
    filter(!is.na(collection_size)) |>
    group_by(collection_size) |>
    summarize(
      number_of_collections = n()
    ) |>
    ungroup()
  ggplot(summary, aes(x=number_of_collections, y=collection_size)) +
    geom_col() +
    scale_y_discrete(limits=c("few", "some", "half", "most", "all")) +
    labs(
      title="Granularity of Collection Data",
      x="Number of collections in size category",
      y="Collection size category"
    ) +
    standard_bars_theme
}
