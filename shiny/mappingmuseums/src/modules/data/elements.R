item_count_ordering <- c(
  "0",
  "1",
  "2",
  "3-4",
  "5-8",
  "9-16",
  "17-32",
  "33-64",
  "65+"
)

get_data_by_museum <- function(events, museums) {
  event_counts <- events |>
    group_by(initial_museum_id) |>
    summarize(
      number_of_events = n_distinct(event_id),
      number_of_collections = n_distinct(collection_id)
    ) |>
    ungroup() |>
    mutate(
      number_of_events = case_when(
        number_of_events == 0 ~ "0",
        number_of_events == 1 ~ "1",
        number_of_events == 2 ~ "2",
        number_of_events < 5 ~ "3-4",
        number_of_events < 9 ~ "5-8",
        number_of_events < 17 ~ "9-16",
        number_of_events < 33 ~ "17-32",
        number_of_events < 65 ~ "33-64",
        TRUE ~ "65+"
      ),
      number_of_collections = case_when(
        number_of_collections == 0 ~ "0",
        number_of_collections == 1 ~ "1",
        number_of_collections == 2 ~ "2",
        number_of_collections < 5 ~ "3-4",
        number_of_collections < 9 ~ "5-8",
        number_of_collections < 17 ~ "9-16",
        number_of_collections < 33 ~ "17-32",
        number_of_collections < 65 ~ "33-64",
        TRUE ~ "65+"
      )
    ) |>
    select(
      museum_id=initial_museum_id,
      number_of_events,
      number_of_collections
    )

  museums |>
    filter(
      year_closed_1 > 1999 & year_closed_1 < 9999
    ) |>
    left_join(event_counts, by="museum_id") |>
    mutate(
      number_of_events=ifelse(is.na(number_of_events), 0, number_of_events),
      number_of_collections=ifelse(is.na(number_of_collections), 0, number_of_collections),
      number_of_events = factor(number_of_events, item_count_ordering),
      number_of_collections = factor(number_of_collections, item_count_ordering)
    )
}

events_per_museum_matrix <- function(data_by_museum) {
  summary <- data_by_museum |>
    group_by(number_of_events, number_of_collections) |>
    summarize(
      count=n()
    ) |>
    ungroup() |>
    mutate(
      percentage=round(count / sum(count) * 100, 1)
    ) |>
    select(
      number_of_events,
      number_of_collections,
      count,
      percentage
    )

  ggplot(summary, aes(x=number_of_events, y=number_of_collections, fill=count)) +
    geom_tile(show.legend=FALSE) +
    geom_text(aes(label=paste0(count, " (", percentage, "%)"))) +
    heatmap_fill_scale +
    labs(
      title="Granularity of Data Collected for Each Museum",
      x="Number of events recorded",
      y="Number of collections/objects recorded"
    ) +
    standard_bars_theme
}

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
      title="Granularity of Data Collected for Each Museum",
      x="Number of events recorded",
      y="Number of collections/objects recorded"
    ) +
    standard_bars_theme
}

collection_distribution_bars <- function() {
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
      title="Distribution of Collection Sizes",
      x="Number of collections in size category",
      y="Collection size category"
    ) +
    standard_bars_theme
}
