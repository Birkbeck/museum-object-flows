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
      number_of_events_category = case_when(
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
      number_of_collections_category = case_when(
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
      number_of_events_category,
      number_of_collections,
      number_of_collections_category
    )

  museums |>
    filter(
      year_closed_1 > 1999 & year_closed_1 < 9999
    ) |>
    left_join(event_counts, by="museum_id") |>
    mutate(
      number_of_events=ifelse(
        is.na(number_of_events), 0, number_of_events
      ),
      number_of_events_category=ifelse(
        is.na(number_of_events_category), "0", number_of_events_category
      ),
      number_of_collections=ifelse(
        is.na(number_of_collections), 0, number_of_collections
      ),
      number_of_collections_category=ifelse(
        is.na(number_of_collections_category), "0", number_of_collections_category
      ),
      number_of_events_category = factor(
        number_of_events_category, item_count_ordering
      ),
      number_of_collections_category = factor(
        number_of_collections_category, item_count_ordering
      )
    )
}

events_per_museum_matrix <- function(data_by_museum) {
  summary <- data_by_museum |>
    group_by(number_of_events_category, number_of_collections_category) |>
    summarize(
      count=n()
    ) |>
    ungroup() |>
    mutate(
      percentage=round(count / sum(count) * 100, 1)
    ) |>
    select(
      number_of_events_category,
      number_of_collections_category,
      count,
      percentage
    )

  ggplot(summary, aes(x=number_of_events_category, y=number_of_collections_category, fill=count)) +
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

events_per_museum_boxplots <- function(data_by_museum) {
  ggplot(data_by_museum, aes(x=subject_broad, y=number_of_events)) +
    geom_boxplot() +
    geom_text(
      data=data_by_museum |> filter(number_of_events >= 20),
      aes(label=museum_name),
      position=position_jitter(width=0.8, height=0.1, seed=1)
    ) +
    coord_flip() +
    labs(
      title="Events per Museum by Subject Matter",
      x="Subject matter of museum",
      y="Number of events recorded"
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

events_per_collection <- function() {
  summary <- dispersal_events |>
    select(
      museum_id=initial_museum_id,
      museum_name=initial_museum_name,
      subject_broad=initial_museum_subject_broad,
      collection_id,
      event_stage_in_path
    ) |>
    group_by(museum_id, museum_name, collection_id, subject_broad) |>
    summarize(sequence_length = max(event_stage_in_path)) |>
    ungroup() |>
    group_by(museum_id, museum_name, subject_broad) |>
    summarize(events_per_collection=mean(sequence_length)) |>
    ungroup()
  ggplot(summary, aes(x=subject_broad, y=events_per_collection)) +
    geom_boxplot() +
    geom_text(
      data=summary |> filter(events_per_collection >= 3),
      aes(label=museum_name),
      position=position_jitter(width=0.8, height=0.1, seed=1)
    ) +
    coord_flip() +
    labs(
      title="Events per Collection by Subject Matter",
      x="Subject matter of museum",
      y="Events per collection"
    ) +
    standard_bars_theme
}

collection_distribution_bars <- function() {
  summary <- dispersal_events |>
    mutate(
      collection_size = case_when(
        is.na(collection_quantity) ~ collection_size,
        collection_quantity == 1 ~ "1",
        collection_quantity < 11 ~ "2-10",
        collection_quantity < 101 ~ "11-100",
        collection_quantity < 1001 ~ "101-1,000",
        collection_quantity < 10001 ~ "1,001-10,000",
        collection_quantity < 100001 ~ "10,001-100,000"
      )
    ) |>
    select(collection_id, collection_size) |>
    distinct() |>
    filter(!is.na(collection_size)) |>
    group_by(collection_size) |>
    summarize(
      number_of_collections = n()
    ) |>
    ungroup() |>
    mutate(
      percentage_of_collections = round(number_of_collections / sum(number_of_collections) * 100, 1)
    )
  ggplot(summary, aes(x=number_of_collections, y=collection_size)) +
    geom_col(fill="lightblue") +
    geom_text(
      aes(label=paste0(number_of_collections, "\n(", percentage_of_collections, "%)"))
    ) +
    scale_y_discrete(
      limits=c(
        "1",
        "2-10",
        "11-100",
        "101-1,000",
        "1,001-10,000",
        "10,001-100,000",
        "few",
        "some",
        "half",
        "most",
        "all"
      )
    ) +
    labs(
      title="Distribution of Collection Sizes",
      y="Collection size quantity/category",
      x="Number of collections in size category"
    ) +
    standard_bars_theme
}

collection_distribution_heatmap <- function() {
  museums_in_subject <- dispersal_events |>
    group_by(initial_museum_subject_broad) |>
    summarize(number_of_museums_in_subject=n_distinct(initial_museum_id)) |>
    ungroup()
  summary <- dispersal_events |>
    mutate(
      collection_size = case_when(
        is.na(collection_quantity) ~ collection_size,
        collection_quantity == 1 ~ "1",
        collection_quantity < 11 ~ "2-10",
        collection_quantity < 101 ~ "11-100",
        collection_quantity < 1001 ~ "101-1,000",
        collection_quantity < 10001 ~ "1,001-10,000",
        collection_quantity < 100001 ~ "10,001-100,000"
      )
    ) |>
    select(initial_museum_subject_broad, collection_id, collection_size) |>
    distinct() |>
    filter(!is.na(collection_size)) |>
    group_by(initial_museum_subject_broad, collection_size) |>
    summarize(
      count = n()
    ) |>
    ungroup() |>
    left_join(museums_in_subject, by="initial_museum_subject_broad") |>
    mutate(
      count_per_museum = round(count / number_of_museums_in_subject, 1)
    )
  ggplot(summary, aes(y=initial_museum_subject_broad, x=collection_size, fill=count_per_museum)) +
    geom_tile(show.legend=FALSE) +
    geom_text(aes(label=count_per_museum)) +
    geom_vline(xintercept=6.5) +
    scale_x_discrete(
      limits=c(
        "1",
        "2-10",
        "11-100",
        "101-1,000",
        "1,001-10,000",
        "10,001-100,000",
        "few",
        "some",
        "half",
        "most",
        "all"
      )
    ) +
    heatmap_fill_scale +
    labs(
      title="Distribution of Collection Sizes (collections per museum)",
      y="Subject matter of museum",
      x="Collection size quantity/category"
    ) +
    standard_bars_theme +
    theme(
      axis.text.x = element_text(angle=30)
    )
}
