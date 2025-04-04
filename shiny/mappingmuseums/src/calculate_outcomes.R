get_outcomes_by_museum <- function(events_table) {
  event_outcomes <- get_outcomes_by_museum_for_type(events_table, "event_core_type") |>
    select(museum_id=initial_museum_id, outcome_main_event=outcome)
  recipient_outcomes <- get_outcomes_by_museum_for_type(events_table, "recipient_core_type") |>
    select(museum_id=initial_museum_id, outcome_main_recipient=outcome)
  left_join(event_outcomes, recipient_outcomes, by="museum_id")
}

get_outcomes_by_museum_for_type <- function(events_table, event_attribute) {
  events_with_numeric_collection_size <- events_table |>
    filter(event_stage_in_path == 0) |>
    mutate(
      collection_size=recode(
        collection_size,
        "all"=100,
        "most"=80,
        "half"=50,
        "some"=25,
        "few"=5
      )
    )
  collection_totals <- events_with_numeric_collection_size |>
    group_by(initial_museum_id) |>
    summarize(
      total=sum(collection_size)
    ) |>
    ungroup()
  outcomes_under_counted <- events_with_numeric_collection_size |>
    left_join(collection_totals, by="initial_museum_id") |>
    filter(total <= 100) |>
    group_by(initial_museum_id, .data[[event_attribute]]) |>
    summarize(
      percent=sum(collection_size)
    ) |>
    ungroup()
  outcomes_over_counted <- events_with_numeric_collection_size |>
    left_join(collection_totals, by="initial_museum_id") |>
    filter(total > 100) |>
    group_by(initial_museum_id, .data[[event_attribute]]) |>
    summarize(
      total=sum(collection_size)
    ) |>
    ungroup() |>
    group_by(initial_museum_id) |>
    mutate(
      percent=total / sum(total) * 100
    ) |>
    ungroup() |>
    select(initial_museum_id, .data[[event_attribute]], percent)
  rbind(outcomes_under_counted, outcomes_over_counted) |>
    group_by(initial_museum_id) |>
    mutate(total = sum(percent)) |>
    ungroup() |>
    pivot_wider(names_from=.data[[event_attribute]], values_from=percent) |>
    mutate(unknown=unknown + 100 - total) |>
    select(-total) |>
    pivot_longer(
      cols=c(-initial_museum_id),
      names_to="category",
      values_to="percent"
    ) |>
    group_by(initial_museum_id) |>
    summarize(
      outcome = {
        over_50 <- category[!is.na(percent) & percent > 50]
        if (length(over_50) == 1) {
          paste("mostly", over_50)
        } else {
          "mixed"
        }
      },
      ) |>
    ungroup()
}
