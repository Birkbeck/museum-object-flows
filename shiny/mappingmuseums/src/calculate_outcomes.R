get_outcomes_by_museum <- function(events_table) {
  events_with_numeric_collection_size <- events_table |>
    filter(event_stage_in_path == 1) |>
    mutate(
      collection_size=recode(
        collection_size,
        "all"=100,
        "most"=80,
        "half"=50,
        "some"=25,
        "few"=5
      ),
      destination_type=case_when(
        is.na(destination_latitude) ~ "unknown",
        origin_lad==destination_lad ~ "same LAD",
        origin_region==destination_region ~ "same region",
        origin_country==destination_country ~ "UK",
        TRUE ~ "abroad"
      )
    )
  event_outcomes <- get_outcomes_by_museum_for_type(
    events_with_numeric_collection_size, "event_core_type"
  ) |>
    select(museum_id=initial_museum_id, outcome_event_type=outcome)
  recipient_outcomes <- get_outcomes_by_museum_for_type(
    events_with_numeric_collection_size, "recipient_core_type"
  ) |>
    select(museum_id=initial_museum_id, outcome_recipient_type=outcome)
  destination_outcomes <- get_outcomes_by_museum_for_type(
    events_with_numeric_collection_size, "destination_type"
  ) |>
    select(museum_id=initial_museum_id, outcome_destination_type=outcome)
  event_outcomes |>
    left_join(recipient_outcomes, by="museum_id") |>
    left_join(destination_outcomes, by="museum_id")
}

get_outcomes_by_museum_for_type <- function(events_with_numeric_collection_size, event_attribute) {
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
  outcome_proportions <- rbind(outcomes_under_counted, outcomes_over_counted) |>
    group_by(initial_museum_id) |>
    mutate(total = sum(percent)) |>
    ungroup() |>
    pivot_wider(names_from=.data[[event_attribute]], values_from=percent) |>
    mutate(unknown=unknown + 100 - total) |>
    select(-total)
  if (event_attribute == "destination_type") {
    outcome_proportions |>
      mutate(
        `same region`=`same region` + `same LAD`,
        `UK`=`UK` + `same region`,
        outcome=case_when(
          `same LAD` > 50 ~ "mostly same LAD",
          `same region` > 50 ~ "mostly same region",
          `UK` > 50 ~ "mostly UK",
          `abroad` > 50 ~ "mostly abroad",
          `unknown` > 50 ~ "mostly unknown",
          TRUE ~ "mixed"
        )
      )
  }
  outcome_proportions |>
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
