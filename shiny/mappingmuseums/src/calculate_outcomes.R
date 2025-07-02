sizes_mapping <- c(
  "all"=100,
  "most"=80,
  "half"=50,
  "some"=25,
  "few"=5,
  "NA"=1
)
non_outcome_events <- c(
  "recorded"
)

get_outcomes_by_museum <- function(events_table) {
  initial_events <- events_table |>
    left_join(
      events_table |> select(previous_event_id=event_id, previous_event_type=event_type),
      by="previous_event_id"
    ) |>
    filter(
      (event_stage_in_path == 1 & !event_type %in% non_outcome_events)
      | (event_stage_in_path == 2 & previous_event_type %in% non_outcome_events)
    )
  events_with_numeric_collection_size <- initial_events |>
    mutate(
      collection_size=recode(
        collection_size,
        "all"=100,
        "most"=80,
        "half"=50,
        "some"=25,
        "few"=5,
        "NA"=1 # TODO: doesn't work, check why some have NA size
      ),
      destination_type=case_when(
        recipient_type=="end of existence" ~ "end of existence",
        recipient_type=="N/A" ~ "no move",
        is.na(destination_latitude) ~ "unknown",
        origin_lad==destination_lad ~ "within the same LAD",
        origin_region==destination_region ~ "within the same region",
        origin_country==destination_country ~ "within the UK",
        TRUE ~ "abroad"
      )
    )
  event_outcomes <- get_outcomes_by_museum_for_type(
    events_with_numeric_collection_size, "event_core_type", "unknown"
  ) |>
    select(museum_id=initial_museum_id, outcome_event_type=outcome)
  recipient_outcomes <- get_outcomes_by_museum_for_type(
    events_with_numeric_collection_size, "recipient_core_type", "unspecified actor"
  ) |>
    select(museum_id=initial_museum_id, outcome_recipient_type=outcome)
  destination_outcomes <- get_outcomes_by_museum_for_type(
    events_with_numeric_collection_size, "destination_type", "unknown"
  ) |>
    select(museum_id=initial_museum_id, outcome_destination_type=outcome)
  recipient_counts <- events_with_numeric_collection_size |>
    select(museum_id=initial_museum_id, recipient_id, recipient_quantity) |>
    unique() |>
    filter(!is.na(recipient_id)) |>
    mutate(
      recipient_quantity=ifelse(
        recipient_quantity=="many", NaN, as.numeric(recipient_quantity)
      )
    ) |>
    group_by(museum_id) |>
    summarize(
      outcome_recipient_count=sum(recipient_quantity)
    ) |>
    ungroup()
  largest_recipient_shares <- calculate_largest_recipient_shares(initial_events)
  event_outcomes |>
    left_join(recipient_outcomes, by="museum_id") |>
    left_join(destination_outcomes, by="museum_id") |>
    left_join(recipient_counts, by="museum_id") |>
    left_join(largest_recipient_shares, by="museum_id") |>
    mutate(
      outcome_recipient_count=case_when(
        is.nan(outcome_recipient_count) ~ "many",
        is.na(outcome_recipient_count) ~ "0",
        outcome_recipient_count > 5 ~ "> 5",
        TRUE ~ as.character(outcome_recipient_count)
      )
    )
}

calculate_largest_recipient_shares <- function(events) {
  categorical_max <- function(sizes) {
    largest <- "NA"
    for (size in sizes) {
      print(size)
      print(sizes_mapping[size])
      if (is.na(size)) {
        next
      }
      if (sizes_mapping[size] > sizes_mapping[largest]) {
        largest <- size
      }
    }
    return(largest)
  }
  events |>
    group_by(initial_museum_id) |>
    summarize(outcome_largest_share=paste0("'", categorical_max(collection_size), "'")) |>
    ungroup() |>
    select(
      museum_id=initial_museum_id,
      outcome_largest_share
    )
}

get_outcomes_by_museum_for_type <- function(events_with_numeric_collection_size,
                                            event_attribute,
                                            unknown_label) {
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
    pivot_wider(
      names_from=.data[[event_attribute]],
      values_from=percent,
      values_fill=0
    ) |>
    mutate(!!sym(unknown_label):=.data[[unknown_label]] + 100 - total) |>
    select(-total)
  if (event_attribute == "destination_type") {
    outcome_proportions <- outcome_proportions |>
      mutate(
        `within the same LAD`=`within the same LAD` + `no move`,
        `within the same region`=`within the same region` + `within the same LAD`,
        `within the UK`=`within the UK` + `within the same region`,
        outcome=case_when(
          `end of existence` >= 50 ~ "mostly end of existence",
          `no move` >= 50 ~ "mostly no move",
          `within the same LAD` >= 50 ~ "mostly within the same LAD",
          `within the same region` >= 50 ~ "mostly within the same region",
          `within the UK` >= 50 ~ "mostly within the UK",
          `abroad` >= 50 ~ "mostly abroad",
          .data[[unknown_label]] >= 50 ~ paste("mostly", unknown_label),
          TRUE ~ "mixed destinations"
        )
      )
    write.csv(outcome_proportions, "outcome_destination_proportions.csv")
    return(outcome_proportions)
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
