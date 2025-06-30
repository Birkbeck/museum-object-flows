closure_length_categories <- c(
  "all",
  "unknown",
  "0-1 years",
  "1-2 years",
  "2-4 years",
  "4-8 years",
  "8-16 years",
  "16+ years"
)

get_super_events <- function(dispersal_events, museums) {
  museums |>
    filter(
      year_closed_1 != 9999,
      museum_id %in% dispersal_events$initial_museum_id
    ) |>
    mutate(
      event_level="super",
      event_description="closure",
      event_year=(year_closed_1 + year_closed_2) / 2
    ) |>
    select(
      museum_id,
      museum_name,
      event_level,
      event_description,
      event_date=year_closed,
      event_year,
      year1=year_closed_1,
      year2=year_closed_2
    )
}

get_initial_transactions <- function(dispersal_events) {
  dispersal_events |>
    filter(sender_name == initial_museum_name & event_type != "displayed") |>
    mutate(
      event_level="sub",
      event_description=ifelse(
        is.na(collection_description),
        paste(collection_id, event_type, "to", recipient_name),
        paste(collection_description, event_type, "to", recipient_name)
      ),
      year1 = sub("/.*", "", event_date),
      year2 = sub(".*/", "", event_date),
      year1 = sub("-.*", "", year1),
      year2 = sub("-.*", "", year2),
      year1 = sub("\\?", "", year1),
      year2 = sub("\\?", "", year2),
      year1 = as.numeric(year1),
      year2 = as.numeric(year2),
      year2 = ifelse(is.na(year2), year1, year2),
      event_year = (year1 + year2) / 2
    ) |>
    select(
      museum_id=initial_museum_id,
      museum_name=initial_museum_name,
      event_level,
      event_description,
      event_date,
      event_year,
      year1,
      year2
    )
}

get_closure_timeline_events <- function(dispersal_events, museums) {
  rbind(
    get_super_events(dispersal_events, museums),
    get_initial_transactions(dispersal_events)
  )
}

get_closure_lengths_by_museum <- function(dispersal_events, museums) {
  closure_super_events <- get_super_events(dispersal_events, museums)
  closure_sub_events <- get_initial_transactions(dispersal_events)

  closure_lengths <- closure_sub_events |>
    filter(!is.na(event_year)) |>
    group_by(museum_id) |>
    summarize(
      earliest = min(event_year),
      latest = max(event_year)
    ) |>
    ungroup() |>
    left_join(closure_super_events |> select(museum_id, event_year), by="museum_id") |>
    rename(closure_date=event_year) |>
    mutate(
      earliest = ifelse(earliest < closure_date, closure_date, earliest),
      time_between_closure_and_earliest = earliest - closure_date,
      latest_including_closure_date = ifelse(closure_date > latest, closure_date, latest),
      length_of_closure = latest_including_closure_date - closure_date,
      closure_length_category = case_when(
        is.na(length_of_closure) ~ "unknown",
        length_of_closure < 1 ~ "0-1 years",
        length_of_closure < 2 ~ "1-2 years",
        length_of_closure < 4 ~ "2-4 years",
        length_of_closure < 8 ~ "4-8 years",
        length_of_closure < 16 ~ "8-16 years",
        TRUE ~ "16+ years"
      ),
      closure_length_category = factor(closure_length_category, closure_length_categories)
    ) |>
    left_join(museums, by="museum_id") #|>
    #filter(!is.na(length_of_closure)) # remove open museums
  closure_lengths
}
