probability_happened_by_end_of_year <- function(year_1, year_2, cut_off_year)  {
  # year_1 and year_2 are opposite ends of an inclusive range of years during which an event happened
  ifelse(
    # if year 2 is before cut_off_year, it must have happened before cut_off_year
    # [ -- ] -- c
    year_2 < cut_off_year,
    1,
    ifelse(
      # if year 1 is after cut_of_year, it could not have happened before cut_off_year
      # c -- [ -- ]
      year_1 > cut_off_year,
      0,
      # probability is the proportion of the years in the inclusive range that come before cut_off_year
      # [ -- c -- ]
      (cut_off_year - year_1 + 1) / (year_2 - year_1 + 1)
    )
  )
}

probability_happened_before_year <- function(year_1, year_2, cut_off_year)  {
  probability_happened_by_end_of_year(year_1, year_2, cut_off_year - 1)
}

museums_in_time_period_raw_figures <- function(museums, start_year, end_year) {
  # raw opening, closing, change, etc figures for museums in period between
  # 1st January start_year and 31st December end_year
  # if end == start, period is 1 year january to december
  years_in_period <- end_year - start_year + 1
  museums |>
    mutate(
      prob_opened_before_start = probability_happened_before_year(year_opened_1, year_opened_2, start_year),
      prob_closed_before_start = probability_happened_before_year(year_closed_1, year_closed_2, start_year),
      prob_opened_before_end = probability_happened_before_year(year_opened_1, year_opened_2, end_year + 1),
      prob_closed_before_end = probability_happened_before_year(year_closed_1, year_closed_2, end_year + 1),
      prob_open_at_start = prob_opened_before_start * (1 - prob_closed_before_start),
      prob_open_at_end = prob_opened_before_end * (1 - prob_closed_before_end),
      prob_closed_in_period = prob_closed_before_end - prob_closed_before_start,
      prob_opened_in_period = prob_opened_before_end - prob_opened_before_start,
      prob_open_for_some_of_period = prob_opened_before_end * (1 - prob_closed_before_start)
    ) |>
    summarize(
      period_total = sum(prob_open_for_some_of_period),
      start_total = sum(prob_open_at_start),
      end_total = sum(prob_open_at_end),
      average_total = (start_total + end_total) / 2,
      closures = sum(prob_closed_in_period),
      openings = sum(prob_opened_in_period),
      change = openings - closures,
      change_pc = change / start_total * 100,
      closure_rate = closures / average_total * 100,
      opening_rate = openings / average_total * 100,
      yearly_closures = closures / years_in_period,
      yearly_openings = openings / years_in_period,
      turnover = openings + closures,
      turnover_pc = turnover / average_total * 100
    )
}

museums_in_time_period <- function(museums, start_year, end_year) {
  # rounded opening, closing, change, etc figures for museums in period between
  # 1st January start_year and 31st December end_year
  # if end == start, period is 1 year january to december
  museums |>
    museums_in_time_period_raw_figures(start_year, end_year) |>
    mutate(
      period_total = round(period_total, 0),
      start_total = round(start_total, 0),
      end_total = round(end_total, 0),
      closures = round(closures, 0),
      openings = round(openings, 0),
      change = round(change, 0),
      change_pc = round(change_pc, 1),
      closure_rate = round(closure_rate, 1),
      opening_rate = round(opening_rate, 1),
      yearly_closures = round(yearly_closures, 1),
      yearly_openings = round(yearly_openings, 1),
      turnover = round(turnover, 0),
      turnover_pc = round(turnover_pc, 1)
    ) |>
    select(-average_total)
}

get_open_and_close_data <- function(data, dimension, start_year, end_year) {
  data |>
    group_by(.data[[dimension]]) |>
    museums_in_time_period(start_year, end_year)
}
get_2_way_open_and_close_data <- function(data, dimension1, dimension2, start_year, end_year) {
  data |>
    group_by(.data[[dimension1]], .data[[dimension2]]) |>
    museums_in_time_period(start_year, end_year)
}

get_museums_in_time_period <- function(museums, start_year, end_year) {
  museums |>
    mutate(
      prob_opened_before_start = probability_happened_before_year(year_opened_1, year_opened_2, start_year),
      prob_closed_before_start = probability_happened_before_year(year_closed_1, year_closed_2, start_year),
      prob_opened_before_end = probability_happened_before_year(year_opened_1, year_opened_2, end_year + 1),
      prob_closed_before_end = probability_happened_before_year(year_closed_1, year_closed_2, end_year + 1),
      prob_open_at_start = prob_opened_before_start * (1 - prob_closed_before_start),
      prob_open_at_end = prob_opened_before_end * (1 - prob_closed_before_end),
      prob_closed_in_period = prob_closed_before_end - prob_closed_before_start,
      prob_opened_in_period = prob_opened_before_end - prob_opened_before_start,
      prob_open_for_some_of_period = prob_opened_before_end * (1 - prob_closed_before_start)
    )
}

get_open_in_time_period <- function(museums, start_year, end_year, measure) {
  museums |>
    get_museums_in_time_period(start_year, end_year) |>
    filter(.data[[measure]] > 0) |>
    mutate(
      year_opened = paste(year_opened_1, year_opened_2, sep=":"),
      year_closed = paste(year_closed_1, year_closed_2, sep=":")
    ) |>
      select(
          museum_id,
          name_of_museum,
          size,
          governance,
          accreditation,
          subject_matter,
          region,
          year_opened,
          year_closed,
          .data[[measure]]
      )
}

get_closures_in_time_period <- function(museums, start_year, end_year) {
  museums |>
    get_museums_in_time_period(start_year, end_year) |>
    filter(prob_closed_in_period > 0) |>
    mutate(
      year_opened = paste(year_opened_1, year_opened_2, sep=":"),
      year_closed = paste(year_closed_1, year_closed_2, sep=":")
    ) |>
      select(
          museum_id,
          name_of_museum,
          size,
          governance,
          accreditation,
          subject_matter,
          region,
          year_opened,
          year_closed,
          prob_closed_in_period
      )
}

get_openings_in_time_period <- function(museums, start_year, end_year) {
  museums |>
    get_museums_in_time_period(start_year, end_year) |>
    filter(prob_opened_in_period > 0) |>
    mutate(
      year_opened = paste(year_opened_1, year_opened_2, sep=":"),
      year_closed = paste(year_closed_1, year_closed_2, sep=":")
    ) |>
      select(
          museum_id,
          name_of_museum,
          size,
          governance,
          accreditation,
          subject_matter,
          region,
          year_opened,
          year_closed,
          prob_opened_in_period
      )
}
