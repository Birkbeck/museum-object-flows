lengthUI <- function(id) {
  fluidPage(
    fluidRow(
      p("How long does it take for a museum to close?"),
      p("We can plot the date of museum closures and dispersal events onto a timeline."),
      p(
        "Below, the closure is shown as a red point, and the related dispersal events (transfers of collections from the museum to new owners or their physical movement out of the museum) are shown as blue points."
      ),
      virtualSelectInput(
        NS(id, "lengthOfClosureMuseum"),
        "View timeline for museum:",
        choices=c("a", "b", "c"),
        selected=NULL,
        multiple=FALSE,
        search=TRUE,
        width="500px"
      ),
      plotlyOutput(NS(id, "becmTimeline"), height="200px"),
    ),
    hr(style=hr_style),
    fluidRow(
      p(
        "Overall, dispersal events tend to occur in the same year as the museum's closure. But, in a significant number of cases, dispersal events continue for years after the initial closure. Dispersal events sometimes don't begin until a number of years after the closure, and in a small number of cases, dispersal events are recorded which pre-date the museum closure."
      ),
      plotOutput(NS(id, "allMuseumsTimeline"), width="900px", height="1600px"),
    ),
    hr(style=hr_style),
    fluidRow(
      p(
        "The vast majority of museum collection dispersals are completed in the same year as the closure. But over one fifth of museums' closure timelines are spread over more than one year. 10 museums have a closure timeline spread across 6 or more years."
      ),
      p(
        "Lengths of closure have been calculated by subtracting the latest year of an event from the earliest (e.g. 2006 - 2000 = 6). Where years are recorded with uncertainty: 2017/2018 is treated as 2017.5, 2017? is treated as 2017."
      ),
      plotOutput(NS(id, "timelinesSummary"), width="900px", height="200px"),
    ),
    hr(style=hr_style),
    fluidRow(
      p("Museums where the timeline of closure is unknown:"),
      DTOutput(NS(id, "unknownTimelines")),
    ),
    hr(style=hr_style),
    fluidRow(
      p(
        "In some instances, dispersal events occur over a number of years after (and sometimes before) the museum closure."
      ),
      plotlyOutput(NS(id, "longestTimelines"), height="720px"),
    ),
    hr(style=hr_style),
    fluidRow(
      p(
        "Some museums closure is pre-dated by collections dispersal events."
      ),
      plotlyOutput(NS(id, "earliestFirstEventTimelines"), height="720px"),
    ),
    hr(style=hr_style),
    fluidRow(
      p(
        "In a small number of cases, it is many years after the museum's closure before collections dispersal begins."
      ),
      plotlyOutput(NS(id, "latestFirstEventTimelines"), height="720px"),
    ),
  )
}

lengthServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    museums_list <- dispersal_events |>
      mutate(
        name=paste0(initial_museum_name, " (", initial_museum_id, ")")
      ) |>
      arrange(name) |>
      select(name, museum_id=initial_museum_id) |>
      distinct()
    updateVirtualSelect(
      inputId="lengthOfClosureMuseum",
      choices=museums_list$name,
      selected="British Empire and Commonwealth Museum (mm.domus.SW043)"
    )
    length_of_closure_museum <- reactive({
      museums_list |>
        filter(name==input$lengthOfClosureMuseum)
    })  

    event_dates_table <- reactive({get_event_dates_table()})
    output$becmTimeline <- renderPlotly({
      becm_events_timeline(event_dates_table(), length_of_closure_museum())
    })
    output$allMuseumsTimeline <- renderPlot({
      all_museums_timeline(event_dates_table())
    }, width=900, height=1600)
    output$timelinesSummary <- renderPlot({
      timelines_summary(event_dates_table())
    }, width=900, height=200)
    output$unknownTimelines <- renderDT({
      event_dates_table() |>
        filter(
          closure_length_category=="unknown",
          event_level=="super"
        ) |>
        select(museum, museum_id, closure_date=event_date)
    })
    output$longestTimelines <- renderPlotly({
      longest_timelines(event_dates_table())
    })
    output$earliestFirstEventTimelines <- renderPlotly({
      earliest_first_event_timelines(event_dates_table())
    })
    output$latestFirstEventTimelines <- renderPlotly({
      latest_first_event_timelines(event_dates_table())
    })
  })
}

get_event_dates_table <- function() {
  closure_super_events <- dispersal_events |>
    group_by(initial_museum_id, initial_museum_name, super_event_date) |>
    summarize(count=n()) |>
    ungroup() |>
    mutate(
      event_level="super",
      event_description="closure"
    ) |>
    select(
      museum=initial_museum_name,
      museum_id=initial_museum_id,
      event_level,
      event_date=super_event_date,
      event_description
    )
  closure_events <- dispersal_events |>
    mutate(
      event_level="sub",
      event_description=paste(collection_id, event_type, "to", recipient_name)
    ) |>
    select(
      museum=initial_museum_name,
      museum_id=initial_museum_id,
      event_level,
      event_date,
      event_description
    ) |>
    rbind(closure_super_events) |>
    mutate(
      year1 = sub("/.*", "", event_date),
      year2 = sub(".*/", "", event_date),
      year1 = sub("-.*", "", year1),
      year2 = sub("-.*", "", year2),
      year1 = sub("\\?", "", year1),
      year2 = sub("\\?", "", year2),
      year1 = as.numeric(year1),
      year2 = as.numeric(year2),
      year2 = ifelse(is.na(year2), year1, year2),
      year = (year1 + year2) / 2,
    ) |>
    filter(year > 1000) |>
    select(
      museum,
      museum_id,
      event_level,
      event_date,
      year1,
      year2,
      year,
      event_description
    )

  museum_ordering <- closure_events |>
    filter(event_level=="super") |>
    distinct() |>
    arrange(year)
  
  closure_lengths <- closure_events |>
    filter(event_level=="sub") |>
    filter(!is.na(year)) |>
    group_by(museum_id) |>
    summarize(
      earliest = min(year),
      latest = max(year)
    ) |>
    left_join(closure_events |> filter(event_level=="super") |> select(museum_id, year), by="museum_id") |>
    rename(closure_date=year) |>
    mutate(
      time_between_closure_and_earliest = earliest - closure_date,
      earliness_of_first_event = closure_date - earliest,
      earliest_including_closure = ifelse(earliest < closure_date, earliest, closure_date),
      latest_including_closure = ifelse(latest > closure_date, latest, closure_date),
      length_of_closure = latest_including_closure - earliest_including_closure
    )

  closure_events <- closure_events |> left_join(closure_lengths, by="museum_id") |>
    mutate(
      closure_length_category = ifelse(
        is.na(length_of_closure),
        "unknown",
        ifelse(
          length_of_closure == 0,
          "< 1 year",
          ifelse(
            length_of_closure < 6,
            "1-5 years",
            ifelse(
              length_of_closure < 11,
              "6-10 years",
              ifelse(
                length_of_closure < 16,
                "11-15 years",
                "> 15 years"
              )
            )
          )
        )
      )
    )
}

becm_events_timeline <- function(closure_events, museum_choice) {
  becm_events <- closure_events |>
    filter(museum_id %in% museum_choice$museum_id)
  
  ggplot(becm_events, aes(x=year, y=museum, label=event_description)) +
    geom_point(data=becm_events |> filter(event_level=="sub"), colour="blue", position=position_jitter(width=0, height=0.2, seed=1)) +
    geom_point(data=becm_events |> filter(event_level=="super"), colour="red") +
    labs(
      x="Year of Event",
      y="Museum"
    ) +
    standard_bars_theme
}

all_museums_timeline <- function(closure_events) {
  museum_ordering <- closure_events |>
    filter(event_level=="super") |>
    distinct() |>
    arrange(year)
  ggplot(closure_events, aes(x=year, y=fct_rev(factor(museum, museum_ordering$museum)))) +
    geom_point(
      data=closure_events |> filter(event_level=="sub"),
      colour="blue",
      position=position_jitter(width=0, height=0.2, seed=1)
    ) +
    geom_point(data=closure_events |> filter(event_level=="super"), colour="red") +
    labs(
      x="Year of Event",
      y="Museum"
    ) +
    timeline_theme +
    theme(
      axis.text = element_text(size=9)
    )
}

timelines_summary <- function(closure_events) {
  museums_by_closure_length <- closure_events |>
    select(museum_id, closure_length_category) |>
    distinct() |>
    group_by(closure_length_category) |>
    summarize(count=n()) |>
    mutate(
      percentage = round(count / sum(count) * 100, 1)
    ) |>
    ungroup()
  
  closure_length_categories <- c(
    "< 1 year",
    "1-5 years",
    "6-10 years",
    "11-15 years",
    "> 15 years",
    "unknown"
  )
  
  ggplot(museums_by_closure_length, aes(x=factor(closure_length_category, closure_length_categories), fill=count)) +
    geom_tile(aes(y=1), alpha=0.7, colour="black") +
    geom_text(aes(y=1, label=count), nudge_y=0.2, size=5) +
    geom_text(aes(y=1, label=paste0(percentage, "%")), nudge_y=-0.2, size=4) +
    labs(
      title="Number of Museums by Length of Closure Timeline",
      x="Length of Closure Timeline",
      y=""
    ) +
    scale_fill_continuous(low="white", high="purple") +
    standard_bars_theme
}
  
longest_timelines <- function(closure_events) {
  longest_closures <- closure_events |>
    select(museum_id, length_of_closure) |>
    distinct() |>
    top_n(10, length_of_closure)
  longest_timelines <- closure_events |>
    filter(museum_id %in% longest_closures$museum_id) |>
    arrange(length_of_closure)
  order_by_length <- longest_timelines |>
    filter(event_level=="super") |>
    arrange(length_of_closure)
  time_labels <- longest_closures |>
    select(museum_id) |>
    left_join(
      longest_timelines |>
        distinct(),
      by="museum_id"
    ) |>
    mutate(label_x=earliest_including_closure + length_of_closure / 2)
  
  ggplot(
    longest_timelines,
    aes(x=year, y=factor(museum, order_by_length$museum), label=event_description)
  ) +
    geom_point(data=longest_timelines |> filter(event_level=="sub"), colour="blue", position=position_jitter(width=0, height=0.2, seed=1)) +
    geom_point(data=longest_timelines |> filter(event_level=="super"), colour="red") +
    geom_text(
      data=time_labels,
      aes(
        x=label_x,
        y=museum,
        label=ifelse(
          length_of_closure == 1,
          paste(length_of_closure, "year"),
          paste(length_of_closure, "years")
        )
      ),
      nudge_y=0.3
    ) +
    labs(
      title="Longest Closures (years between first and last event)",
      x="Year of Event",
      y="Museum"
    ) +
    standard_bars_theme
}

earliest_first_event_timelines <- function(closure_events) {
  earliest_starts <- closure_events |>
    select(museum_id, earliness_of_first_event) |>
    distinct() |>
    top_n(10, earliness_of_first_event) |>
    filter(earliness_of_first_event > 0)
  earliest_start_timelines <- closure_events |>
    filter(museum_id %in% earliest_starts$museum_id) |>
    arrange(earliness_of_first_event)
  order_by_length <- earliest_start_timelines |>
    filter(event_level=="super") |>
    arrange(-time_between_closure_and_earliest)
  time_labels <- earliest_starts |>
    select(museum_id) |>
    left_join(
      earliest_start_timelines |>
        distinct(),
      by="museum_id"
    ) |>
    mutate(label_x=closure_date - earliness_of_first_event / 2)
  
  ggplot(
    earliest_start_timelines,
    aes(x=year, y=factor(museum, order_by_length$museum), label=event_description)
  ) +
    geom_point(data=earliest_start_timelines |> filter(event_level=="sub"), colour="blue", position=position_jitter(width=0, height=0.2, seed=1)) +
    geom_point(data=earliest_start_timelines |> filter(event_level=="super"), colour="red") +
    geom_text(
      data=time_labels,
      aes(
        x=label_x,
        y=museum,
        label=ifelse(
          earliness_of_first_event == 1,
          paste(earliness_of_first_event, "year"),
          paste(earliness_of_first_event, "years")
        )
      ),
      nudge_y=0.3
    ) +
    labs(
      title="Longest Lags Between First Dispersal Event and Closure",
      x="Year of Event",
      y="Museum"
    ) +
    standard_bars_theme
}

latest_first_event_timelines <- function(closure_events) {
  latest_starts <- closure_events |>
    select(museum_id, time_between_closure_and_earliest) |>
    distinct() |>
    top_n(10, time_between_closure_and_earliest)
  latest_start_timelines <- closure_events |>
    filter(museum_id %in% latest_starts$museum_id) |>
    arrange(time_between_closure_and_earliest)
  order_by_length <- latest_start_timelines |>
    filter(event_level=="super") |>
    arrange(time_between_closure_and_earliest)
  time_labels <- latest_starts |>
    select(museum_id) |>
    left_join(
      latest_start_timelines |>
        distinct(),
      by="museum_id"
    ) |>
    mutate(label_x=closure_date + time_between_closure_and_earliest / 2)
  
  ggplot(
    latest_start_timelines,
    aes(x=year, y=factor(museum, order_by_length$museum), label=event_description)
  ) +
    geom_point(data=latest_start_timelines |> filter(event_level=="sub"), colour="blue", position=position_jitter(width=0, height=0.2, seed=1)) +
    geom_point(data=latest_start_timelines |> filter(event_level=="super"), colour="red") +
    geom_text(
      data=time_labels,
      aes(
        x=label_x,
        y=museum,
        label=ifelse(
          time_between_closure_and_earliest == 1,
          paste(time_between_closure_and_earliest, "year"),
          paste(time_between_closure_and_earliest, "years")
        )
      ),
      nudge_y=0.3
    ) +
    labs(
      title="Longest Lags Between Closure and First Dispersal Event",
      x="Year of Event",
      y="Museum"
    ) +
    standard_bars_theme
}
