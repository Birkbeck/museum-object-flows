museums <- read_csv("data/all_museums_data.csv") |>
  filter(!nation %in% c("Channel Islands", "Isle of Man")) |>
  mutate(`No filter`="All")
museums_including_crown_dependencies <- read_csv("data/all_museums_data.csv")

actor_types_csv <- "data/query_results/actor_types.csv"
actor_types <- read_csv(actor_types_csv)

event_types_csv <- "data/query_results/event_types.csv"
event_types <- read_csv(event_types_csv)

dispersal_events_csv <- "data/query_results/dispersal_events.csv"
dispersal_events <- read_csv(dispersal_events_csv)

explanations <- read_csv("explanations.csv")

field_names <- data.frame(
  name=c("Size", "Governance", "Accreditation", "Subject Matter", "Country/Region"),
  value=c("size", "governance_main", "accreditation", "main_subject", "region")
)
filter_field_choices <- museums |>
  select(size, governance_main, accreditation, main_subject, region) |>
  pivot_longer(
    cols=c(size, governance_main, accreditation, main_subject, region), names_to=c("field")
  ) |>
  mutate(label=unname(tidy_labels[value])) |>
  unique() |>
  arrange(fct_rev(factor(value, museum_attribute_ordering)))
subject_filter_field_choices <- museums |>
  select(main_subject, subject_matter) |>
  unique() |>
  mutate(subject_matter=fct_rev(factor(subject_matter, levels=museum_attribute_ordering))) |>
  arrange(subject_matter)
by_default_ignore <- c("unknown", "Unknown", "Other_Government")

