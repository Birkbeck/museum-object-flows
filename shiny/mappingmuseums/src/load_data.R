set.seed(1)

core_actor_types_with_children <- c(
  "armed forces",
  "civic organisation",
  "company",
  "education/research",
  "heritage",
  "leisure",
  "library/archive",
  "other organisation",
  "service",
  "society",
  "storage",
  "temporary museum",
  "trader",
  "transport provider"
)

calculate_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert degrees to radians
  radians <- function(degrees) {
    degrees * pi / 180
  }
  earth_radius <- 6371
  dlat <- radians(lat2 - lat1)
  dlon <- radians(lon2 - lon1)
  lat1 <- radians(lat1)
  lat2 <- radians(lat2)
  # Haversine formula
  a <- sin(dlat / 2) * sin(dlat / 2) +
    cos(lat1) * cos(lat2) * sin(dlon / 2) * sin(dlon / 2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  # Distance in kilometers
  distance <- earth_radius * c
  return(distance)
}

dispersal_events_csv <- "data/query_results/dispersal_events.csv"
dispersal_events <- read_csv(dispersal_events_csv) |>
  mutate(
    event_stage_in_path = event_stage_in_path + 1,
    recipient_type = case_when(
      is.na(recipient_type) ~ "N/A",
      recipient_type %in% core_actor_types_with_children ~ paste("unspecified", recipient_core_type),
      recipient_type == "actor" ~ "unspecified actor",
      TRUE ~ recipient_type
    ),
    recipient_core_type = case_when(
      recipient_type == "N/A" ~ "N/A",
      recipient_type == "unspecified actor" ~ "unspecified actor",
      TRUE ~ recipient_core_type
    ),
    sender_type = case_when(
      is.na(sender_type) ~ "N/A",
      sender_type %in% core_actor_types_with_children ~ paste("unspecified", sender_core_type),
      sender_type == "actor" ~ "unspecified actor",
      TRUE ~ sender_type
    ),
    sender_core_type = case_when(
      is.na(sender_type) ~ "N/A",
      sender_type == "unspecified actor" ~ "unspecified actor",
      TRUE ~ sender_core_type
    ),
    collection_status = case_when(
      collection_status == "collection" ~ "Items from a museum's collection",
      collection_status == "loan" ~ "Items loaned to a museum",
      collection_status == "handling" ~ "Items for handling",
      collection_status == "museum-stuff" ~ "Other items (e.g. furniture)"
    ),
    initial_museum_all = "all",
    sender_all = ifelse(!is.na(sender_size), "all", NA),
    recipient_all = ifelse(!is.na(recipient_size), "all", NA),
    distance=calculate_distance(
      origin_latitude,
      origin_longitude,
      destination_latitude,
      destination_longitude
    ),
    distance_category=case_when(
      is.na(distance) ~ "unknown",
      distance == 0 ~ "0",
      distance < 1 ~ "0 - 1",
      distance < 10 ~ "1 - 10",
      distance < 100 ~ "10 - 100",
      distance < 1000 ~ "100 - 1,000",
      TRUE ~ "1,001+"
    ),
    distance_category=factor(
      distance_category,
      c("all", "unknown", "0", "0 - 1", "1 - 10", "10 - 100", "100 - 1,000", "1,001+")
    )
  )

senders <- dispersal_events |>
  select(
    actor_id=sender_id,
    name=sender_name,
    quantity=sender_quantity,
    sector=sender_sector,
    type=sender_type,
    size=sender_size,
    governance=sender_governance,
    accreditation=sender_accreditation,
    town=sender_town,
    county=sender_county,
    postcode=sender_postcode,
    region=sender_region,
    country=sender_country
  )
recipients <- dispersal_events |>
  select(
    actor_id=recipient_id,
    name=recipient_name,
    quantity=recipient_quantity,
    sector=recipient_sector,
    type=recipient_type,
    size=recipient_size,
    governance=recipient_governance,
    accreditation=recipient_accreditation,
    town=recipient_town,
    county=recipient_county,
    postcode=recipient_postcode,
    region=recipient_region,
    country=recipient_country
  )
actors <- rbind(senders, recipients) |>
  unique()

initial_museums <- dispersal_events |>
  select(
    museum_id=initial_museum_id,
    museum_name=initial_museum_name,
    size=initial_museum_size,
    governance=initial_museum_governance,
    governance_broad=initial_museum_governance_broad,
    subject_broad=initial_museum_subject_broad,
    subject=initial_museum_subject,
    region=initial_museum_region,
    country=initial_museum_country,
    accreditation=initial_museum_accreditation
  ) |>
  distinct() |>
  mutate(
    name=paste0(museum_name, " (", museum_id, ")")
  ) |>
  arrange(name)

collection_types <- dispersal_events |>
  mutate(
    collection_type = str_remove_all(collection_types, "\\[|\\]|'") |>
      str_split(",\\s*")
  ) |>
  unnest(collection_type) |>
  select(collection_type) |>
  unique() |>
  filter(collection_type != "")

museums_including_crown_dependencies <- read_csv("data/query_results/museums.csv") |>
  mutate(
    year_opened = ifelse(
      year_opened_1 == year_opened_2,
      year_opened_1,
      paste(year_opened_1, year_opened_2, sep=":")
    ),
    year_closed = case_when(
      year_closed_1 == 9999 ~ "N/A",
      year_closed_1 == year_closed_2 ~ as.character(year_closed_1),
      TRUE ~ paste(year_closed_1, year_closed_2, sep=":")
    ),
    all=factor(all, museum_attribute_ordering),
    size=factor(size, museum_attribute_ordering),
    governance=factor(governance, museum_attribute_ordering),
    governance_broad=factor(governance_broad, museum_attribute_ordering),
    subject=factor(subject, museum_attribute_ordering),
    subject_broad=factor(subject_broad, museum_attribute_ordering),
    accreditation=factor(accreditation, museum_attribute_ordering),
    region=factor(region, museum_attribute_ordering),
    country=factor(country, museum_attribute_ordering)
  )

closure_reasons <- dispersal_events |>
  select(
    museum_id=initial_museum_id,
    reason=super_event_cause_types,
    super_reasons=super_event_causes
  ) |>
  distinct() |>
  separate_rows(reason, sep = "; ") |>
  separate_wider_delim(
    reason,
    " - ",
    names=c("reason_core", "reason_core_or_child", "reason_specific"),
    too_few="align_start"
  ) |>
  mutate(
    reason_core_or_child=ifelse(
      is.na(reason_core_or_child),
      reason_core,
      paste(reason_core, "-", reason_core_or_child)
    ),
    reason_specific=ifelse(
      is.na(reason_specific),
      reason_core_or_child,
      paste(reason_core_or_child, "-", reason_specific)
    )
  ) |>
  left_join(museums_including_crown_dependencies, by="museum_id")

closure_outcomes <- get_outcomes_by_museum(dispersal_events)
closure_lengths <- get_closure_lengths_by_museum(
  dispersal_events, museums_including_crown_dependencies
)
closure_timeline_events <- get_closure_timeline_events(
  dispersal_events, museums_including_crown_dependencies
)

museums_including_crown_dependencies <- museums_including_crown_dependencies |>
  left_join(closure_outcomes, by="museum_id") |>
  left_join(
    closure_reasons |>
      select(museum_id, reasons_for_closure=super_reasons) |>
      unique(),
    by="museum_id"
  ) |>
  mutate(
    outcome_event_type=factor(outcome_event_type, museum_attribute_ordering),
    outcome_recipient_type=factor(outcome_recipient_type, museum_attribute_ordering),
    outcome_recipient_count=factor(outcome_recipient_count, museum_attribute_ordering),
    outcome_largest_share=factor(outcome_largest_share, museum_attribute_ordering),
    outcome_destination_type=factor(outcome_destination_type, museum_attribute_ordering)
  )
museums <- museums_including_crown_dependencies |>
  filter(!country %in% c("Channel Islands", "Isle of Man"))

size_labels <- unique(select(museums_including_crown_dependencies, label=size))
governance_broad_labels <- unique(select(museums_including_crown_dependencies, label=governance_broad))
governance_labels <- unique(select(museums_including_crown_dependencies, label=governance))
subject_broad_labels <- unique(select(museums_including_crown_dependencies, label=subject_broad))
subject_labels <- unique(select(museums_including_crown_dependencies, label=subject))
accreditation_labels <- unique(select(museums_including_crown_dependencies, label=accreditation))
region_labels <- unique(select(museums_including_crown_dependencies, label=region))
country_labels <- unique(select(museums_including_crown_dependencies, label=country))
reason_core_labels <- unique(select(closure_reasons, label=reason_core))
event_core_types <- dispersal_events |>
  select(label=event_core_type) |>
  unique()
sender_core_types <- dispersal_events |>
  select(label=sender_core_type) |>
  unique()
recipient_core_types <- dispersal_events |>
  select(label=recipient_core_type) |>
  unique()
collection_status_labels <- dispersal_events |>
  select(label=collection_status) |>
  unique()

subject_labels_map <- museums_including_crown_dependencies |>
  select(subject_broad, subject) |>
  unique()

regions <- read_csv("data/regions.csv") |>
  mutate(group=paste(L1, L2, L3))

actor_types_csv <- "data/query_results/actor_types.csv"
actor_types <- read_csv(actor_types_csv)
 
event_types_csv <- "data/query_results/event_types.csv"
event_types <- read_csv(event_types_csv)

explanations <- read_csv("explanations.csv")

field_names <- data.frame(
  name=c("All", "Size", "Governance", "Accreditation", "Subject Matter", "Country/Region", "Country"),
  value=c("all", "size", "governance_broad", "accreditation", "subject_broad", "region", "country")
)
filter_field_choices <- museums_including_crown_dependencies |>
  select(all, size, governance_broad, accreditation, subject_broad, region, country) |>
  pivot_longer(
    cols=c(all, size, governance_broad, accreditation, subject_broad, region, country),
    names_to=c("field"),
    values_to=c("label")
  ) |>
  unique()

subject_filter_field_choices <- museums |>
  select(subject_broad, subject) |>
  unique() |>
  mutate(subject=fct_rev(factor(subject, levels=museum_attribute_ordering))) |>
  arrange(subject)
by_default_ignore <- c("unknown", "Unknown", "Other_Government")

sector_type_ordering_table <- actor_types |>
  mutate(
    public_proportion=public_instances / total_instances,
    private_proportion=private_instances / total_instances,
    third_proportion=third_instances / total_instances,
    university_proportion=university_instances / total_instances,
    hybrid_proportion=hybrid_instances / total_instances,
    unknown_proportion=unknown_instances / total_instances
  ) |>
  select(type_name, public_proportion, private_proportion, third_proportion, university_proportion, hybrid_proportion, unknown_proportion) |>
  bind_rows(
    tibble(
      type_name = c("public", "private", "third", "hybrid"),
      public_proportion = c(1, 0, 0, 0),
      private_proportion = c(0, 1, 0, 0),
      third_proportion = c(0, 0, 1, 0),
      university_proportion = c(0, 0, 0, 0),
      hybrid_proportion = c(0, 0, 0, 1),
      unknown_proportion = c(0, 0, 0, 0)
    )
  ) |>
  mutate(
    type_name = paste0("NA@", type_name)
  ) |>
  bind_rows(
    tibble(
      type_name = c("National@public", "National@museum", "National@organisation"),
      public_proportion = c(4, 4, 4),
      private_proportion = c(0, 0, 0),
      third_proportion = c(0, 0, 0),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Other_Government@public", "Other_Government@museum", "Other_Government@organisation"),
      public_proportion = c(3, 3, 3),
      private_proportion = c(0, 0, 0),
      third_proportion = c(0, 0, 0),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Local_Authority@public", "Local_Authority@museum", "Local_Authority@organisation"),
      public_proportion = c(2, 2, 2),
      private_proportion = c(0, 0, 0),
      third_proportion = c(0, 0, 0),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("University@university", "University@museum", "University@organisation"),
      public_proportion = c(0, 0, 0),
      private_proportion = c(0, 0, 0),
      third_proportion = c(0, 0, 0),
      university_proportion = c(2, 2, 2),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Unknown@unknown", "Unknown@museum", "Unknown@organisation"),
      public_proportion = c(0, 0, 0),
      private_proportion = c(0, 0, 0),
      third_proportion = c(0, 0, 0),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(2, 2, 2)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Independent@third", "Independent@museum", "Independent@organisation"),
      public_proportion = c(0, 0, 0),
      private_proportion = c(0, 0, 0),
      third_proportion = c(6, 6, 6),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Independent-Not_for_profit@third", "Independent-Not_for_profit@museum", "Independent-Not_for_profit@organisation"),
      public_proportion = c(0, 0, 0),
      private_proportion = c(0, 0, 0),
      third_proportion = c(6, 6, 6),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Independent-English_Heritage@third", "Independent-English_Heritage@museum", "Independent-English_Heritage@organisation"),
      public_proportion = c(0, 0, 0),
      private_proportion = c(0, 0, 0),
      third_proportion = c(5, 5, 5),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Independent-National_Trust@third", "Independent-National_Trust@museum", "Independent-National_Trust@organisation"),
      public_proportion = c(0, 0, 0),
      private_proportion = c(0, 0, 0),
      third_proportion = c(4, 4, 4),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Independent-National_Trust_for_Scotland@third", "Independent-National_Trust_for_Scotland@museum", "Independent-National_Trust_for_Scotland@organisation"),
      public_proportion = c(0, 0, 0),
      private_proportion = c(0, 0, 0),
      third_proportion = c(3, 3, 3),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Independent-Historic_Environment_Scotland@third", "Independent-Historic_Environment_Scotland@museum", "Independent-Historic_Environment_Scotland@organisation"),
      public_proportion = c(0, 0, 0),
      private_proportion = c(0, 0, 0),
      third_proportion = c(2, 2, 2),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  bind_rows(
    tibble(
      type_name = c("Private@private", "Private@museum", "Private@organisation"),
      public_proportion = c(0, 0, 0),
      private_proportion = c(2, 2, 2),
      third_proportion = c(0, 0, 0),
      university_proportion = c(0, 0, 0),
      hybrid_proportion = c(0, 0, 0),
      unknown_proportion = c(0, 0, 0)
    )
  ) |>
  mutate(
    ordering = public_proportion * 1e6
    + university_proportion * 1e5
    + hybrid_proportion * 1e4
    + unknown_proportion * 1e3
    + third_proportion * 1e2
    + private_proportion * 1e1
  ) |>
  arrange(ordering, desc=TRUE)

sector_type_ordering <- sector_type_ordering_table$type_name

