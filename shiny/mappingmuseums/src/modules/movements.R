library(ggmap)
library(jsonlite)

stadia_key <- read.csv("keys.csv") |> filter(service == "stadia") |> pull(key)
register_stadiamaps(stadia_key, write = TRUE)

movementsUI <- function(id) {
  fluidPage(
    fluidRow(
      p("Where do the collections of closed museums go? The diagrams below detail the movements of objects as they leave closed museums.")
    ),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          width=3,
          p("Group collections by origin museum type:"),
          selectInput(
            NS(id, "collectionDistancesCategory"),
            label="Group by:",
            choices=field_names$name,
            selected="Subject Matter"
          ),
          p("Highlight collection types (show in red):"),
          pickerInput(
            NS(id, "collectionDistancesTypes"),
            "Collection Type:",
            choices=c(),
            selected=c(),
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          )
        ),
        mainPanel(
          plotlyOutput(NS(id, "movementDistances"), width="100%", height="900px"),
          p("This scatter plot shows the distance travelled by objects and collections in their initial transfer from a closed museum. Use the 'group by' box to group collections according to the type of museum that they come from. Use the 'collection type' box to highlight specific types of collection in red. Hover over a point to see details of the collection transfer that it represents.")
        )
      )
    ),
    hr(style=hr_style),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          width=3,
          p("Involving collections:"),
          pickerInput(
            NS(id, "collectionMapTypes"),
            "Collection Type:",
            choices=c(),
            selected=c(),
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ),
          p("Involving collections from museum type:"),
          pickerInput(
            NS(id, "movementsSizeFilter"), 
            "Size:", 
            choices=tidy_labels_size,
            selected=tidy_labels_size,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ),
          pickerInput(
            NS(id, "movementsGovernanceFilter"), 
            "Governance:", 
            choices=tidy_labels_governance,
            selected=tidy_labels_governance,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ),
          pickerInput(
            NS(id, "movementsAccreditationFilter"), 
            "Accreditation:", 
            choices=tidy_labels_accreditation,
            selected=tidy_labels_accreditation,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ),
          pickerInput(
            NS(id, "movementsSubjectFilter"), 
            "Subject Matter:", 
            choices=tidy_labels_subject,
            selected="War & Conflict",
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ),
          pickerInput(
            NS(id, "movementsRegionFilter"), 
            "Country/Region:", 
            choices=tidy_labels_country_region,
            selected=tidy_labels_country_region,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ),
          ),
        mainPanel(
          p("This map shows the initial movements of collections and objects from a closed museum. Arrows point from the closed museum to the recipients of its collection.
By default, this map only shows collections originating in war & conflict museums. On the side panel, change filters for specific types of collection and specific types of museum. Altering the filters in the side panel will also update the table below so that it shows only the transactions depicted on the map."),
          plotOutput(NS(id, "movementsMap"), width="720px", height="1000px")
        )
      )
    ),
    fluidRow(
      DTOutput(NS(id, "movementsTable"))
    )
  )
}

movementsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    group_movements_by_category <- reactive({input$collectionDistancesCategory})
    updatePickerInput(
      inputId="collectionDistancesTypes",
      choices=collection_types_in_moves(),
      selected=c(),
      )
    highlight_movements_of_types <- reactive({input$collectionDistancesTypes})
    output$movementDistances <- renderPlotly({
      distances_scatter(group_movements_by_category(), highlight_movements_of_types())
    })
    
    updatePickerInput(
      inputId="collectionMapTypes",
      choices=collection_types_in_moves(),
      selected=collection_types_in_moves(),
      )
    movements_size_filter_choices <- reactive({input$movementsSizeFilter})
    movements_governance_filter_choices <- reactive({input$movementsGovernanceFilter})
    movements_accreditation_filter_choices <- reactive({input$movementsAccreditationFilter})
    movements_subject_filter_choices <- reactive({input$movementsSubjectFilter})
    movements_region_filter_choices <- reactive({input$movementsRegionFilter})
    movements_collection_type_choices <- reactive({input$collectionMapTypes})
    output$movementsMap <- renderPlot({
      movements_map(
        movements_collection_type_choices(),
        movements_size_filter_choices(),
        movements_governance_filter_choices(),
        movements_accreditation_filter_choices(),
        movements_subject_filter_choices(),
        movements_region_filter_choices()
      )
    }, width=720, height=1000)
    
    output$movementsTable <- renderDT({
      movements_table(
        movements_collection_type_choices(),
        movements_size_filter_choices(),
        movements_governance_filter_choices(),
        movements_accreditation_filter_choices(),
        movements_subject_filter_choices(),
        movements_region_filter_choices()
      )
    }, options=list(pageLength=100))
  })
}

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

movements_table <- function(collection_type_filter, size_filter, governance_filter, accreditation_filter, subject_filter, region_filter) {
  size_filter <- names(tidy_labels_size)[match(size_filter, tidy_labels_size)]
  governance_filter <- names(tidy_labels_governance)[match(governance_filter, tidy_labels_governance)]
  accreditation_filter <- names(tidy_labels_accreditation)[match(accreditation_filter, tidy_labels_accreditation)]
  subject_filter <- names(tidy_labels_subject)[match(subject_filter, tidy_labels_subject)]
  region_filter <- names(tidy_labels_country_region)[match(region_filter, tidy_labels_country_region)]
  
  movements <- dispersal_events |>
    filter(destination_lat < 90) |>
    filter(sender_size %in% size_filter) |>
    filter(sender_governance %in% governance_filter | sender_governance_broad %in% governance_filter) |>
    filter(sender_accreditation %in% accreditation_filter) |>
    filter(sender_subject %in% subject_filter) |>
    filter(sender_region %in% region_filter | sender_country %in% region_filter) |>
    mutate(
      from=origin_id,
      to=destination_id
    ) |>
    rowwise() |>
    mutate(
      distance=as.numeric(calculate_distance(origin_lat, origin_long, destination_lat, destination_long))
    ) |>
    select(
      sender_name,
      sender_size,
      sender_governance,
      sender_subject,
      origin_region,
      origin_lat,
      origin_long,
      recipient_name,
      destination_region,
      destination_lat,
      destination_long,
      collection_type,
      distance
    )

  if (length(collection_type_filter) > 0) {
    movements <- movements |>
      filter(str_detect(collection_type, str_c(collection_type_filter, collapse = "|")))
  }

  movements
}

movements_map <- function(collection_type_filter, size_filter, governance_filter, accreditation_filter, subject_filter, region_filter) {
  size_filter <- names(tidy_labels_size)[match(size_filter, tidy_labels_size)]
  governance_filter <- names(tidy_labels_governance)[match(governance_filter, tidy_labels_governance)]
  accreditation_filter <- names(tidy_labels_accreditation)[match(accreditation_filter, tidy_labels_accreditation)]
  subject_filter <- names(tidy_labels_subject)[match(subject_filter, tidy_labels_subject)]
  region_filter <- names(tidy_labels_country_region)[match(region_filter, tidy_labels_country_region)]
  
  movements <- dispersal_events |>
    filter(destination_lat < 90) |>
    filter(sender_size %in% size_filter) |>
    filter(sender_governance %in% governance_filter | sender_governance_broad %in% governance_filter) |>
    filter(sender_accreditation %in% accreditation_filter) |>
    filter(sender_subject %in% subject_filter) |>
    filter(sender_region %in% region_filter | sender_country %in% region_filter) |>
    mutate(
      from=origin_id,
      to=destination_id
    ) |>
    rowwise() |>
    mutate(
      distance=as.numeric(calculate_distance(origin_lat, origin_long, destination_lat, destination_long))
    ) |>
    filter(!is.na(destination_lat)) |>
    filter(!is.na(origin_lat))

  if (length(collection_type_filter) > 0) {
    movements <- movements |>
      filter(str_detect(collection_type, str_c(collection_type_filter, collapse = "|")))
  }
  
  from_nodes_counts <- movements |>
    mutate(
      name=from,
      lat=origin_lat,
      long=origin_long,
      label=sender_name,
      governance=sender_governance
    ) |>
    select(name, lat, long, label, governance) |>
    group_by(name, lat, long, label, governance) |>
    summarize(from_count = n()) |>
    ungroup()
  
  to_nodes_counts <- movements |>
    mutate(
      name=to,
      lat=destination_lat,
      long=destination_long,
      label=recipient_name,
      governance=recipient_governance
    ) |>
    select(name, lat, long, label, governance) |>
    group_by(name, lat, long, label, governance) |>
    summarize(to_count = n()) |>
    ungroup()
  
  node_counts <- from_nodes_counts |>
    full_join(to_nodes_counts, by="name") |>
    mutate(
      count = ifelse(
        is.na(to_count),
        from_count,
        ifelse(
          is.na(from_count), 
          to_count,
          ifelse(
            from_count > to_count,
            from_count,
            to_count
          )
        )
      )
    ) |>
    mutate(
      lat = ifelse(is.na(lat.x), lat.y, lat.x),
      long = ifelse(is.na(long.x), long.y, long.x),
      label = ifelse(is.na(label.x), label.y, label.x),
      governance = ifelse(is.na(governance.x), governance.y, governance.x)
    ) |>
    select(name, lat, long, label, governance, count)
  
  size_edges <- movements |>
    mutate(
      transaction_size = ifelse(is.na(transaction_size), 1, transaction_size),
    ) |>
    select(from, to, origin_lat, origin_long, destination_lat, destination_long, distance, transaction_size) |>
    group_by(from, to, origin_lat, origin_long, destination_lat, destination_long, distance) |>
    summarize(count = sum(transaction_size)) |>
    ungroup() |>
    mutate(
      label=""
    )
  
  edges <- size_edges
  
  g <- graph_from_data_frame(edges)
  
  layout <- create_layout(g, layout="stress") |>
    left_join(node_counts, by="name")
  
  layout$x <- layout$long
  layout$y <- layout$lat
  
  # Get the base map using ggmap
  bbox <- c(
    left=min(filter(node_counts, !is.nan(long))$long) - 2,
    right=max(filter(node_counts, !is.nan(long))$long) + 2,
    bottom=min(filter(node_counts, !is.nan(lat))$lat) - 0.5,
    top=max(filter(node_counts, !is.nan(lat))$lat) + 0.5
  )
  map <- get_stadiamap(bbox, zoom = 6, source = "stadia", maptype = "stamen_toner_lite")
  
  # Plotting with ggmap and adding the other layers
  ggmap(map) +
    geom_segment(
      data=size_edges,
      aes(x=origin_long, y=origin_lat, xend=destination_long, yend=destination_lat),
      arrow=arrow(angle=30, length=unit(0.25, "cm"), ends="last", type="closed")
    ) +
    scale_edge_radius(name=count) +
    geom_point(
      data=node_counts,
      aes(x=long, y=lat),
      color=map_points_red,
      size=3
    ) +
    geom_text(
      data=from_nodes_counts,
      aes(x=long, y=lat, label=ifelse(!is.na(governance), label, "")), 
      size=3,
      family="sans-serif"
    ) +
    scale_edge_alpha('Edge direction', guide='edge_direction') +
    labs(
      title="Movement of Collections from Museums"
    ) +
    network_theme
}

collection_types_in_moves <- function() {
   coll_types <- dispersal_events |>
    mutate(
      # replace single quotes with double except for internal
      collection_type = str_replace_all(collection_type, "^\\[|\\]$", function(x) gsub("'", "\"", x)),
      collection_type = str_replace_all(collection_type, "'([^']+)'", "\"\\1\""),
      # replace empty lists with unknown 
      collection_type = ifelse(collection_type == "['']", '["unknown"]', collection_type),
      # remove extra whitespace
      collection_type = str_replace_all(collection_type, "\\s*,\\s*", ","),
      distance = calculate_distance(origin_lat, origin_long, destination_lat, destination_long)
    ) |>
    mutate(collection_type = lapply(collection_type, fromJSON)) |>
    unnest_longer(collection_type) |>
    group_by(collection_type) |>
    summarize(
      mean_distance = mean(distance),
      count = n()
    ) |>
    ungroup() |>
   arrange(desc(mean_distance))
  return(as.data.frame(coll_types)[["collection_type"]]) 
}

distances_scatter <- function(group_by_category, highlighted_collections) {
  y_label <- paste(group_by_category, "of origin museum")

  if (group_by_category == "Governance") {
    group_by_category <- "origin_governance"
  } else if (group_by_category == "Size") {
    group_by_category <- "origin_size"
  } else if (group_by_category == "Subject Matter") {
    group_by_category <- "origin_subject"
  } else if (group_by_category == "Accreditation") {
    group_by_category <- "origin_accreditation"
  } else if (group_by_category == "Country/Region") {
    group_by_category <- "origin_region"
  }

  collection_movements <- dispersal_events |>
    mutate(
      # replace single quotes with double except for internal
      collection_type = str_replace_all(collection_type, "^\\[|\\]$", function(x) gsub("'", "\"", x)),
      collection_type = str_replace_all(collection_type, "'([^']+)'", "\"\\1\""),
      # replace empty lists with unknown 
      collection_type = ifelse(collection_type == "['']", '["unknown"]', collection_type),
      # remove extra whitespace
      collection_type = str_replace_all(collection_type, "\\s*,\\s*", ","),
      distance = calculate_distance(origin_lat, origin_long, destination_lat, destination_long),
      collection = paste0(collection, " (", origin_museum, ")")
    )

  chart <- ggplot(
    collection_movements,
    aes(
      x=distance,
      y=factor(.data[[group_by_category]], museum_attribute_ordering),
      label=collection,
      group=collection_type
    )
  )

  if (length(highlighted_collections) == 0) {
    chart <- chart +
      geom_point(
        data=collection_movements,
        shape=21,
        fill="black",
        colour="black",
        alpha=0.5,
        lwd=1,
        position=position_jitter(width=0, height=0.2, seed=1)
      )
  } else {
    chart <- chart +
      geom_point(
        data=collection_movements |> filter(!str_detect(collection_type, str_c(highlighted_collections, collapse = "|"))),
        shape=21,
        fill="black",
        colour="black",
        alpha=0.2,
        lwd=1,
        position=position_jitter(width=0, height=0.2, seed=1)
      ) +
      geom_point(
        data=collection_movements |> filter(str_detect(collection_type, str_c(highlighted_collections, collapse = "|"))),
        shape=21,
        fill="red",
        colour="red",
        alpha=0.9,
        lwd=1,
        position=position_jitter(width=0, height=0.2, seed=1)
      )
  }
  
  chart +
    scale_y_discrete(labels=tidy_labels) +
    labs(
      title="Distance Travelled by Collections When Leaving a Closed Museum",
      x="Distance (km)",
      y=y_label
    ) +
    theme_minimal()
}
