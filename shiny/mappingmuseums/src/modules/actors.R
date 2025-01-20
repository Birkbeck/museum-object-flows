actorsUI <- function(id) {
  fluidPage(
    fluidRow(
      p("Who is involved in the dispersal of museum collections?"),
      p("This tab introduces the types of actor in the dispersal data and how they associate with one another."),
      h3("Actor Types"),
      p("The types of actor involved in transactions with museums are shown below. The leaves of the tree include highly specific categories. These are grouped into core categories which are used in some of the visual summaries in the following tabs."),
      plotOutput(NS(id, "actorTypes"), width="80%", height="1500px")
    ),
    fluidRow(
      p("Who transacts with closed museums? The network diagram shows known transactions involving (former) museum collections. Actors are grouped by their most specific type and their economic sector."),
      p("Use the filters to show only transactions involving collections originating from particular types of museum. By default, the network is filtered to show only actors involved in transfers of collections originating in arts museums."),
      p("The matrix below shows the number of transactions from one type of actor to another and the table below that provides details of all transactions shown in the diagrams."),
      ),
    fluidRow(h3("Actors")),
    fluidRow(
      DTOutput(NS(id, "transactionsTable"))
    )
  )
}

actorsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$actorTypes <- renderPlot({
      actor_types_hierarchy()
    })
    output$transactionsTable <- renderDT({
      dispersal_events |>
        select(
          actor_name=sender_name,
          actor_type=sender_type,
          actor_governance=sender_governance,
          actor_sector=sender_sector,
          actor_town=sender_town,
          actor_county=sender_county,
          actor_postcode=sender_postcode
        ) |>
        rbind(
          dispersal_events |>
            select(
              actor_name=recipient_name,
              actor_type=recipient_type,
              actor_governance=recipient_governance,
              actor_sector=recipient_sector,
              actor_town=recipient_town,
              actor_county=recipient_county,
              actor_postcode=recipient_postcode
            )
        ) |>
        distinct()
    }, options=list(pageLength=100))
  })
}

actor_types_hierarchy <- function() {
  # add dummy types to use as spaces between groups
  actor_types <- actor_types |>
    mutate(definition = "")

  counter <- 1
  types_with_sub_types <- actor_types |>
    filter(!is.na(sub_type_of)) |>
    select(type_name=sub_type_of) |>
    distinct()
  for (i in 1:nrow(types_with_sub_types)) {
    new_row_1 <- data.frame(
      type_name = as.character(counter),
      sub_type_of = types_with_sub_types$type_name[i],
      is_core_category = FALSE,
      public_instances=0,
      university_instances=0,
      third_instances=0,
      private_instances=0,
      hybrid_instances=0,
      unknown_instances=0,
      total_instances=0,
      definition = "dummy"
    )
    new_row_2 <- data.frame(
      type_name = paste("z", as.character(counter)),
      sub_type_of = types_with_sub_types$type_name[i],
      is_core_category = FALSE,
      public_instances=0,
      university_instances=0,
      third_instances=0,
      private_instances=0,
      hybrid_instances=0,
      unknown_instances=0,
      total_instances=0,
      definition = "dummy"
    )
    counter <- counter + 1
    actor_types <- actor_types |>
      rbind(new_row_1) |>
      rbind(new_row_2)
  }

  actor_types <- actor_types |>
    mutate(
      public_proportion = public_instances / total_instances,
      university_proportion = university_instances / total_instances,
      third_proportion = third_instances / total_instances,
      private_proportion = private_instances / total_instances,
      hybrid_proportion = hybrid_instances / total_instances,
      sector = ifelse(
        public_proportion >= 0.5,
        "Mostly public sector",
        ifelse(
          university_proportion >= 0.5,
          "Mostly university sector",
          ifelse(
            third_proportion >= 0.5,
            "Mostly third sector",
            ifelse(
              private_proportion >= 0.5,
              "Mostly private sector",
              "Mixed/Unknown"
            )
          )
        )
      )
    )
 
  core_actor_types <- actor_types |>
    filter(is_core_category == "TRUE") |>
    select(type_name)
  dummy_actor_types <- actor_types |>
    filter(definition == "dummy") |>
    select(type_name)
  
  actor_edges <- actor_types |>
    arrange(sub_type_of, type_name) |>
    filter(sub_type_of != "") |>
    select(
      from=sub_type_of,
      to=type_name
    ) |>
    mutate(is_to_dummy = to %in% dummy_actor_types$type_name)

  graph <- graph_from_data_frame(actor_edges, directed=TRUE)
  V(graph)$distance_to_root <- distances(graph, v=V(graph), to=which(V(graph)$name == "actor"))
  max_distance <- max(V(graph)$distance_to_root)
  layout <- create_layout(graph, layout="dendrogram", circular=FALSE) |>
    left_join(actor_types |> select(name=type_name, sector, is_core_category), by="name")
  layout$y <- layout$distance_to_root - max_distance
  layout$is_core_category <- layout$name %in% core_actor_types$type_name
  layout$is_dummy <- layout$name %in% dummy_actor_types$type_name
  
  ggraph(layout) + 
    geom_edge_diagonal(
      aes(colour = ifelse(is_to_dummy, "dummy", "normal")),
      show.legend=FALSE
    ) +
    geom_node_point(
      data=layout |> filter(!is_dummy),
      aes(
        fill=sector,
        colour=is_core_category
      ),
      shape=21,
      size=4,
      stroke=2
    ) +
    geom_node_text(
      data=layout |> filter(!is_dummy),
      aes(label=name),
      size=5,
      angle=0,
      vjust="center",
      hjust="left",
      nudge_y=0.05
    ) +
    coord_flip() +
    scale_y_continuous(limits=c(-max_distance, 1)) +
    scale_fill_manual(
      values=c(
        "Mostly public sector"="#F4A4C1",
        "Mostly university sector"="#99FFE3",
        "Mostly third sector"="#99DAFF",
        "Mostly private sector"="#FFE099",
        "Mixed/Unknown"="lightgrey"
      ), 
      name="",
      na.value="black"
    ) +
    scale_colour_manual(
      values=c("TRUE"="black", "FALSE"="lightgrey"),
      labels=c("TRUE"="core categories", "FALSE"="non-core categories"),
      name=""
    ) +
    scale_edge_colour_manual(
      values=c("dummy"="white", "normal"="lightgrey")
    ) +
    labs(
      title="Hierarchy of actor types involved in dispersal of museum collections"
    ) +
    type_hierarchy_theme
}

