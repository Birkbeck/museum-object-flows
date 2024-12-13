actorsUI <- function(id) {
  fluidPage(
    fluidRow(
      p("Who is involved in the dispersal of museum collections?"),
      p("This tab introduces the types of actor in the dispersal data and how they associate with one another."),
      h3("Actor Types"),
      p("The types of actor involved in transactions with museums are shown below. The leaves of the tree include highly specific categories. These are grouped into core categories which are used in some of the visual summaries in the following tabs."),
      plotOutput(NS(id, "actorTypes"), width="80%", height="1200px")
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
    }, height=1200)
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
  core_actor_types <- actor_types |>
    clean_names() |>
    filter(is_core_category == "TRUE") |>
    select(type_name)
  
  actor_edges <- actor_types |>
    filter(sub_type_of != "") |>
    select(
      from=sub_type_of,
      to=type_name
    )
  
  graph <- graph_from_data_frame(actor_edges, directed=TRUE)
  V(graph)$distance_to_root <- distances(graph, v=V(graph), to=which(V(graph)$name == "actor"))
  max_distance <- max(V(graph)$distance_to_root)
  layout <- create_layout(graph, layout="dendrogram", circular=FALSE)
  layout$y <- layout$distance_to_root - max_distance
  
  ggraph(layout) + 
    geom_edge_diagonal(colour="lightgrey") +
    geom_node_point(
      aes(fill=ifelse(name %in% core_actor_types$type_name, "Core category", "")),
      size=4, shape=21, colour="black"
    ) +
    geom_node_text(
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
      values = c("Core category"="red"), 
      name = "",
      na.value = "lightgrey"
    ) +
    labs(
      title="Hierarchy of actor types involved in dispersal of museum collections"
    ) +
    type_hierarchy_theme +
    theme(
      legend.position="top"
    )
}

