library(janitor)
library(ggplot2)
library(igraph)
library(ggraph)
library(tidyverse)
library(readr)

taxonomy_theme <- theme(
  panel.background = element_rect(fill="white"),
  plot.margin = unit(c(1, 1, 1, 1), "cm"),
  plot.title = element_text(size="18"),
  legend.position = "right",
  legend.title = element_text(size="14"),
  legend.text = element_text(size="12"),
  legend.background = element_rect(fill="white"),
  legend.key = element_rect(fill="white")
)

draw_taxonomy <- function(types) {
  types <- types |>
    mutate(
      label=ifelse(
        str_detect(type_name, ": "),
        # get name after ": "
        str_replace(type_name, ".*?:\\s*", ""),
        type_name
      ),
      is_dummy=FALSE
    )
  dummy_types <- types |>
    filter(is_core_category) |>
    mutate(
      sub_type_of=type_name,
      type_name=as.character(row_number()),
      label=type_name,
      is_core_category=FALSE,
      is_dummy=TRUE,
    ) |>
    rbind(
      types |>
        filter(is_core_category) |>
        mutate(
          sub_type_of=type_name,
          type_name=paste("z", row_number()),
          label=type_name,
          is_core_category=FALSE,
          is_dummy=TRUE,
        )
    )
  types <- rbind(types, dummy_types)
  edges <- types |>
    mutate(
      sub_type_of=ifelse(is.na(sub_type_of), " ", sub_type_of)
    ) |>
    arrange(sub_type_of, type_name) |>
    select(
      from=sub_type_of,
      to=type_name
    ) |>
    mutate(is_to_dummy = to %in% dummy_types$type_name)

  graph <- graph_from_data_frame(edges, directed=TRUE)
  V(graph)$distance_to_root <- distances(graph, v=V(graph), to=which(V(graph)$name == " "))
  max_distance <- max(V(graph)$distance_to_root)
  layout <- create_layout(graph, layout="dendrogram", circular=FALSE) |>
    left_join(types |> select(name=type_name, is_core_category, label), by="name")
  layout$is_dummy <- layout$name %in% dummy_types$type_name
  layout$y <- layout$distance_to_root - max_distance
  layout$x <- -layout$x

  ggraph(layout) + 
    geom_edge_diagonal(
      aes(colour = ifelse(is_to_dummy, "dummy", "normal")),
      show.legend=FALSE
    ) +
    geom_node_point(
      data=layout |> filter(!is_dummy),
      aes(colour=is_core_category),
      shape=21,
      size=2,
      stroke=1
    ) +
    geom_node_text(
      data = layout |> filter(!is_dummy),
      aes(label=label),
      size=3,
      angle=0,
      vjust="center",
      hjust="left",
      nudge_y=0.02
    ) +
    coord_flip() +
    scale_y_continuous(limits=c(-2,1)) +
    scale_colour_manual(
      values=c("TRUE"="black", "FALSE"="lightgrey"),
      labels=c("TRUE"="core categories", "FALSE"="sub-categories"),
      name="",
      guide=guide_legend(reverse=TRUE),
      na.translate=FALSE
    ) +
    scale_edge_colour_manual(
      values=c("dummy"="white", "normal"="lightgrey")
    ) +
    taxonomy_theme
}

building_use_types_csv <- "classifications/building-use-types.csv"
building_use_types <- read_csv(building_use_types_csv)
building_uses_hierarchy <- draw_taxonomy(building_use_types)
ggsave(
  file="classifications/building_use_types.png",
  plot=building_uses_hierarchy,
  width=14,
  height=13
)
