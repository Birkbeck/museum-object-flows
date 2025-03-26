small_chart_size_px <- "300px"
hr_style <- "border-top: 1px solid #000000;"

public_private_colours <- c(
  "private"="#E69F00",
  "mostly private"="#FFE099",
  "third"="#0072B2",
  "mostly third"="#80D1FF",
  "public"="#D81B60",
  "mostly public"="#F4A4C1",
  "university"="#009E73",
  "mostly university"="#66FFD5",
  "hybrid"="#CC79A7",
  "mostly hybrid"="#EAC8DB",
  "unknown"="darkgrey"
)
public_private_colours_js <- JS(
  'd3.scaleOrdinal().domain(["private", "third", "public", "university", "hybrid", "unknown"]).range(["#E69F00", "#0072B2", "#D81B60", "#009E73", "#CC79A7", "darkgrey"])'
)

public_private_fill_scale <- scale_fill_manual(
  values=public_private_colours,
  na.value="darkgrey",
  labels=c(
    "public"="public ",
    "private"="private ",
    "third"="third ",
    "university"="university  ",
    "hybrid"="hybrid  ",
    "unknown"="unknown  "
  ),
  breaks=c(
    "public", "university", "unknown", "hybrid", "third", "private"
  ),
  name="Sector:"
)
public_private_colour_scale <- scale_colour_manual(
  values=public_private_colours,
  na.value="darkgrey",
  labels=c(
    "public"="public sector",
    "private"="private sector",
    "third"="third sector",
    "university"="university",
    "hybrid"="Hybrid",
    "unknown"="unknown"
  ),
  breaks=c(
    "public", "university", "unknown", "hybrid", "third", "private"
  ),
  name="Sector"
)
public_private_edge_scale <- scale_edge_colour_manual(
  values=public_private_colours,
  na.value="darkgrey",
  labels=c(
    "Museum"="Museum",
    "Public"="Public sector",
    "Private"="Private sector",
    "Third"="Third sector",
    "University"="University",
    "Hybird"="Hybrid"
  ),
  name="Organisation Type"
)

size_colours <- c(
  "unknown"="#E84A84",
  "small"="#00FFD4",
  "medium"="#00B394",
  "large"="#006655",
  "huge"="#001A15"
)
governance_colours <- c(
  "Unknown"="lightgrey",
  "Private"="#E69F00",
  "Independent"="#0D4373",
  "Independent-Not_for_profit"="#0D4373",
  "Independent-National_Trust_for_Scotland"="#1878CE",
  "Independent-National_Trust"="#1878CE",
  "Independent-English_Heritage"="#60ABEC",
  "Independent-Historic_Environment_Scotland"="#BBDBF7",
  "University"="#009E73",
  "National"="#720E32",
  "Local_Authority"="#CD185A",
  "Other Government"="#ED5F93"
)
subject_colours <- c(
  "War_and_conflict" = "#A44600",
  "Utilities" = "#F8E97C",
  "Transport" = "#4C99D1",
  "Services" = "#66C2A2",
  "Sea_and_seafaring" = "#9ED2F3",
  "Science_and_technology" = "#007652",
  "Rural_Industry" = "#BFB82E",
  "Personality" = "#E0AFC7",
  "Other" = "#666666",
  "Natural_world" = "#009E73",
  "Mixed" = "#F5C85C",
  "Medicine_and_health" = "#3E88B2",
  "Local_Histories" = "#00578A",
  "Leisure_and_sport" = "#F0E442",
  "Industry_and_manufacture" = "#D55E00",
  "Food_and_drink" = "#B37800",
  "Communications" = "#56B4E9",
  "Buildings" = "#0072B2",
  "Belief_and_identity" = "#9A5C7D",
  "Arts" = "#CC79A7",
  "Archaeology" = "#E5955C"
)
country_region_colours <- c(
    "Scotland"="#0072B2",
    "Wales"="#009E73",
    "Northern Ireland"="#CC79A7",
    "England"="#D55E00",
    "North East (English Region)"="#56B4E9",
    "North West (English Region)"="#56B4E9",
    "Yorkshire and The Humber (English Region)"="#56B4E9",
    "East Midlands (English Region)"="#E69F00",
    "West Midlands (English Region)"="#E69F00",
    "South East (English Region)"="#F0E442",
    "South West (English Region)"="#F0E442",
    "East of England (English Region)"="#F0E442",
    "London (English Region)"="#F0E442"
)
accreditation_colours <- c(
    "Accredited"="#E66100",
    "Unaccredited"="#5D3A9B"
)
museum_attribute_colours <- c(
    size_colours,
    governance_colours,
    subject_colours,
    country_region_colours,
    accreditation_colours
)

heatmap_fill_scale <- scale_fill_continuous(low="white", high="#E4A8F0")

title_size <- 22
subtitle_size <- 18
axis_title_size <- 18
axis_text_size <- 18
legend_title_size <- 18

standard_bars_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size=title_size),
    plot.subtitle = element_text(size=subtitle_size),
    legend.text = element_text(size=18),
    axis.title.x = element_text(size=axis_title_size),
    axis.title.y = element_text(size=axis_title_size),
    axis.text.x = element_text(size=axis_text_size),
    axis.text.y = element_text(size=axis_text_size)
  )

timeline_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size=title_size),
    axis.title = element_text(size=axis_title_size),
    axis.text = element_text(size=axis_text_size)
  )

network_theme <- theme(
  panel.background = element_rect(fill="white"),
  plot.title = element_text(size=title_size),
  plot.subtitle = element_text(size=subtitle_size),
  legend.position = "right",
  legend.title = element_text(size=legend_title_size),
  legend.text = element_text(size=axis_text_size),
  legend.key = element_rect(fill="white"),
  axis.title = element_text(size=axis_title_size),
  axis.text = element_text(size=axis_text_size)
)

type_hierarchy_theme <- theme(
  panel.background = element_rect(fill="white"),
  plot.margin = unit(c(1, 1, 1, 1), "cm"),
  plot.title = element_text(size="18"),
  legend.position = "right",
  legend.title = element_text(size="14"),
  legend.text = element_text(size="12"),
  legend.background = element_rect(fill="white"),
  legend.key = element_rect(fill="white")
)

generate_title <- function(title_text) {
  m <- as.numeric(format(Sys.Date(), "%m"))
  d <- as.numeric(format(Sys.Date(), "%d"))
  if (m==12 || m == 1 && d <= 6) {
    return(paste("\U0001F384", title_text, "\U0001F384"))
  } else {
    return(title_text)
  }
}
