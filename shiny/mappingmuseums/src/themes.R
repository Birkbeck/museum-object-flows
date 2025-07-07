small_chart_size_px <- "300px"
hr_style <- "border-top: 1px solid #000000;"
card_style <- "border: 4px double lightgrey;"

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
#public_private_colours_js <- JS(
#  'd3.scaleOrdinal().domain(["private", "third", "public", "university", "hybrid", "unknown"]).range(["#E69F00", "#0072B2", "#D81B60", "#009E73", "#CC79A7", "darkgrey"])'
#)

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
  "unknown size"="#E84A84",
  "small"="#00FFD4",
  "medium"="#00B394",
  "large"="#006655",
  "huge"="#001A15"
)
governance_colours <- c(
  "unknown size"="lightgrey",
  "private"="#E69F00",
  "independent"="#0D4373",
  "not-for-profit"="#0D4373",
  "National Trust for Scotland"="#1878CE",
  "National Trust"="#1878CE",
  "English Heritage"="#60ABEC",
  "Historic Environment Scotland"="#BBDBF7",
  "university"="#009E73",
  "national"="#720E32",
  "local authority"="#CD185A",
  "other government"="#ED5F93"
)
subject_colours <- c(
  "war & conflict" = "#A44600",
  "utilities" = "#F8E97C",
  "transport" = "#4C99D1",
  "services" = "#66C2A2",
  "sea & seafaring" = "#9ED2F3",
  "science & technology" = "#007652",
  "rural industry" = "#BFB82E",
  "personality" = "#E0AFC7",
  "other" = "#666666",
  "natural world" = "#009E73",
  "mixed" = "#F5C85C",
  "medicine & health" = "#3E88B2",
  "local histories" = "#00578A",
  "leisure & sport" = "#F0E442",
  "industry & manufacture" = "#D55E00",
  "food & drink" = "#B37800",
  "communications" = "#56B4E9",
  "buildings" = "#0072B2",
  "belief & identity" = "#9A5C7D",
  "arts" = "#CC79A7",
  "archaeology" = "#E5955C"
)
country_region_colours <- c(
  "Scotland"="#0072B2",
  "Wales"="#009E73",
  "Northern Ireland"="#CC79A7",
  "England"="#D55E00",
  "North East"="#56B4E9",
  "North West"="#56B4E9",
  "Yorks & Humber"="#56B4E9",
  "East Midlands"="#E69F00",
  "West Midlands"="#E69F00",
  "South East"="#F0E442",
  "South West"="#F0E442",
  "East of England"="#F0E442",
  "London"="#F0E442"
)
accreditation_colours <- c(
  "accredited"="#E66100",
  "unaccredited"="#5D3A9B"
)
museum_attribute_colours <- c(
  size_colours,
  governance_colours,
  subject_colours,
  country_region_colours,
  accreditation_colours
)

white = "#FFFFFF"
red = "#FF99A1"
green = "#99FFEE"
blue = "#99E0FF"
dark_blue = "#008ECC"
purple = "#E4A8F0"

#heatmap_fill_scale <- scale_fill_continuous(transform="pseudo_log", low=white, high=purple)
heatmap_fill_scale <- scale_fill_continuous(low=white, high=purple)

open_close_line_type_scale <- scale_linetype_manual(
  values=c(
    "total"="solid",
    "change"="solid",
    "percentage_change"="solid",
    "opening_count"="dotted",
    "opening_rate"="dotted",
    "closure_count"="longdash",
    "closure_rate"="longdash"
  )
)

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

generate_title <- function(title_text) {
  m <- as.numeric(format(Sys.Date(), "%m"))
  d <- as.numeric(format(Sys.Date(), "%d"))
  if (m==12 || m == 1 && d <= 6) {
    return(paste("\U0001F384", title_text, "\U0001F384"))
  } else {
    return(title_text)
  }
}

# restrict maps to britain and ireland
BNG_MIN_X <- -5e4
BNG_MAX_X <- 7e5
BNG_MIN_Y <- 0
BNG_MAX_Y <- 1.3e6
map_x_scale <- scale_x_continuous(limits=c(BNG_MIN_X, BNG_MAX_X))
map_y_scale <- scale_y_continuous(limits=c(BNG_MIN_Y, BNG_MAX_Y))
