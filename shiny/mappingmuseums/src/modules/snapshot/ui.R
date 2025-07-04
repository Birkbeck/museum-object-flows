snapshotUI <- function(id) {
  fluidPage(

    text_box("SNAPSHOT-TOP"),

    sidebarLayout(
      sidebarPanel(
        width=3,
        style = sidebar_style,

        div(
          style = "text-align: right;",
          actionButton(NS(id, "reset"), "Reset options")
        ),

        h3("View"),

        form_item(
          "Single year or range of years",
          tooltip_single_or_range,
          radioButtons(
            NS(id, "yearOrRange"),
            label="",
            choices=c("Single year", "Range of years"),
            selected="Single year",
            inline=TRUE
          )
        ),

        uiOutput(NS(id, "yearSlider")),

        form_item(
          "Main museum attribute",
          tooltip_main_attribute,
          selectInput(
            NS(id, "mainAxis"),
            label="",
            choices=field_names$name,
            selected="Governance"
          )
        ),

        form_item(
          "!! Show only",
          "<p>Add or remove values from the chart</p>",
          pickerInput(
            NS(id, "mainAxisShowOnly"),
            "", 
            choices=NULL,
            selected=NULL,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          )
        ),

        form_item(
          "Secondary museum attribute (for heatmaps only)",
          tooltip_secondary_attribute,
          disabled(
            selectInput(
              NS(id, "secondAxis"),
              label="",
              choices=field_names$name,
              selected="Country/Region"
            )
          )
        ),

        form_item(
          "!! Show only",
          "<p>Add or remove values from the chart</p>",
          pickerInput(
            NS(id, "secondAxisShowOnly"),
            "", 
            choices=NULL,
            selected=NULL,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          )
        ),

        uiOutput(NS(id, "mainPlotOptions")),

        h3("Filters"),

        form_item(
          "Museum governance",
          tooltip_museum_governance,
          pickerInput(
            NS(id, "governanceFilter"), 
            "",
            choices=governance_broad_labels$label,
            selected=governance_broad_labels$label,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ) 
        ),
        
        form_item(
          "Museum size",
          tooltip_museum_size,
          pickerInput(
            NS(id, "sizeFilter"), 
            "",
            choices=size_labels$label,
            selected=size_labels$label,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          ) 
        ),
        
        form_item(
          "Museum subject",
          tooltip_museum_subject,
          pickerInput(
            NS(id, "subjectFilter"), 
            "",
            choices=subject_broad_labels$label,
            selected=subject_broad_labels$label,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          )  
        ),
        
        form_item(
          "Museum subject (specific)",
          tooltip_museum_subject_specific,
          pickerInput(
            NS(id, "subjectSpecificFilter"), 
            "",
            choices=NULL,
            selected=NULL,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          )
        ),
        
        form_item(
          "Museum country or region",
          tooltip_museum_country_region,
          pickerInput(
            NS(id, "regionFilter"), 
            "",
            choices=region_labels$label,
            selected=region_labels$label,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          )   
        ),
        
        form_item(
          "Museum accreditation",
          tooltip_museum_accreditation,
          pickerInput(
            NS(id, "accreditationFilter"), 
            "",
            choices=accreditation_labels$label,
            selected=accreditation_labels$label,
            options=pickerOptions(
              actionsBox=TRUE, 
              size=10,
              selectedTextFormat="count > 3"
            ), 
            multiple=TRUE
          )   
        )
      ),

      mainPanel(
        plotlyOutput(NS(id, "mainPlot"), height="800px", width="100%"),
        uiOutput(NS(id, "mainPlotExplanation")),
        fluidRow(
          text_box("SNAPSHOT-BOTTOM Click on one of the small charts below to see it enlarged in the main panel above.")
        ),
        fluidRow(
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "museumMapSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "museumMap")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "museumCountsSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "museumCounts")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "museumHeatmapSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "museumHeatmap")
            )
          )
        )
      )
    ),
    fluidRow(
      h3("Museums Open During Period"),
      downloadButton(NS(id, "downloadSnapshotTable"), label="Download table as CSV"),
      DTOutput(NS(id, "openMuseumsTable"))
    )
  )
}
