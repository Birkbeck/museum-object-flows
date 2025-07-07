lengthUI <- function(id) {
  fluidPage(

    text_box("LENGTH-TOP"),

    sidebarLayout(
      sidebarPanel(
        width=3,
        style = sidebar_style,

        div(
          style = "text-align: right;",
          actionButton(NS(id, "reset"), "Reset options")
        ),

        form_subtitle("View", tooltip_view),

        div(uiOutput(NS(id, "mainPlotOptions"))),

        form_item(
          "Museums attribute",
          tooltip_main_attribute,
          selectInput(
            NS(id, "museumGrouping"),
            label="",
            choices=c(field_names$name),
            selected="All"
          )
        ),

        form_subtitle("Filter", tooltip_filter),

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
          "Museum location",
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
        ),

        form_item(
          "Example museum",
          tooltip_example_museum,
          virtualSelectInput(
            NS(id, "exampleMuseum"),
            "",
            choices=NULL,
            selected=NULL,
            multiple=FALSE,
            disableSelectAll=FALSE,
            search=TRUE
          )
        )
      ),

      mainPanel(
        uiOutput(NS(id, "errorMessage")),
        plotlyOutput(NS(id, "mainPlot"), height="720px", width="100%"),
        uiOutput(NS(id, "mainPlotExplanation")),
        fluidRow(
          text_box("LENGTH-BOTTOM Click on one of the small charts below to see it enlarged in the main panel above.")
        ),
        fluidRow(
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "lengthTileChartSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "lengthTileChart")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "lengthLineChartSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "lengthLineChart")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "lengthScatterSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "lengthScatter")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "exampleTimelinesSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "exampleTimelines")
            )
          )
        )
      )
    ),
    fluidRow(
      h3("Lengths of Closure"),
      downloadButton(NS(id, "downloadLengthsTable"), label="Download table as CSV")
    ),
    fluidRow(
      DTOutput(NS(id, "closureLengthsTable"))
    )
  )
}
