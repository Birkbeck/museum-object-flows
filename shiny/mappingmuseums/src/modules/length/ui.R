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
          "Group museums by",
          "<p>Select which attribute museums should be grouped by.</p>",
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
          "<p>The governance structure of the museum</p>",
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
          "<p>The size of the museum. Museum sizes are based on approximate annual visitor numbers:</p><p><strong>Small: </strong>0 - 10,000 annual visitors.</p><p><strong>Medium: </strong>10,000 - 50,000 annual visitors</p><p><strong>Large: </strong>50,000 - 1,000,000 annual visitors.</p><p><strong>Huge: </strong>More than 1,000,000 annual visitors.</p>",
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
          "<p>The subject matter of the museum.</p>",
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
          "<p>Specific categories of museum subject matter.</p>",
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
          "Museum country/region",
          "<p>Where in the United Kingdom the museum is located.</p>",
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
          "<p>Whether or not the museum was accredited at the time of closure.</p>",
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
          "<p>Select a museum to display its closure timelines.</p>",
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
