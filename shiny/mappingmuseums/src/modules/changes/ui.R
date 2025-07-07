changesUI <- function(id) {
  fluidPage(

    text_box("CHANGES-TOP"),

    sidebarLayout(
      sidebarPanel(
        width=3,
        style = sidebar_style,

        div(
          style = "text-align: right;",
          actionButton(NS(id, "reset"), "Reset options")
        ),

        form_subtitle("View", tooltip_view),

        form_item(
          "Time Period",
          "<p>Move the slider to adjust the date range under consideration. Data in the charts reflects museum closures and new museum openings between 1st January of the year at the start of the range and 31st December of the year at the end of the range.</p>",
          sliderInput(
            NS(id, "yearRange"),
            label="",
            value=c(2000, 2025),
            min=1960,
            max=2025,
            step=1,
            sep="",
            ticks=TRUE,
            width="100%"
          )
        ),

        form_item(
          "Main museum attribute",
          tooltip_main_attribute,
          selectInput(
            NS(id, "mainAxis"),
            label="",
            choices=field_names$name,
            selected="All"
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

        uiOutput(NS(id, "mainPlotOptions")),

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
          "Museum country/region",
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
        uiOutput(NS(id, "errorMessage")),
        plotlyOutput(NS(id, "mainPlot"), height="720px", width="100%"),
        uiOutput(NS(id, "mainPlotExplanation")),
        fluidRow(
          text_box("CHANGES-BOTTOM Click on one of the small charts below to see it enlarged in the main panel above.")
        ),
        fluidRow(
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "openingsClosuresSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "openingsClosures")
            )
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "startEndSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "startEnd")
            )
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "changeSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "change")
            )
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "openingsVsClosuresScatterSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "openingsVsClosuresScatter")
            )
          ),

          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "timeSeriesSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "timeSeriesLine")
            )
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "openingRatesSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "openingRateLine")
            )
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "closureRatesSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "closureRateLine")
            )
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "openingClosureRatesSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "openingClosureRateLine")
            )
          ),

          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "openingsSmall2Way"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "openings2Way")
            )
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "closuresSmall2Way"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "closures2Way")
            )
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "openStartSmall2Way"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "openStart2Way")
            )
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "openEndSmall2Way"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "openEnd2Way")
            )
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "changeSmall2Way"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "change2Way")
            )
          ),

          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "openingsMap"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "openingsMap")
            )
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "closuresMap"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "closuresMap")
            )
          )
        )
      )
    ),
    fluidRow(
      h3("Museum Closures"),
      downloadButton(NS(id, "downloadClosuresTable"), label="Download table as CSV"),
      DTOutput(NS(id, "closuresTable"))
    ),
    fluidRow(
      h3("Museum Openings"),
      downloadButton(NS(id, "downloadOpeningsTable"), label="Download table as CSV"),
      DTOutput(NS(id, "openingsTable"))
    )
  )
}
