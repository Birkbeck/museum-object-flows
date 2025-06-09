changesUI <- function(id) {
  fluidPage(

    text_box("CHANGES-TOP"),

    sidebarLayout(
      sidebarPanel(
        width=3,
        style = "height: 90vh; overflow-y: auto;",

        div(
          style = "text-align: right;",
          actionButton(NS(id, "reset"), "Reset options")
        ),

        h3("View"),

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
          "<p>Select which attribute to group museums by</p>",
          selectInput(
            NS(id, "mainAxis"),
            label="",
            choices=field_names$name,
            selected="All"
          )
        ),

        form_item(
          "Secondary museum attribute (for heatmaps only)",
          "<p>Select a second museum attribute to group museums by on the horizontal axis of the heatmap.</p>",
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

        h3("Filters"),

        form_item(
          "Museum governance",
          "<p>The governance structure of the museum</p>",
          pickerInput(
            NS(id, "governanceFilter"), 
            "",
            choices=filter(governance_labels, internal_label != "Independent")$tidy_label,
            selected=filter(governance_labels, internal_label != "Independent")$tidy_label,
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
            choices=size_labels$tidy_label,
            selected=filter(size_labels, default_filter)$tidy_label,
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
            choices=subject_broad_labels$tidy_label,
            selected=filter(subject_broad_labels, default_filter)$tidy_label,
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
            choices=filter(country_region_labels, internal_label != "England")$tidy_label,
            selected=filter(country_region_labels, internal_label != "England")$tidy_label,
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
            choices=accreditation_labels$tidy_label,
            selected=filter(accreditation_labels, default_filter)$tidy_label,
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
              NS(id, "absoluteChangeSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "absoluteChange")
            )
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "percentageChangeSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "percentageChange")
            )
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "absoluteChangeSmall2Way"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "absoluteChange2Way")
            )
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "percentageChangeSmall2Way"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "percentageChange2Way")
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
