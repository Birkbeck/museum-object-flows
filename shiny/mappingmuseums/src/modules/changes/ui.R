changesUI <- function(id) {
  fluidPage(
    fluidRow(
      p("Changes in the museum sector across the UK during a selected time period (between the start of the first year and the end of the last year). The numbers in the charts below show estimated numbers which take into account uncertain opening and closure dates."),
      p("Click on a smaller chart to view it enlarged in the main panel.")
    ),

    sidebarLayout(
      sidebarPanel(
        width=3,
        style = "height: 90vh; overflow-y: auto;",

        h3("View"),

        tagList(
          tags$span(
            tags$strong("Time Period: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Time Period",
              `data-content` = "<p>Move the slider to adjust the date range under consideration. Data in the charts reflects museum closures and new museum openings between 1st January of the year at the start of the range and 31st December of the year at the end of the range.</p>"
            )
          ),
          tags$script(popover_js),
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

        tagList(
          tags$span(
            tags$strong("Main museum attribute: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Main museum attribute",
              `data-content` = "<p>Select which attribute to group museums by</p>"
            )
          ),
          tags$script(popover_js),
          selectInput(
            NS(id, "mainAxis"),
            label="",
            choices=field_names$name,
            selected="All"
          )
        ),

        tagList(
          tags$span(
            tags$strong("Secondary museum attribute: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Secondary museum attribute",
              `data-content` = "<p>Select a second museum attribute to group museums by on the horizontal axis of the heatmap.</p>"
            )
          ),
          tags$script(popover_js),
          selectInput(
            NS(id, "secondAxis"),
            label="",
            choices=field_names$name,
            selected="Country/Region"
          )
        ),

        uiOutput(NS(id, "mainPlotOptions")),

        h3("Filters"),

        tagList(
          tags$span(
            tags$strong("Museum governance: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Museum governance",
              `data-content` = "<p>The governance structure of the museum</p>"
            )
          ),
          tags$script(popover_js),
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
        
        tagList(
          tags$span(
            tags$strong("Museum size: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Museum size",
              `data-content` = "<p>The size of the museum. Museum sizes are based on approximate annual visitor numbers:</p><p><strong>Small: </strong>0 - 10,000 annual visitors.</p><p><strong>Medium: </strong>10,000 - 50,000 annual visitors</p><p><strong>Large: </strong>50,000 - 1,000,000 annual visitors.</p><p><strong>Huge: </strong>More than 1,000,000 annual visitors.</p>"
            )
          ),
          tags$script(popover_js),
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
        
        tagList(
          tags$span(
            tags$strong("Museum subject: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Museum subject",
              `data-content` = "<p>The subject matter of the museum.</p>"
            )
          ),
          tags$script(popover_js),
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
        
        tagList(
          tags$span(
            tags$strong("Museum subject (specific): "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Museum subject (specific)",
              `data-content` = "<p>Specific categories of museum subject matter.</p>"
            )
          ),
          tags$script(popover_js),
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
        
        tagList(
          tags$span(
            tags$strong("Museum country/region: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Museum country or region",
              `data-content` = "<p>Where in the United Kingdom the museum is located.</p>"
            )
          ),
          tags$script(popover_js),
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
        
        tagList(
          tags$span(
            tags$strong("Museum accreditation: "),
            tags$i(
              class = "fa fa-info-circle",
              style = "color: #007bff; cursor: pointer;",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Museum accreditation",
              `data-content` = "<p>Whether or not the museum was accredited at the time of closure.</p>"
            )
          ),
          tags$script(popover_js),
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
          p("Click on one of the small charts below to see it enlarged in the main panel above.")
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
