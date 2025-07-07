outcomesUI <- function(id) {
  fluidPage(

    text_box("OUTCOMES-TOP - note: there are some cases of 'all' the collection going to multiple recipients. These are either due to a recipient having a quantity of 'many' or because of collections being divided into all+few or all+some"),

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
          "Main axis",
          "<p>Select how outcomes should be displayed on the diagrams.</p><p><strong>Main event:</strong> Show outcomes of closure in terms of events</p><p><strong>Main actor:</strong> Show outcomes of closure in terms of recipients</p>",
          selectInput(
            NS(id, "outcomeType"),
            label="",
            choices=c(
              "Outcome event type",
              "Outcome recipient type",
              "Outcome recipient count",
              "Outcome largest recipient share",
              "Outcome destination type"
            ),
            selected="Outcome event type"
          )
        ),

        form_item(
          "Secondary axis (for heatmaps only)",
          "<p>For the 2-dimensional heatmap.</p><p>Select which museum attribute to show on the <i>x</i>-axis.</p>",
          disabled(
            selectInput(
              NS(id, "museumGrouping"),
              label="",
              choices=c(
                field_names$name,
                "Core reason for closure",
                "Outcome event type",
                "Outcome recipient type",
                "Outcome recipient count",
                "Outcome largest recipient share",
                "Outcome destination type"
              ),
              selected="Governance"
            )
          )
        ),

        form_item(
          "Show only outcomes",
          "<p>Select which outcomes should appear in the visualizations. Removing some outcomes could improve the readability of charts.</p>",
          pickerInput(
            NS(id, "outcomeFilter"),
            label="",
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

        div(uiOutput(NS(id, "mainPlotOptions"))),

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
        )
      ),

      mainPanel(
        uiOutput(NS(id, "errorMessage")),
        div(uiOutput(NS(id, "mainPlot")), style = "height: 1200px; width: 100%;"),
        div(uiOutput(NS(id, "mainPlotExplanation")), style = "margin-top: 20px;"),
        fluidRow(
          text_box("OUTCOMES-BOTTOM Click on one of the small charts below to see it enlarged in the main panel above.")
        ),
        fluidRow(
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "outcomesBarChartSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "outcomesBarChart")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "outcomesHeatmapSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "outcomesHeatmap")
            ),
          ),
          column(
            3,
            style=card_style,
            plotOutput(
              NS(id, "outcomesLineChartSmall"),
              width=small_chart_size_px,
              height=small_chart_size_px,
              click=NS(id, "outcomesLineChart")
            )
          )
        )
      )
    ),
    fluidRow(
      h3("Outcomes of Museum Closure"),
      downloadButton(NS(id, "downloadOutcomesTable"), label="Download table as CSV")
    ),
    fluidRow(
      DTOutput(NS(id, "closureOutcomesTable"))
    )
  )
}
