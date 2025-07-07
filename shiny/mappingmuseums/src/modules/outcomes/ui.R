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
          tooltip_main_attribute_outcomes,
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
          "Museums attribute (for heatmaps only)",
          tooltip_secondary_attribute,
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
          "!! Show only outcomes",
          tooltip_show_only_outcomes,
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
