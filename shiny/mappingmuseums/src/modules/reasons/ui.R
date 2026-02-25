reasonsUI <- function(id) {
  fluidPage(

    text_box(top_reasons),

    sidebarLayout(
      sidebarPanel(
        width=3,
        style = sidebar_style,

        div(class="scroll-hint", "▼ Scroll for more options"),

        div(
          style = "text-align: right;",
          actionButton(NS(id, "reset"), "Reset options")
        ),

        form_subtitle("View", tooltip_view),

        div(uiOutput(NS(id, "mainPlotOptions"))),
        
        form_item(
          "Reasons - level of detail",
          tooltip_reason_type_level,
          radioButtons(
            NS(id, "reasonLevel"),
            label="",
            choices=c(
              "Core categories",
              "Core categories and their sub-categories",
              "Most specific"
            ),
            selected="Core categories"
          )
        ),

        shinyjs::hidden(
          tags$div(
            id=NS(id, "museumGroupingFormItem"),
            form_item(
              "Museums attribute",
              tooltip_secondary_attribute,
              radioButtons(
                NS(id, "museumGrouping"),
                label="",
                choices=field_names()$name,
                selected="Governance"
              )
            )
          )
        ),

        form_subtitle("Filter", tooltip_filter),

        tags$details(
          tags$summary("Reasons"),
          form_item(
            "Reason core category",
            tooltip_reason_filter,
            pickerInput(
              NS(id, "reasonFilter"),
              label="",
              choices=reason_core_labels()$label,
              selected=reason_core_labels()$label,
              options=pickerOptions(
                actionsBox=TRUE, 
                size=10,
                selectedTextFormat="count > 3"
              ), 
              multiple=TRUE
            ) 
          )
        ),

        tags$details(
          tags$summary("Museum attributes"),

          form_item(
            "Museum accreditation",
            tooltip_museum_accreditation,
            pickerInput(
              NS(id, "accreditationFilter"), 
              "",
              choices=accreditation_labels()$label,
              selected=accreditation_labels()$label,
              options=pickerOptions(
                actionsBox=TRUE, 
                size=10,
                selectedTextFormat="count > 3"
              ), 
              multiple=TRUE
            )   
          ),

          form_item(
            "Museum governance",
            tooltip_museum_governance,
            pickerInput(
              NS(id, "governanceFilter"), 
              "",
              choices=governance_broad_labels()$label,
              selected=governance_broad_labels()$label,
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
              choices=size_labels()$label,
              selected=size_labels()$label,
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
              choices=subject_broad_labels()$label,
              selected=subject_broad_labels()$label,
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
              choices=region_labels()$label,
              selected=region_labels()$label,
              options=pickerOptions(
                actionsBox=TRUE, 
                size=10,
                selectedTextFormat="count > 3"
              ), 
              multiple=TRUE
            )
          )

        )
      ),

      mainPanel(
        uiOutput(NS(id, "errorMessage")),
        div(
          withSpinner(uiOutput(NS(id, "mainPlot"))),
          style = "height: 1200px; width: 100%;"
        ),
        fluidRow(
          column(
            3,
            style=card_style,
            withSpinner(
              plotOutput(
                NS(id, "reasonsBarChartSmall"),
                width=small_chart_size_px,
                height=small_chart_size_px,
                click=NS(id, "reasonsBarChart")
              )
            )
          ),
          column(
            3,
            style=card_style,
            withSpinner(
              plotOutput(
                NS(id, "reasonsHeatmapSmall"),
                width=small_chart_size_px,
                height=small_chart_size_px,
                click=NS(id, "reasonsHeatmap")
              )
            )
          ),
          column(
            3,
            style=card_style,
            withSpinner(
              plotOutput(
                NS(id, "reasonsLineChartSmall"),
                width=small_chart_size_px,
                height=small_chart_size_px,
                click=NS(id, "reasonsLineChart")
              )
            )
          )
        )
      )
    ),
    fluidRow(
      h3("Reasons for Closure"),
      downloadButton(NS(id, "downloadReasonsTable"), label="Download table as CSV")
    ),
    fluidRow(
      DTOutput(NS(id, "closureReasonsTable"))
    )
  )
}
