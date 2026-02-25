mm_db_choices <- c(
  "museum_id",
  "museum_name",
  "governance_broad",
  "governance",
  "size",
  "subject_broad",
  "subject",
  "accreditation",
  "address_1",
  "address_2",
  "address_3",
  "village_town_city",
  "postcode",
  "lad",
  "region",
  "country",
  "year_opened",
  "year_closed",
  "notes"
)

mm_db_selected <- c(
  "museum_id",
  "museum_name",
  "governance_broad",
  "governance",
  "size",
  "subject",
  "accreditation",
  "address_1",
  "address_2",
  "address_3",
  "village_town_city",
  "postcode",
  "lad",
  "region",
  "country",
  "year_opened",
  "year_closed",
  "notes"
)

databaseUI <- function(id) {

  fluidPage(
    text_box(top_database),

    search_form_item(
      "Search",
      db_tooltip_search,
      textInput(
        NS(id, "freeText"),
        label="",
        value=""
      )
    ),

    h3("Advanced Filters"),

    hr(),

    tags$details(
      tags$summary("Museum attributes"),

      search_form_item(
        "Accreditation",
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
  
      search_form_item(
        "Governance",
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
      
      search_form_item(
        "Size",
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
      
      search_form_item(
        "Subject",
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
      
      search_form_item(
        "Subject (specific)",
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
      )
    ),
      
  
    hr(),

    tags$details(
      tags$summary("Museum location"),

      search_form_item(
        "Country",
        tooltip_museum_country,
        pickerInput(
          NS(id, "countryFilter"), 
          "",
          choices=country_labels()$label,
          selected=country_labels()$label,
          options=pickerOptions(
            actionsBox=TRUE, 
            size=10,
            selectedTextFormat="count > 3"
          ), 
          multiple=TRUE
        )   
      ),
  
      search_form_item(
        "Region",
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
      ),
  
      search_form_item(
        "Local Authority District",
        tooltip_local_authority_district,
        virtualSelectInput(
          NS(id, "ladFilter"),
          "",
          choices=lad_labels()$label,
          selected=lad_labels()$label,
          multiple=TRUE,
          disableSelectAll=FALSE,
          search=TRUE
        )
      ),
  
      search_form_item(
        "Address",
        tooltip_address,
        textInput(
          NS(id, "addressFilter"), 
          label="",
          value=""
        ) 
      ),
      
    ),
  
    hr(),
  
    tags$details(
      tags$summary("Time period"),

      search_form_item(
        "Filter by",
        tooltip_existence_or_open_close,
        radioButtons(
          NS(id, "existenceOrOpenClose"),
          label="",
          choices=c(
            "Museums that were open in time period",
            "Museum opening and closure dates"
          ),
          selected="Museums that were open in time period",
          inline=FALSE
        )
      ),

      uiOutput(NS(id, "timePeriodSearch"))

    ),

    hr(),

    br(),

    actionButton(NS(id, "reset"), "Reset filters"),

    search_form_item(
      "Show columns",
      tooltip_show_columns,
      virtualSelectInput(
        NS(id, "tableSelect"),
        label="",
        choices=mm_db_choices,
        selected=mm_db_selected,
        multiple=TRUE,
        disableSelectAll=FALSE,
        search=TRUE
      )
    ),

    downloadButton(NS(id, "download"), label="Download table as CSV"),

    p(""),
    hr(),

    DTOutput(NS(id, "searchTable"))

  )
}
