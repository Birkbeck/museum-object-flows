library(commonmark)
library(DT)
library(ggplot2)
library(ggraph)
library(ggrepel)
library(googlesheets4)
library(htmltools)
library(htmlwidgets)
library(igraph)
library(janitor)
library(jsonlite)
library(Matrix)
library(plotly)
library(readr)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(SnowballC)
library(tidyverse)
library(yaml)

source("src/labels.R")
source("src/texts.R")
source("src/mapping_museums_tables.R")
source("src/themes.R")
source("src/calculate_outcomes.R")
source("src/calculate_closure_lengths.R")
source("src/load_data.R")
source("src/ui_elements.R")

source("src/modules/home/ui.R")
source("src/modules/home/server.R")

source("src/modules/database/ui.R")
source("src/modules/database/server.R")

source("src/modules/snapshot/ui.R")
source("src/modules/snapshot/server.R")

source("src/modules/changes/ui.R")
source("src/modules/changes/server.R")

source("src/modules/reasons/ui.R")
source("src/modules/reasons/server.R")

source("src/modules/length/ui.R")
source("src/modules/length/server.R")

source("src/modules/outcomes/ui.R")
source("src/modules/outcomes/server.R")

source("src/modules/events/ui.R")
source("src/modules/events/server.R")

source("src/modules/dispersal/ui.R")
source("src/modules/dispersal/server.R")

source("src/modules/glossary/ui.R")
source("src/modules/glossary/server.R")

source("src/modules/taxonomies/ui.R")
source("src/modules/taxonomies/server.R")

source("src/modules/data_collection/ui.R")
source("src/modules/data_collection/server.R")

source("src/modules/data_collection_analysis/ui.R")
source("src/modules/data_collection_analysis/server.R")

source("src/modules/interpreting_data/ui.R")
source("src/modules/interpreting_data/server.R")

source("src/modules/help/ui.R")
source("src/modules/help/server.R")

PRODUCTION <- FALSE
USE_PASSWORD <- FALSE 

user_base <- readRDS("users.rds")

gs4_auth(path = "gsheets-service-account.json")

config <- fromJSON("config.json")
ANALYTICS_SHEET_ID = config$analytics_sheet_id

app_style <- "
body, p, li { font-size: 18px;}

/* Remove default browser arrow */
summary::-webkit-details-marker {
  display: none;
}
summary::marker {
  display: none;
}

/* Add a custom arrow before the text */
summary::before {
  content: '▶ ';   /* closed state */
  font-size: 14px;
  display: inline-block;
  margin-right: 5px;
}

/* Change arrow when details is open */
details[open] summary::before {
  content: '▼ ';   /* open state */
}

.scroll-hint {
  text-align: left;
  font-size: 12px;
  color: #777;
  animation: bounce 1.5s infinite;
}
@keyframes bounce {
  0%, 100% { transform: translateY(0); }
  50% { transform: translateY(3px); }
}

/* top-level tabs (first nav-tabs inside .tabbable) — keep default */
.nav-tabs > li > a { background-color: inherit; }
/* second-level (nested) tabs: select nav-tabs that are inside a tab-pane */
.tab-pane .nav-tabs > li > a {
  background-color: #eee9ff !important;
}
.tab-pane .nav-tabs > li.active > a,
.tab-pane .nav-tabs > li.active > a:focus,
.tab-pane .nav-tabs > li.active > a:hover {
  background-color: #ffffdd !important;
}
"

make_app_content_ui <- function() {
  fluidPage(
    tags$head(
      tags$style(HTML(app_style)),
    ),
    useShinyjs(),
    if (USE_PASSWORD) {
      actionButton("logout", "Logout")
    },
    titlePanel(generate_title("Mapping Museums Database")),
    tags$head(
      tags$style(type="text/css", ".nav-tabs {font-size: 16px}")
    ),
    tabsetPanel(
      id="tabPanelMain",
      tabPanel(
        value="home",
        tags$span("Home", title="Go back to the home page"),
        homeUI("home")
      ),
      tabPanel(
        value="mappingMuseums",
        tags$span("All Museums", title=""),
        tabsetPanel(
          id="tabPanelMappingMuseums",
          tabPanel(
            value="database",
            tags$span("Database search", title="Search the Mapping Museums database"),
            databaseUI("database")
          ),
          tabPanel(
            value="sectorSnapshot",
            tags$span(
              "Sector Snapshot",
              title="Data on museums open in a chosen time period"
            ),
            snapshotUI("snapshot")
          ),
          tabPanel(
            value="sectorChanges",
            tags$span(
              "Sector Changes",
              title="Changes in museum numbers over a chosen time period"
            ),
            changesUI("changes")
          )
        )
      ),
      tabPanel(
        value="museumClosure",
        tags$span("Museum Closure", title=""),
        tabsetPanel(
          id="tabPanelMuseumClosure",
          tabPanel(
            value="reasonsForClosure",
            tags$span(
              "Reasons for Closure",
              title="Reasons why museums have closed"
            ),
            reasonsUI("reasons")
          ),
          tabPanel(
            value="collectionsAfterClosure",
            tags$span(
              "Collections after Closure",
              title="What happens to collections after closure"
            ),
            outcomesUI("outcomes")
          ),
          tabPanel(
            value="lengthOfDisposal",
            tags$span(
              "Disposal Period",
              title="How long it takes for museums to close"
            ),
            lengthUI("length")
          )
        )
      ),
      tabPanel(
        value="detailsOfDispersal",
        tags$span("Details of Disposal", title=""),
        tabsetPanel(
          id="tabPanelDetailsOfDispersal",
          tabPanel(
            value="eventsAfterClosure",
            tags$span(
              "Events after closure",
              title="What happens after closure"
            ),
            eventsUI("events")
          ),
          tabPanel(
            value="objectDestinations",
            tags$span(
              "Object destinations",
              title="The flow of objects away from closed museums"
            ),
            dispersalUI("dispersal")
          )
        )
      ),
      tabPanel(
        value="aboutTheData",
        tags$span(
          "About the data",
          title="A description of the data concerning object disposal"
        ),
        tabsetPanel(
          id="tabPanelAboutTheData",
          tabPanel(
            value="glossary",
            tags$span("Glossary", title="Definitions of key terms"),
            glossaryUI("glossary")
          ),
          tabPanel(
            value="taxonomies",
            tags$span("Taxonomies", title="Type hierarchies"),
            taxonomiesUI("taxonomies")
          ),
          tabPanel(
            value="interpretingTheData",
            tags$span("Interpreting the Data", title=""),
            interpretingDataUI("interpreting_data")
          ),
          tabPanel(
            value="dataCollection",
            tags$span("Data Collection", title=""),
            dataCollectionUI("data_collection")
          ),
          tabPanel(
            value="dataCollectionAnalysis",
            tags$span("Data Collection Analysis", title=""),
            dataCollectionAnalysisUI("data_collection_analysis")
          )
        )
      ),
      tabPanel(
        value="help",
        tags$span("Help", title="Introduction and guide to using the app"),
        helpUI("Help")
      )
    )
  )
}

log_interaction <- function(session_id, tab_id) {
  timestamp <- Sys.time()
  if (!PRODUCTION) {
    message(sprintf("[%s] Session %s: %s", timestamp, session_id, tab_id))
    return(invisible(NULL))
  }
  interaction_data <- data.frame(
    session_id=session_id,
    tab_id=tab_id,
    timestamp=format(timestamp, tz = "UTC", usetz = TRUE),
    stringsAsFactors=FALSE
  )
  tryCatch(
    {
      googlesheets4::sheet_append(
        ss = ANALYTICS_SHEET_ID,
        data = interaction_data,
        sheet = 1
      )
    },
    error = function(e) {
      message(sprintf("Logging failed: %s", conditionMessage(e)))
      invisible(NULL)
    }
  )
}

function(input, output, session) {

  # unique id to track session activity
  session_id <- paste0(
    format(Sys.time(), "%Y%m%d%H%M%S"),
    "_",
    substr(session$token, 1, 8)
  )
  
  # call login module supplying data frame, 
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- reactiveVal(FALSE)
  observeEvent(input$logout, {
    logout_init(TRUE)
    session$reload()
  })
  
  output$user_table <- renderTable({
    # use req to only render results when credentials()$user_auth is TRUE
    req(credentials()$user_auth)
    credentials()$info
  })

  observeEvent(credentials()$user_auth, {
    if (!USE_PASSWORD || credentials()$user_auth) {
      output$appContent <- renderUI({
        make_app_content_ui()
      })
      # home
      homeServer("home")
      # database
      databaseServer("database")
      # mapping museums
      snapshotServer("snapshot")
      changesServer("changes")
      # museum closure
      reasonsServer("reasons")
      outcomesServer("outcomes")
      # details of dispersal
      eventsServer("events")
      dispersalServer("dispersal")
      lengthServer("length")
      # about the data
      glossaryServer("glossary")
      taxonomiesServer("taxonomies")
      dataCollectionServer("data_collection")
      dataCollectionAnalysisServer("data_collection_analysis")
      interpretingDataServer("interpreting_data")
      # help
      helpServer("help")
      observeEvent(input$tabPanelMain, {
        log_interaction(session_id, input$tabPanelMain)
      }, ignoreInit = TRUE)
      observeEvent(input$tabPanelMappingMuseums, {
        if (input$tabPanelMain != "mappingMuseums") return()
        log_interaction(session_id, input$tabPanelMappingMuseums)
      }, ignoreInit = TRUE)
      observeEvent(input$tabPanelMuseumClosure, {
        if (input$tabPanelMain != "museumClosure") return()
        log_interaction(session_id, input$tabPanelMuseumClosure)
      }, ignoreInit = TRUE)
      observeEvent(input$tabPanelDetailsOfDispersal, {
        if (input$tabPanelMain != "detailsOfDispersal") return()
        log_interaction(session_id, input$tabPanelDetailsOfDispersal)
      }, ignoreInit = TRUE)
      observeEvent(input$tabPanelAboutTheData, {
        if (input$tabPanelMain != "aboutTheData") return()
        log_interaction(session_id, input$tabPanelAboutTheData)
      }, ignoreInit = TRUE)
      session$onSessionEnded(function() {
        log_interaction(session_id, "sessionEnded")
      })
    } else {
      output$app_content <- renderUI({
        h3("Please log in.")
      })
    }
  })
}
