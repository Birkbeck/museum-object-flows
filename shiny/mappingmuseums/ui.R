library(networkD3)
library(plotly)
library(shiny)
library(shinyauthr)
library(shinyWidgets)

source("src/labels.R")

PRODUCTION <- as.logical(Sys.getenv("PRODUCTION"))

if (PRODUCTION) {
  error_style <- ".shiny-output-error{color: white;}"
} else {
  error_style <- ".shiny-output-error{color: red;}"
}

fluidPage(
  tags$head(tags$style(error_style)),

  if (PRODUCTION) {
    # add login panel UI function
    shinyauthr::loginUI(id = "login")
  },
  
  uiOutput("appContent"),
)

