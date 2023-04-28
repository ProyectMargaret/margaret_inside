#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(gargle)
library(DT)

# options(gargle_oauth_cache = ".secrets")
# googlesheets4::gs4_auth(path = ".secrets")

sheet_url <- "https://docs.google.com/spreadsheets/d/1PJ9BktJdDX3LJmYgBCxKf5B6DuxIpRLd/export?format=csv&gid=1403943995"
data <- readr::read_csv(sheet_url)


ui <- fluidPage(
  titlePanel("Margaret Inside - Research Progress Tracker"),
  tags$head(tags$style(HTML("
    .dataTables_wrapper {
      width: 100% !important;
    }
  "))),
  fluidRow(
    mainPanel(
      DT::dataTableOutput("researchDataTable"),
      width = "100%"
    )
  )
)



server <- function(input, output) {
  output$researchDataTable <- DT::renderDataTable({
    datatable(data, options = list(pageLength = 25, scrollX = TRUE))
  })
}

shinyApp(ui = ui, server = server)
