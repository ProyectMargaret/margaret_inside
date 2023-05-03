library(shiny)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(gargle)
library(DT)
library(shinydashboard)
library(flexdashboard)

sheet_url <- "https://docs.google.com/spreadsheets/d/1PJ9BktJdDX3LJmYgBCxKf5B6DuxIpRLd/export?format=csv&gid=1403943995"
data <- readr::read_csv(sheet_url)

ui <- dashboardPage(
  dashboardHeader(title = "Margaret Inside - Research Progress Tracker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Cumplimiento", tabName = "cumplimiento", icon = icon("check-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      ),
      
      # Data tab content
      tabItem(tabName = "data",
              fluidRow(
                box(DT::dataTableOutput("researchDataTable", height = "auto"), width = 12)
              )
      ),
      
      # Cumplimiento tab content
      tabItem(tabName = "cumplimiento",
              fluidRow(
                box(title = "Cumplimiento", 
                    flexdashboard::gaugeOutput("cumplimientoGauge"), 
                    width = 6
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  output$researchDataTable <- DT::renderDataTable({
    datatable(data, options = list(pageLength = 25, scrollX = TRUE))
  })
  
  cumplimiento_percentage <- reactive({
    total_rows <- nrow(data)
    cumplimiento_final_count <- sum(data$"Estado de cumplimiento" == "Cumplimiento final")
    percentage <- (cumplimiento_final_count / total_rows) * 100
    return(percentage)
  })
  
  output$cumplimientoGauge <- flexdashboard::renderGauge({
    gauge(value = cumplimiento_percentage(), min = 0, max = 100, symbol = "%", 
          label = "Porcentaje de cumplimiento final", 
          gaugeSectors(success = c(80, 100), warning = c(50, 79), danger = c(0, 49)))
  })
}

shinyApp(ui = ui, server = server)
