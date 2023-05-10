library(shiny)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(gargle)
library(DT)
library(shinydashboard)
library(flexdashboard)

sheet_url1 <- "https://docs.google.com/spreadsheets/d/1unFxjUoR1Fk92PCWQQ6h4DAYQbgrXzD3sX6jSxlHnw0/edit#gid=0"
sheet_url2 <- "https://docs.google.com/spreadsheets/d/1PJ9BktJdDX3LJmYgBCxKf5B6DuxIpRLd/export?format=csv&gid=1403943995"
sheet_url3 <- "https://docs.google.com/spreadsheets/d/14Hrkn7Tisj5yPHE1mRgYOyKxJFjtUKT_J9VCAEh7M-E/export?format=csv&gid=0"

data1 <- read_sheet(sheet_url1)
data2 <- readr::read_csv(sheet_url2)
data3 <- read_sheet(sheet_url3)

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
                box(DT::dataTableOutput("researchDataTable1", height = "auto"), width = 12),
                fluidRow(tags$hr()), # Agregar una fila separadora
                box(DT::dataTableOutput("researchDataTable2", height = "auto"), width = 12),
                fluidRow(tags$hr()), # Agregar una fila separadora
                box(DT::dataTableOutput("researchDataTable3", height = "auto"), width = 12),
              )
      ),
      
      # Cumplimiento tab content
      tabItem(tabName = "cumplimiento",
              fluidRow(
                box(title = "Cumplimiento - SHARIK JIMENA GUZMAN ARROYO", 
                    flexdashboard::gaugeOutput("cumplimientoGauge"), 
                    width = 6
                ),
                box(title = "Cumplimiento - SEBASTIAN ROBLEDO GIRALDO", 
                    flexdashboard::gaugeOutput("cumplimientoGauge2"), 
                    width = 6
                ),
                box(title = "Cumplimiento - PEPITO PEREZ", 
                    flexdashboard::gaugeOutput("cumplimientoGauge3"), 
                    width = 6
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  output$researchDataTable1 <- DT::renderDataTable({
    datatable(data1, options = list(pageLength = 25, scrollX = TRUE)) 
  })
  
  output$researchDataTable2 <- DT::renderDataTable({
    datatable(data2, options = list(pageLength = 25, scrollX = TRUE)) 
  })
  
  output$researchDataTable3 <- DT::renderDataTable({
    datatable(data3, options = list(pageLength = 25, scrollX = TRUE)) 
  })
  
  cumplimiento_percentage <- reactive({
    total_rows <- nrow(data1)
    cumplimiento_final_count <- sum(data1$"Estado de cumplimiento" == "Cumplimiento final")
    percentage <- (cumplimiento_final_count / total_rows) * 100
    return(percentage)
  })
  
  output$cumplimientoGauge <- flexdashboard::renderGauge({
    gauge(value = cumplimiento_percentage(), min = 0, max = 100, symbol = "%", 
          label = "Porcentaje de cumplimiento final", 
          gaugeSectors(success = c(80, 100), warning = c(50, 79), danger = c(0, 49)))
  }) 
  
  cumplimiento_percentage2 <- reactive({
    total_rows <- nrow(data2)
    cumplimiento_final_count <- sum(data2$"Estado de cumplimiento" == "Cumplimiento final")
    percentage <- (cumplimiento_final_count / total_rows) * 100
    return(percentage)
  })
  
  output$cumplimientoGauge2 <- flexdashboard::renderGauge({
    gauge(value = cumplimiento_percentage2(), min = 0, max = 100, symbol = "%", 
          label = "Porcentaje de cumplimiento final", 
          gaugeSectors(success = c(80, 100), warning = c(50, 79), danger = c(0, 49)))
  }) 
  
  cumplimiento_percentage3 <- reactive({
    total_rows <- nrow(data3)
    cumplimiento_final_count <- sum(data3$"Estado de cumplimiento" == "Cumplimiento final")
    percentage <- (cumplimiento_final_count / total_rows) * 100
    return(percentage)
  })
  
  output$cumplimientoGauge3 <- flexdashboard::renderGauge({
    gauge(value = cumplimiento_percentage3(), min = 0, max = 100, symbol = "%", 
          label = "Porcentaje de cumplimiento final", 
          gaugeSectors(success = c(80, 100), warning = c(50, 79), danger = c(0, 49)))
  })
  
}

shinyApp(ui = ui, server = server)
