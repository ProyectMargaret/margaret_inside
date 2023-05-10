library(shiny)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(gargle)
library(DT)
library(shinydashboard)
library(flexdashboard)
library(tidyr)
# 
# sheet_url1 <- "https://docs.google.com/spreadsheets/d/1unFxjUoR1Fk92PCWQQ6h4DAYQbgrXzD3sX6jSxlHnw0/export?format=csv&gid=0"
# sheet_url2 <- "https://docs.google.com/spreadsheets/d/1PJ9BktJdDX3LJmYgBCxKf5B6DuxIpRLd/export?format=csv&gid=1403943995"
# sheet_url3 <- "https://docs.google.com/spreadsheets/d/14Hrkn7Tisj5yPHE1mRgYOyKxJFjtUKT_J9VCAEh7M-E/export?format=csv&gid=0"
# 
# data1 <- readr::read_csv(sheet_url1)
# data2 <- readr::read_csv(sheet_url2)
# data3 <- readr::read_csv(sheet_url3)

# Google Sheet URL
sheet_url_main <- "https://docs.google.com/spreadsheets/d/1KxFzf-pVIudzWF9NVPZgzGw2v9KYmMnwIRV5WGeP4Mg/export?format=csv&gid=0"

# Read URLs from the Google Sheet
urls_data <- readr::read_csv(sheet_url_main)

# Make sure the "enlace" column is of type character
urls_data$enlace <- as.character(urls_data$enlace)

# Initialize empty data frame to store data from all URLs
all_data <- data.frame()

# Loop over each URL
for(i in 1:nrow(urls_data)) {
  sheet_url <- urls_data[i, "enlace"]
  data <- readr::read_csv(sheet_url$enlace)
  all_data <- rbind(all_data, data)
}

# Assuming all_data is your combined data frame
new_dataframe <- all_data %>%
  group_by(`Investigador Principal`, `Estado de cumplimiento`) %>%
  summarise(count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = `Estado de cumplimiento`, values_from = count, values_fill = 0) %>% 
  tidyr::drop_na()

# Display the new data frame
print(new_dataframe)


# Display the new data frame
print(new_dataframe)


ui <- dashboardPage(
  dashboardHeader(title = "Margaret Inside - Research Progress Tracker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Summary", tabName = "summary", icon = icon("list")) # New tab
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
      
      tabItem(tabName = "summary",
              fluidRow(
                box(DT::dataTableOutput("summaryTable", height = "auto"), width = 12)
              )
    )
  )
)
)

server <- function(input, output) {
  output$researchDataTable <- DT::renderDataTable({
    datatable(all_data, options = list(pageLength = 25, scrollX = TRUE)) 
  })
  output$summaryTable <- DT::renderDataTable({
    datatable(new_dataframe, options = list(pageLength = 25, scrollX = TRUE)) 
  })
  
  # Rest of your server code...
}

shinyApp(ui = ui, server = server)
