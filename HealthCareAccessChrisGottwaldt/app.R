#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
install.packages(c("shiny", "shinydashboard", "DT", "tidyverse", "plotly", "httr"), repos = "http://cran.us.r-project.org")

library(tidyverse, quietly = TRUE)
library(shiny, quietly = TRUE)
library(shinydashboard, quietly = TRUE)
library(DT, quietly = TRUE)
library(plotly, quietly = TRUE)
library(httr, quietly = TRUE)

#api call
# Define the API endpoint
api_url <- "https://data.medicaid.gov/api/1/metastore/schemas/dataset/items/6165f45b-ca93-5bb5-9d06-db29c692a360"

# Make the GET request
response <- GET(api_url)

data = read_csv(content(response)$distribution[[1]]$downloadURL)
# head(data)

# shiny part
# Define UI
ui <- fluidPage(
  titlePanel("Medicaid and CHIP Enrollment Trend"),
  fluidRow(
    selectInput("state_select", "Select State:", choices = unique(data$`State Abbreviation`))),
  fluidRow(
    plotlyOutput("enrollment_plot")
  ),
  fluidRow(
    plotlyOutput("state_lines_plot")
  )
)


# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    data %>%
      filter(`State Abbreviation` == input$state_select)
  })
  
  output$enrollment_plot <- renderPlotly({
    ggplotly(
      ggplot(filtered_data(), aes(as.Date(`Report Date`, format="%m/%d/%Y"), `Total Medicaid and CHIP Enrollment`)) +
        geom_line() +
        labs(title = "Medicaid and CHIP Enrollment Trend", x = "Report Date", y = "Total Enrollment")
    )
  })
  
  output$state_lines_plot <- renderPlotly({
    top_states <- data %>%
      group_by(`State Name`) %>%
      summarise(max_enrollment = max(`Total Medicaid and CHIP Enrollment`)) %>%
      arrange(desc(max_enrollment)) %>%
      slice_head(n = 10) %>%
      pull(`State Name`)
    
    data %>%
      filter(`State Name` %in% top_states) %>%
      plot_ly(
        x = ~as.Date(`Report Date`, format="%m/%d/%Y"),
        y = ~`Total Medicaid and CHIP Enrollment`,
        color = ~`State Name`,
        text = ~paste(`State Name`, "<br>Enrollment: ", `Total Medicaid and CHIP Enrollment`),
        mode = "lines+markers"
      ) %>%
      layout(
        title = "\nTop 10 Medicaid and CHIP Enrollment States",
        xaxis = list(title = "Report Date"),
        yaxis = list(title = "Total Enrollment"),
        showlegend = TRUE
      )
  })
}

# Run the application
shinyApp(ui, server)

