# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)

# Load dataset
data <- read.csv("data/DIG.csv")  # Replace with your actual file path

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "DIG Trial Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary"),
      menuItem("Plots", tabName = "plots"),
      menuItem("Relationships", tabName = "relationships")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "summary", 
              h2("Summary Statistics"),
              tableOutput("summary")),  # Display summary table
      tabItem(tabName = "plots", 
              h2("Exploratory Plots"),
              plotOutput("plot")),  # Display plot
      tabItem(tabName = "relationships", 
              h2("Variable Relationships"))
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Render summary table
  output$summary <- renderTable({
    summary(data)  # Summarize the dataset
  })
  
  # Render plot
  output$plot <- renderPlot({
    ggplot(data, aes(x = BMI, y = AGE)) + geom_point()  # Replace with valid column names
  })
}

# Run Shiny App
shinyApp(ui, server)
