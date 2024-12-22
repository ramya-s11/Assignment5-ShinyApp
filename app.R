library(shiny)
library(shinydashboard)

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
      tabItem(tabName = "summary", h2("Summary Statistics")),
      tabItem(tabName = "plots", h2("Exploratory Plots")),
      tabItem(tabName = "relationships", h2("Variable Relationships"))
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
