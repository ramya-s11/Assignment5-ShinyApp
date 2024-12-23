library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(corrplot)
raw_data <- read.csv("data/DIG_cleaned.csv")

# Define UI
ui <- dashboardPage(
  skin = "purple",
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
              sliderInput("age_slider", "Filter by Age:",
                          min = min(raw_data$AGE, na.rm = TRUE), 
                          max = max(raw_data$AGE, na.rm = TRUE), 
                          value = c(min(raw_data$AGE, na.rm = TRUE), max(raw_data$AGE, na.rm = TRUE))),
              selectInput("summary_var", "Select Variable for Summary:",
                          choices = names(raw_data), selected = "BMI"),
              verbatimTextOutput("summary_output")),
      
      tabItem(tabName = "plots", 
              h2("Exploratory Plots"),
              selectInput("xvar", "Select X Variable:", 
                          choices = names(raw_data), selected = "AGE"),
              selectInput("yvar", "Select Y Variable:", 
                          choices = names(raw_data), selected = "BMI"),
              selectInput("facet_var", "Facet by (Optional):", 
                          choices = c("None", names(raw_data)), selected = "None"),
              sliderInput("jitter_amount", "Adjust Jitter Amount (for Scatter):",
                          min = 0, max = 1, value = 0.2, step = 0.1),
              plotOutput("scatter_plot")),
      
      tabItem(tabName = "relationships", 
              h2("Variable Relationships"),
              selectizeInput("selected_vars", 
                             "Select Variables for Heatmap:", 
                             choices = names(raw_data), 
                             multiple = TRUE, 
                             selected = c("AGE", "BMI", "EJF_PER", "KLEVEL", "CREAT")),
              plotOutput("correlation_heatmap", height = "600px"))
    )
  )
)

# Define Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    raw_data %>%
      filter(AGE >= input$age_slider[1], AGE <= input$age_slider[2])
  })
  
  output$summary_output <- renderPrint({
    req(input$summary_var)
    summary(filtered_data()[[input$summary_var]])
  })
  
  output$scatter_plot <- renderPlot({
    req(input$xvar, input$yvar)
    
    data <- filtered_data()
    
    if (input$facet_var != "None") {
      data[[input$facet_var]] <- as.factor(data[[input$facet_var]])
    }
    
    p <- ggplot(data, aes_string(x = input$xvar, y = input$yvar)) + theme_minimal()
    
    # Scatter plot with jitter and smoothing
    p <- p +
      geom_jitter(width = input$jitter_amount, height = input$jitter_amount, alpha = 0.2, color = "steelblue") +
      geom_smooth(method = "loess", se = FALSE, color = "darkorange", linewidth = 1)
    
    if (input$facet_var != "None") {
      p <- p + facet_wrap(as.formula(paste("~", input$facet_var)))
    }
    
    p
  })
  
  output$correlation_heatmap <- renderPlot({
    req(input$selected_vars)
    numeric_data <- filtered_data() %>%
      select(all_of(input$selected_vars)) %>%
      select(where(is.numeric))
    
    validate(
      need(ncol(numeric_data) > 1, "Select at least two numeric variables for the heatmap.")
    )
    
    corr_matrix <- cor(numeric_data, method = "pearson", use = "complete.obs")  # Default to Pearson
    corrplot(corr_matrix, method = "color", type = "upper",
             tl.col = "black", tl.cex = 0.8,
             col = colorRampPalette(c("purple", "white", "darkred"))(200))
  })
}

shinyApp(ui, server)
