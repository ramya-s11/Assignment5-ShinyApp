library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(corrplot)


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
      # Summary Tab
      tabItem(tabName = "summary", 
              h2("Summary Statistics"),
              sliderInput("age_slider", "Filter by Age:",
                          min = min(raw_data$AGE, na.rm = TRUE), 
                          max = max(raw_data$AGE, na.rm = TRUE), 
                          value = c(min(raw_data$AGE, na.rm = TRUE), max(raw_data$AGE, na.rm = TRUE))),
              selectInput("summary_var", "Select Variable for Summary:",
                          choices = names(data), selected = "BMI"),
              verbatimTextOutput("summary_output")),
      
      # Plots Tab
      tabItem(tabName = "plots", 
              h2("Exploratory Plots"),
              selectInput("xvar", "Select X Variable:", 
                          choices = names(data), selected = "AGE"),
              selectInput("yvar", "Select Y Variable:", 
                          choices = names(data), selected = "BMI"),
              selectInput("facet_var", "Facet by (Optional):", 
                          choices = c("None", names(data)), selected = "None"),
              sliderInput("jitter_amount", "Adjust Jitter Amount:",
                          min = 0, max = 1, value = 0.2, step = 0.1),
              plotOutput("scatter_plot")),
      
      # Relationships Tab
      tabItem(tabName = "relationships", 
              h2("Variable Relationships"),
              selectizeInput("selected_vars", 
                             "Select Variables for Heatmap:", 
                             choices = names(data), 
                             multiple = TRUE, 
                             selected = c("AGE", "BMI", "EJF_PER", "KLEVEL", "CREAT")),
              plotOutput("correlation_heatmap", height = "600px"))
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive filtered data
  filtered_data <- reactive({
    data %>%
      filter(
        AGE >= input$age_slider[1], AGE <= input$age_slider[2]
      )
  })
  
  # Summary statistics
  output$summary_output <- renderPrint({
    req(input$summary_var)
    summary(filtered_data()[[input$summary_var]])
  })
  
  # Scatter plot
  output$scatter_plot <- renderPlot({
    req(input$xvar, input$yvar)
    
    # Calculate correlation coefficient
    corr_value <- cor(filtered_data()[[input$xvar]], filtered_data()[[input$yvar]], use = "complete.obs")
    
    p <- ggplot(filtered_data(), aes_string(x = input$xvar, y = input$yvar)) +
      geom_jitter(width = input$jitter_amount, height = input$jitter_amount, alpha = 0.2, color = "blue") +  
      geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 1) +  
      theme_minimal() +
      labs(
        title = paste("Scatter Plot:", input$xvar, "vs", input$yvar, "(r =", round(corr_value, 2), ")"),
        x = input$xvar, y = input$yvar
      )
    
    # Add faceting if a valid variable is selected
    if (input$facet_var != "None") {
      p <- p + facet_wrap(as.formula(paste("~", input$facet_var)),
                          labeller = as_labeller(c(`1` = "Male", `2` = "Female")))  
    }
    
    p
  })
  
  # Correlation heatmap
  output$correlation_heatmap <- renderPlot({
    req(input$selected_vars)
    numeric_data <- filtered_data() %>%
      select(all_of(input$selected_vars)) %>%
      select(where(is.numeric))
    
    validate(
      need(ncol(numeric_data) > 1, "Select at least two numeric variables for the heatmap.")
    )
    
    corr_matrix <- cor(numeric_data, use = "complete.obs")
    corrplot(corr_matrix, method = "color", type = "upper",
             tl.col = "black", tl.cex = 0.8,
             col = colorRampPalette(c("blue", "white", "red"))(200))
  })
}

# Run the application 
shinyApp(ui, server)


