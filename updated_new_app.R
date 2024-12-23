library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(corrplot)
raw_data <- read.csv("data/DIG_cleaned.csv")

# Define UI
ui <- dashboardPage(
  skin = "purple",  # Optional dashboard skin for overall appearance
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
                          choices = names(raw_data), selected = "BMI"),
              verbatimTextOutput("summary_output")),
      
      # Plots Tab
      tabItem(tabName = "plots", 
              h2("Exploratory Plots"),
              selectInput("xvar", "Select X Variable:", 
                          choices = names(raw_data), selected = "AGE"),
              selectInput("yvar", "Select Y Variable:", 
                          choices = names(raw_data), selected = "BMI"),
              selectInput("facet_var", "Facet by (Optional):", 
                          choices = c("None", names(raw_data)), selected = "None"),
              selectInput("plot_type", "Choose Plot Type:", 
                          choices = c("Scatter", "Boxplot", "Density"), selected = "Scatter"),
              sliderInput("jitter_amount", "Adjust Jitter Amount (for Scatter):",
                          min = 0, max = 1, value = 0.2, step = 0.1),
              plotOutput("scatter_plot")),
      
      # Relationships Tab
      tabItem(tabName = "relationships", 
              h2("Variable Relationships"),
              selectizeInput("selected_vars", 
                             "Select Variables for Heatmap:", 
                             choices = names(raw_data), 
                             multiple = TRUE, 
                             selected = c("AGE", "BMI", "EJF_PER", "KLEVEL", "CREAT")),
              selectInput("cor_method", "Select Correlation Method:",
                          choices = c("pearson", "spearman", "kendall"), selected = "pearson"),
              plotOutput("correlation_heatmap", height = "600px"))
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive filtered data
  filtered_data <- reactive({
    raw_data %>%
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
    req(input$xvar, input$yvar, input$plot_type)
    
    data <- filtered_data()
    p <- ggplot(data, aes_string(x = input$xvar, y = input$yvar)) +
      theme_minimal() +
      scale_color_manual(values = c("blue", "orange", "green"))  # Custom color palette
    
    if (input$plot_type == "Scatter") {
      p <- p +
        geom_jitter(width = input$jitter_amount, height = input$jitter_amount, alpha = 0.2, color = "steelblue") +  
        geom_smooth(method = "loess", se = FALSE, color = "darkorange", linewidth = 1)  # Custom smooth line color
    } else if (input$plot_type == "Boxplot") {
      p <- p + geom_boxplot(aes_string(group = input$xvar), color = "darkgreen", fill = "lightgreen")  # Custom boxplot colors
    } else if (input$plot_type == "Density") {
      p <- ggplot(data, aes_string(x = input$xvar, fill = input$facet_var)) +
        geom_density(alpha = 0.4) +
        scale_fill_manual(values = c("blue", "orange", "green")) +  # Custom density fill colors
        labs(title = paste("Density Plot:", input$xvar), x = input$xvar, fill = "Facet")
    }
    
    # Add faceting if a valid variable is selected
    if (input$facet_var != "None" && input$plot_type != "Density") {
      p <- p + facet_wrap(as.formula(paste("~", input$facet_var)))
    }
    
    p
  })
  
  # Correlation heatmap
  output$correlation_heatmap <- renderPlot({
    req(input$selected_vars, input$cor_method)
    numeric_data <- filtered_data() %>%
      select(all_of(input$selected_vars)) %>%
      select(where(is.numeric))
    
    validate(
      need(ncol(numeric_data) > 1, "Select at least two numeric variables for the heatmap.")
    )
    
    corr_matrix <- cor(numeric_data, method = input$cor_method, use = "complete.obs")
    corrplot(corr_matrix, method = "color", type = "upper",
             tl.col = "black", tl.cex = 0.8,
             col = colorRampPalette(c("purple", "white", "darkred"))(200))  # Custom heatmap colors
  })
}

# Run the application 
shinyApp(ui, server)
