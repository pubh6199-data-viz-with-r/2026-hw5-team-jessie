# placeholder for shiny app

library(shiny)
library(bslib)
library(plotly)
library(dplyr)

ui <- page_sidebar(
  
  # Create collapsible side panel on left of page:
  
  title = "Plastics Production Chemical Facilities and Maternal Health in the U.S.",
  sidebar = sidebar(
    title = "Dashboard Overview",
    width = 300,
    open = "closed",
    "Insert description here later"),
  
  # Create dropdown bar at top:
  
  selectInput("mh_outcome", "Choose maternal health outcome", 
              choices = c("Cesarean Deliveries", "Low Birth Weight", 
                          "Fertility Rate", "Preterm Births")),
  
  # Create two sections in one row for faceted line graph and scatterplot:
  
  layout_columns(col_widths = c(6, 6), plotOutput("linegraph"), plotlyOutput("scatterplot")),
  
  # Designate section below the two plots for the map:
  
  layout_columns(col_widths = 12, leafletOutput("map", height = 600))
  
  
  )
  

server <- function(input, output, session) {
  
  # Create reactive scatterplot based on dropdown selection:
  
  filtered_data <- reactive({ # Create reactive data frame
    req(input$mh_outcome)
    
    outcome_col <- switch(input$mh_outcome, # Change the user's dropdown selection into the name of a column in dataframe
                          "Cesarean Deliveries" = "avg_cesarean",
                          "Low Birth Weight" = "avg_lbw", 
                          "Fertility Rate" = "avg_fertility", 
                          "Preterm Births" = "avg_preterm")
    
    mhcf_2023_scatterdf %>% mutate(selected_value = .data[[outcome_col]]) # update the df here depending on if want to visualize all 50 or just the 38
    
  })
  
  outcome_label <- reactive({switch(input$mh_outcome,
                          "Cesarean Deliveries" = "Cesarean Delivery Rate (%)", 
                          "Low Birth Weight" = "Low Birth Weight Rate (%)",
                          "Fertility Rate" = "Fertility Rate (births per 1,000 women)", 
                          "Preterm Births" = "Preterm Birth Rate (%)")})
  
  outcome_tooltip <- reactive({switch(input$mh_outcome,
                          "Cesarean Deliveries" = "Rate (%)", 
                          "Low Birth Weight" = "Rate (%)",
                          "Fertility Rate" = "Live births per 1,000 women", 
                          "Preterm Births" = "Rate (%)")})
  
  # Create reactive scatterplot:
  
  output$scatterplot <- renderPlotly({
    
    scatter <- ggplot(
      
      filtered_data(), 
      
      aes(x = count, y = selected_value, text = paste("State:", state_name, 
                                          "<br>Chemical facilities:", count,
                                          paste0("<br>", outcome_tooltip(),":"), 
                                          round(selected_value, 2)))) +
    
    geom_point(size = 3, alpha = 0.5, color = "steelblue") + 
    
    scale_x_log10() + # data is skewed; try logging
    
    labs(title = paste("Number of Chemical Facilities and", input$mh_outcome), 
           x = "Number of Chemical Facilities per State", 
           y = outcome_label()) +
      
    theme_minimal() 
    
    ggplotly(scatter, tooltip = "text") 
     
  })
  
}



shinyApp(ui, server)
