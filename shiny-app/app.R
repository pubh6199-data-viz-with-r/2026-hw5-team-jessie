library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(sf)
library(leaflet)
library(classInt)
library(ggrepel)

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
  
  # Create two sections in one row for line graph and scatterplot:
  
  layout_columns(
    col_widths = c(6, 6), 
    
    card(
      card_header("Comparing Maternal Health Outcomes by State"),
      
      layout_columns(
        col_widths = c(2, 10),
        
      div(  
        selectizeInput(
          "selected_states", 
          "Select up to 5 states:", 
          choices = state.abb, 
          selected = NULL,
          multiple = TRUE, 
          options = list(maxItems = 5),
          width = "70px"
         )
        ),
        
        plotlyOutput("linegraph", height = 600)
      )
    ),
    
    card(plotlyOutput("scatterplot")
    )
  ),
  
  # Designate section below the two plots for the map:
  
  layout_columns(
    col_widths = 12, 
    leafletOutput("map", height = 600)
  )
)





server <- function(input, output, session) {
  
   # Create reactive line graph based on dropdown selection:
  
output$linegraph <- renderPlotly({ #create linegraph and calls on plotOutput("linegraph) from ui
  
    
  outcome_col <- switch(input$mh_outcome,      #user picks an option and it pulls from the called dataset
                        "Cesarean Deliveries" = "Cesarean Deliveries Percent",
                        "Low Birth Weight" = "Low Birth Weight Percent", 
                        "Fertility Rate" = "Fertility Rate per 1,000 women",
                        "Preterm Births" = "Preterm Birth Percent"
                        )
  
  national_col <- switch(input$mh_outcome,     #user picks an option and it pulls from the called dataset
                         "Cesarean Deliveries" = "ntl_cesarean",
                         "Low Birth Weight" = "ntl_lbw",
                         "Fertility Rate" = "ntl_fertility",
                         "Preterm Births" = "ntl_preterm"
                        )
  
  line_data <- state_data_2014_2024 %>%      #preparing data for line graph                    
    filter(State %in% input$selected_states) %>%      #only states selected will show
    left_join(all50_state_facilities, by = c("State" = "state_abb")) %>%      #add state facility data
    mutate(
      selected_value = .data[[outcome_col]],      #only outcome the user selects is shown
      Series = State  
      )     
  
  national_data <- ntl_maternal_avgs %>%
    mutate(
      selected_value = .data[[national_col]],
      Series = "National" 
    )
  
    line_data <- bind_rows(line_data, national_data)  # makes it so you dont have to have 5 states to see a graph, but can see it with 1-4 too, and national avg is fixed

  
  line_plot <- ggplot(
    line_data,          #user selects outcome, and state will be assigned a color 
    aes(
      x = Year,
      y = selected_value,
      color = Series,
      group = Series,
        )
      ) +
    
    geom_line(linewidth = 0.4) +    #line to connect the time series together
    geom_point(size = 0.2) +       #plotting points per year
    
    
    labs(
      title = paste(input$mh_outcome, "Over Time"),
      subtitle = "Hover over lines to see chemical facility counts",
      x = "Year",
      y = outcome_label(),     #the reactive label
      color = "State"
    ) +
    scale_x_continuous(
      breaks = c(2014, 2016, 2018, 2020, 2022, 2024)
    ) +
    
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.title.y = element_text(size = 9),
      plot.title = element_text(size = 13),
      plot.subtitle = element_text(size = 10)
    )
  ggplotly(line_plot, tooltip = "none")
})



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
  
  # Create reactive bivariate chloropleth map based on dropdown selection:
  
  map_df <- reactive({
    
    req(input$mh_outcome)
    
    county_facility_maternal_sf %>% 
      mutate(outcome_value = switch(input$mh_outcome,
                                    "Cesarean Deliveries" = `Cesarean Delivery Percent` , 
                                    "Low Birth Weight" = `Low birthweight raw value`,
                                    "Fertility Rate" = `Fertility Rate`, 
                                    "Preterm Births" = preterm_birth_rate))
  })
  
  ## Classify into 3x3 bivariate bins
  map_df_bivariate <- reactive({
    
    mapbi_df <- map_df()
    
    mapbi_df$count_bin <- case_when(mapbi_df$count == 0 ~ 1, mapbi_df$count <= 5 ~ 2, TRUE ~ 3)
      
    mapbi_df$outcome_bin <- cut(mapbi_df$outcome_value,
                                breaks = classInt::classIntervals(mapbi_df$outcome_value, n = 3, 
                                style = "quantile")$brks,
                                include.lowest = TRUE, labels = FALSE)
    
    mapbi_df$bi_class <- paste0(mapbi_df$count_bin, "-", mapbi_df$outcome_bin)
    
    mapbi_df
  })
  
  # Insert bivariate color palette (used recommendations from https://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/)
  
  bi_colors <- c(
    "1-1" = "#e8e8e8",
    "1-2" = "#b0d5df",
    "1-3" = "#64acbe",
    "2-1" = "#e4acac",
    "2-2" = "#ad9ea5",
    "2-3" = "#627f8c",
    "3-1" = "#c85a5a",
    "3-2" = "#985356",
    "3-3" = "#574249"
  )
  
  # Create leaflet output
  
  output$map <- renderLeaflet({
    
    mapbi_df <- map_df_bivariate()
    
    mapbi_df$color <- bi_colors[mapbi_df$bi_class]
    
    leaflet(mapbi_df) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      setView(lng = -98, lat = 39, zoom = 4) %>% 
      addPolygons(fillColor = ~color, fillOpacity = 0.8, color = "white", weight = 0.3,
                  popup = ~paste0("County: ", NAME,
                                  "<br>Facilities: ", count,
                                  "<br>", input$mh_outcome, ": ", round(outcome_value, 2)))
  })

}


shinyApp(ui, server)
