library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(sf)
library(leaflet)
library(classInt)
library(ggrepel)

ui <- page_sidebar(
  
 # title = "Plastic Chemical Facilities and Maternal Health Explorer",
   
  title = NULL,
  
  # Create collapsible side panel on left of page:
  
  sidebar = sidebar(
    title = div(
      "Overview", style = "font-size:20px; font-weight:600;"),
    width = 400,
    open = "closed",
    div(
      style = "font-size:14px;",
    p(
      "This interactive dashboard explores the relationship between plastic chemical production facilities and maternal health outcomes in the United States."
      ),
    
    strong("Background"),
    
    p(
      "Women in the US are at greater risk of experiencing adverse maternal health outcomes. These disparate outcomes are a result of a variety of factors, including environmental exposures. Chemicals involved in plastic production are linked to maternal health effects such as decreased fertility, preterm birth, cesarean deliveries, and low birth weight. Facilities that produce these toxic chemicals can contaminate air, water, and soil, exposing surrounding communities to a suite of hazardous chemicals."
      ),
    
    p(
      "Examining whether the number of plastic chemical production facilities is related to adverse maternal health outcomes may improve understanding and help identify policy interventions that reduce maternal health disparities."
    ),
    
    strong("Methods"),
    
    p(
      "Information on chemical production facilities was obtained from the most recent round of EPA Chemical Data Reporting in 2024. State level maternal health data was obtained from the CDC’s National Vital Statistics System. The dashboard was developed in R, and coding assistance was provided by ChatGPT (GPT-5.5)."
    ),
    
    strong("The Dashboard"),
    
    p(
      "Users can begin the investigation by selecting for fertility, preterm birth, cesarean deliveries, or low birth weight, which then produces three visualizations for exploration. First, the line graph shows maternal health trends over time, and compares state data to the national average. Next, the scatterplot investigates the relationship between the frequency of facilities and maternal health by state. Lastly, the map provides an interactive, geographic visual of the relationship between chemical facilities and maternal health."
    )
    
  )),
   
  div(
    style = "
    background:#7490a4;
    color:white;
    padding:18px 25px;
    margin:-1rem -1rem 1rem -1rem;
    display:flex;
    justify-content:space-between;
    align-items:center;
  ",
    
    tags$div(
      style="font-size:24px;font-weight:600; color:white;",
      "Plastic Chemical Facilities and Maternal Health Explorer"
    ),
  ),
  
  # Create dropdown bar at top:
  div(
    style = "display:flex; align-items:baseline; gap:10px;",
    
    tags$span(
      "Choose maternal health outcome:",
      style = "font-weight:600;"
    ),
    
    selectInput(
      "mh_outcome",
      label = NULL,
      choices = c(
        "Fertility Rate",
        "Preterm Births",
        "Cesarean Deliveries",
        "Low Birth Weight"
      ),
      width = "250px")),
  
#  selectInput("mh_outcome", "Choose maternal health outcome", 
#              choices = c("Fertility Rate", "Preterm Births",
#                          "Cesarean Deliveries", "Low Birth Weight" 
#                           )),
  
  # Create two sections in one row for line graph and scatterplot:
  
  layout_columns(
    col_widths = c(6, 6), 
    
    card(
      card_header(div(style = "line-height:1.2;",
      tags$div("Maternal Health Trends in the U.S. (2014-2024)", 
               style = "font-size:16px; font-weight:600; margin-bottom:5px;"),
      tags$div("Compare state maternal health data to the national average; select up to 3 states",
               style = "font-size:12px; color:#6c757d; font-style: italic;"))),
      
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
    
    card(
      card_header(div(style = "line-height:1.2;",     
                  tags$div("Chemical Facilities and Maternal Health by State",
                           style = "font-size:16px; font-weight:600; margin-bottom:5px;"),
                  tags$div("Explore the relationship between facilities reported in 2024 and the selected maternal health outcome",
                           style = "font-size:12px; color:#6c757d; font-style: italic;"))),
         
      plotlyOutput("scatterplot", height = 600)
    )
  ),
  
  # Designate section below the two plots for the map:
  
  card(
    card_header(tags$div("Chemical Facilities and Maternal Health by State Map", style ="font-size:16px;")),
    
    layout_columns(
    col_widths = 12, 
    leafletOutput("map", height = 800)
  )
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
      
      scale_y_continuous(
        limits = c(0, NA)
      ) +
    
      labs(
        title = paste(input$mh_outcome),
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
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 9, hjust = 0.5),
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
                          "Fertility Rate" = "Births per 1,000 women", 
                          "Preterm Births" = "Rate (%)")})
  
  # Create reactive scatterplot:
  
  output$scatterplot <- renderPlotly({
    
    scatter <- ggplot(
      
      filtered_data(), 
      
      aes(x = count, y = selected_value, text = paste("State:", state_name, 
                                          "<br>Chemical facilities:", count,
                                          paste0("<br>", outcome_tooltip(),":"), 
                                          round(selected_value, 2)))) +
    
    geom_point(size = 3, alpha = 0.5, color = "#64acbe") + 
    
    scale_x_log10() + # data is skewed; try logging
    
    labs(title = paste(input$mh_outcome),
         x = "Number of Chemical Facilities", y = outcome_label()) +
      
    theme_minimal() +
    
    theme(axis.title = element_text(size = 9),
          plot.title = element_text(size = 9, hjust = 0.5))
    
    ggplotly(scatter, tooltip = "text") 
     
  })
  
  # Create reactive bivariate chloropleth map based on dropdown selection:
  
  map_df <- reactive({
    
    req(input$mh_outcome)
    
    state_facility_maternal_sf %>% # use state level data, if we want to visualize by state not county
      mutate(outcome_value = switch(input$mh_outcome,
                                    "Cesarean Deliveries" = avg_cesarean, 
                                    "Low Birth Weight" = avg_lbw,
                                    "Fertility Rate" = avg_fertility, 
                                    "Preterm Births" = avg_preterm))
    
#    county_facility_maternal_sf %>% 
#      mutate(outcome_value = switch(input$mh_outcome,
#                                    "Cesarean Deliveries" = `Cesarean Delivery Percent` , 
#                                    "Low Birth Weight" = `Low birthweight raw value`,
#                                    "Fertility Rate" = `Fertility Rate`, 
#                                    "Preterm Births" = preterm_birth_rate))
  })
  
  ## Classify into 3x3 bivariate bins
  map_df_bivariate <- reactive({
    
    mapbi_df <- map_df()
    
    mapbi_df$count_bin <- case_when(mapbi_df$count == 0 ~ 1, mapbi_df$count <= 10 ~ 2, TRUE ~ 3)
    
#   mapbi_df$count_bin <- case_when(mapbi_df$count == 0 ~ 1, mapbi_df$count <= 5 ~ 2, TRUE ~ 3)
      
    mapbi_df$outcome_bin <- cut(mapbi_df$outcome_value,
                                breaks = classInt::classIntervals(mapbi_df$outcome_value, n = 3, 
                                style = "quantile")$brks,
                                include.lowest = TRUE, labels = FALSE)
    
    mapbi_df$bi_class <- paste0(mapbi_df$outcome_bin, "-", mapbi_df$count_bin)
    
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
    
    legend_html <- paste0(
      "
<div style='background:white;
            padding:10px;
            padding:10px;
            border:1px solid #ccc;
            border-radius:5px;
            font-size:10px;'>

<b>Bivariate Map Legend</b><br><br>

<div style='display:flex;
            align-items:center;'>

  <!-- LEFT SIDE LABEL -->
  <div style='text-align:center;
              margin-right:12px;
              font-weight:bold;
              line-height:1.2;'>

    ↑<br>
    Higher<br>
    # of<br> 
    Chemical<br>
    Facilities

  </div>

  <!-- MATRIX -->
  <table style='border-collapse:collapse;'>

    <tr>
      <td style='background:#64acbe;width:25px;height:25px'></td>
      <td style='background:#627f8c;width:25px;height:25px'></td>
      <td style='background:#574249;width:25px;height:25px'></td>
    </tr>

    <tr>
      <td style='background:#b0d5df;width:25px;height:25px'></td>
      <td style='background:#ad9ea5;width:25px;height:25px'></td>
      <td style='background:#985356;width:25px;height:25px'></td>
    </tr>

    <tr>
      <td style='background:#e8e8e8;width:25px;height:25px'></td>
      <td style='background:#e4acac;width:25px;height:25px'></td>
      <td style='background:#c85a5a;width:25px;height:25px'></td>
    </tr>

  </table>

</div>

<!-- BOTTOM LABEL -->
<div style='margin-top:8px;
            text-align:center;
            font-weight:bold;'>

  →<br>
  Higher ", outcome_label(), "
  
</div>

</div>
"
    
    )
    
    popup_label_units <- switch(input$mh_outcome,
                                "Fertility Rate" = "Fertility Rate (births<br>per 1,000 women)",
                                outcome_label())
    
    leaflet(mapbi_df) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      setView(lng = -98, lat = 39, zoom = 4) %>% 
      addPolygons(fillColor = ~color, fillOpacity = 0.8, color = "white", weight = 0.3,
                  popup = ~paste0("County: ", NAME,
                                  "<br>Facilities: ", count,
                                  "<br>", popup_label_units, ": ", round(outcome_value, 2))) %>% 
                                #  "<br>", input$mh_outcome, ": ", round(outcome_value, 2))) %>% 
      
       addControl(html = legend_html, position = "bottomright")
  })

}


shinyApp(ui, server)
