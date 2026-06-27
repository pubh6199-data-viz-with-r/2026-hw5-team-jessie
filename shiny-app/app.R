# Load packages
library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(sf)
library(leaflet)
library(classInt)
library(ggrepel)
library(ggplot2)
library(tidyverse)
library(tmap)
library(tigris)
library(tidycensus)

# Import datasets
state_data_2014_2024 <- readRDS("app-data/state_data_2014_2024.rds")
mhcf_2023_scatterdf <- readRDS("app-data/mhcf_2023_scatterdf.rds")
state_facility_maternal_sf <- readRDS("app-data/state_facility_maternal_sf.rds")
all50_state_facilities <- readRDS("app-data/all50_state_facilities.rds")
ntl_maternal_avgs <- readRDS("app-data/ntl_maternal_avgs.rds")

# Load EPA Chemical Data Reporting (CDR) Data
cdr <- X2024_CDR_Manufacture_Import_Information
glimpse(cdr)

# Check how many rows of data
nrow(cdr) # 50,116 rows

# Drop CBI facilities, importing facilities, and missing values 
cdr <- cdr[cdr$`SITE NAICS ACTIVITY 1` != "CBI" & 
             !is.na(cdr$`SITE NAICS ACTIVITY 1`) &
             cdr$`SITE NAICS ACTIVITY 1` != "Import", ]

# Check how many rows of data
nrow(cdr) # 25,830, so dropped 24,286 rows total

# Load spreadsheet with specific plastic production chemicals and their CAS #s
plastic_cas_df <- Plastic_Production_Chemical_CAS
glimpse(plastic_cas_df)

# Create vector of all the CAS #s for easier filtering
plastic_cas <- plastic_cas_df$`CAS #` 
plastic_cas

# Filter cdr df down to just the plastic production chemicals identified
cdr_plastic <- cdr %>% 
  filter(`CHEMICAL ID` %in% plastic_cas) # 610 production facilities

# Upload maternal health data below

# Load libraries and data
#install.packages("dplyr")
library(dplyr)
library(readxl)

# Load LBW data by county from the National County Health Rankings Database 
lbw_county <- LBW_County_Data
glimpse(lbw_county)

# Load fertility rate and cesarean birth rate data by county from the CDC Wonder Natality Database 
fertility_cesarean_county <- Fertility_Rate_Cesarean_Delivery_Rate_County_Data
glimpse(fertility_cesarean_county)

# Load preterm birth data by county from the CDC Wonder Natality Database 
preterm_county <- Preterm_Birth_County_Data
glimpse(preterm_county)

#Load state data from CDC Stats of the State Data with maternal outcome data for 2005, 2014-2024
State_Data_2014_2024 <- read_excel("data/State Maternal Data 2005, 2014-2024.xlsx")
view(State_Data_2014_2024)

# Remove 2005 from state maternal outcome data to only have 2014-2024
# Add full state names as column
# Make year numeric 
state_data_2014_2024 <- State_Data_2014_2024 %>%
  filter(Year != 2005) %>%
  mutate(
    State_Name = state.name[match(State, state.abb)],
    Year = as.numeric(Year)
  ) %>%
  select(State_Name, State, Year, everything())
view(state_data_2014_2024)

# Keep only preterm birth data, which is birth before 37 weeks of gestational age
preterm_county <- Preterm_Birth_County_Data %>%
  filter(`OE Gestational Age` %in% c(
    "20 - 27 weeks",
    "28 - 31 weeks",
    "32 - 35 weeks",
    "36 weeks"
  )) 
glimpse(preterm_county)

# Add up all preterm birth data to get a total 
preterm_county <- preterm_county %>%
  group_by(State, County, Year) %>%
  summarise(
    preterm_births = sum(Births, na.rm = TRUE),
    .groups = "drop"
  )

# Make sure values are numeric because will be using total birth number from fertility_cesarean_county dataset in the preterm_county data
glimpse(preterm_county)
glimpse(fertility_cesarean_county)
fertility_cesarean_county <- fertility_cesarean_county %>%
  mutate(Year = as.numeric(Year),
         `Total Number of Births` = as.numeric(`Total Number of Births`))

# Create a new column with the summarized preterm births 
# Calculate the total number of preterm births and the preterm birth rate
preterm_county <- preterm_county %>%
  left_join(
    fertility_cesarean_county %>% select(State, County, Year, `Total Number of Births`),
    by = c("State", "County", "Year")
  ) %>%
  mutate(
    preterm_birth_rate = preterm_births / `Total Number of Births`
  )
glimpse(preterm_county)

# Remove total state data in the rows of the LBW county data  
lbw_county <- lbw_county %>%
  filter(!`County Name` %in% state.name)
glimpse(lbw_county)
view(fertility_cesarean_county)

#Check datasets are all cleaned and ready to be used 
dim(preterm_county)
dim(fertility_cesarean_county)
dim(lbw_county)
dim(state_data_2014_2024)
glimpse(preterm_county)
glimpse(fertility_cesarean_county)
glimpse(lbw_county)
glimpse(state_data_2014_2024)

# Make characters numeric
# Change county name to be consistent with the chemical data
fertility_cesarean_county <- fertility_cesarean_county %>%
  mutate(
    `Female Population` = as.numeric(`Female Population`),
    `Fertility Rate` = as.numeric(`Fertility Rate`))
preterm_county <- preterm_county %>%
  mutate(
    County = sub(",.*", "", County))
fertility_cesarean_county <- fertility_cesarean_county %>%
  mutate(
    County = sub(",.*", "", County))

options(tigris_use_cache = TRUE)

# Check for missing values in county name column
sum(is.na(cdr_plastic$`SITE COUNTY / PARISH`)) # 43 missing values

# Use the zipcodeR package which has zip codes and county names, to join county names by zip code to the 43 rows missing county name info
#install.packages("zipcodeR")
library(zipcodeR)

# Create df with zipcodes
zipcodeR_df <- zipcodeR::zip_code_db

# Join the county names to the cdr_plastic dataframe
zipcodeR_df <- zipcodeR_df %>% select(zipcode, county) # Select only relevant columns, so that the full dataframe doesn't get joined to cdr_plastic df

cdr_plastic2 <- cdr_plastic %>% 
  left_join(zipcodeR_df, by = c("SITE POSTAL CODE" = "zipcode")) %>% 
  mutate(zipcodeR_county = county)

names(cdr_plastic2) # Check that join worked, and all columns needed were added

# Join didn't work for zip codes that had the additional 4 digits. Update zip code column to only include the 5 first digits
cdr_plastic <- cdr_plastic %>%  mutate(
  zip_code = substr(`SITE POSTAL CODE`, 1, 5))

# Re-join
cdr_plastic2 <- cdr_plastic %>% 
  left_join(zipcodeR_df, by = c("zip_code" = "zipcode")) %>% 
  mutate(zipcodeR_county = county)

# Check that join worked and there are no missing county names
sum(is.na(cdr_plastic$county_name)) # 0 missing county names

# Select for only necessary columns to make df more viewable
cdr_plastic3 <- cdr_plastic2 %>% 
  select(`CHEMICAL NAME`, `CHEMICAL ID`, `SITE NAME`, `SITE ADDRESS LINE1`, `SITE CITY`,
         `SITE COUNTY / PARISH`, `SITE STATE`, `SITE POSTAL CODE`, `SITE NAICS CODE 1`,
         `SITE NAICS ACTIVITY 1`, zipcodeR_county, zip_code)

# Now that all county names are included, add the ones from zipcodeR to fill the missing names in the original df
cdr_plastic4 <- cdr_plastic3 %>% mutate(
  county_name = coalesce(`SITE COUNTY / PARISH`, zipcodeR_county)) # Looking at the data, there are some wrong entries in the original data which is now under "county_name", so plan on actually using "zipcodeR_county" column for analysis, which has more accurate names.

# Rearrange columns so that it's easier to read
cdr_plastic5 <- cdr_plastic4 %>%  select(`CHEMICAL NAME`, `CHEMICAL ID`, `SITE NAME`, 
                                         `SITE ADDRESS LINE1`, `SITE CITY`, zipcodeR_county, `SITE STATE`, 
                                         zip_code, `SITE NAICS CODE 1`, `SITE NAICS ACTIVITY 1`)

# Standardize cases- county names all currently lower case
cdr_plastic5$zipcodeR_county <- toupper(cdr_plastic5$zipcodeR_county)

# Rename df
chemfacilities <- cdr_plastic5

# Download county shapefiles
counties_sf <- tigris::counties(cb = TRUE, year = 2023) # Chose to download 2023 since this is the most recent year of data in our CDR and maternal health data

# Standardize cases in shapefile df before merging
counties_sf$NAMELSAD <- toupper(counties_sf$NAMELSAD)

# Download state shapefiles so that state borders appear on map
states_sf <- tigris::states(cb = TRUE, year = 2023)

# Group by county and summarize number of facilities by county 
chemfacilities_counties <- chemfacilities %>% 
  group_by(zipcodeR_county, `SITE STATE`) %>% 
  summarise(count = n())

sum(chemfacilities_counties$count) # Check that all 610 facilities are included; true

# Join new count column to shapefile df (counties_sf) on county and state name
county_facilities_sf <- counties_sf %>% 
  left_join(chemfacilities_counties, by = c("NAMELSAD" = "zipcodeR_county", "STUSPS" = "SITE STATE"))

# Replace NA values in count column with 0
county_facilities_sf$count[is.na(county_facilities_sf$count)] <- 0 
sum(county_facilities_sf$count) # 607 total facilities, since the 3 in Puerto Rico were dropped

# Create univariate chloropleth just to visualize number of chemical facilities by county
tm_shape(county_facilities_sf) + # Read in county shapefile
  tm_polygons("count", palette = "brewer.reds", title = "Number of Chemical Facilities per County") +
  tm_shape(states_sf) + # Read in state shapefile
  tm_borders(col = "black", lwd = 2)

# Combine county maternal health outcomes with the shapefile df with facility counts

lbwcounty_forjoin <- lbw_county %>%  # Prepare the lbw_county df for join
  filter(Year == 2023) %>% 
  mutate(county_name = toupper(`County Name`)) %>% 
  select('State Abbreviation', county_name, 'Year', 'Low birthweight raw value')

pretermcounty_forjoin <- preterm_county %>% # Prepare the preterm_county df for join
  mutate(county_name = toupper(County)) %>% 
  filter(Year == 2023) %>% 
  select(State, county_name, preterm_birth_rate) 

fertility_cesarean_county_forjoin <- fertility_cesarean_county %>% 
  mutate(countyname = toupper(County)) %>% 
  filter(Year == 2023) %>% 
  select(State, countyname, `Fertility Rate`, `Cesarean Delivery Percent`)

# Join maternal health outcomes by state abbreviation and county names
county_facility_maternal_sf <- county_facilities_sf %>% 
  left_join(lbwcounty_forjoin, by = c("STUSPS" = "State Abbreviation", "NAMELSAD" = "county_name")) %>% 
  left_join(pretermcounty_forjoin, by = c("STATE_NAME" = "State", "NAMELSAD" = "county_name")) %>% 
  left_join(fertility_cesarean_county_forjoin, by = c("STATE_NAME" = "State", "NAMELSAD" = "countyname"))

sum(is.na(county_facility_maternal_sf$`Low birthweight raw value`)) # 123 counties with no lbw data
sum(is.na(county_facility_maternal_sf$preterm_birth_rate)) # 2558 counties with no preterm birth data
sum(is.na(county_facility_maternal_sf$`Fertility Rate`)) # 2558 counties with no fertility rate data
sum(is.na(county_facility_maternal_sf$`Cesarean Delivery Percent`)) # 2558 counties with no c-section rate data

# Export county_facility_maternal_sf into Excel, so can upload to Google as a public sheet for Shiny Assistant to reference to provide bivariate chloropleth map code
#install.packages("writexl")
library(writexl)
write_xlsx(county_facility_maternal_sf, "county_facility_maternal_sf.xlsx")


# Create new df with facility count by state, in case it's better visualization for map
chemfacilities_states <- chemfacilities %>% 
  group_by(`SITE STATE`) %>% 
  summarise(count = n())

# Join to state shapefile df
state_facilities_sf <- states_sf %>% 
  left_join(chemfacilities_states, by = c("STUSPS" = "SITE STATE"))

# Replace NA values in count column with 0
state_facilities_sf$count[is.na(state_facilities_sf$count)] <- 0 
sum(state_facilities_sf$count) # 610 states

# Drop territories
state_facilities_sf <- state_facilities_sf %>% 
  filter(!STUSPS %in% c("PR", "MP", "GU", "VI", "AS", "DC"))

# Create columns containing state averages grouped by state and year
state_maternalhealth_avgs <- State_Maternal_Outcome_Data %>% 
  group_by(Year, State) %>% 
  mutate(avg_cesarean = mean(`Cesarean Deliveries Percent`, na.rm = TRUE),
         avg_lbw = mean(`LBW Percent`, na.rm = TRUE),
         avg_fertility = mean(`Fertility Rate`, na.rm = TRUE), 
         avg_preterm = mean(`Preterm Births Percent`, na.rm = TRUE), 
         avg_births = mean(`Number of Births`, na.rm = TRUE))

# Join maternal health outcomes by state abbreviation 
state_facility_maternal_sf <- state_facilities_sf %>% 
  left_join(state_maternalhealth_avgs, by = c("STUSPS" = "State")) 

hist(chemfacilities_states$count, breaks = 50)

# Aggregate number of chemical production facilities by state
state_facilities <- chemfacilities %>% 
  group_by(`SITE STATE`) %>% 
  summarise(count = n()) # 39 states have chemical facilities based on the chemicals included

# Check that the group_by() included all 610 facilities
sum(state_facilities$count) # = 610

# Create df with all 50 states in case we want to include for analysis
length(state.abb) # Use built in R vector, state.abb which has all 50 states (the CDR data had NA values and typos, otherwise would have extracted from there)

all_50 <- data.frame(state_abb = state.abb)

all50_state_facilities <- all_50 %>% 
  left_join(state_facilities, by = c(state_abb = "SITE STATE"))

all50_state_facilities$count[is.na(all50_state_facilities$count)] <- 0 # Replace NA values with 0 in case we want to explore this in the scatterplot

sum(all50_state_facilities$count) # = 607; 3 facilities now missing, because 3 facilities were in Puerto Rico which we will exclude from our analysis (?)

# Create new df with just 2023 rates for all 50 states, for scatterplot; and join facility counts
mhcf_2023_scatterdf <- state_maternalhealth_avgs %>% 
  left_join(all50_state_facilities, by = c("State" = "state_abb")) %>% 
  filter(Year %in% c(2023), State != "District of Columbia") %>% 
  select(avg_cesarean, avg_lbw, avg_fertility, avg_preterm, avg_births, count) 

sum(mhcf_2023_scatterdf$count) # Check that all facilities accounted for- true (all 607 present)

# Add column with state names for tooltip display
statenamesandabbvs <- data.frame(state_name = state.name, state_abb = state.abb)

# Join back to mhcf_2023_scatterdf
mhcf_2023_scatterdf <- mhcf_2023_scatterdf %>%  
  left_join(statenamesandabbvs, by = c("State" = "state_abb"))

# Create df option with only the 38 states that have facilities, in case we choose this route for visualization
## Update: we will not use this dataframe, and will include all 50 states, but leaving here in case
mhcf_2023_scatterdf_only38 <- mhcf_2023_scatterdf %>% 
  filter(count != 0)

# Create scatterplot
mhcf_scatterplot <- mhcf_2023_scatterdf %>% # mh = maternal health, cf = chemical facilities
  ggplot(aes(x = count, y = avg_cesarean)) + 
  geom_point(alpha = 0.5) + 
  labs(title = "Number of Chemical Facilities and Maternal Health Outcomes", 
       x = "Number of Chemical Facilities per State", 
       y = "Average Rate of Cesarean Deliveries") +
  theme_minimal()
mhcf_scatterplot

# Notes: for Shiny, we can alter title label and y axis label to reflect which outcome is selected for

# your code here
# mhcf_2023_scatterdf # Use this for plotting

# your code below 
# state_data_2014_2024 # Use this for plotting
# view(state_data_2014_2024)
# view(all50_state_facilities)

# creating national averages for each maternal health outcome 

ntl_maternal_avgs <- state_data_2014_2024 %>%
  filter(State != "District of Columbia") %>%   #filter out DC
  group_by(Year) %>%
  summarise(
    ntl_cesarean = mean(`Cesarean Deliveries Percent`, na.rm = TRUE),
    ntl_lbw = mean(`Low Birth Weight Percent`, na.rm = TRUE),
    ntl_fertility = mean(`Fertility Rate per 1,000 women`, na.rm = TRUE),
    ntl_preterm = mean(`Preterm Birth Percent`, na.rm = TRUE),
    .groups = "drop"
  )

view(ntl_maternal_avgs)       


###### BEGIN SHINY APP CODE BELOW ###### s

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
      style = "font-size:12px;",
    p(
      "This interactive dashboard explores the relationship between plastic chemical production facilities and maternal health outcomes in the United States."
      ),
    
    strong("Background"),
    
    p(
      "Women in the US are at greater risk of experiencing adverse maternal health outcomes. These disparate outcomes are a result of a variety of factors, including environmental exposures. Chemicals involved in plastic production are linked to maternal health effects such as decreased fertility, preterm birth, cesarean deliveries, and low birth weight. Facilities that produce these toxic chemicals can contaminate air, water, and soil, exposing surrounding communities to a suite of hazardous chemicals."
      ),
    
    p(
      "Examining whether the presence of plastic chemical production facilities is related to adverse maternal health outcomes may improve understanding and help identify policy interventions that reduce maternal health disparities."
    ),
    
    strong("Methods"),
    
    p(
      "Chemical production facility data was obtained from the most recent round of EPA Chemical Data Reporting in 2024. A list of the plastic production chemicals included in this analysis is available",
      tags$a(href = "https://docs.google.com/spreadsheets/d/1SMKLifv7GmNUcRvL_HIJr1Ib90fFDOkrgmID7XN3OO8/edit?gid=0#gid=0", 
      "here.",
      target = "_blank" 
      ),
      "State-level maternal health data was obtained from the CDC’s National Vital Statistics System. The dashboard was developed in R, and coding assistance was provided by ChatGPT (GPT-5.5)."
    ),
    
    strong("The Dashboard"),
    
    p(
      "Users can begin the investigation by selecting for fertility, preterm birth, cesarean deliveries, or low birth weight, which then produces three visualizations for exploration. First, the line graph shows maternal health trends over time, and compares state data to the national average. Next, the scatterplot investigates the relationship between the frequency of facilities and maternal health by state. Lastly, the map provides an interactive visualization of the geographic relationship between chemical facilities and maternal health."
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
            "States:", 
            choices = state.abb, 
            selected = state.abb[1], #NULL,
            multiple = TRUE, 
            options = list(maxItems = 3),
            width = NULL
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
    
#    outcome_col <- switch(input$mh_outcome,      #user picks an option and it pulls from the called dataset

#    validate(     
#     need(length(input$selected_states) > 0, "Please select at least one state.")
 #   )     # makes it so you don thave to have 5 states to see a graph, but can see it with 1-4 too
    
    req(input$selected_states)
    
    outcome_col <- switch(input$mh_outcome,

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
      ) %>% 
      select(Year, selected_value, Series)
    
    national_data <- ntl_maternal_avgs %>%
      mutate(
        selected_value = .data[[national_col]],
        Series = "National" 
      ) %>% 
      select(Year, selected_value, Series)
    
    line_data <- bind_rows(line_data, national_data)  # makes it so you dont have to have 5 states to see a graph, but can see it with 1-4 too, and national avg is fixed


    state_series <- setdiff(unique(line_data$Series), "National")
    
    line_colors <- c(
      setNames(
        colorRampPalette(c("#e4acac", "#ad9ea5"))(length(state_series)),
        state_series
      ),
      "National" = "#8B0000"
    )
    
 #   label_data <- line_data %>%
#      group_by(State) %>%
#     filter(Year == max(Year)) %>%
#      ungroup() 

    
    line_plot <- ggplot(
      line_data,          #user selects outcome, and state will be assigned a color 
      aes(
        x = Year,
        y = selected_value,
        color = Series,
        group = Series,
        text = paste(
                     paste0(outcome_tooltip(),":"), 
                     round(selected_value, 2))

      )
    ) +
      
      geom_line(linewidth = 0.6) +
      geom_line(
        data = dplyr::filter(line_data, Series == "National"),
        linewidth = 0.8
      ) +
      geom_point(size = 1.0) +
      
#      geom_text(              #State abbreviation pops out next to state line
 #       data = label_data,
#        aes(label = State),
 #       hjust = 4,
#        size = 3,
 #       color = "black"
#      ) +
      
 #     scale_y_continuous(
 #       limits = c(0, NA)
 #     ) +
      
      labs(
        title = paste(input$mh_outcome),
        subtitle = "Hover over lines to see chemical facility counts",
        x = "Year",
        y = outcome_label(),     #the reactive label
        color = NULL
      ) +
      
      scale_color_manual(values = line_colors) +
      

      scale_x_continuous(
        breaks = c(2014, 2016, 2018, 2020, 2022, 2024)
      ) +
      
      theme_minimal() +
      theme(
        legend.position = "right",
        axis.title.y = element_text(size = 9),

        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 9, hjust = 0.5),
        plot.subtitle = element_text(size = 10)
      )
    ggplotly(line_plot, tooltip = "text") %>% 
      layout(dragmode = FALSE) %>%
      config(displayModeBar = FALSE, scrollZoom = FALSE) 
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
    
    ggplotly(scatter, tooltip = "text") %>%
      layout(dragmode = FALSE) %>%
      config(displayModeBar = FALSE, scrollZoom = FALSE)
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
      setView(lng = -98, lat = 39, zoom = 3.5) %>% 
      addPolygons(fillColor = ~color, fillOpacity = 0.8, color = "white", weight = 0.3,
                  popup = ~paste0("County: ", NAME,
                                  "<br>Facilities: ", count,
                                  "<br>", popup_label_units, ": ", round(outcome_value, 2))) %>% 
                                #  "<br>", input$mh_outcome, ": ", round(outcome_value, 2))) %>% 
      
       addControl(html = legend_html, position = "bottomright")
  })

}


shinyApp(ui, server)
