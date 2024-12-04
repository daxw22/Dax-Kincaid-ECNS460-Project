library(shiny)
library(tidyverse)
library(sf)
library(tmap)
library(ggplot2)
library(DT)

# Load and prepare data
fl_counties <- st_read("USA_Counties/USA_Counties.shp") |>
  st_make_valid() |>
  filter(STATE_NAME == "Florida") |>
  select(NAME, geometry)


florida_storms=read.csv("hurdat2-noaa.csv")

#Cleaning 
#splitting up columns
#Column 1: AL011851
florida_storms<-florida_storms|>
  mutate(year = str_sub(AL011851., 1, 4),
         month_day = str_sub(AL011851., 5, nchar(AL011851.)))|>
  select(-AL011851.)|>
  mutate(month = str_sub(month_day, 1, 2),
         day = str_sub(month_day, 3, nchar(month_day)))|>
  select(-month_day)|>
  mutate(date = as.Date(paste(year, month, day, sep = "-")))|>
  select(-year, -month, -day)

#Column 2:
florida_storms<-florida_storms|>
  separate(X..UNNAMED.., into = c("storm_name", "latitude"), sep = ", ")

#Renaming variables
florida_storms<-florida_storms|>
  rename(time_military = "X")|>
  rename(wind_knot = "X.2")|>
  rename(pressure_millibars = "X.3")|>
  rename(longitude ="X14.")
#Removing unneccessary variables
florida_storms<-florida_storms|>
  select(-X.1)|>
  select(-matches("^X\\.(4|5|6|7|8|9|10|11|12|13|14|15|16)$"))
#Removing commas
florida_storms<-florida_storms|>
  mutate_all(~gsub(",", "", .))

#Subsetting past 2000
storms_2000<-florida_storms|>
  slice(43316:n())

#clean data further
storms_2000 <- storms_2000 |>
  mutate(storm_type = storm_name) |>
  mutate(storm_name = ifelse(!is.na(date), NA, storm_name)) |>
  fill(storm_name, .direction = "down") |>
  filter(!is.na(date))|>
  filter(!str_detect(storm_name, "UNNAMED")) %>% 
  mutate_all(~gsub(" 1", "", .)) |>
  mutate(date = as.Date(date, format = "%Y-%m-%d")) |>
  mutate(wind_knot = as.numeric(wind_knot))

# Convert to spatial data
storms_2000 <- storms_2000 %>%
  mutate(
    # Check for South (S) in latitude and West (W) in longitude before cleaning
    latitude_south = str_detect(latitude, "S"),  # Check if latitude is South
    longitude_west = str_detect(longitude, "W"),  # Check if longitude is West
    
    # Clean latitude and longitude by removing all non-numeric characters except '-' and '.'
    latitude_clean = gsub("[^0-9.-]", "", latitude),
    longitude_clean = gsub("[^0-9.-]", "", longitude),
    
    # Convert to numeric after cleaning
    latitude_clean = as.numeric(latitude_clean),
    longitude_clean = as.numeric(longitude_clean),
    
    # Apply directionality logic: negative if South or West
    latitude = ifelse(latitude_south, -latitude_clean, latitude_clean),  # Make negative if South
    longitude = ifelse(longitude_west, -longitude_clean, longitude_clean)  # Make negative if West
  ) %>%
  filter(
    latitude >= -90 & latitude <= 90,  # Latitude should be between -90 and 90
    longitude >= -180 & longitude <= 180  # Longitude should be between -180 and 180
  )

# Convert to spatial data
storms_2000 <- storms_2000 %>%
  st_as_sf(
    coords = c("longitude", "latitude"), 
    crs = 4326
  )
# Check and align CRS with fl_counties
if (st_crs(fl_counties) != st_crs(storms_2000)) {
  storms_2000 <- st_transform(storms_2000, st_crs(fl_counties))
}


# Shiny app
ui <- fluidPage(
  titlePanel("Florida Hurricanes Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("county", "Select a County:", choices = fl_counties$NAME),
      helpText("Select a county to view hurricanes passing through it.")
    ),
    mainPanel(
      tmapOutput("map"),
      DTOutput("stats"),
      uiOutput("storm_info")
    )
  )
)

server <- function(input, output, session) {
  # Reactive map of hurricanes filtered by county
  output$map <- renderTmap({
    county <- fl_counties %>% filter(NAME == input$county)
    hurricanes_in_county <- storms_2000 %>%
      st_intersects(county, sparse = FALSE) %>%
      as.vector()
    filtered_hurricanes <- storms_2000[hurricanes_in_county, ]
    
    # Create the base map with the county
    base_map <- tm_shape(fl_counties) +
      tm_borders() +
      tm_shape(county) +
      tm_fill(col = "lightblue", alpha = 0.5) +
      tm_layout(main.title = paste("Hurricanes in", input$county))
    
    # If no hurricanes are recorded, return the base map with a clear label
    if (nrow(filtered_hurricanes) == 0) {
      base_map
    }
    
    # If hurricanes exist, add them to the map
    else{
    base_map +
      tm_shape(filtered_hurricanes) +
      tm_bubbles(size = 0.1, col = "red", border.col = "black", alpha = 0.7)}
  })
  
  
  # Reactive table of hurricanes and associated data
  output$stats <- renderDT({
    county <- fl_counties %>% filter(NAME == input$county)
    hurricanes_in_county <- storms_2000 %>%
      st_intersects(county, sparse = FALSE) %>%
      as.vector()
    filtered_hurricanes <- storms_2000[hurricanes_in_county, ] %>%
      as.data.frame() %>%
      select(storm_name, date, time_military, wind_knot, pressure_millibars, storm_type)  # Select relevant columns
    
    if (nrow(filtered_hurricanes) == 0) {
      return(datatable(
        data.frame(
          Message = paste("No storms recorded for", input$county)
        ),
        options = list(pageLength = 1, dom = 't'),
        rownames = FALSE
      ))
    }
    
    # Generate the table with descriptive column names
    datatable(
      filtered_hurricanes,
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE,
      colnames = c("Storm Name", "Date", "Time Recorded", "Wind Speed (knots)", "Pressure (mb)", "Storm Type")
    )
  })
  
  # Reactive display of storm-related info and mitigation link
  output$storm_info <- renderUI({
    county <- fl_counties %>% filter(NAME == input$county)
    hurricanes_in_county <- storms_2000 %>%
      st_intersects(county, sparse = FALSE) %>%
      as.vector()
    filtered_hurricanes <- storms_2000[hurricanes_in_county, ]
    
    if (nrow(filtered_hurricanes) == 0) {
      return(NULL)  # No storms, so no link is displayed
    }
    
    # Display the generic mitigation link
    else {
      tagList(
        h4("Learn how to mitigate hurricane damage to homes:"),
        a("Click here for hurricane damage mitigation strategies", href = "https://community.fema.gov/ProtectiveActions/s/article/Hurricane-Protect-Your-Property-Mitigation-and-Retrofitting#:~:text=Install%20hurricane%20shutters%2C%20cover%20windows,carports%2C%20porches%2C%20and%20garages.")
      )
    }
  })
}


# Run the app
shinyApp(ui = ui, server = server)

