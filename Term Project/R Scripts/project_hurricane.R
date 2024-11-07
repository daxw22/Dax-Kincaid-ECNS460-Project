rm(list=ls())
#TERM PROJECT

# import libraries
library(tidyverse)
library(sf)
library(tmap)
library(units)
library(dplyr)

# Florida Counties -----------------------------------------
# Load Data
fl_counties = st_read("USA_Counties/USA_Counties.shp") |>
  st_make_valid() |>
  filter(STATE_NAME == "Florida") |>
  select(NAME, everything())

tmap_mode("view")


# Hurricane Data -------------------------------------------
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



##############################################################################




# Creating Visuals -----------------------------------------
# Basic Map of Florida Counties
florida_counties_map <- qtm(fl_counties, 
                            fill = "lightblue",  # Color the counties
                            borders = "black",   # Add borders
                            main.title = "Florida Counties") 

florida_counties_map  # View the map
tmap_save(florida_counties_map, "output/fl_county_map.png")

#See Storm Locations
storm_map <- tm_shape(fl_counties) + 
  tm_borders() +  # Add county borders
  tm_shape(storms_2000) + 
  tm_bubbles(size = 0.1, col = "red", border.col = "black", alpha = 0.7, 
             legend.size.is.portrait = TRUE) +  # Show storm locations as bubbles
  tm_layout(main.title = "Storm Locations in Florida", 
            legend.outside = TRUE, 
            frame = FALSE)

storm_map 
tmap_save(storm_map, "output/storm_map.png")


# Storm Tracks
storms_2023_2024 <- storms_2000 %>%
  filter(year(date) %in% c(2020:2024))  # Filter storms for 2023 and 2024

# Ensure points are ordered by date for each storm
storm_tracks <- storms_2023_2024 %>%
  group_by(storm_name) %>%
  arrange(storm_name, date) %>%  # Ensure proper order by storm name and date
  st_as_sf()  # Ensure it's an sf object if it's not already

# Combine points into a single LINESTRING per storm
storm_tracks <- storm_tracks %>%
  group_by(storm_name) %>%
  summarise(geometry = st_union(geometry)) %>%  # Combine points into a single geometry for each storm
  st_cast("LINESTRING")  # Convert the unioned geometry to LINESTRING

# Plot the storm tracks for 2023 and 2024
storm_tracks_map <- tm_shape(fl_counties) + 
  tm_borders() +  # Add county borders
  tm_shape(storm_tracks) + 
  tm_lines(col = "blue", lwd = 2, alpha = 0.7) +  # Plot storm tracks as blue lines
  tm_layout(main.title = "Storm Tracks in Florida (2020-2024)", 
            legend.outside = TRUE, 
            frame = FALSE)

# Display the map
storm_tracks_map
tmap_save(storm_tracks_map, "output/storm_tracks_map.png")


# Visualize frequency of hurricanes by year 
storm_frequency <- storms_2000 %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  group_by(storm_name, year) %>%  # Group by storm name and year
  slice(1) %>%  # Retain only the first occurrence per storm and year
  ungroup() %>% 
  ggplot(aes(x = year, fill = wind_knot)) +
  geom_histogram(binwidth = 1, color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Wind Speed (knots)") +
  labs(
    title = "Frequency of Storms Affecting Florida by Year",
    x = "Year",
    y = "Number of Storms"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )
storm_frequency
ggsave("output/storm_frequency.png", plot = storm_frequency, bg = "white")



# Visualize hurricane paths over Florida counties
storm_map_wind <- tm_shape(fl_counties) +
  tm_borders(col = "gray70") +
  tm_shape(storms_2000) +
  tm_dots(col = "wind_knot", size = 0.1, palette = "YlOrRd", title = "Wind Speed (knots)") +
  tm_layout(
    title = "Spatial Distribution of Storm Paths over Florida",
    title.position = c("center", "top"),
    legend.position = c("left", "bottom"),
    frame = FALSE
  )
storm_map_wind
tmap_save(storm_map_wind, "output/storm_map_wind.png")


wind_speed <- ggplot(storms_2000, aes(x = wind_knot)) +
  geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black") +
  labs(title = "Wind Speed Frequency(knots)", x = "Wind Speed", y = "Frequency")
wind_speed
ggsave("output/wind_speed.png", plot = wind_speed, bg = "white")





