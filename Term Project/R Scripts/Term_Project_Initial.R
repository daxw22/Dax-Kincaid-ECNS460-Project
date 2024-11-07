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
qtm(fl_counties) # test to see functionality


# Hurricane Data -------------------------------------------
florida_storms=read.csv("hurdat2-noaa.csv")
#--------------
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
  filter(!str_detect(storm_name, "UNNAMED"))
