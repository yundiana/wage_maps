# get packages using Pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(geojsonio, leaflet, leaflet.extras, dplyr, readr, stringr, sf, tidyverse, mapview, geojsonsf)

get_dataset_and_process_it <- function() {
  # Load the CSV with data for all UK
  file <- paste0("data/ashe_", tolower(area) ,".csv")
  ashe <- read_csv(file) %>% 
    # Remove duplicated columns
    select(-statistics, -sex, -"calendar-years",-workingpattern, -'ashe-working-pattern', -hoursandearnings, -workplaceorresidence) %>% 
    # Rename for convenience the data colums
    rename(
      'stat' = 'ashe-statistics', 
      'sex' = 'ashe-sex', 
      'geo_code' = 'admin-geography',
      'location' = 'ashe-workplace-or-residence',
      'hours' = 'ashe-hours-and-earnings',
      'value' = V4_2
    )
  
  # Set year as global variable
  i <- 2016
  while (i < 2019) {
    year <<- i
    process_local_authorities_dataset(filter(ashe, time == i, stat == 'mean'))
    i <- i + 1
  }  
}

lf_options <- leafletOptions(minZoom = 10, maxZoom = 14)
area <- "London"
get_dataset_and_process_it()

lf_options <- leafletOptions(minZoom = 10, maxZoom = 14)
area <- "Manchester"
get_dataset_and_process_it()

lf_options <- leafletOptions(minZoom = 10, maxZoom = 14)
area <- "Liverpool"
get_dataset_and_process_it()