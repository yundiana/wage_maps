# get packages using Pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(geojsonio, leaflet, leaflet.extras, dplyr, readr, stringr, sf, tidyverse, mapview, geojsonsf)

url <- "https://opendata.arcgis.com/datasets/0e07a8196454415eab18c40a54dfbbef_0.geojson"

local_authorities <- geojson_sf(url) %>%
  # Rename for convenience the data colums
  rename(geo_code = lad19cd, name = lad19nm)

pal <- colorNumeric("viridis", NULL, na.color = "#D1D1D1")
pct_val <- c(-12,105)
pal_pct <- colorNumeric("viridis", pct_val, na.color = "#D1D1D1")
map_dir <- paste0(getwd(), "/html/")

get_popup <- function(dataset) {
  popup <- paste0("<b>", dataset$name ,":</b>",
                  '<br>Mean Annual Salary: GBP ', dataset$value,
                  '<br>Male Mean Annual Salary: GBP ', dataset$value.x,
                  '<br>Female Mean Annual Salary: GBP ', dataset$value.y,
                  '<br>Difference:  ', dataset$diff, ' (', dataset$ratio, '%)')
  return(popup)
}

# function for filtering, preparing and processing the dataset to generate maps
process_local_authorities_dataset <- function(dataset) {
  annual_dataset <- filter(dataset, hours =='annual-pay-gross')
  parm_location <- "workplace"
  process_by_location(annual_dataset, parm_location)
  parm_location <- "residence"
  process_by_location(annual_dataset, parm_location)
}

# Process map by locations
process_by_location <- function(dataset, parm_location) {
  dataset <- filter(dataset, location == parm_location)
  inner_join(filter(dataset, sex == 'male'), filter(dataset, sex == 'female'), by = 'geo_code') -> map_dataset
  inner_join(filter(dataset, sex == 'all'),  map_dataset, by = 'geo_code') -> map_dataset
  inner_join(local_authorities, map_dataset, by = 'geo_code') -> map_dataset
  map_dataset$diff <- map_dataset$value.x - map_dataset$value.y
  map_dataset$ratio <- round(map_dataset$value.x / map_dataset$value.y * 100, digits = 2) - 100
  
  area_lc <- tolower(area)
  map_name <- paste0("map_", area_lc, "_", year, "_", parm_location)
  salary_map(map_dataset, parm_location, paste0(map_name, "_mean"))
  percentage_map(map_dataset, parm_location, paste0(map_name, "_pct"))
  group_map(map_dataset, parm_location, paste0(map_name, "_group"))
}

# function to generate map in a separate html
generate_map_file <- function(map, map_name) {
  html_file <- paste0(map_dir, "map.html")
  # To continue execution dispite failure use try, sometimes some images failed, but the general is working
  try(mapshot(map, url = html_file))
  
  map_file <- paste0(map_name, ".html")
  file.rename(html_file, paste0(map_dir, map_file))
  
  rmd_file <- str_replace(paste0(map_name, ".rmd"), 'map_', '')
                         
  writeLines(paste0('<iframe style="width:100%; height: 70vh;" src="html/', map_file, '" title="Map"></iframe>'), con=rmd_file)
}

# function to generate leaflet for annual salary map
salary_map <- function(dataset, location, map_name) {
  label <- paste0(dataset$name, " has an average annual pay gross of GBP ", as.character(dataset$value))
  
  popup <- get_popup(dataset)
  
  map <- leaflet(dataset, options = lf_options)%>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(stroke = TRUE, 
                weight = 2, 
                smoothFactor = 0.2, 
                fillColor = ~pal(value), 
                color = "#001a00",
                label = label,
                fillOpacity = 0.7,
                popup = popup,
    ) %>%
    addLegend(pal = pal, bins = 10, values = ~value, opacity = 1.0, na.label = "No Data", title = "GBP / year", position = "bottomright")  %>%
    addControl(paste0("Average annual salary (GBP) in ", area ," Local Authorities by ", location, " location in year ", year), position = "topright")
  
  generate_map_file(map, map_name)
}

# function to generate leaflet percentage difference between men and women
percentage_map <- function(dataset, location, map_name) {
  popup <- get_popup(dataset)
  
  label <- paste0(dataset$name, "  has an excess percentage of ", as.character(dataset$ratio))
  
  map <- leaflet(dataset, options = lf_options)%>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(stroke = TRUE, 
                weight = 2, 
                smoothFactor = 0.2, 
                fillColor = ~pal_pct(ratio), 
                color = "#001a00",
                label = label,
                fillOpacity = 0.7,
                popup = popup,
    ) %>%
    addLegend(pal = pal_pct, bins = 10, values = pct_val, opacity = 1.0, na.label = "NA", title = "%", position = "bottomright")  %>%
    addControl(paste0("Percentage of Excess in male mean annual salary over female mean annual salary in ", area ," Local Authorities by ", location, " location in year ", year), position = "topright")
  
  
  generate_map_file(map, map_name)
}

# function to generate leaflet grouped map by 5 quantiles
group_map <- function(dataset, location, map_name) {
  popup <- get_popup(dataset)
  
  label <- paste0(dataset$name, " has an average annual pay gross of GBP ", as.character(dataset$value))
  
  group_pal <- colorQuantile("viridis", dataset$value, na.color = "#D1D1D1", n = 5)
  
  # generate a special label for quantiles
  group_pal_colors <- unique(group_pal(sort(dataset$value))) # hex codes
  group_pal_labs <- quantile(dataset$value, seq(0, 1, .2), na.rm=TRUE) # depends on n from pal
  group_pal_labs <- paste(lag(group_pal_labs), group_pal_labs, sep = " - ")[-1] # first lag is NA
  
  map <- leaflet(dataset, options = lf_options)%>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(stroke = TRUE, 
                weight = 2, 
                smoothFactor = 0.2, 
                fillColor = ~group_pal(value), 
                color = "#001a00",
                label = label,
                fillOpacity = 0.7,
                popup = popup,
    ) %>%
    addLegend(colors = group_pal_colors, labels = group_pal_labs, opacity = 1.0, na.label = "NA", title = "Group Range", position = "bottomright")  %>%
    addControl(paste0(" Local Authorities in ", area ," divided in 5 groups according annual average salary by ", location, " location in year ", year), position = "topright")
  
  generate_map_file(map, map_name)
}