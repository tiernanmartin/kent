# SETUP ----

library(magrittr)
library(scales)
library(tidyverse)
library(lwgeom)
library(sf)
library(RSocrata)
library(leaflet)
library(lubridate) 
library(googlesheets)    # devtools::install_github('jennybc/googlesheets')
library(googledrive)     # devtools::install_github('tidyverse/googledrive') 
library(mapview)         # devtools::install_github("r-spatial/mapview@develop")
library(knitr)
library(miscgis)         # devtools::install_github("tiernanmartin/miscgis")
library(snakecase)
library(placement)       # devtools::install_github("DerekYves/placement")
library(rprojroot)

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())
# CREATE DATA ----

# Kent boundary

# data source: https://hub.arcgis.com/datasets/3fdb7c41de8548c5ab5f96cb1ef303e2_446
kc_cities <- st_read("https://opendata.arcgis.com/datasets/3fdb7c41de8548c5ab5f96cb1ef303e2_446.geojson")

kent_bound_sf <- kc_cities %>% 
  filter(CITYNAME %in% "Kent") %>% 
  arrange(desc(SHAPE_Area)) %>% 
  slice(1)


# SAVE & UPLOAD TO DRIVE: King County Cities ---- 

kc_cities_fp <- root_file("./1-data/2-external/kc-cities.gpkg")

drive_folder_id <- as_id("0B5Pp4V6eCkhrQ29lVGsxaS1ERXM")

st_write(obj = kc_cities, dsn = kc_cities_fp, layer = 'kc_cities', driver = 'GPKG', layer_options = 'OVERWRITE=TRUE')

# drive_upload(media = kc_cities_fp, path = drive_folder_id)

drive_update(file = as_id("1_A5B3rbGjdxzIqrrwfhUJPK4DKi6G0En"), kc_cities_fp)

# SAVE & UPLOAD TO DRIVE: City of Kent Boundary ---- 

kent_bound_fp <- root_file("./1-data/3-interim/kent-boundary.gpkg")

drive_folder_id <- as_id("0B5Pp4V6eCkhrRFRYbWpoM3pWYkU")

st_write(obj = kent_bound_sf,dsn = kent_bound_fp, layer = 'kent_boundary', driver = 'GPKG', layer_options = 'OVERWRITE=TRUE')

drive_upload(media = kent_bound_fp, path = drive_folder_id)

drive_update(file = as_id("1C4b7TKmrKzWxj53ajJkvb-ddS2bE9NZ0"), kent_bound_fp)
