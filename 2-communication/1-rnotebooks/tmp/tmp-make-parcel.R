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
library(esri2sf)

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())
# CREATE DATA ----

# King County Parcels

kc_p_url <- "https://opendata.arcgis.com/datasets/c7a17b7ad3ec44b7ae64796dca691d72_1722.geojson"

kc_p_load <- read_sf(kc_p_url, stringsAsFactors = FALSE)

kc_p <- rename_if(kc_p_load, not_sfc, to_screaming_snake_case)

# Kent Parcels

kent_p <- kc_p %>% 
  filter(toupper(CTYNAME) %in% 'KENT')

# SAVE & UPLOAD TO DRIVE: King County Parcels ---- 

kc_p_fp <- root_file("1-data/2-external/kc-parcels.gpkg")

drive_folder_id <- as_id("0B5Pp4V6eCkhrQ29lVGsxaS1ERXM") # ~/2-external/

st_write(obj = kc_p,dsn = kc_p_fp, layer = 'kc_parcels', driver = 'GPKG', layer_options = 'OVERWRITE=TRUE')

drive_upload(media = kc_p_fp, path = drive_folder_id)

drive_update(file = as_id(""), kc_p_fp)

# SAVE & UPLOAD TO DRIVE: King County Parcels ---- 

kent_p_fp <- root_file("1-data/3-interim/kent-parcels.gpkg")

drive_folder_id <- as_id("0B5Pp4V6eCkhrRFRYbWpoM3pWYkU") # ~/3-interim/

st_write(obj = kent_p,dsn = kent_p_fp, layer = 'kent_parcels', driver = 'GPKG', layer_options = 'OVERWRITE=TRUE')

drive_upload(media = kent_p_fp, path = drive_folder_id)

drive_update(file = as_id(""), kent_p_fp)