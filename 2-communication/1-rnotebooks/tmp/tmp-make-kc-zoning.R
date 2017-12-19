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

# King County Consolidated Zoning

kc_zng_url <- "https://opendata.arcgis.com/datasets/7edc8f5c0efd4ef38f9bd7ebbdb51822_2493.geojson"

kc_zng <-  read_sf(kc_zng_url, stringsAsFactors = FALSE)


# SAVE & UPLOAD TO DRIVE ---- 

kc_zng_fp <- root_file("1-data/2-external/code-enforcement-cases-2017.gpkg")

drive_folder_id <- as_id("0B5Pp4V6eCkhrQ29lVGsxaS1ERXM") # ~/2-external/

st_write(obj = kc_zng,dsn = kc_zng_fp, layer = 'kc_zoning_consolidated', driver = 'GPKG', layer_options = 'OVERWRITE=TRUE')

drive_upload(media = kc_zng_fp, path = drive_folder_id)

drive_update(file = as_id(""), kc_zng_fp)
