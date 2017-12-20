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

# LOAD DATA: Code Enforcement Cases ----

# Code enforcement cases

cases_url <- "https://docs.google.com/spreadsheets/d/1xmpKg63VrDfZzaYJbqiK3-0FQhQIp9Ur586fuMBjEv0/edit?usp=sharing"

data_sheet <- gs_url(cases_url, lookup = NULL, visibility = NULL, verbose = TRUE)

col_types <- list(col_character(),
                  col_character(),
                  col_date(format = "%m/%d/%Y"),
                  col_character(),
                  col_character(),
                  col_character(),
                  col_character(),
                  col_date(format = "%m/%d/%Y")
)

cases <- data_sheet %>% 
  gs_read(range = cell_limits(c(1, 1), c(NA, 8)), 
          colnames = TRUE, 
          na = c("","#N/A"),
          col_types = col_types) %>% 
  set_colnames(str_replace_all(colnames(.),"\\s","_")) 



# GEOCODE DATA ----

geocode_fun <- function(address){
  geocode_url(address, 
              auth="standard_api", 
              privkey="AIzaSyCcDHXFWGdwZrijiKuRNTvS7DWLuZP5dAA",
              clean=TRUE, 
              add_date='today', 
              verbose=TRUE) %>% 
    as_tibble
}

# slow operation

c <- cases %>% 
  mutate(ADDR = if_else(is.na(RFS_ADDRESS),OTHER_LOCATION,RFS_ADDRESS),
         ADDR = str_c(ADDR, "Kent", "WA", sep = ", "),
         ADDR_TBL = map(ADDR,geocode_fun))

c_trim <- c %>% 
  unnest %>% 
  rename_all(to_screaming_snake_case) %>% 
  filter(LOCATION_TYPE %!in% 'APPROXIMATE') %>% 
  filter(STATUS %!in% "ZERO_RESULTS")

c_sf <- c_trim %>% 
  mutate(geometry = map2(LNG, LAT, ~st_point(c(.x,.y))),
         geometry = st_sfc(geometry)) %>% 
  st_as_sf %>% 
  st_set_crs(4326)
  
# SAVE & UPLOAD TO DRIVE ---- 

c_fp <- root_file("./1-data/3-interim/code-enforcement-cases-2017.gpkg")

drive_folder_id <- as_id("0B5Pp4V6eCkhrRFRYbWpoM3pWYkU") # ~/3-interim/

st_write(obj = c_sf,dsn = c_fp, layer = 'code_enforcement_cases_2017', driver = 'GPKG', layer_options = 'OVERWRITE=TRUE')

drive_upload(media = c_fp, path = drive_folder_id)

drive_update(file = as_id("1bNxVfC8klsCmpQ7VvbDycL0mYsPUYUgA"), c_fp)
