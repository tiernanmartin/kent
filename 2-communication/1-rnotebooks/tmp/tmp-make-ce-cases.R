# SETUP ----

library(magrittr)
library(tidyverse)
library(scales)
library(sf)
library(RSocrata)
library(leaflet)
library(stringr)
library(lubridate)
library(anytime)
library(forcats)
library(googlesheets)    # devtools::install_github('jennybc/googlesheets')
library(googledrive)     # devtools::install_github('tidyverse/googledrive')
library(mapedit)         # devtools::install_github("r-spatial/mapedit")
library(mapview)         # devtools::install_github("r-spatial/mapview@develop")
library(leaflet.extras)  # devtools::install_github("bhaskarvk/leaflet.extras")
library(knitr)
library(miscgis)         # devtools::install_github("tiernanmartin/miscgis")
library(snakecase)
library(placement)       # devtools::install_github("DerekYves/placement")
library(rprojroot)

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())
# LOAD DATA ----

# Kent boundary

# data source: https://hub.arcgis.com/datasets/3fdb7c41de8548c5ab5f96cb1ef303e2_446
cities <- st_read("https://opendata.arcgis.com/datasets/3fdb7c41de8548c5ab5f96cb1ef303e2_446.geojson")

kent_bound_sf <- cities %>% 
  filter(CITYNAME %in% "Kent") %>% 
  arrange(desc(SHAPE_Area)) %>% 
  slice(1)

kent_cntr <- kent_bound_sf %>% 
  st_centroid() %>% 
  mutate(LNG = map_dbl(geometry,1),
         LAT = map_dbl(geometry,2)) %>% 
  st_drop_geometry()

# Code enforcement cases

url <- "https://docs.google.com/spreadsheets/d/1xmpKg63VrDfZzaYJbqiK3-0FQhQIp9Ur586fuMBjEv0/edit?usp=sharing"

data_sheet <- gs_url(url, lookup = NULL, visibility = NULL, verbose = TRUE)

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

# King County Consolidated Zoning

zng_fp <- root_file("1-data/2-external/zoning_kc_consol_20")

zng_load <- zng_fp %>% 
  make_or_read({
                 
                 dr_id <- as_id("0B5Pp4V6eCkhrOTUwT29WQl9STVk")
                 
                 zip_dir <- root_file("1-data/2-external")
                 
                 target_name <- "zoning_kc_consol_20"
                 
                 drive_read_zip(dr_id = dr_id,
                                .tempdir = FALSE,
                                dir_path = zip_dir,
                                read_fun = st_read,
                                target_name = target_name,
                                layer = "zoning_kc_consol_20", 
                                stringsAsFactors = FALSE)
                 
               },
               {
                 st_read(zng_fp, layer = "zoning_kc_consol_20", stringsAsFactors = FALSE)
               })


# GEOCODE DATA ----

geocode_fun <- function(address){
  geocode_url(address, 
              auth="standard_api", 
              privkey="AIzaSyBIPZUTQtrvCxLYEpXcgcOF5xXRawzXKRE",
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
  
  
# EDA ----

# Check the PROBLEM category levels

c_trim %>% 
  transmute(PROBLEM = factor(PROBLEM)) %>% 
  count(PROBLEM, sort = TRUE) %>% 
  print(n = Inf)

cat_list <- c("RATS/BUGS", "BUILDING", "MOLD", "DANG BLDG", "ROOF LEAKS")

c_bldg_sf <- filter(c_sf, PROBLEM %in% cat_list)



# MAP DATA ----

orange <- "#e9673e"

leaflet() %>% 
  addProviderTiles(provider = providers$CartoDB.PositronNoLabels) %>% 
  addCircleMarkers(data = c_bldg_sf, 
                   color = orange,
                   weight = .5,
                   opacity = 1) %>% 
  addPolygons(data = kent_bound_sf,
              color = "#434343",
              opacity = 1,
              fillOpacity = 0) %>% 
  setView(kent_cntr$LNG,kent_cntr$LAT,  12)
# SAVE & UPLOAD TO DRIVE ---- 

