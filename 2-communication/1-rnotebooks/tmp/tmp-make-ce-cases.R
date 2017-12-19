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
                                read_fun = read_sf,
                                target_name = target_name,
                                layer = "zoning_kc_consol_20", 
                                stringsAsFactors = FALSE)
                 
               },
               {
                 read_sf(zng_fp, layer = "zoning_kc_consol_20", stringsAsFactors = FALSE)
               })

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
  
c_fp <- root_file("./1-data/3-interim/code-enforcement-cases-2017.gpkg")

drive_folder_id <- as_id("0B5Pp4V6eCkhrRFRYbWpoM3pWYkU")

st_write(obj = c_sf,dsn = c_fp, layer = 'code_enforcement_cases_2017', driver = 'GPKG', layer_options = 'OVERWRITE=TRUE')

# drive_upload(media = c_fp, path = drive_folder_id)

drive_update(file = as_id("1MnVFK7kXlqduUwtBuH9gi3ztm9h22P4n"), c_fp)



# SPATIAL OVERLAY ----

# prep zoning

zng <- zng_load %>% 
  st_transform(2926)

zng_subd <- zng %>% 
  st_subdivide(max_vertices = 256) %>% 
  st_collection_extract() %>% 
  st_cast('MULTIPOLYGON')

# prep cases (check the PROBLEM category levels) 

cat_list <- c("RATS/BUGS", "BUILDING", "MOLD", "DANG BLDG", "ROOF LEAKS")

c_bldg_sf <- filter(c_sf, PROBLEM %in% cat_list)

c_2926 <- st_transform(c_sf, 2926)

# c_2926 <- c_sf %>% slice(1:10) %>% st_transform(2926)   # just a workaround until my query quota refills

# spatial overlay

st_intersects(c_2926[1,], zng_subd) # WARNING: might take a while to run

st_join(c_2926[1,], zng_subd)

tic()
c_zng <- st_join(c_2926, zng_subd)
toc()

# EDA ----

c_sf %>% 
  st_drop_geometry() %>% 
  transmute(PROBLEM = factor(PROBLEM)) %>% 
  count(PROBLEM, sort = TRUE) %>% 
  print(n = Inf)


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

