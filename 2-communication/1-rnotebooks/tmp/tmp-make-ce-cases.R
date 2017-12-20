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

# LOAD DATA: Others ----

# Kent boundary
# see root_file("2-communication/1-rnotebooks/tmp/tmp-make-kent-boundary.R")

kent_bound_fp <- root_file("1-data/3-interim/kent-boundary.gpkg")

kent_bound <- kent_bound_fp %>% 
  make_or_read({
    
    dr_id <- as_id("1C4b7TKmrKzWxj53ajJkvb-ddS2bE9NZ0")
    
    drive_read(dr_id = dr_id,
               .tempfile = FALSE,
               path = kent_bound_fp,
               read_fun = read_sf, 
               stringsAsFactors = FALSE)
    
  },
  {
    read_sf(kent_bound_fp)
  })

# King County Consolidated Zoning
# See root_file("2-communication/1-rnotebooks/tmp/tmp-make-kc-zoning.R")

zng_fp <- root_file("1-data/2-external/kc-zoning.gpkg")

zng_load <- zng_fp %>% 
  make_or_read({
                 
                 dr_id <- as_id("")
                 
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

# King County Parcels

p_fp <- root_file("1-data/2-external/EXTR_Parcel_20171013.csv")

p_load <- p_fp %>% 
  make_or_read({
    
    dr_id <- as_id("0B5Pp4V6eCkhraF9jOTl3bURiMkU") 
    
    zip_dir <- root_file("1-data/2-external")
    
    target_name <- "EXTR_Parcel_20171013.csv"
    
    drive_read_zip(dr_id = dr_id,
                   .tempdir = FALSE,
                   dir_path = zip_dir,
                   read_fun = read_csv,
                   target_name = target_name)
    
    
  },
               {
                 read_csv(p_fp)
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
  
# SPATIAL OVERLAY ----

# prep zoning

zng <- zng_load %>% 
  st_transform(2926)

zng_subd <- zng %>% 
  st_subdivide(max_vertices = 256) %>% 
  st_collection_extract() %>% 
  st_cast('MULTIPOLYGON')

zng_list <- c("Single-Family Residential", 
              "Multi-Family Residential",
              "General Mixed Use",
              "Mobile Home Park",
              "Mixed Use Commercial/Residential")

# spatial overlay

c_2926 <- st_transform(c_sf, 2926)

c_zng <- c_2926 %>% 
  st_join(zng_subd) %>% 
  select(RFS_NUM:LNG,CONSOL20)


# prep cases (check the PROBLEM category levels) 

cat_list <- c("RATS/BUGS", "BUILDING", "MOLD", "DANG BLDG", "ROOF LEAKS")

c_bldg_sf <- c_zng %>% 
  filter(PROBLEM %in% cat_list) %>% 
  filter(CONSOL20 %in% zng_list)





# EDA ----

c_bldg_sf %>% 
  transmute(PROBLEM = factor(PROBLEM),
            ZONING = factor(CONSOL20)) %>% 
  count(ZONING, PROBLEM,sort = TRUE) %>% 
  print(n = Inf)


# MAP DATA ----

orange <- "#e9673e"

leaflet() %>% 
  addProviderTiles(provider = providers$CartoDB.PositronNoLabels) %>% 
  addCircleMarkers(data = st_transform(c_bldg_sf,4326), 
                   color = orange,
                   weight = .5,
                   opacity = 1) %>% 
  addPolygons(data = kent_bound_sf,
              color = "#434343",
              opacity = 1,
              fillOpacity = 0) %>% 
  setView(kent_cntr$LNG,kent_cntr$LAT,  12)
# SAVE & UPLOAD TO DRIVE ---- 

c_fp <- root_file("./1-data/3-interim/code-enforcement-cases-2017.gpkg")

drive_folder_id <- as_id("0B5Pp4V6eCkhrRFRYbWpoM3pWYkU")

st_write(obj = c_zng,dsn = c_fp, layer = 'code_enforcement_cases_2017', driver = 'GPKG', layer_options = 'OVERWRITE=TRUE')

# drive_upload(media = c_fp, path = drive_folder_id)

drive_update(file = as_id("1MnVFK7kXlqduUwtBuH9gi3ztm9h22P4n"), c_fp)
