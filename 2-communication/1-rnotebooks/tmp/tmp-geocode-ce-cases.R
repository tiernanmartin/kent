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

# LOAD DATA: Code Enforcement Cases (2014-16) ----

c_14_16_fp <- root_file("1-data/2-external/code-enforcement-cases-2014-2016.gpkg")

c_14_16_load <- c_14_16_fp %>% 
  make_or_read({
    
    dr_id <- as_id("0B5Pp4V6eCkhrV3NRQl9vLXJtTGM")
    
    zip_dir <- root_file("1-data/2-external")
    
    target_name <- "ce-cases-2014-2016"
    
    drive_read_zip(dr_id = dr_id,
                   .tempdir = FALSE,
                   dir_path = zip_dir,
                   read_fun = read_sf,
                   target_name = target_name, 
                   stringsAsFactors = FALSE)
    
  },
  {
    read_sf(c_14_16_fp, stringsAsFactors = FALSE)
    
  })

c_14_16 <- c_14_16_load %>% 
  st_drop_geometry() %>% 
  rename_if(not_sfc, to_screaming_snake_case) %>% 
  select(RFS_NUM = RS_NUM,
         DATE_RECEIVED = RS_DATE_RE,
         RFS_ADDRESS = ADDRESS,
         PROBLEM,
         PROBLEM_STATUS = RS_STAT)

# LOAD DATA: Code Enforcement Cases (2017) ----

# Code enforcement cases

c_17_url <- "https://docs.google.com/spreadsheets/d/1xmpKg63VrDfZzaYJbqiK3-0FQhQIp9Ur586fuMBjEv0/edit?usp=sharing"

data_sheet <- gs_url(c_17_url, lookup = NULL, visibility = NULL, verbose = TRUE)

col_types <- list(col_character(),
                  col_character(),
                  col_date(format = "%m/%d/%Y"),
                  col_character(),
                  col_character(),
                  col_character(),
                  col_character(),
                  col_date(format = "%m/%d/%Y")
)

c_17 <- data_sheet %>% 
  gs_read(range = cell_limits(c(1, 1), c(NA, 8)), 
          colnames = TRUE, 
          na = c("","#N/A"),
          col_types = col_types) %>% 
  set_colnames(str_replace_all(colnames(.),"\\s","_")) 

# JOIN DATA: Code Enforcement Cases (2014-17) ----

c_14_17 <- c_14_16 %>% 
  bind_rows(c_17) %>% 
  mutate(ADDR = if_else(is.na(RFS_ADDRESS),OTHER_LOCATION,RFS_ADDRESS),
         ADDR = str_c(ADDR, "Kent", "WA", sep = ", ")) %>% 
  arrange(RFS_NUM,DATE_RECEIVED) %>% 
  select(RFS_NUM:PROBLEM_STATUS,ADDR) %>% 
  distinct() # remove duplicate records

# GEOCODE DATA ----

geocode_fun <- function(id, address){
  
  if(as.integer(id)<= 2500){
    return(
      geocode_url(address, 
                auth="standard_api", 
                privkey="AIzaSyCV7tvAz6Tq5PMYq9VErUXs4hv42NPWDRU",
                clean=TRUE, 
                add_date='today', 
                verbose=TRUE) %>% 
      as_tibble
      )
  } else if(between(as.integer(id),2501,5000)){
    return(
    geocode_url(address, 
                auth="standard_api",  
                privkey="AIzaSyAC19c3TtQwrSiQYKYDaf-9nDIqahirnD8", 
                clean=TRUE, 
                add_date='today', 
                verbose=TRUE) %>% 
      as_tibble
    )
  } else if(between(as.integer(id),5001,7500)){
    return(
    geocode_url(address, 
                auth="standard_api",  
                privkey="AIzaSyCYhgjxQ0PRqo9VTHUrK1KnzaI65AGwZgs", 
                clean=TRUE, 
                add_date='today', 
                verbose=TRUE) %>% 
      as_tibble
    )
  } else if(between(as.integer(id),7501,10000)){
    return(
    geocode_url(address, 
                auth="standard_api", 
                privkey="AIzaSyDe7E03paIZRJopnUW8ZDOMPkaBxefP-7A",
                clean=TRUE, 
                add_date='today', 
                verbose=TRUE) %>% 
      as_tibble
    )
  }
}

# slow operation


c_14_17_geocode <- c_14_17 %>%
  rownames_to_column("ID") %>%  
  mutate(ADDR_TBL = map2(ID,ADDR,geocode_fun))
  

c_14_17_trim <- c_14_17_geocode %>% 
  unnest %>% 
  rename_all(to_screaming_snake_case) %>% 
  filter(LOCATION_TYPE %!in% 'APPROXIMATE') %>% 
  filter(STATUS %!in% "ZERO_RESULTS")

c_14_17_sf <- st_as_sf(c_14_17_trim, coords = c("LNG", "LAT"), crs = 4326)
  
# SAVE & UPLOAD TO DRIVE ---- 

c_fp <- root_file("./1-data/3-interim/code-enforcement-cases-2014-2017.gpkg")

drive_folder_id <- as_id("0B5Pp4V6eCkhrRFRYbWpoM3pWYkU") # ~/3-interim/

st_write(obj = c_14_17_sf,dsn = c_fp, layer = 'code_enforcement_cases_2004_2017', driver = 'GPKG', layer_options = 'OVERWRITE=TRUE')

# drive_upload(media = c_fp, path = drive_folder_id)

drive_update(file = as_id("1CtrRh1BSyOeaxVHELan_I6vk0ePKBFlC"), c_fp)
