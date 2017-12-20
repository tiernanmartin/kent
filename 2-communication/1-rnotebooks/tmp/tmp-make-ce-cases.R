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

# see root_file("2-communication/1-rnotebooks/tmp/tmp-geocode-ce-cases.R")

c_fp <- root_file("1-data/3-interim/code-enforcement-cases-2017.gpkg")

c_load <- c_fp %>% 
  make_or_read({
    dr_id <- as_id("")
    
    drive_read(dr_id = dr_id,
               .tempfile = FALSE,
               path = c_fp,
               read_fun = read_sf, 
               stringsAsFactors = FALSE)
  },
               {
                 read_sf(c_fp, stringsAsFactors = FALSE)
               })
c_sf <- c_load %>% 
  select(RFS_NUM:LNG,STATUS,ERROR_MESSAGE)

# LOAD DATA: Kent boundary ----

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

# LOAD DATA: King County Consolidated Zoning ----

# See root_file("2-communication/1-rnotebooks/tmp/tmp-make-kc-zoning.R")

zng_fp <- root_file("1-data/3-interim/kc-zoning.gpkg")

zng_load <- zng_fp %>% 
  make_or_read({
                 
                 dr_id <- as_id("1n8W_8ksv0ZTDXSxjVLHLsOmym1uGy2DD")
                 
                 zip_dir <- root_file("1-data/3-interim")
                 
                 target_name <- "kc-zoning.gpkg"
                 
                 drive_read_zip(dr_id = dr_id,
                                .tempdir = FALSE,
                                dir_path = zip_dir,
                                read_fun = read_sf,
                                target_name = target_name, 
                                stringsAsFactors = FALSE)
                 
               },
               {
                 read_sf(zng_fp, stringsAsFactors = FALSE)
               })


# LOAD DATA: King County Parcels ----

# See root_file("2-communication/1-rnotebooks/tmp/tmp-make-kc-parcel.R") 

p_fp <- root_file("1-data/2-external/kc-parcels.gpkg")

p_load <- p_fp %>% 
  make_or_read({
    
    dr_id <- as_id("1L4WkfQxgr637jSJTIGE2j9ZHHEYrnAF7") 
    
    zip_dir <- root_file("1-data/2-external")
    
    target_name <- "kc-parcels.gpkg"
    
    drive_read_zip(dr_id = dr_id,
                   .tempdir = FALSE,
                   dir_path = zip_dir,
                   read_fun = read_sf,
                   target_name = target_name,
                   stringsAsFactors = FALSE)
    
    
  },
               {
                 read_sf(p_fp, stringsAsFactors = FALSE)
               })

# LOAD DATA: Kent Parcels ----

p_kent_fp <- root_file("1-data/3-interim/kent-parcels.gpkg")

p_kent_load <- p_kent_fp %>% 
  make_or_read({
    dr_id <- as_id("1d13l2qrWTVLzAPmqFztxIok6GS_AgJnu")
    
    zip_dir <- root_file("1-data/3-interim")
    
    target_name <- "kent-parcels.gpkg"
    
    drive_read_zip(dr_id = dr_id,
                   .tempdir = FALSE,
                   dir_path = zip_dir,
                   read_fun = read_sf,
                   target_name = target_name,
                   stringsAsFactors = FALSE)
  },
               {
                 read_sf(p_kent_fp, stringsAsFactors = FALSE)
               })

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

# add save code here
