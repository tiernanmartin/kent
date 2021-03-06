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

c_fp <- root_file("1-data/3-interim/code-enforcement-cases-2014-2017.gpkg")

c_load <- c_fp %>% 
  make_or_read({
    dr_id <- as_id("1CtrRh1BSyOeaxVHELan_I6vk0ePKBFlC")
    
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

# LOAD DATA: Kent center----

kent_cntr <- kent_bound %>% 
  st_centroid() %>% 
  mutate(LNG = map_dbl(geom,1),
         LAT = map_dbl(geom,2))

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
    
    dr_id <- as_id("1ST6g5NAcY0yLSZi7S-FJf_Y2Ol2SviZ9") 
    
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
    dr_id <- as_id("1XPxEl1JuPTuXPK6zhjIMhDyJjhROIGLJ")
    
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

# zng <- zng_load %>% 
#   st_transform(2926)
# 
# zng_subd <- zng %>% 
#   st_subdivide(max_vertices = 256) %>% 
#   st_collection_extract() %>% 
#   st_cast('MULTIPOLYGON')
# 
# zng_list <- c("Single-Family Residential", 
#               "Multi-Family Residential",
#               "General Mixed Use",
#               "Mobile Home Park",
#               "Mixed Use Commercial/Residential")

# spatial overlay

c_2926 <- st_transform(c_sf, 2926)

p_2926 <- st_transform(p_kent_load, 2926)

c_p <- st_join(c_2926,p_2926)


# prep cases (check the PROBLEM category levels) 

pu_list <- c("Single Family(Res Use/Zone)",
                     "Apartment",
                     "Condominium(Residential)",
                     "Duplex",
                     "Single Family(C/I Zone)",
                     "Mobile Home Park",
                     "Mobile Home",
                     "Apartment(Subsidized)"
                     )

cat_list <- c("RATS/BUGS", 
              "BUILDING", 
              "MOLD", 
              "DANG BLDG", 
              "ROOF LEAKS", 
              "MISC")

c_bldg_sf <- c_p %>% 
  filter(PROBLEM %in% cat_list) %>% 
  filter(PRESENTUSE_DESC %in% pu_list)


# EDA ----

c_bldg_sf %>% 
  st_drop_geometry() %>% 
  transmute(PROBLEM = factor(PROBLEM),
            PRESENTUSE_DESC = factor(PRESENTUSE_DESC)) %>% 
  count(PRESENTUSE_DESC, PROBLEM,sort = TRUE) %>% 
  print(n = Inf)


# MAP DATA ----

orange <- "#e9673e"

leaflet() %>% 
  addProviderTiles(provider = providers$CartoDB.PositronNoLabels) %>% 
  addCircleMarkers(data = st_transform(c_bldg_sf,4326), 
                   color = orange,
                   weight = .5,
                   opacity = 1) %>% 
  addPolygons(data = kent_bound,
              color = "#434343",
              opacity = 1,
              fillOpacity = 0) %>% 
  setView(kent_cntr$LNG,kent_cntr$LAT,  12)
# SAVE & UPLOAD TO DRIVE ---- 

c_bldg_fp <- root_file("1-data/3-interim/ce-cases-resbldg-2014-2017.gpkg")

drive_folder_id <- as_id("0B5Pp4V6eCkhrRFRYbWpoM3pWYkU") # ~/3-interim/

st_write(obj = c_bldg_sf,dsn = c_bldg_fp, layer = 'ce_cases_resbldg_2014_2017', driver = 'GPKG', layer_options = 'OVERWRITE=TRUE')

drive_upload(media = c_bldg_fp, path = drive_folder_id)

drive_update(file = as_id("1CwOjQ8mk-9ZSAKUBmCw1l6dGptEM0O_1"), c_bldg_fp)
