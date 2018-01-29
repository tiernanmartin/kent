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
library(RColorBrewer)

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

# LOAD DATA: Code Enforcement Cases ----

# see root_file("2-communication/1-rnotebooks/tmp/tmp-make-ce-cases.R")

c_fp <- root_file("1-data/3-interim/ce-cases-resbldg-2014-2017.gpkg")

c <- c_fp %>% 
  make_or_read({
    dr_id <- as_id("0B5Pp4V6eCkhrRFRYbWpoM3pWYkU")
    
    drive_read(dr_id = dr_id,
               .tempfile = FALSE,
               path = c_fp,
               read_fun = read_sf, 
               stringsAsFactors = FALSE)
  },
               {
                 read_sf(c_fp, stringsAsFactors = FALSE)
               })


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

# LOAD DATA: King County Zoning in Kent ----

# See root_file("2-communication/1-rnotebooks/tmp/tmp-make-kc-zoning.R")

kent_zng_gpkg_fp <- root_file("1-data/3-interim/kent-zoning.gpkg")

kent_zng_load <- kent_zng_gpkg_fp %>% 
  make_or_read({
    
    dr_id <- as_id("1oad_qjGFXPAltNzUp7aLu0_D42ade") 
    
    drive_read(dr_id = dr_id,
               .tempdir = FALSE,
               path = kent_zng_gpkg_fp,
               read_fun = read_sf, 
               stringsAsFactors = FALSE)
    
  },
  {
    read_sf(kent_zng_gpkg_fp, stringsAsFactors = FALSE)
  })

kent_zng <- filter(kent_zng_load,ZONING_CAT_3 %!in% "OTHER")

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


# SUBSET DATA: Multi-family only ----

mf_uses <- c("4-Plex",
        "Apartment",
        "Apartment(Mixed Use)",
        "Apartment(Subsidized)",
        "Duplex",
        "Townhouse Plat",
        "Triplex",
        "Vacant(Multi-family)")

zng_mf <- filter(kent_zng, C %in% "Multi-Family Residential")

c_mf <-filter(c, PRESENTUSE_DESC %in% mf_uses)

kent_mf <-filter(p_kent_load, PRESENTUSE_DESC %in% mf_uses)

# EDA ----

# What the most common problem type (by Present Use category)
c %>% 
  st_drop_geometry() %>% 
  transmute(PROBLEM = factor(PROBLEM),
            PRESENTUSE_DESC = factor(PRESENTUSE_DESC)) %>% 
  count(PRESENTUSE_DESC, PROBLEM,sort = TRUE) %>% 
  print(n = Inf)

# What's the most number of problems that can be cited on a given day?
c %>% 
  st_drop_geometry() %>% 
  count(RFS_ADDRESS, DATE_RECEIVED, sort = TRUE)

c %>% 
  st_drop_geometry() %>% 
  subset_duplicated("RFS_ADDRESS") %>% 
  group_by(RFS_ADDRESS) %>% 
  summarise(NUMBER_OF_UNIQUE_DATES = length(unique(DATE_RECEIVED))) %>% 
  arrange(desc(NUMBER_OF_UNIQUE_DATES))

# Make a subset of the multi-family, "repeat offenders"
c_mf_ro <- c %>% 
  st_drop_geometry() %>% 
  subset_duplicated("RFS_ADDRESS") %>% 
  group_by(RFS_ADDRESS) %>% 
  summarise(NUMBER_OF_UNIQUE_DATES = length(unique(DATE_RECEIVED))) %>% 
  right_join(c_mf, by = "RFS_ADDRESS") %>%    # note: joins to `c_mf`, NOT `C`
  filter(!is.na(NUMBER_OF_UNIQUE_DATES)) %>% 
  st_sf

# MAP DATA ----

cols <- RColorBrewer::brewer.pal(12,"Set3")[c(3,4,12)]

pal <- colorFactor(cols,domain = kent_zng$ZONING_CAT_3_FCT,ordered = TRUE)

orange <- "#e9673e"

show_cases_with_zoning <- function(){
  leaflet() %>% 
  addProviderTiles(provider = providers$CartoDB.PositronNoLabels) %>% 
  addPolygons(data = st_transform(kent_zng,4326),
              smoothFactor = 0,
              opacity = 0,
              fillColor = ~pal(ZONING_CAT_3_FCT), fillOpacity = .33) %>%  
  addCircleMarkers(data = st_transform(c,4326), 
                   color = orange,
                   weight = .5,
                   opacity = 1) %>% 
  addPolygons(data = kent_bound,
              color = "#434343",
              opacity = 1,
              fillOpacity = 0) %>% 
  setView(kent_cntr$LNG,kent_cntr$LAT,  12)
}

# show_cases_with_zoning()

show_mf_zoning <- function(){
  leaflet() %>% 
  addProviderTiles(provider = providers$CartoDB.PositronNoLabels) %>% 
  addPolygons(data = st_transform(zng_mf,4326),
              smoothFactor = 0,
              opacity = 0,
              fillColor = cols[[2]], fillOpacity = .33) %>% 
  addPolygons(data = st_transform(kent_mf, 4326),
              smoothFactor = 0, 
              opacity = 1, color = cols[[2]], weight = .25,
              fillColor = cols[[2]],fillOpacity = .5) %>%  
  addPolygons(data = kent_bound,
              color = "#434343",
              opacity = 1,
              fillOpacity = 0) %>% 
  setView(kent_cntr$LNG,kent_cntr$LAT,  12)
}

# show_mf_zoning()

show_mf_cases <- function(){
  leaflet() %>% 
  addProviderTiles(provider = providers$CartoDB.PositronNoLabels) %>% 
  addPolygons(data = st_transform(zng_mf,4326),
              smoothFactor = 0,
              opacity = 0,
              fillColor = cols[[2]], fillOpacity = .33) %>% 
  addPolygons(data = st_transform(kent_mf, 4326),
              smoothFactor = 0, 
              opacity = 1, color = cols[[2]], weight = .25,
              fillColor = cols[[2]],fillOpacity = .5) %>% 
  addCircleMarkers(data = st_transform(c_mf,4326),
                   opacity = 1, color = "#434343", weight = .5,
                   fillOpacity = 0
                   ) %>%
  addPolygons(data = kent_bound,
              color = "#434343",
              opacity = 1,
              fillOpacity = 0) %>% 
  setView(kent_cntr$LNG,kent_cntr$LAT,  12)
}

# show_mf_cases()

show_mf_ro_cases <- function(){
  leaflet() %>% 
  addProviderTiles(provider = providers$CartoDB.PositronNoLabels) %>% 
  addPolygons(data = st_transform(zng_mf,4326),
              smoothFactor = 0,
              opacity = 0,
              fillColor = cols[[2]], fillOpacity = .33) %>% 
  addPolygons(data = st_transform(kent_mf, 4326),
              smoothFactor = 0, 
              opacity = 1, color = cols[[2]], weight = .25,
              fillColor = cols[[2]],fillOpacity = .5) %>% 
  addCircleMarkers(data = st_transform(c_mf_ro,4326),
                   opacity = 1, color = "#434343", weight = .5,
                   fillOpacity = 0
                   ) %>%
  addPolygons(data = kent_bound,
              color = "#434343",
              opacity = 1,
              fillOpacity = 0) %>% 
  setView(kent_cntr$LNG,kent_cntr$LAT,  12)
}

# show_mf_ro_cases()


# SAVE & UPLOAD TO DRIVE ---- 

c_bldg_fp <- root_file("1-data/3-interim/ce-cases-resbldg-2014-2017.gpkg")

drive_folder_id <- as_id("0B5Pp4V6eCkhrRFRYbWpoM3pWYkU") # ~/3-interim/

st_write(obj = c_bldg_sf,dsn = c_bldg_fp, layer = 'ce_cases_resbldg_2014_2017', driver = 'GPKG', layer_options = 'OVERWRITE=TRUE')

drive_upload(media = c_bldg_fp, path = drive_folder_id)

drive_update(file = as_id("1CwOjQ8mk-9ZSAKUBmCw1l6dGptEM0O_1"), c_bldg_fp)
