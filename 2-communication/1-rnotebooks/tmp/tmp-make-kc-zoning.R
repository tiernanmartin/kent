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
library(zip)

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())
# LOAD DATA: King County Consolidated Zoning ----

# King County Consolidated Zoning

# source: ftp://ftp.kingcounty.gov/gis-web/GISData/zoning_kc_consol_20_SHP.zip

kc_zng_fp <- root_file("1-data/2-external/zoning_kc_consol_20")

kc_zng_load <- kc_zng_fp %>% 
  make_or_read({
    
    dr_id <- as_id("12TZn4z6XzZ2fmvHyvDIXFHFQfmJ70hJM")
    
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
               {read_sf(kc_zng_fp, layer = "zoning_kc_consol_20", stringsAsFactors = FALSE)})

kc_zng <- rename_if(kc_zng_load, not_sfc, to_screaming_snake_case)


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


# CREATE DATA: King County Zoning in Kent ----

sf_list <-c("Single-Family Residential")

mf_list <- c("Multi-Family Residential","Mobile Home Park")

mixed_list <- c("Mixed Use Commercial/Residential", "General Mixed Use")
 

kc_zng_2926 <- kc_zng %>% 
  transmute(C = CONSOL_20) %>% 
  mutate(ZONING_CAT_3 = case_when(
    C %in% sf_list ~ "SF",
    C %in% mf_list ~ "MF",
    C %in% mixed_list ~ "MIXED",
    TRUE ~ "OTHER"
  )) %>%
  mutate(ZONING_CAT_3_FCT = factor(ZONING_CAT_3, ordered = TRUE)) %>% 
  st_transform(2926)

kent_bound_2926 <- st_transform(kent_bound, 2926)




# SAVE & UPLOAD TO DRIVE: King County Zoning ---- 

# Note: if working on a Windows system, first download the Rtools package:
# "https://cran.r-project.org/bin/windows/Rtools/"

kc_zng_gpkg_fp <- root_file("1-data/2-external/kc-zoning.gpkg")

kc_zng_zip_fp <- root_file("1-data/2-external/kc-zoning.zip")

drive_folder_id <- as_id("0B5Pp4V6eCkhrQ29lVGsxaS1ERXM") # ~/2-external/

st_write(obj = kc_zng,
         dsn = kc_zng_gpkg_fp, 
         layer = 'kc_zoning_consolidated', 
         driver = 'GPKG', 
         layer_options = 'OVERWRITE=TRUE')

zip_pithy(kc_zng_zip_fp, kc_zng_gpkg_fp)

drive_upload(media = kc_zng_zip_fp, path = drive_folder_id)

drive_update(file = as_id("1n8W_8ksv0ZTDXSxjVLHLsOmym1uGy2DD"), kc_zng_gpkg_fp)

# SAVE & UPLOAD TO DRIVE: King County Zoning in Kent ----  

kc_zng_gpkg_fp <- root_file("1-data/2-external/kc-zoning.gpkg")

kc_zng_zip_fp <- root_file("1-data/2-external/kc-zoning.zip")

drive_folder_id <- as_id("0B5Pp4V6eCkhrQ29lVGsxaS1ERXM") # ~/2-external/

st_write(obj = kc_zng,
         dsn = kc_zng_gpkg_fp, 
         layer = 'kc_zoning_consolidated', 
         driver = 'GPKG', 
         layer_options = 'OVERWRITE=TRUE')

zip_pithy(kc_zng_zip_fp, kc_zng_gpkg_fp)

drive_upload(media = kc_zng_zip_fp, path = drive_folder_id)

drive_update(file = as_id("1n8W_8ksv0ZTDXSxjVLHLsOmym1uGy2DD"), kc_zng_gpkg_fp)
