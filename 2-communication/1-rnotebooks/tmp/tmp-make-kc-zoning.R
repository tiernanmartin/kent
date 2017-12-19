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
# CREATE DATA ----

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


# SAVE & UPLOAD TO DRIVE ---- 

kc_zng_gpkg_fp <- root_file("1-data/2-external/kc-zoning.gpkg")

kc_zng_zip_fp <- root_file("1-data/2-external/kc-zoning.zip")

drive_folder_id <- as_id("0B5Pp4V6eCkhrQ29lVGsxaS1ERXM") # ~/2-external/

st_write(obj = kc_zng,dsn = kc_zng_gpkg_fp, layer = 'kc_zoning_consolidated', driver = 'GPKG', layer_options = 'OVERWRITE=TRUE')

zip(kc_zng_zip_fp,kc_zng_gpkg_fp)

# drive_upload(media = kc_zng_zip_fp, path = drive_folder_id)

drive_update(file = as_id("1EQIDABm4gYdksu45ftYD7X4lueHZfvRt"), kc_zng_gpkg_fp)
