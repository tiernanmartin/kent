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
# CREATE DATA ----

# King County Data Present Use

lu_fp <- root_file("1-data/2-external/EXTR_LookUp.csv")

lu_load <- lu_fp %>% 
  make_or_read({
    dr_id <- as_id("1-L42pHb7lySqonanSwBbXSH9OZrKHp2A")
    
    drive_read(dr_id = dr_id,
               .tempfile = FALSE,
               path = lu_fp,
               read_fun = read_csv)
  },
               {
                 read_csv(lu_fp)
               })

pu <- lu_load %>% 
  rename_all(to_screaming_snake_case) %>% 
  filter(LU_TYPE == 102) %>% 
  select(PRESENTUSE = LU_ITEM,
         PRESENTUSE_DESC = LU_DESCRIPTION)


# King County Parcels

kc_p_url <- "https://opendata.arcgis.com/datasets/c7a17b7ad3ec44b7ae64796dca691d72_1722.geojson"


kc_p_load <- read_sf(kc_p_url, stringsAsFactors = FALSE)
## 1230.39 sec elapsed

kc_p <- kc_p_load %>% 
  rename_if(not_sfc, to_screaming_snake_case) %>% 
  left_join(pu, by = "PRESENTUSE")

# Kent Parcels

kent_p <- kc_p %>% 
  filter(toupper(CTYNAME) %in% 'KENT')

# SAVE & UPLOAD TO DRIVE: King County Parcels ---- 
# Note: if working on a Windows system, first download the Rtools package:
# "https://cran.r-project.org/bin/windows/Rtools/"

kc_p_fp <- root_file("1-data/2-external/kc-parcels.gpkg")

kc_p_zip_fp <- root_file("1-data/2-external/kc-parcels.zip")

drive_folder_id <- as_id("0B5Pp4V6eCkhrQ29lVGsxaS1ERXM") # ~/2-external/

st_write(obj = kc_p,dsn = kc_p_fp, layer = 'kc_parcels', driver = 'GPKG', layer_options = 'OVERWRITE=TRUE')

zip_pithy(kc_p_zip_fp, kc_p_fp)

# drive_upload(media = kc_p_zip_fp, path = drive_folder_id)

drive_update(file = as_id("1ST6g5NAcY0yLSZi7S-FJf_Y2Ol2SviZ9"), kc_p_fp)

# SAVE & UPLOAD TO DRIVE: King County Parcels ---- 

kent_p_fp <- root_file("1-data/3-interim/kent-parcels.gpkg")

kent_p_zip_fp <- root_file("1-data/3-interim/kent-parcels.zip")

drive_folder_id <- as_id("0B5Pp4V6eCkhrRFRYbWpoM3pWYkU") # ~/3-interim/

st_write(obj = kent_p,dsn = kent_p_fp, layer = 'kent_parcels', driver = 'GPKG', layer_options = 'OVERWRITE=TRUE')

zip_pithy(kent_p_zip_fp, kent_p_fp)

# drive_upload(media = kent_p_zip_fp, path = drive_folder_id)

drive_update(file = as_id("1XPxEl1JuPTuXPK6zhjIMhDyJjhROIGLJ"), kent_p_fp)