# SETUP ----

library(magrittr)
library(tidyverse)
# library(scales)
# library(sf)
# library(RSocrata)
# library(leaflet)
# library(stringr)
# library(lubridate)
# library(anytime)
library(forcats)
library(googlesheets)    # devtools::install_github('jennybc/googlesheets')
library(googledrive)     # devtools::install_github('tidyverse/googledrive')
# library(mapedit)         # devtools::install_github("r-spatial/mapedit")
# library(mapview)         # devtools::install_github("r-spatial/mapview@develop")
# library(leaflet.extras)  # devtools::install_github("bhaskarvk/leaflet.extras")
# library(knitr)
# library(miscgis)         # devtools::install_github("tiernanmartin/miscgis")

options(httr_oob_default=TRUE) 
gs_auth(new_user = TRUE) 
# LOAD DATA ----

url <- "https://docs.google.com/spreadsheets/d/1ea5_3mez_ZP3HJrKLbKGdMa-CMN4tilUElh0xFFpvtA/edit?usp=sharing"

data_sheet <- gs_url(url, lookup = NULL, visibility = NULL, verbose = TRUE)

# TRANSFORM DATA ----

data_long <- 
  data_sheet %>% 
  gs_read() %>% 
  gather(FEATURE_TYPE,FEATURE,-City) %>% 
  arrange(fct_inorder(City))


# SAVE & UPLOAD TO DRIVE ---- 

fp <- "./1-data/4-ready/policy-research.csv"
drive_id <- as_id("0B5Pp4V6eCkhrOVpGaTB1S3NZUjQ")

write_csv(data_long, fp)

drive_upload(media = fp,
             path = drive_id,
             name = "Policy Research Matrix (Long)",
             type = "spreadsheet")
