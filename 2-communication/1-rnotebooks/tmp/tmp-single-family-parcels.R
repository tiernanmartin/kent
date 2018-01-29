# SETUP ----

library(sf)
library(snakecase)
library(googledrive)
library(rprojroot)
library(miscgis)
library(stringdist)
library(fuzzyjoin)
library(placement)       # devtools::install_github("DerekYves/placement")

library(tidyverse)

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

# LOAD DATA: Kent Parcels ----

make_parcel <- function(){
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
}

p_kent_load <- make_parcel()

p <- p_kent_load %>% 
  transmute(PIN, 
            PROP_NAME,
            ADDR_PARCEL = ADDR_FULL,
            CITY_NAME_PARCEL = CTYNAME,
            ADDR_FULL_PARCEL = str_c(ADDR_PARCEL, CITY_NAME_PARCEL, "WA", sep = ", "),
            PRESENTUSE_DESC) %>% 
  st_drop_geometry()

# LOAD DATA: KC Account ----

make_acct <- function(){ 
  
  acct_fp <- root_file("1-data/2-external/kc_real_prop_acct_extract.rds")
  
  acct_dr_id <- as_id("19f4AUMAEshnDNJqGjVsurFthbKGcKYvh")
  
  acct <- 
    make_or_read2(fp = acct_fp,
                  dr_id = acct_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){
                    realprop <- read.socrata("https://data.kingcounty.gov/resource/mmfz-e8xr.csv",
                                             email = "FAKE_NAME@FAKE_EMAIL.COM",
                                             password = "FAKE_PASSWORD" # CHANGE TO REAL BEFORE RUNNING
                    )
                    
                    r <- realprop %>% 
                      rename_all(to_screaming_snake_case)  
                    
                    drive_folder_id <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                    
                    write_rds(r, fp, compress = "gz")
                    
                    drive_upload(media = fp, path = drive_folder_id)
                    
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,path = fp,.tempfile = FALSE,read_fun = read_rds)
                  },
                  read_expr = function(fp){read_rds(fp)})
  
}

acct_load <- make_acct()

acct <- acct_load %>% 
  transmute(PIN = str_c(str_pad(MAJOR,6,"left","0"),str_pad(MINOR,4,"left","0"), sep = ""),
            ADDR_KCTP = ADDRLINE,
            CITY_NAME_KCTP = str_replace(CITYSTATE, "\\s+WA",""),
            ADDR_FULL_KCTP = str_c(ADDR_KCTP, CITY_NAME_KCTP, "WA", sep = ", ")
            ) 

# CREATE FUNCTION: geocode ---- 

geocode_fun <- function(id, address){
  
  if(as.integer(id)<= 2500){
    return(
      geocode_url(address, 
                  auth="standard_api", 
                  privkey="AIzaSyDCVtmhyE2vkE-EunhvBuRwI75MgjA2veA",
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
                  privkey="AIzaSyCV7tvAz6Tq5PMYq9VErUXs4hv42NPWDRU",
                  clean=TRUE, 
                  add_date='today', 
                  verbose=TRUE) %>% 
        as_tibble
    )
  } else if(between(as.integer(id),10001,12500)){
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


# CREATE DATA: Single Family Parcel in Kent ----

# Exclude PO Box addresses

po_box_pattern <- "^BOX|POB|PO\\sBOX|P\\sO\\sBOX|P\\.O\\."

acct_no_pobox <- acct %>% 
  filter(!str_detect(ADDR_KCTP,po_box_pattern)) 

# join, select sf, select non-matching parcel-kctp addresses

# Step One: fuzzy join using the Jaro-Winker method

# use stringdist(..., method = "jw")
# The Jaro-Winker distance was developed at the U.S. Census Bureau for linking
# source: https://datascience.stackexchange.com/a/10812/45287

p_sf <- inner_join(p, acct_no_pobox, by = "PIN") %>% 
  mutate(SF_LGL = str_detect(PRESENTUSE_DESC, "Single Family") & !str_detect(PRESENTUSE_DESC, "Vacant"),
         ADDR_STR_DIST = stringdist(ADDR_PARCEL, ADDR_KCTP, method = "jw"),
         ADDR_MISMATCH_LGL = ! ADDR_PARCEL %in% ADDR_KCTP,
         ADDR_FUZZY_MISMATCH_LGL = ADDR_STR_DIST > 0.15
         ) %>% 
  filter(SF_LGL) %>% 
  select(PIN,
         ADDR_PARCEL,
         ADDR_KCTP,
         ADDR_MISMATCH_LGL,
         ADDR_FUZZY_MISMATCH_LGL,
         ADDR_STR_DIST,
         ADDR_FULL_PARCEL,
         ADDR_FULL_KCTP,
         CITY_NAME_PARCEL,
         CITY_NAME_KCTP, 
         ADDR_FUZZY_MISMATCH_LGL,
         everything()) %>% 
  arrange(desc(ADDR_FUZZY_MISMATCH_LGL), ADDR_STR_DIST)

# Step Two: group by ADDR_FUZZY_MISMATCH_LGL and only geocode those addresses

p_ready_to_geocode <- p_sf %>% 
  gather(key = "ADD_SOURCE",value = "FULL_ADDR", matches("ADDR_FULL")) %>%  
  group_by(ADDR_FUZZY_MISMATCH_LGL) %>% 
  mutate(ROWNUM = row_number()) %>%  
  ungroup() 
 
p_standard <- p_ready_to_geocode %>% 
  mutate(ADDR_STANDARD_TBL = if_else(ADDR_FUZZY_MISMATCH_LGL,
                                       map2(ROWNUM,FULL_ADDR,geocode_fun),
                                       list(tibble(new = NA_character_))) 
  ) %>% 
  unnest %>% 
  select(-new)



tmp <- p_standard %>% 
  unnest() %>% 
  select(-new) %>% 
  mutate(FAILED_FIRST = status %in% c("OVER_QUERY_LIMIT","CONNECTION_ERROR")) %>% 
  nest(lat:geocode_dt)
  
tmp_1 <- tmp %>% 
  filter(!FAILED_FIRST) %>% 
  unnest()


tmp_2 <- tmp %>% 
  filter(FAILED_FIRST) %>% 
  select(-data) %>% 
  mutate(ROWNUM = row_number()) %>% 
  mutate(ADDR_STANDARD_TBL = map2(ROWNUM,FULL_ADDR,geocode_fun)) %>% 
  unnest()

tmp_3 <- bind_rows(tmp_1, tmp_2) %>% 
  rename_all(to_screaming_snake_case)

tmp_4 <- tmp_3 %>% 
  select(PIN,ADD_SOURCE,FULL_ADDR, FORMATTED_ADDRESS) %>%   
  rownames_to_column("ID") %>% 
  mutate(FORMATTED = ADD_SOURCE) %>% 
  spread(ADD_SOURCE,FULL_ADDR) %>% 
  spread(FORMATTED, FORMATTED_ADDRESS,sep = "_") %>% 
  group_by(PIN) %>% 
  summarise_all(first_not_na) %>% 
  mutate(FORMATTED_ADDR_ST_PARCEL = str_replace(FORMATTED_ADDR_FULL_PARCEL,",.*$",""),
         FORMATTED_ADDR_ST_KCTP = str_replace(FORMATTED_ADDR_FULL_KCTP,",.*$",""),
         FORMATTED_ADDR_STR_DIST = stringdist(FORMATTED_ADDR_ST_PARCEL, FORMATTED_ADDR_ST_KCTP, method = "jw"))


tmp_5 <- tmp_4 %>% 
  select(PIN, matches('FORMATTED'),-matches("ST_PARCEL|ST_KCTP")) %>% 
  right_join(p_sf, by = "PIN") %>% 
  mutate(FORMATTED_ADDR_FUZZY_MISMATCH_LGL = FORMATTED_ADDR_STR_DIST > 0.13) %>% 
  mutate(RENTED_ADDRESS_LGL =  FORMATTED_ADDR_FUZZY_MISMATCH_LGL)


# Histogram

tmp_5 %>% 
  filter(FORMATTED_ADDR_STR_DIST > 0) %>% 
  {qplot(x = FORMATTED_ADDR_STR_DIST,data = .)}

# View the data
tmp_5 %>% 
  filter(FORMATTED_ADDR_STR_DIST > 0) %>% 
    select(PIN, matches('FORMATTED')) %>% arrange(FORMATTED_ADDR_STR_DIST) %>% View()

# How many records?
tmp_5 %>% filter(FORMATTED_ADDR_FUZZY_MISMATCH_LGL) %>% nrow()
# [1] 2756

renter_hu_sf <- 2756

renter_hu <- 19235

renter_hu_moe <- 665

str_c(
  (renter_hu - renter_hu_sf)-renter_hu_moe, 
  (renter_hu - renter_hu_sf)+renter_hu_moe,
  sep = " - "
)
 







