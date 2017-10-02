# SETUP ----

library(magrittr)
library(tidyverse)
library(stringr)
library(lubridate)
library(anytime)
library(forcats)
library(googlesheets)    # devtools::install_github('jennybc/googlesheets')
library(googledrive)     # devtools::install_github('tidyverse/googledrive')

options(httr_oob_default=TRUE) 
# LOAD DATA ----

drive_path <- "~/Futurewise/Kent Rental Inspection/1-data/3-interim/Outreach Data"

fp <- "./1-data/3-interim/outreach-data.csv"

drive_download(drive_path, fp,type = "csv",overwrite = TRUE)

data <- read_csv(fp)

# TRANSFORM + SUMMARIZE ----

issues <- 
  data %>% 
  filter(QUESTION_ID %in% paste0("Q",str_pad(1:15,2,pad = "0"))) %>%  
  group_by(QUESTION_ID) %>% 
  summarise(QUESTION_TEXT = first(QUESTION_TEXT),
            N_TRUE = sum(as.logical(RESPONSE)),
            N_EAST = sum(if_else(as.logical(RESPONSE) & FORUM %in% "East Hill",TRUE,FALSE)),   
            N_VALLEY = sum(if_else(as.logical(RESPONSE) & FORUM %in% "Valley",TRUE,FALSE)),
            N_WEST = sum(if_else(as.logical(RESPONSE) & FORUM %in% "West Hill",TRUE,FALSE))) %>% 
  arrange(desc(N_TRUE)) 

# UPLOAD/UPDATE ----

issues_fp <- "./1-data/4-ready/home-issues.csv" # where the data will be save locally
issues_drive_path <- "~/Futurewise/Kent Rental Inspection/1-data/4-ready/home-issues"

write_csv(issues, issues_fp)

# drive_upload(issues_fp,issues_drive_path) # first time this script was run the file was uploaded
  
drive_issue_id <- if(!exists('drive_issue_id')|length(drive_issue_id)==0){drive_get(path = issues_drive_path) %>% as_id()}else{drive_issue_id} # create drive id 

drive_update(drive_issue_id,issues_fp)

