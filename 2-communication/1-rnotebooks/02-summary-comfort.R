# SETUP ----

library(magrittr)
library(tidyverse)
library(stringr)
library(lubridate)
library(anytime)
library(forcats)
library(googlesheets)    # devtools::install_github('jennybc/googlesheets')
library(googledrive)     # devtools::install_github('tidyverse/googledrive')
library(scales)

options(httr_oob_default=TRUE) 
# LOAD DATA ----

drive_path <- "~/Futurewise/Kent Rental Inspection/1-data/3-interim/Outreach Data"

fp <- "./1-data/3-interim/outreach-data.csv"

drive_download(drive_path, fp,type = "csv",overwrite = TRUE)

data <- read_csv(fp)

# TRANSFORM + SUMMARIZE ----

comfort <- 
  data %>% 
  filter(QUESTION_ID %in% "Q17") %>% 
  select(QUESTION_ID,FORUM,RESPONSE) %>% 
  drop_na() %>% 
  {bind_rows(count(., FORUM,RESPONSE),
             count(., RESPONSE) %>% mutate(FORUM = "Combined"))} %>% 
  mutate(FCT = factor(RESPONSE)) %>% 
  complete(FORUM,FCT,fill = list(n = 0)) %>% # no respondents checked "Neutral" in East Hill, but it should be added as an option in the summary
  transmute(FORUM = factor(FORUM, levels = c("East Hill",
                                             "Valley",
                                             "West Hill",
                                             "Combined")),
            RESPONSE = as.character(FCT),
            N = n) %>% 
  group_by(FORUM) %>% 
  mutate(PCT = percent(smart_round(N/sum(N),2))) %>% 
  select_all(toupper) %>% 
  arrange(FORUM) 

# UPLOAD/UPDATE ----

comfort_fp <- "./1-data/4-ready/comfort-levels.csv" # where the data will be save locally
comfort_drive_path <- "~/Futurewise/Kent Rental Inspection/1-data/4-ready/comfort-levels"
drive_comfort <- drive_get(comfort_drive_path)

write_csv(comfort, comfort_fp)

# drive_upload(comfort_fp,comfort_drive_path) # first time this script was run the file was uploaded

drive_update(drive_comfort,comfort_fp)

