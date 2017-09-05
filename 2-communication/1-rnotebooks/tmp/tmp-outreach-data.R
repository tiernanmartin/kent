# Setup ----

library(magrittr)
library(tidyverse)
library(scales)
library(sf)
library(RSocrata)
library(leaflet)
library(stringr)
library(lubridate)
library(anytime)
library(forcats)
library(googlesheets)    # devtools::install_github('jennybc/googlesheets')
library(googledrive)     # devtools::install_github('tidyverse/googledrive')
library(mapedit)         # devtools::install_github("r-spatial/mapedit")
library(mapview)         # devtools::install_github("r-spatial/mapview@develop")
library(leaflet.extras)  # devtools::install_github("bhaskarvk/leaflet.extras")
library(knitr)
library(miscgis)         # devtools::install_github("tiernanmartin/miscgis")

options(httr_oob_default=TRUE) 
gs_auth(new_user = TRUE) 
# Load data ----

url <- "https://docs.google.com/spreadsheets/d/1spLfAtyRqCKi7gxcPb4JMWE5dFtYVYZJD7J3i2ufXp8/edit?usp=sharing"

data_sheet <- gs_url(url, lookup = NULL, visibility = NULL, verbose = TRUE)


# create a column types list for each of the datasets

col_types_list_disc <- 
  list(rerun(4,col_character()),
       list(col_logical()),
       rerun(5,col_character())
  ) %>% flatten()

col_types_list_surv <- 
  list(rerun(4,col_character()),
       list(col_logical()),
       rerun(11,col_character()),
       rerun(17,col_logical())
  ) %>% flatten()

col_types_list_qs <- 
  list(rerun(5,col_character()), 
       rerun(3,col_logical())
  ) %>% flatten()

discussion_data <- 
  data_sheet %>% 
  gs_read(ws = "Discussion Data", 
          range = cell_limits(c(2, 2), c(NA, 11)), 
          colnames = TRUE, 
          na = c("","#N/A"),
          col_types = col_types_list_disc) %>% 
  set_colnames(str_replace_all(colnames(.),"\\s","_")) 

survey_data <- 
  data_sheet %>% 
  gs_read(ws = "Survey Data", 
          range = cell_limits(c(4, 2), c(NA, 34)), 
          colnames = TRUE,
          na = c("","#N/A"),
          col_types = col_types_list_surv) %>% 
  set_colnames(str_replace_all(colnames(.),"\\s","_")) 

questions_data <- 
  data_sheet %>% 
  gs_read(ws = "Questions",
          range = cell_limits(c(2,2),c(NA,9)),
          colnames = TRUE,
          col_types = col_types_list_qs) %>% 
  set_colnames(str_replace_all(colnames(.),"\\s","_")) %>% 
  set_colnames(str_replace_all(colnames(.),"\\(|\\)","")) 

# Transform the data ----

# make survey data long

d <- discussion_data %>% arrange(QUESTION_ID)

q <- 
  questions_data %>% 
  select(QUESTION_ID, QUESTION_TEXT,QUESTION_TEXT_SHORT,TOPIC) %>% 
  mutate(QUESTION_TEXT = if_else(is.na(QUESTION_TEXT_SHORT),QUESTION_TEXT,QUESTION_TEXT_SHORT))

s <- 
  survey_data %>% 
  gather(QUESTION_ID, RESPONSE, matches("^Q[[:digit:]]{2}$")) %>% # spead all question columns ("Q##")
  left_join(q, by = "QUESTION_ID") %>% 
  arrange(QUESTION_ID)

# check out the datasets

list(d,s) %>% walk(glimpse)

# bind the two tibbles and export

comb <- bind_rows(d,s)



# Get summaries ----

# Home issues

issues_fp <- "./1-data/4-ready/east-hill-issues.csv" # where the data will be save locally
issues_drive_path <- "~/Futurewise/Kent Rental Inspection/1-data/4-ready/home-issues" 


comb %>% 
  filter(QUESTION_ID %in% paste0("Q",str_pad(1:15,2,pad = "0"))) %>%  
  group_by(QUESTION_ID) %>% 
  summarise(QUESTION_TEXT = first(QUESTION_TEXT),
            N_TRUE = sum(as.logical(RESPONSE)),
            N_EAST = sum(if_else(as.logical(RESPONSE) & FORUM %in% "East Hill",TRUE,FALSE)),   
            N_VALLEY = sum(if_else(as.logical(RESPONSE) & FORUM %in% "Valley",TRUE,FALSE)),
            N_WEST = sum(if_else(as.logical(RESPONSE) & FORUM %in% "West Hill",TRUE,FALSE))) %>% 
  arrange(desc(N_TRUE)) %>% 
  write_csv(issues_fp)

# drive_upload(issues_fp,issues_drive_path) # first time this script was run the file was uploaded
  
drive_issue_id <- if(!exists('drive_issue_id')){drive_get(path = issues_drive_path) %>% as_id()}else{drive_issue_id} # create drive id 

drive_update(drive_issue_id,issues_fp)



# Q17 (Comfortable with home inspection)

comfort_fp <- "./1-data/4-ready/comfort-levels.csv" # where the data will be save locally
comfort_drive_path <- "~/Futurewise/Kent Rental Inspection/1-data/4-ready/comfort-levels" 
comfort_name <- "comfort-levels" # the name of the file in Google Drive

comb %>% 
  filter(QUESTION_ID %in% "Q17") %>% 
  select(QUESTION_ID,FORUM,RESPONSE) %>% 
  drop_na() %>% 
  count(FORUM,RESPONSE) %>% 
  mutate(FCT = factor(RESPONSE)) %>% 
  complete(FORUM,FCT,fill = list(n = 0)) %>% # no respondents checked "Neutral" in East Hill, but it should be added as an option in the summary
  transmute(FORUM,
            RESPONSE = as.character(FCT),
            N = n) %>% 
  group_by(FORUM) %>% 
  mutate(PCT = percent(smart_round(N/sum(N),2))) %>% 
  select_all(toupper) %>% 
  write_csv(comfort_fp)

# drive_upload(comfort_fp,comfort_drive_path) # first time this script was run the file was uploaded

drive_comfort_id <- if(!exists('drive_comfort_id')){drive_get(path = comfort_drive_path) %>% as_id()}else{drive_comfort_id}

drive_update(drive_comfort_id,comfort_fp)
