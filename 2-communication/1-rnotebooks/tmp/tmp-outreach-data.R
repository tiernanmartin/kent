# Setup ----

library(magrittr)
library(tidyverse)
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
       rerun(15,col_logical())
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
          range = cell_limits(c(4, 2), c(NA, 32)), 
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

drive

drive_upload(file = ,
             path = )

# Get summaries ----

# Home issues

issues_fp <- "./1-data/4-ready/east-hill-issues.csv" # where the data will be save locally
issues_name <- "home-issues" # the name of the file in Google Drive


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
  
drive_issue_id <- if(!exists('drive_issue_id')){drive_find(pattern = issues_name) %>% as_id()}

drive_update(drive_issues,issues_fp)




# Q17 (Comfortable with home inspection)

comb %>% 
  filter(QUESTION_ID %in% "Q17") %>% 
  select(QUESTION_ID,RESPONSE) %>% 
  mutate(RESPONSE_LGL = case_when(RESPONSE %in% "Agree" ~ TRUE,
                                  RESPONSE %in% "Disagree" ~ FALSE,
                                  TRUE ~ NA)) %>% 
  summarise(N = n(),
            N_TRUE = sum(RESPONSE_LGL, na.rm = TRUE),
            N_FALSE = sum(!RESPONSE_LGL, na.rm = TRUE),
            PCT_TRUE = scales::percent(mean(RESPONSE_LGL,na.rm = TRUE))) %>% 
  kable()
