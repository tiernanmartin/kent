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

d <- discussion_data

q <- questions_data %>% select(QUESTION_ID, QUESTION_TEXT_SHORT,TOPIC)

s <- 
  survey_data %>% 
  gather(QUESTION_ID, RESPONSE, Q16:Q15) %>% 
  left_join(q, by = "QUESTION_ID")
