library(tidyverse)

talkers = read_csv('Input/pdfs/extraction/named_entities.csv') %>% dplyr::select(-X1) %>%
  filter(!grepl('[A-Z]{4}',Subject))

attendance = read_csv('Input/scraped_data/temp_cleaned_data.csv') %>% dplyr::select(-X1) 

