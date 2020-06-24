library(tidyverse)


# Download nmvr data using STC API from https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=2010002101
get_nmvr_data <- function() {
  # Province latitude and longitude data
  provinces_latlong <- read_csv('data/raw/provinces_latlong.csv') %>% 
    janitor::clean_names()
  
  # Download data
  file_path <- './data/raw/nmvr_data.zip'
  url <- 'https://www150.statcan.gc.ca/n1/tbl/csv/20100021-eng.zip'
  download.file(url, file_path)
  nmvr_file <- unz(file_path, '20100021.csv')
  
  # Preprocess file and return dataframe
  nmvr_data <- read_csv(nmvr_file) %>% 
    janitor::clean_names() %>% 
    filter(str_detect(vehicle_type, '^Total')) %>% 
    filter(geo != 'Canada') %>% 
    select(year = ref_date, geo, fuel_type, amount = value) %>%
    replace_na(list(amount = 0)) %>% 
    mutate(geo = recode(geo, `British Columbia and the Territories` = 'British Columbia')) %>% # Fix later
    left_join(provinces_latlong, by = c('geo' = 'province')) %>% 
    group_by(geo, fuel_type) %>% 
    mutate(cumsum = cumsum(amount))
  
  return (nmvr_data)
}


# Download nmvs data using STC API from https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=2010000101
get_nmvs_data <- function() {
  # Download data
  file_path <- './data/raw/nmvs_data.zip'
  url <- 'https://www150.statcan.gc.ca/n1/tbl/csv/20100001-eng.zip'
  download.file(url, file_path)
  nmvs_file <- unz(file_path, '20100001.csv')
  
  # Preprocess file and return dataframe
  col_types = cols(
    STATUS = col_character(),
    SYMBOL = col_character(),
    TERMINATED = col_character()
  )
  
  nmvs_data <- read_csv(nmvs_file, col_types = col_types) %>% 
    janitor::clean_names() %>% 
    filter(seasonal_adjustment == 'Unadjusted') %>% 
    replace_na(list(value = 0)) %>% 
    mutate(value = ifelse(sales == 'Units', value, value*1000)) %>% # Convert dollars in 1000 to dollars in $1 unit
    dplyr::select(ref_date, geo, vehicle_type, origin_of_manufacture, sales, value) %>% 
    separate(ref_date, c('year', 'month'), sep='-') %>% 
    group_by(year, geo, vehicle_type, origin_of_manufacture, sales) %>% summarise(value = sum(value)) %>%  # Aggregate value by year 
    ungroup()
    
  return (nmvs_data)
}

