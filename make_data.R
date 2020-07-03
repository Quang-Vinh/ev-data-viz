# Script to process data 


library(rgdal)
library(rmapshaper)

source('load_data.R')


# Download nmvr and nmvs datasets
update_dataset('nmvr')
update_dataset('nmvs')


# CMA map
can_cma_shapes <- 
  readOGR(
    dsn = "./data/raw/lcma000b16a_e/lcma000b16a_e.shp", 
    encoding='UTF-8',
    use_iconv = TRUE) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84")) %>% 
  ms_simplify()

writeOGR(
  obj = can_cma_shapes,
  dsn = './data/processed/shapes',
  layer = 'can_cma_shapes',
  driver = 'ESRI Shapefile',
  overwrite_layer = TRUE)