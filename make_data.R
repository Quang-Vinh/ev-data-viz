source('load_data.R')

# Download nmvr and nmvs datasets
update_dataset()

# CMA map
can_prov <- readOGR(dsn = "./data/raw/lcma000b16a_e/lcma000b16a_e.shp")
can_cma <- readOGR(dsn = "./data/raw/lcma000b16a_e/lcma000b16a_e.shp")
can <- spTransform(can_prov, CRS("+proj=longlat +datum=WGS84"))

fake_cma <- read_csv('./data/raw/fake_cma.csv')