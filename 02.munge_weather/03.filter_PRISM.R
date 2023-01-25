library(tidyverse)
library(lubridate)
library(prism)
library(raster)
library(parallel)

### Link PRISM grid cells to the counties where trials occur

### RUN ONCE:
download.file("https://www2.census.gov/geo/tiger/TIGER2017/COUNTY/tl_2017_us_county.zip",
              destfile = "data/prism/tl_2017_us_county.zip")
system("unzip data/prism/tl_2017_us_county.zip -d data/prism/county_outlines/")

# Get lat/long for trials
trials <- read_csv("data/trial_data/metadata/locations_gps.csv",
                   col_types = "ciccccccc") %>%
  mutate(across(Latitude:Longitude, ~ str_split(.x, "; "))) %>%
  unnest(c("Latitude", "Longitude")) %>%
  mutate(across(Latitude:Longitude, as.numeric)) %>%
  dplyr::select(Longitude, Latitude)

# Subset the shapefile to contain only counties that contained trials
counties <- sf::read_sf("data/prism/county_outlines/tl_2017_us_county.shp")
counties <- as(counties, "Spatial")
counties <- shapefile("data/prism/county_outlines/tl_2017_us_county.shp", verbose = TRUE)
county_intersect <- SpatialPoints(trials, proj4string = CRS(projection(counties))) %>%
  sp::over(., counties) %>%
  distinct()
counties <- subset(counties, COUNTYNS %in% county_intersect$COUNTYNS)
write_rds(counties, "data/prism/prism_counties.rds")
# counties <- read_rds("data/prism/prism_counties.rds")

# Function to subset PRISM files by overlap between grid cells and counties
prism_set_dl_dir("data/prism/raw")
prism_path <- prism_get_dl_dir()
prism_files <- prism_archive_ls()

prism_sub <- function(f) {
  prismfile_fp <- paste0(prism_path, "/", f, "/", f, ".bil")
  
  meta_d <- pd_get_date(prismfile_fp)
  param_name <- pd_get_type(prismfile_fp) %>%
    str_remove(., paste0(prism_path, "/"))
  
  rast <- raster(prismfile_fp)
  extract(rast, counties, cellnumbers = TRUE, df = TRUE) %>%
    rename(Value = !! f, Cell = cell) %>%
    mutate(Variable = param_name, 
           COUNTYNS = counties$COUNTYNS[ID], 
           Longitude = xFromCell(rast, Cell), 
           Latitude = yFromCell(rast, Cell), 
           Year = year(meta_d), 
           Month = month(meta_d)) %>%
    dplyr::select(Variable, Year:Month, COUNTYNS, Cell, Longitude:Latitude, Value)
}

# Subset PRISM files in parallel
cl <- makeCluster(detectCores()/2, outfile = "prism_sub.log")
clusterEvalQ(cl, {
  library(tidyverse)
  library(lubridate)
  library(prism)
  library(raster)
})
clusterExport(cl, list("prism_sub", "prism_path", "counties"))
on.exit(stopCluster(cl))

files_sub <- prism_archive_subset(type = "ppt", temp_period = "monthly")
res_ppt <- parSapplyLB(cl, files_sub, function(f) {
    if (file.exists(paste0("data/prism/subset/", f, "_subset.csv"))) return(TRUE)
    cat(f, "\n")
    write_csv(prism_sub(f), paste0("data/prism/subset/", f, "_subset.csv"))
  })

res_ppt <- mclapply(files_sub, function(f) {
  cat(f, "\n")
  write_csv(prism_sub(f), paste0("data/prism/subset/", f, "_subset.csv"))
}, mc.cores = 60)

files_sub <- prism_archive_subset(type = "tmin", temp_period = "monthly")
res_tmin <- parSapplyLB(cl, files_sub, function(f) {
    cat(f, "\n")
    write_csv(prism_sub(f), paste0("data/prism/subset/", f, "_subset.csv"))
  })

files_sub <- prism_archive_subset(type = "tmax", temp_period = "monthly")
res_tmax <- parSapplyLB(cl, files_sub, function(f) {
    cat(f, "\n")
    write_csv(prism_sub(f), paste0("data/prism/subset/", f, "_subset.csv"))
  })

stopCluster(cl)
