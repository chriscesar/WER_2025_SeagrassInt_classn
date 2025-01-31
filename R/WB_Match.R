# WB_Match.R ####
# Import classification data and append WB info

## load packages ####
ld_pkgs <- c("tidyverse", "tictoc","sf","nngeo")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE);rm(ld_pkgs)

tictoc::tic.clearlog()

#Load data####
tic("Load data")
source("R/metadata.R")

# load data ####
## WB shapes ####
wbDat_TRAC <- sf::st_read(paste0(GISfol_TRAC,"C3_TRAC_2022.shp"))

## Seagrass data ####
df0 <- readxl::read_xlsx(paste0(datfol_pre,"WFD25_Seagrass_PreQA_DataPrep.xlsx"),
                         sheet = "02_Edits_01")
toc(log = TRUE)

# Convert & join data to nearest WB####
tic("Convert & join data to nearest WB")
# Convert df0 to an sf object (assuming Eastings and Northings as coordinates)
df0_sf <- st_as_sf(df0,
                   coords = c("Quadrat Full Easting",
                              "Quadrat Full Northing"),
                   crs = 27700)

# Ensure both objects are in the same CRS
if (!st_crs(df0_sf) == st_crs(wbDat_TRAC)) {
  stop("CRS mismatch between df0_sf and wbDat_TRAC. Fix it!")
} else {print("CRS for df0_sf and wbDat_TRAC match. Continue!")
}

# Calculate distances from each point to all polygons
distances <- st_distance(df0_sf, wbDat_TRAC)

# Find the index of the nearest polygon for each point
nearest_idx <- apply(distances, 1, which.min)

# Extract the nearest polygons and their attributes
nearest_polygons <- wbDat_TRAC[nearest_idx, ] %>% 
  dplyr::select(.,c(WATER_BODY:WATER_BO_4,RIVER_BASI,SURVEILLAN))

# Add the nearest polygon attributes and distance to the points
df0_sf_with_nearest <- df0_sf %>%
  mutate(
    # replace 'id' with the identifier column in base_WBs
    nearest_polygon_id = nearest_polygons$OBJECTID,
    distance_to_nearest = as.numeric(apply(distances, 1, min))
  ) %>%
  cbind(st_drop_geometry(nearest_polygons)) %>% 
  relocate(.,distance_to_nearest,.after = last_col())

# df0_sf_with_nearest %>% names()
#   dplyr::select(.,Water.body:Ruppia.Present,
# WATER_BODY,
# WATER_BO_1,WATER_BO_2,WATER_BO_3,WATER_BO_4,OPERATIONA,
# RIVER_BASI,SURVEILLAN,
# distance_to_nearest)

toc(log=TRUE)

# Write data ####
tic("Write data")
write.csv(df0_sf_with_nearest,
          file=paste0(datfol_pre,"2025_Pre_GIS_out.csv"),
          row.names = FALSE)
toc(log=TRUE)

unlist(tictoc::tic.log())
