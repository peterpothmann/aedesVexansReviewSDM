# create a map 

library(tidyverse)
library(readxl)
library(readODS)
library(sf)

# set paths
mainDir <- "C:/Users/pothmann/01-projects/aedesVexansReview/"
dataDir <- paste0(mainDir, "data/")

# read country data if no centroid geometry is present, create and write one 
if(!file.exists(paste0(dataDir, "edit/gadm/country_centroids.gpkg"))) {
  country <- st_read(paste0(dataDir, "raw/gadm/gadm_410-levels.gpkg"), layer = "ADM_0")
  country <- country |> 
    st_make_valid() |> 
    st_centroid()
  st_write(c, paste0(dataDir, "edit/gadm/country_centroids.gpkg"))
  }

# read country boarder data
countryBoarder <- st_read(paste0(dataDir, "raw/gadm/gadm_410-levels.gpkg"), layer = "ADM_0") |> 
  st_make_valid() |> 
  st_simplify(countryBoarder, dTolerance = 10000)

# read review data
reviewTable <- read_ods(paste0(dataDir, "raw/review_aedes_vexans.ods")) |> 
  filter(is.na(sortOut))

# read gbif occurence data
occurenceData <- read_tsv(paste0(dataDir, "raw/gbif/0202791-240321170329656.csv")) |>
  drop_na(decimalLongitude, decimalLatitude)  |> 
  distinct(decimalLongitude, decimalLatitude, .keep_all = T) |> 
  st_as_sf(coords=c("decimalLongitude", "decimalLatitude"), crs = 4326)

occurenceDataCoordinates <- occurenceData |> 
  st_coordinates()

occurenceData <- occurenceData |> 
  bind_cols(occurenceDataCoordinates)

# clean review data
reviewTable <- reviewTable |> 
  separate_rows(COUNTRY, sep = ",") |> 
  mutate(COUNTRY = trimws(COUNTRY)) |> 
  distinct(key, COUNTRY, .keep_all = TRUE) |> 
  group_by(COUNTRY) |> 
  summarise(n = n())

mapData <- countryBoarder |> 
  left_join(reviewTable)

ggplot(mapData) +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_b() +
  geom_sf(occurenceData, mapping = aes(), color = 'orange', alpha = 0.15 )


# ggplot(mapData) + 
#   geom_hex(occurenceData, mapping = aes(x = X, y = Y)) +
#   geom_sf(mapping = aes(color = n)) +
#   geom_sf(countryBoarder, mapping = aes(), fill = "transparent", color = "black")


# ggplot() +
#   geom_sf(data = mapData, mapping = aes(fill = n)) +
#   stat_density_2d(data = occurenceData, 
#                  aes(x = X, y = Y, fill = ..density..), 
#                  geom = 'raster', 
#                  contour = F) 
 # geom_sf(data = occurenceData, mapping = aes(), color = "red", alpha = 0.5) + 
  #scale_fill_viridis_b()
