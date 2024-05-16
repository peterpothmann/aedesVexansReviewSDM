# create a map 

library(tidyverse)
library(readxl)
library(readODS)
library(sf)
library(patchwork)

# set paths
mainDir <- "C:/Users/pothmann/01-projects/aedesVexansReview/"
dataDir <- paste0(mainDir, "data/")

# read simplified country data
#countryBoarder <- st_read(paste0(dataDir, "raw/countries/ne_10m_admin_0_countries"))

# read country boarder data
countryBoarder <- st_read(paste0(dataDir, "raw/gadm/gadm_410-levels.gpkg"), layer = "ADM_0") |>
  st_make_valid() #|>
  #st_simplify(countryBoarder, dTolerance = 10000)

# read review data
reviewTable <- read_ods(paste0(dataDir, "raw/review_aedes_vexans.ods"))

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
mapData <- reviewTable |> 
  separate_rows(COUNTRY, sep = ",") |> 
  mutate(COUNTRY = trimws(COUNTRY)) |> 
  distinct(key, COUNTRY, .keep_all = TRUE) |> 
  group_by(COUNTRY) |> 
  summarise(n = n_distinct(COUNTRY))

mapData <- countryBoarder |> 
  left_join(mapData, by = join_by(COUNTRY == COUNTRY))

p1 <- ggplot(mapData) +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_b() +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  labs(title = "Number of studies per country")

p2 <- ggplot(occurenceData) +
  geom_sf(mapData, mapping = aes()) + 
  geom_sf(mapping = aes(), color = 'orange', alpha = 0.5, size = 0.9) +
  theme_minimal() +
  labs(title = "GBIF occurence records of Aedes vexans")

p1 /
p2

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


# mapMetrics
# % der Studien subnational, national, international
percGeographicLevel <- reviewTable |> 
  group_by(geographicLevel) |> 
  summarise(n = n()) |> 
  mutate(percentage = (n / sum(n)) * 100)

# % der Studien pro Kontinent (schwierig, da man nicht weiß wo vexans eigentlich vor kommt)
percContinent <- reviewTable |>
  separate_rows(continent, sep = ", ") |> 
  group_by(continent) |> 
  summarise(n = n()) |> 
  mutate(percentage = (n / sum(n)) * 100)

