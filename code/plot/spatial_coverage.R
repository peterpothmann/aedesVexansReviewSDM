# create a map 

library(tidyverse)
library(readxl)
library(readODS)
library(sf)
library(patchwork)

# set paths
mainDir <- "C:/Users/pothmann/01-projects/aedesVexansReview/"
dataDir <- paste0(mainDir, "data/")
plotPath <- paste0(mainDir, "paper/plots/")
tmpDir <- "C:/Users/pothmann/tmp/"
# read simplified country data
#countryBoarder <- st_read(paste0(dataDir, "raw/countries/ne_10m_admin_0_countries"))

# read country boarder data
countryBoarder <- readRDS(paste0(dataDir, "edit/gadm/countries.rds")) |> 
  select(unit, geom) |> 
  filter(unit != "Antarctica")

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
  count(COUNTRY)

mapData <- countryBoarder |> 
  left_join(mapData, by = join_by(unit == COUNTRY))

p1 <- ggplot(mapData) +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_b() +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  labs(title = "a) Number of studies per country")+
  theme(plot.title = element_text(face = "bold"))

p2 <- ggplot(occurenceData) +
  geom_sf(mapData, mapping = aes()) + 
  geom_sf(mapping = aes(), color = 'orange', alpha = 0.5, size = 0.9) +
  theme_minimal() +
  labs(title = "b) GBIF occurrence records of Aedes vexans")+
  theme(plot.title = element_text(face = "bold"))

p1 /
p2

ggsave(filename = "spatial_coverage.png",
       device = "png",
       path = plotPath,
       dpi = 600,
       height = 8,
       width = 14)

# get metrics of the map to use in the text to use in the text 
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