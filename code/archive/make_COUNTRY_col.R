
library(sf)
library(tidyverse)
library(terra)
library(tidyterra)

r <- rast("C:/Users/pothmann/Downloads/vbornetGapMBD(1)/tif/aevemodelMsk.tif")


IT <- st_read("C:/Users/pothmann/01-projects/AedesVexansReview/data/raw/gadm/gadm_410-gpkg/gadm_410.gpkg")
IT <- IT |>  st_make_valid()

IT <- IT |> 
  filter(CONTINENT == "Asia" | CONTINENT == "Europe" | CONTINENT == "Africa")

# v <- vect(IT) 

#rel <- relate(v, ext(r), "intersects")

x <- IT |> 
  rowwise() |> 
  mutate(intersection = relate(IT, ext(r), "intersects"))

s <- st_as_sf(x)

t <- s |> 
  filter(intersection == TRUE)
u <- unique(t$COUNTRY)

paste(u, collapse = ",")

border <- st_read("C:/Users/pothmann/Downloads/Export_Output.shp") |> 
  st_make_valid() |> 
  as_tibble() |> 
  mutate(country = case_when(country == "Czech Republic" ~ "Czechia",
                             country == "France/French Guiana" ~ "French Guiana",
                             country == "Macedonia The Former Yugoslav Republic Of" ~ "North Macedonia",
                             country == "Palestine/Gaza Strip" ~ "Palestine",
                             country == "West Bank" ~ "Palestine",
                             .default = country))

b <- right_join(IT, border, by = join_by(COUNTRY == country))

u <- unique(b$COUNTRY)

paste(u, collapse = ",")


