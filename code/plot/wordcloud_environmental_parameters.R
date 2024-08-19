# make wordcloud for internal presentation

library(wordcloud2)
library(readODS)
library(tidyverse)
library(ggwordcloud)
library(RColorBrewer)

# set patidyverse# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")
plotPath <- paste0(mainDir, "paper/plots/")
tmpDir <- "C:/Users/pothmann/tmp/"

# import environmental data
set.seed(1)
envData <- read_ods(paste0(dataPath, "raw/environmentalData.ods")) |> 
  mutate(envData = tolower(envData)) |> 
  count(envData) |> 
  rowid_to_column(var = "rowid") |> 
  filter(rowid < 80) |> 
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)),
         colour = runif(row_number(), min = 1, max = row_number()))

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

ggplot(envData) +
  geom_text_wordcloud_area(
    aes(
      label = envData,
      size = n,
      angle = angle)
  ) +
  scale_size_area(max_size = 40) +
  theme_minimal()

ggsave(filename = "environmental_data_wordclooud.png",
       device = "png",
       path = plotPath,
       dpi = 600,
       height = 10,
       width = 12)
