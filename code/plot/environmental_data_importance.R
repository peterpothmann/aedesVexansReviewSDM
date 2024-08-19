library(tidyverse)
library(readODS)
library(ggrepel)


# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")
plotPath <- paste0(mainDir, "paper/plots/")

# import environmental data
envData <- read_ods(paste0(dataPath, "raw/environmentalData.ods"))s

data <- envData |> 
  filter(bestParameterVexans == "true" | bestParameterVexans == "false") |> 
  filter(subcategory != "-") |> 
  distinct(author, category, subcategory, .keep_all = TRUE) |> 
  count(category, subcategory, sort = TRUE) |> 
  rename("all" = n)

important <- envData |> 
  filter(bestParameterVexans == "true") |> 
  filter(subcategory != "-") |> 
  distinct(author, category, subcategory, .keep_all = TRUE) |> # deswegen werden es weniger
  count(category, subcategory, sort = TRUE) |> 
  rename("important" = n)

combine <- left_join(data, important, by = c("category", "subcategory")) |> 
  replace_na(list(important = 0)) |> 
  mutate(perc = important / all) |> 
  mutate(allStand = {(all-min(all))/(max(all)-min(all))},
         label = paste0(category, "-", subcategory)) 

ggplot(combine, aes(perc, allStand, label = label)) +
  theme_minimal() + 
  annotate("rect", xmin=-0, xmax=0.5, ymin=-0, ymax=0.5, fill="#CC79A7", alpha=0.3) +
  annotate("rect", xmin=0.5, xmax=1, ymin=-0, ymax=0.5, fill="#009E73", alpha=0.3) +
  annotate("rect", xmin=-0, xmax=0.5, ymin=0.5, ymax=1, fill="#56B4E9", alpha=0.3) +
  annotate("rect", xmin=0.5, xmax=1, ymin=0.5, ymax=1, fill="#F0E442", alpha=0.3) +
  geom_point(size = 3) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1), size = 0.5) +
  geom_segment(aes(x = 0, xend = 1, y = 1, yend = 1), size = 0.5) +
  geom_segment(aes(x = 1, xend = 1, y = 1, yend = 0), size = 0.5) +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 0), size = 0.5) +
  geom_segment(aes(x = 0.5, xend = 0.5, y = 0, yend = 1), size = 0.5) +
  geom_segment(aes(x = 0, xend = 1, y = 0.5, yend = 0.5), size = 0.5) +
  geom_label_repel(size = 4, 
                   seed = 9,
                   max.time = 3,
                   point.padding = 0.2,
                   box.padding = 0.45,
                   label.padding = 0.19) +
  xlab("Proportion of studies identifying parameter as important") +
  ylab("Standardised number of uses") +
  labs() +
  theme(text = element_text(size = 14))

ggsave(
  filename = "importance_environmental_params.png",
  device = "png",
  path = plotPath,
  dpi = 900,
  height = 6,
  width = 7,
  bg = "white"
)
