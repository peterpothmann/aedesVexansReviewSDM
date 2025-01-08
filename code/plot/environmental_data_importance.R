# plot 
# vizualze four field importance matrix

# init library
library(tidyverse)
library(readODS)
library(ggrepel)

# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")
plotPath <- paste0(mainDir, "paper/plots/")

# import environmental data
envData <- read_ods(paste0(dataPath, "raw/environmentalData.ods"))

# transform data
data <- envData |> 
  filter(bestParameterVexans == "true" | bestParameterVexans == "false") |> 
  filter(subcategory != "-" | subcategory == "unspec.") |> 
  distinct(author, category, subcategory, .keep_all = TRUE) |> 
  count(category, subcategory, sort = TRUE) |> 
  rename("all" = n)

# make a tibble with the important parameters & count 
important <- envData |> 
  filter(bestParameterVexans == "true") |> 
  filter(subcategory != "-") |> 
  distinct(author, category, subcategory, .keep_all = TRUE) |> # here i will get less n beacause i get only the unique ones
  count(category, subcategory, sort = TRUE) |> 
  rename("important" = n)

# artifical clases - not on final plot
# reviewData <- read_ods(paste0(dataPath, "raw/review_aedes_vexans.ods")) |> 
#   select(key, colinearityAcoounted) 
# 
# arti <- envData |> 
#   filter(subcategory == "artificial") |> 
#   left_join(reviewData) |> 
#   rowwise() |> 
#   mutate(subcategory = paste(subcategory, colinearityAcoounted, collapse = "-"))
# 
# artiAll <- arti |> 
#   filter(bestParameterVexans == "true" | bestParameterVexans == "false") |> 
#   count(category, subcategory) |> 
#   rename("all" = n)
# 
# artiBest <- arti |> 
#   filter(bestParameterVexans == "true") |> 
#   count(category, subcategory) |> 
#   rename("important" = n)
# 
# artiCombine <- left_join(artiAll, artiBest)

# combine and create statistics
combine <- left_join(data, important, by = c("category", "subcategory")) |>
#  bind_rows(artiCombine) |> 
  replace_na(list(important = 0)) |> 
  mutate(perc = important / all) |> 
  mutate(allStand = {(all-min(all))/(max(all)-min(all))},
         label = paste0(category, "-", subcategory)) 

# plot
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
  xlab("Proportion of studies using the variable and flagging it as important") +
  ylab("Number of uses (max. standardisation)") +
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
