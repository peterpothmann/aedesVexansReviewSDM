# treemap plot
# plot to create a tree map based on the environmental data classification

# init library
library(tidyverse)
library(readODS)
library(treemap)
library(treemapify)
library(ggfittext)
library(ggrepel)
library(patchwork)

# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")
plotPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/paper/plots/"

# import environmental data
envData <- read_ods(paste0(dataPath, "raw/environmentalData.ods"))

# remove two values with No data classification
envDataSub <- envData |> 
  filter(subcategory != "-") |> 
  filter(subcategory != "unspec.") |> 
  filter(subcategory != "other")

# make first  letter to upper case
envDataSub$category <- gsub("^(\\w)(\\w+)", "\\U\\1\\L\\2",
                            envDataSub$category, perl = TRUE)

envDataSub$subcategory <- gsub("^(\\w)(\\w+)", "\\U\\1\\L\\2",
                               envDataSub$subcategory, perl = TRUE)

# prepare data
envDataAll <- envDataSub |>
  count(category, subcategory, sort = TRUE) |>
  rename(type_1 = category,
         type_2 = subcategory)

# make tree plot for all
tm <- treemap(
  envDataAll,
  index = c("type_1", "type_2"),
  vSize = "n",
  drop.unused.levels = FALSE
)

tmPlotAllData <- tm$tm %>%
  # calculate end coordinates with height and width
  mutate(x1 = x0 + w,
         y1 = y0 + h) %>%
  # get center coordinates for labels
  mutate(x = (x0 + x1) / 2,
         y = (y0 + y1) / 2) %>%
  # mark primary groupings and set boundary thickness
  mutate(primary_group = ifelse(is.na(type_2), 1.6, .5)) %>%
  # remove colors from primary groupings (since secondary is already colored)
  mutate(color = ifelse(is.na(type_2), NA, color))

# prepare data
envDataRelevant <- envDataSub |>
  filter(bestParameterVexans == "true")
  
envDataRelevantAgg <- envDataRelevant |> 
  count(category, subcategory, sort = TRUE) |>
  rename(type_1 = category,
         type_2 = subcategory)

# make tree plot for all
tm <- treemap(
  envDataRelevantAgg,
  index = c("type_1", "type_2"),
  vSize = "n",
  drop.unused.levels = FALSE
)

tmPlotRelevantData <- tm$tm %>%
  # calculate end coordinates with height and width
  mutate(x1 = x0 + w,
         y1 = y0 + h) %>%
  # get center coordinates for labels
  mutate(x = (x0 + x1) / 2,
         y = (y0 + y1) / 2) %>%
  # mark primary groupings and set boundary thickness
  mutate(primary_group = ifelse(is.na(type_2), 1.6, .5)) %>%
  # remove colors from primary groupings (since secondary is already colored)
  mutate(color = ifelse(is.na(type_2), NA, color))

# make tree plot for all data

plotAllParams <- tmPlotAllData %>%
  ggplot(aes(
    xmin = x0,
    ymin = y0,
    xmax = x1,
    ymax = y1
  )) +
  geom_rect(
    aes(fill = color, size = primary_group),
    show.legend = FALSE,
    color = "black",
    alpha = .3
  ) +
  scale_fill_identity() +
  scale_size(range = range(tmPlotAllData$primary_group)) +
  # make subcategories, normal
  geom_text_repel(data = filter(tmPlotAllData, vSize >= 5),
                  aes(label = type_2,
                  x = x,
                  y = y),
                  direction = "y",
                  seed = 1) +
  # pick out observations that are smaller and annotate with geom_text_repel
  geom_text_repel(
    data = filter(tmPlotAllData, vSize < 5),
    aes(
      x = x,
      y = y,
      label = glue::glue("{type_2}")
    ),
    color = "black",
    xlim = c(1.02, NA),
    size = 4,
    direction = "y",
    force = 1,
    seed = 1
  ) +
  # make main categories
  # ggfittext::geom_fit_text(data = filter(tm_plot_data, is.na(color)),
  #                          aes(label = type_1),
  #                          place = "top")+
  # make a hlo of the main categories
  ggrepel::geom_text_repel(
    data = filter(tmPlotAllData, is.na(color)),
    aes(x = x, y = y, label = type_1),
    bg.color = "white",
    bg.r = 0.25,
    size = 4,
    seed = 7
    #alpha = 0.5
  ) +
  # expand x-axis limits to make room for test annotations
  scale_x_continuous(limits = c(-0.01, 1.1), expand = c(0, 0)) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_void(base_size = 15) +
  labs(title = "All used explanatory variables",
       subtitle = paste0("Total number of observations: n = ", nrow(envDataSub)),
       caption = paste0(nrow(envData) - nrow(envDataSub), " values that could not be classified are not shown.")) +
  theme(plot.title = element_text(face = "bold"))

# make tree plot for relevant data
##################################

# change the color based on the color scheme of tmPlotAllData
colorScheme <- tmPlotAllData |>
  distinct(type_1, type_2, color)

tmPlotRelevantData <- tmPlotRelevantData |>
  select(-color) |>
  left_join(colorScheme, by = c("type_1" = "type_1", "type_2" = "type_2"))


plotRelevantParams <- tmPlotRelevantData %>%
  ggplot(aes(
    xmin = x0,
    ymin = y0,
    xmax = x1,
    ymax = y1
  )) +
  geom_rect(
    aes(fill = color,
        size = primary_group),
    show.legend = FALSE,
    color = "black",
    alpha = .3
  ) +
  scale_fill_identity() +
  scale_size(range = range(tmPlotRelevantData$primary_group)) +
  # make subcategories, normal
  ggrepel::geom_text_repel(
    data = filter(tmPlotRelevantData, vSize >= 3),
    aes(
      label = type_2,
      x = x,
      y = y),
    direction = "y",
    seed = 3) +
  # pick out observations that are smaller and annotate with geom_text_repel
  ggrepel::geom_text_repel(
    data = filter(tmPlotRelevantData, vSize < 3),
    aes(
      x = x,
      y = y,
      label = glue::glue("{type_2}")
    ),
    color = "black",
    xlim = c(1.02, NA),
    size = 4,
    direction = "y",
    force = 1,
    seed = 1
  ) +
  # make main categories
  # ggfittext::geom_fit_text(data = filter(tm_plot_data, is.na(color)),
  #                          aes(label = type_1),
  #                          place = "top")+
  # make a halo of the main categories
  geom_text_repel(
    data = filter(tmPlotRelevantData, is.na(color)),
    aes(x = x, y = y, label = type_1),
    bg.color = "white",
    bg.r = 0.25,
    size = 4,
    direction = "y",
    seed = 1
    #nudge_y = 0.09
    #alpha = 0.5
  ) +
  # expand x-axis limits to make room for test annotations
  scale_x_continuous(limits = c(-0.01, 1.1), expand = c(0, 0)) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_void(base_size = 15) +
  labs(title = "Explanatory variables indicated as relevant for the model results",
       subtitle = paste0("Total number of observations: n = ", nrow(envDataRelevant)),
       caption = "One values that could not be classified are not shown.") +
  theme(plot.title = element_text(face = "bold"))

plotAllParams
#plotAllParams / plotRelevantParams

ggsave(
  filename = "treemap_environmental_params.png",
  device = "png",
  path = plotPath,
  dpi = 900,
  height = 6,
  width = 9
)
  
