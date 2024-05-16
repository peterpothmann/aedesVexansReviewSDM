# test treemap plot
# plot to create a tree based on environmental data classification

library(tidyverse)
library(readODS)
library(treemap)
library(treemapify)
library(ggfittext)
library(ggrepel)
library(patchwork)
# klima in wetter umbennen 


# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")

# import environmental data
envData <- read_ods(paste0(dataPath, "raw/environmentalData.ods")) #|> 
  #filter(bestParameterVexans == "true")

# prepare data
envDataAll <- envData |> 
  count(category, subcategory, sort = TRUE) |> 
  rename(type_1 = category,
         type_2 = subcategory)

# make tree plot for all
tm <- treemap(envDataAll,
        index = c("type_1", "type_2"),
        vSize = "n",
        drop.unused.levels = FALSE
        )

tmPlotAllData <- tm$tm %>% 
  # calculate end coordinates with height and width
  mutate(x1 = x0 + w,
         y1 = y0 + h) %>% 
  # get center coordinates for labels
  mutate(x = (x0+x1)/2,
         y = (y0+y1)/2) %>% 
  # mark primary groupings and set boundary thickness
  mutate(primary_group = ifelse(is.na(type_2), 1.2, .5)) %>% 
  # remove colors from primary groupings (since secondary is already colored)
  mutate(color = ifelse(is.na(type_2), NA, color))

# prepare data
envDataRelevant <- envData |> 
  filter(bestParameterVexans == "true") |> 
  count(category, subcategory, sort = TRUE) |> 
  rename(type_1 = category,
         type_2 = subcategory)

# make tree plot for all
tm <- treemap(envDataRelevant,
              index = c("type_1", "type_2"),
              vSize = "n",
              drop.unused.levels = FALSE)

tmPlotRelevantData <- tm$tm %>% 
  # calculate end coordinates with height and width
  mutate(x1 = x0 + w,
         y1 = y0 + h) %>% 
  # get center coordinates for labels
  mutate(x = (x0+x1)/2,
         y = (y0+y1)/2) %>% 
  # mark primary groupings and set boundary thickness
  mutate(primary_group = ifelse(is.na(type_2), 1.2, .5)) %>% 
  # remove colors from primary groupings (since secondary is already colored)
  mutate(color = ifelse(is.na(type_2), NA, color))

# make tree plot for all data

p1 <- tmPlotAllData %>% 
  ggplot(aes(xmin = x0, ymin = y0, xmax = x1, ymax = y1)) + 
  geom_rect(aes(fill = color, size = primary_group),
            show.legend = FALSE, color = "black", alpha = .3) +
  scale_fill_identity() +
  scale_size(range = range(tmPlotAllData$primary_group)) +
  # make subcategories, normal
  ggfittext::geom_fit_text(data = filter(tmPlotAllData, vSize >= 5),
                           aes(label = type_2), min.size = 1) +
  # pick out observations that are smaller and annotate with geom_text_repel
  ggrepel::geom_text_repel(
    data = filter(tmPlotAllData, vSize < 5),
    aes(x = x, y = y, label = glue::glue("{type_2}")),
    color = "black", xlim = c(1.02, NA), size = 4,
    direction = "y", vjust = .5, force = 3, 
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
    size = 5,
    #alpha = 0.5
    ) +
  # expand x-axis limits to make room for test annotations
  scale_x_continuous(limits = c(-0.01, 1.1), expand = c(0, 0)) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_void() + 
  labs(title = "All parameters considered for the model (n = 480)")

# make tree plot for relevant data

# change the colr based on the color scheme of tmPlotAllData
colorScheme <- tmPlotAllData |> 
  distinct(type_1, type_2, color) 

tmPlotRelevantData <- tmPlotRelevantData |> 
  select(-color) |> 
  left_join(colorScheme, by = c("type_1" = "type_1", "type_2" = "type_2"))


p2 <- tmPlotRelevantData %>% 
  ggplot(aes(xmin = x0, ymin = y0, xmax = x1, ymax = y1)) + 
  geom_rect(aes(fill = color, size = primary_group),
            show.legend = FALSE, color = "black", alpha = .3) +
  scale_fill_identity() +
  scale_size(range = range(tmPlotRelevantData$primary_group)) +
  # make subcategories, normal
  ggfittext::geom_fit_text(data = filter(tmPlotRelevantData, vSize >= 2),
                           aes(label = type_2), min.size = 1) +
  # pick out observations that are smaller and annotate with geom_text_repel
  ggrepel::geom_text_repel(
    data = filter(tmPlotRelevantData, vSize < 2),
    aes(x = x, y = y, label = glue::glue("{type_2}")),
    color = "black", xlim = c(1.02, NA), size = 4,
    direction = "y", vjust = .5, force = 3, 
  ) +
  # make main categories 
  # ggfittext::geom_fit_text(data = filter(tm_plot_data, is.na(color)), 
  #                          aes(label = type_1),
  #                          place = "top")+
  # make a hlo of the main categories
  ggrepel::geom_text_repel(
    data = filter(tmPlotRelevantData, is.na(color)), 
    aes(x = x, y = y, label = type_1), 
    bg.color = "white",
    bg.r = 0.25,
    size = 5,
    #alpha = 0.5
  ) +
  # expand x-axis limits to make room for test annotations
  scale_x_continuous(limits = c(-0.01, 1.1), expand = c(0, 0)) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_void() +
  labs(title = "Parameters indicated as relevant for the model results (n = 68)")

p1 - p2

