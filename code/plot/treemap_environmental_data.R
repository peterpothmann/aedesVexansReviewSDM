# test treemap plot
# plot to create a tree based on environmental data classification

library(tidyverse)
library(readODS)
library(treemap)
library(treemapify)

# klima in wetter umbennen 


# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")

# import environmental data
envData <- read_ods(paste0(dataPath, "raw/environmentalData.ods")) #|> 
  #filter(bestParameterVexans == "true")

# prepare data
envData <- envData |> 
  group_by(category, subcategory, subsubcategory) |> 
  summarise(parameterNumber = n())

# make tree plot
treemap(envData,
        index = c("category", "subcategory"),
        vSize = "parameterNumber"
        )

