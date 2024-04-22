# plot to create a tree based on environmental data classification

library(tidyverse)
library(readODS)
library(data.tree)
library(treemap)
library(ggtree)
library(treeio)
library(phylobase)
library(ape)

# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")

# import environmental data
envData <- read_ods(paste0(dataPath, "raw/environmentalData.ods"))

# filter - only get the observations that are unique for each study - for viz purposes
envData <- envData |>
  group_by(category, subcategory, subsubcategory) #|> 
  # summarise(studiesNumber = n())

# new tibble to add the numer of studies per category as string to name
categoryNumber <- envData |> 
  group_by(category) |> 
   summarise(studyNumberCategory = n())

subcategoryNumber <- envData |> 
  group_by(subcategory) |> 
  summarise(sudyNumberSubcategory = n())

envData <- envData |> 
  left_join(categoryNumber) |> 
  left_join(subcategoryNumber) |> 
  mutate(category = paste0(category, " n = ", studyNumberCategory),
         subcategory = paste0(subcategory, " n = ", sudyNumberSubcategory))# jetzt hier left joinen


# construct tree
envData$pathString <- paste("classification",
                            envData$category,
                            envData$subcategory,
                            sep = "/")

d <- as.Node(envData) |> 
  as.phylo()
write.tree(d, "C:/Users/pothmann/tmp/testTree.tree")

d <- read.tree("C:/Users/pothmann/tmp/testTree.tree")

ggplot(d,) +
  geom_tree()+
  theme_tree() +
  geom_tiplab() +
 # geom_text(aes(label=studiesNumber), hjust = -3) + 
  geom_nodelab(geom='label') +
  hexpand(.05)

