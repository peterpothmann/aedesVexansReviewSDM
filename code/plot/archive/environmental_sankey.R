# try a sankey plot

library(tidyverse)
library(readODS)
library(ggsankey)

# klima in wetter umbennen 


# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")

# import environmental data
envData <- read_ods(paste0(dataPath, "raw/environmentalData.ods")) #|> 
  #filter(category == "water characteristic")

long <- envData |> 
  make_long(category, subcategory, subsubcategory)

ggplot(long, aes(x = x,
                 next_x = next_x,
                 node = node,
                 next_node = next_node,
                 fill = factor(node),
                 label = node)) +
  geom_sankey() + 
  geom_sankey_label()
