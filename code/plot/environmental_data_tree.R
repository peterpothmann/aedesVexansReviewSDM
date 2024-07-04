  # plot to create a tree based on environmental data classification
  
  library(tidyverse)
  library(readODS)
  library(data.tree)
  library(treemap)
  library(ggtree)
  library(treeio)
  library(phylobase)
  library(ape)
  
  # klima in wetter umbennen 
  
  
  # set paths
  mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
  dataPath <- paste0(mainPath, "data/")
  
  # import environmental data
envData <- read_ods(paste0(dataPath, "raw/environmentalData.ods")) #|> 
  #   filter(category == "weather")
  
  # filter - only get the observations that are unique for each study - for viz purposes
  envData <- envData |>
    group_by(category, subcategory, subsubcategory) #|> 
    # summarise(studiesNumber = n())
  
  # new tibble to add the number of studies per category as string to name
  categoryNumber <- envData |> 
    group_by(category) |> 
     summarise(studyNumberCategory = n())
  
  subcategoryNumber <- envData |> 
    group_by(subcategory) |> 
    summarise(sudyNumberSubcategory = n())
  
observationPeriodNumber <- envData |> 
    group_by(subcategory, observationPeriod) |> 
    summarise(sudyNumberObservationPeriod = n())

calculationNumber <- envData |> 
  group_by(subcategory, observationPeriod, calculation) |> 
  summarise(sudyNumberCalculation = n())

# bestParameterVexans <-envData |> 
#   group_by(subcategory, observationPeriod, calculation) |> 
#   summarise(sudyNumberCalculation = n())

  
  envData <- envData |> 
    left_join(categoryNumber) |> 
    left_join(subcategoryNumber) |> 
    left_join(observationPeriodNumber, by = join_by(subcategory == subcategory, observationPeriod == observationPeriod)) |>
    left_join(calculationNumber, by = join_by(subcategory == subcategory, observationPeriod == observationPeriod, calculation == calculation)) |>
    mutate(category = paste0(category, " n = ", studyNumberCategory),
           subcategory = paste0(subcategory, " n = ", sudyNumberSubcategory),
           observationPeriod = paste0(observationPeriod, " n = ", sudyNumberObservationPeriod),
           calculation = paste0(calculation, " n = ", sudyNumberCalculation)) # jetzt hier left joinen
  
  # construct tree
  envData$pathString <- paste("classification",
                              envData$category,
                              envData$subcategory,
                              #envData$subsubcategory,
                              #envData$observationPeriod,
                              #envData$calculation,
                              #envData$bestParameterVexans,
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
  
