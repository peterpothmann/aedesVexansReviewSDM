# bar plot for model approaches
library(tidyverse)
library(readODS)
library(ggfittext)

# set paths
mainDir <- "C:/Users/pothmann/01-projects/aedesVexansReview/"
dataDir <- paste0(mainDir, "data/")
plotPath <- paste0(mainDir, "paper/plots/")
tmpDir <- "C:/Users/pothmann/tmp/"

# import review data
reviewData <- read_ods(paste0(dataDir, "raw/review_aedes_vexans.ods"))

# Create a table of model approaches and classes
modelTypeTable <- reviewData |> 
  count(modelApproaches, modelClass)

# Order modelApproaches
modelTypeTable$modelApproaches <- factor(modelTypeTable$modelApproaches, 
                                         levels = c("knowledge driven", "expert derived", "hybrid predictive", "data driven"), 
                                         ordered = TRUE)

# Precompute sorted levels for modelClass
sorted_modelClass_levels <- modelTypeTable %>%
  group_by(modelApproaches) %>%
  arrange(-n) %>%
  pull(modelClass) %>%
  unique()

# Apply sorted levels globally to modelClass
modelTypeTable$modelClass <- factor(modelTypeTable$modelClass, 
                                    levels = sorted_modelClass_levels, 
                                    ordered = TRUE)

# Create the plot
ggplot(data = modelTypeTable, aes(x = modelApproaches, y = n, fill = modelClass, label = modelClass), color = "black") +
  labs(title = "Frequency of different model types used",
       subtitle = paste0("Total number: ", sum(modelTypeTable$n))) + 
  ylab("Frequency of applied models") +
  xlab("Modeling approach spectrum from knowledge-driven to data-driven") + 
  geom_bar(position = "stack", stat = "identity", color = "black") + 
  geom_bar_text(position = "stack", reflow = TRUE, min.size = 12) + 
  scale_fill_brewer(palette = "Set3") + 
  theme_bw(base_size = 22) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

# Save the plot
ggsave(filename = "model_approaches.png",
       device = "png",
       path = plotPath,
       dpi = 600,
       height = 7,
       width = 11)
