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
reviewData <- read_ods(paste0(dataPath, "raw/review_aedes_vexans.ods"))

modelTypeTable <- reviewData |> 
  count(modelApproaches, modelClass)

modelTypeTable$modelApproaches = factor(modelTypeTable$modelApproaches, levels = c("knowledge driven", "expert derived", "hybrid predictive", "data driven"), ordered = TRUE)
# more complex plot
ggplot(data = modelTypeTable, aes(x = modelApproaches, y = n, fill = modelClass, label = modelClass)) +
  labs(title =  "Frequency of different model types used",
       subtitle = element_text(paste0("Total number: ", sum(modelTypeTable$n)))) + 
  ylab("Frequency of applied models")+
  xlab("Modeling approach spectrum from knowledge-driven to data-driven") + 
  geom_bar(position = "stack", stat = "identity") + 
  # geom_text(aes(x = modelClass, y = n, label = modelType),
  #           position = position_stack(vjust = .5), size = 3.9) + 
  geom_bar_text(position = "stack", reflow = TRUE, min.size = 12) + 
  #scale_fill_hue(c = 90) +
  scale_fill_brewer(palette = "Set3") + 
  theme_bw(base_size = 22) +
  theme(legend.position="none",
        plot.title = element_text(face="bold"))

ggsave(filename = "model_approaches.png",
       device = "png",
       path = plotPath,
       dpi = 600,
       height = 9,
       width = 15)

