# create a stacked barplot of the different occurrence datasources used in the studies

library(tidyverse)
library(readODS)
library(ggfittext)

# set paths
mainDir <- "C:/Users/pothmann/01-projects/aedesVexansReview/"
dataDir <- paste0(mainDir, "data/")
plotPath <- paste0(mainDir, "paper/plots/")

# read review data
reviewTable <- read_ods(paste0(dataDir, "raw/review_aedes_vexans.ods"))

occDataSource <- reviewTable |> 
  separate_rows(occurrenceDataSource, sep = ", ") |> 
  select(occurrenceDataSource) |> 
  mutate(
    category = case_when(
      occurrenceDataSource == "Literature review" ~ "Literature review",
      occurrenceDataSource %in% c( "GBIF", "CULBASE", "VBORNET", "vectorNet", "Kramer et al.") ~ "Databases",
      occurrenceDataSource %in% c("Survey programm", "Field collection") ~ "Primary data collection",
      occurrenceDataSource == "No occurrence data" ~ "No use of occurrence data",
      .default = occurrenceDataSource
    )
  )

occDataSourcePlotTable <- occDataSource |> 
  count(occurrenceDataSource, category, sort = TRUE)

occDataSourcePlotTable$category <- factor(occDataSourcePlotTable$category, levels = c("No occurence data", "Primary data collection", "Databases", "Literature review"))
# improvements:
  # - sort ascending 
  # - 

ggplot(data = occDataSourcePlotTable, aes(x = category, y = n, fill = occurrenceDataSource, label = occurrenceDataSource)) +
  labs(title = "Frequency of used databases",
       subtitle = paste0(paste0("Total number: ", sum(occDataSourcePlotTable$n)))) + 
  ylab("Frequency")+
  xlab("Occurrence data source by categories") + 
  geom_bar(position = "stack", stat = "identity") + 
  geom_bar_text(position = "stack", reflow = TRUE, min.size = 12) +
  scale_fill_brewer(palette = "Set3") + 
  theme_bw(base_size = 20) +
  theme(legend.position="none",
        plot.title = element_text(face="bold"))

ggsave(filename = "occurrence_datasource.png",
       device = "png",
       path = plotPath,
       dpi = 600,
       height = 6,
       width = 12)
  