# metrics occurence data

library(tidyverse)
library(readODS)
library(ggfittext)

# set paths
mainDir <- "C:/Users/pothmann/01-projects/aedesVexansReview/"
dataDir <- paste0(mainDir, "data/")

# read review data
reviewTable <- read_ods(paste0(dataDir, "raw/review_aedes_vexans.ods"))

occDataSource <- reviewTable |> 
  separate_rows(occurenceDataSource, sep = ", ") |> 
  select(occurenceDataSource) |> 
  mutate(
    category = case_when(
      occurenceDataSource == "literature review" ~ "Literature review",
      occurenceDataSource %in% c( "gbif", "CULBASE", "VBORNET", "vectorNet", "Kramer") ~ "Databases",
      occurenceDataSource %in% c("survey programm", "field collection") ~ "Primary data collection",
      occurenceDataSource == "no occurence data" ~ "No use of occurrence data",
      .default = occurenceDataSource
    )
  )

occDataSourcePlotTable <- occDataSource |> 
  count(occurenceDataSource, category, sort = TRUE)

occDataSourcePlotTable$occurenceDataSource <- reorder(occDataSourcePlotTable$occurenceDataSource, occDataSourcePlotTable$n, FUN = sum)

ggplot(data = occDataSourcePlotTable, aes(x = category, y = n, fill = occurenceDataSource, label = occurenceDataSource)) +
  labs(title =  element_text(paste0("Total number: ", sum(occDataSourcePlotTable$n)))) + 
  ylab("Count")+
  xlab("Occurence data source") + 
  geom_bar(position = "stack", stat = "identity") + 
  geom_bar_text(position = "stack", reflow = TRUE) + 
  scale_fill_hue() +
  theme_bw(base_size = 16) +
  theme(legend.position="none")