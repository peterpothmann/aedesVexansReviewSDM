# 

library(tidyverse)
library(readxl)
library(readODS)
library(ggforce)

# set paths
mainDir <- "C:/Users/pothmann/01-projects/aedesVexansReview/"
dataDir <- paste0(mainDir, "data/")
plotPath <- paste0(mainDir, "paper/plots/")
tmpDir <- "C:/Users/pothmann/tmp/"
  
# read review data
reviewTable <- read_ods(paste0(dataDir, "raw/review_aedes_vexans.ods"))

dataType <- reviewTable |> 
  count(occurrenceDataType) |> 
  mutate(perc = round(n / sum(n) * 100, digits = 1),
         ymax = cumsum(perc),
         ymin = c(0, head(ymax, n=-1)),
         label = paste0(occurrenceDataType, "\n (n = ", n, ", perc = ",perc, ")"),
         labelPosition = (ymax + ymin) / 2) |> 
  mutate(end = 2 * pi * cumsum(n)/sum(n),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))

ggplot(dataType) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                   start = start, end = end, fill = occurrenceDataType)) +
  geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label =  label,
                hjust = hjust, vjust = vjust)) +
  coord_fixed() +
  scale_x_continuous(limits = c(-1.8, 1.8),
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1.3, 1.3),   
                     name = "", breaks = NULL, labels = NULL) +
  labs(title = "Occurrence data types",
       subtitle = paste0("Total number: ", sum(dataType$n)))+
  scale_fill_brewer(palette="Set3") +
  theme_void(base_size = 20) +
  theme(legend.position="none",
        plot.title = element_text(face="bold"))

ggsave(filename = "occurrence_data_type.png",
         device = "png",
         path = plotPath,
         dpi = 600,
         height = 6,
         width = 7)
  