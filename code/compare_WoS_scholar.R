# compare results of scholar and WoS 
# search term: WoS: full text vexans and model 

library(tidyverse)
library(readxl)
# set paths
mainPath <- "C:/Users/pothmann/01-projects/AedesVexansReview/"
dataPath <- paste0(mainPath, "data/")

# import 
wos <- read_excel(paste0(dataPath, "raw/wos/savedrecs.xls")) # web of science

review <- read_excel(paste0(dataPath, "raw/review_aedes_vexans.xlsx")) |> 
  mutate("Article Title" = title)

t <- anti_join(review, wos)
write_csv(t, "C:/Users/pothmann/tmp/noMatch1.csv")
               