# Restructure the raw review data
library(tidyverse)
library(readxl)
library(readODS)

dataPath <- "C:/Users/pothmann/01-projects/aedesVexansReview/data/"

# read data
data <- read_excel(paste0(dataPath, "raw/review_aedes_vexans.xlsx"))


# restructure
 sel <- data |> 
   select(key, author, envData, bestParameterVexans) |> 
   separate_rows(envData, sep = ",") |> 
   mutate(envData = trimws(envData))
 
 if(!file.exists(paste0(dataPath, 'edit/environmentalData.ods'))){
   write_ods(sel, paste0(dataPath, 'edit/environmentalData.ods'))
 }

 