library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(tidyverse)

df = read.csv("../tree_data_raw.csv", stringsAsFactors = F)

# df <- tidyr::separate(data=df,
#                       col=Location.1,
#                       into=c("latitude", "longitude"),
#                       sep=",",
#                       remove=FALSE)

saveRDS(df, "./tree_data.rds")

sample_data <- df[c(1:1000),]
saveRDS(sample_data, "./sample_data.rds")
