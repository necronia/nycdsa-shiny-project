library(shinydashboard)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)
#library(ggmap)
#library(googleVis)
library(DT)
library(tidyverse)

tree_df <- readRDS('./tree_data.rds')
tree_df_sample = tree_df[sample(nrow(tree_df),50000),]
air_df <- readRDS('./air_data.rds')
air_311_df <- readRDS('./air_311_data.rds')

#nta_map <- geojsonio::geojson_read("./NTAmap.geojson", what = "sp")
#zip_map <- geojsonio::geojson_read("./ZIPmap.geojson", what = "sp")
nta_map <- readRDS('./nta_map.rds')
zip_map <- readRDS('./zip_map.rds')

choice <- colnames(as.data.frame(nta_map))[8:9]

