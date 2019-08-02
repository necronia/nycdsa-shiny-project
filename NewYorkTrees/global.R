library(shinydashboard)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)
#library(ggmap)
#library(googleVis)
library(DT)
library(tidyverse)

df <- readRDS("./sample_data.rds")
#df <- readRDS('./tree_data.rds')
df <- df %>% 
  mutate(location = paste0(df$latitude,':',df$longitude))

choice <- colnames(df)[4:5]
