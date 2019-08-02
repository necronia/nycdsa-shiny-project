library(shinydashboard)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)
#library(ggmap)
#library(googleVis)
library(DT)
library(tidyverse)

#df <- readRDS("./sample_data.rds")
df <- readRDS('./tree_data.rds')
df <- df %>% 
  mutate(location = paste0(df$latitude,':',df$longitude))

nta <- geojsonio::geojson_read("json/NTAmap.geojson", what = "sp")

choice <- colnames(df)[4:5]

length(nta)
nta$test = c(1:195)
nta$test

test <- df %>% 
  group_by(nta) %>% 
  summarise(cnt = n())

test %>% 
  summarise(sum(cnt))

nta$ntacode
t = data.frame(nta = nta$ntacode)
t = left_join(t, test, by='nta')
t <- t %>% 
  transmute(cnt = ifelse(is.na(cnt),0,cnt))
nta$count = t$cnt

# nta
# df %>% 
#   group_by(nta_name) %>% 
#   summarise(cnt = n()) %>% 
#   arrange(desc(cnt))
# 
# df %>% 
#   summarise(n())
