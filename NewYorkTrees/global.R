library(shinydashboard)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(googleVis)
library(DT)
library(tidyverse)

tree_df <- readRDS('./tree_data.rds')
tree_df_sample = tree_df[sample(nrow(tree_df),50000),]
#air_df <- readRDS('./air_data.rds')
#air_311_df <- readRDS('./air_311_data.rds')
tree_311_df <- readRDS('./tree_311_data.rds')

#nta_map <- geojsonio::geojson_read("./NTAmap.geojson", what = "sp")
#zip_map <- geojsonio::geojson_read("./ZIPmap.geojson", what = "sp")
nta_map <- readRDS('./nta_map.rds')
zip_map <- readRDS('./zip_map.rds')

choice_dist <- c('All','Tree','Complaint')
choice_nta <- colnames(as.data.frame(nta_map))[8:9]
choice_zip <- colnames(as.data.frame(zip_map))[12:13]
choice_zip_meta <- c('All','Tree only','Complaint only')

tree_df_type <- tree_df %>% 
  group_by(spc_common) %>% 
  summarise(cnt = n()) %>% 
  filter(!is.na(spc_common)) %>% 
  arrange(desc(cnt)) %>% 
  top_n(20)

tree_df_status <- tree_df %>% 
  group_by(status) %>% 
  summarise(cnt = n()) %>% 
  filter(!is.na(status)) %>% 
  arrange(desc(cnt)) 

tree_df_health <- tree_df %>% 
  group_by(health) %>% 
  summarise(cnt = n()) %>% 
  filter(!is.na(health) & health!='') %>% 
  arrange(desc(cnt)) 

tree_df_dbh <- tree_df %>% 
  mutate(tree_dbh = ifelse(tree_dbh>50,50,tree_dbh)) %>% 
  group_by(tree_dbh) %>% 
  summarise(cnt = n()) %>% 
  filter(!is.na(tree_dbh) & tree_dbh>0) %>% 
  arrange(desc(cnt))

tree311_df_type <- tree311_df %>% 
  group_by(Complaint.Type) %>% 
  summarise(cnt = n()) %>% 
  arrange(desc(cnt))

tree311_df_desc <- tree311_df %>% 
  group_by(Descriptor) %>% 
  summarise(cnt = n()) %>% 
  arrange(desc(cnt))

nta_map_view <- nta_map
nta_map_view$countperarea = ifelse(nta_map$countperarea > 2000, 2000, nta_map$countperarea)
zip_map_view <- zip_map 
zip_map_view$countperarea = ifelse(zip_map$countperarea > 2500, 2500, zip_map$countperarea)