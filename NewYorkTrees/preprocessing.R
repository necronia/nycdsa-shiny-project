library(shiny)
library(dplyr)
library(tidyr)
library(tidyverse)

## load csv
tree_df = read.csv("../tree_data_raw.csv", stringsAsFactors = F)
air_df = read.csv("../air_data_raw.csv", stringsAsFactors = F)
air311_df = read.csv("../311_air_data_raw.csv", stringsAsFactors = F)

## load maps
nta_map <- geojsonio::geojson_read("json/NTAmap.geojson", what = "sp")
zip_map <- geojsonio::geojson_read("json/ZIPmap.geojson", what = "sp")

## cleansing data
tree_df <- tree_df %>% 
  mutate(location = paste0(tree_df$latitude,':',tree_df$longitude))

df_count <- tree_df %>% 
  group_by(nta) %>% 
  summarise(cnt = n())

df_temp = data.frame(nta = nta_map$ntacode, area = as.numeric(as.character(nta_map$shape_area)))
df_temp = left_join(df_temp, df_count, by='nta')
df_temp <- df_temp %>% 
  transmute(cntpa = ifelse(is.na(cnt),0,cnt/(area/1.076e7)), #feet->killometer
            cnt = ifelse(is.na(cnt),0,cnt)) 
nta_map$countperarea = df_temp$cntpa
nta_map$count = df_temp$cnt


## save datas
saveRDS(tree_df, "./tree_data.rds")
saveRDS(air_df, "./air_data.rds")
saveRDS(air311_df, "./air_311_data.rds")

#geojsonio::geojson_write(input = nta_map, file = "./NTAmap.geojson")
#geojsonio::geojson_write(input = zip_map, file = "./ZIPmap.geojson")
saveRDS(nta_map, "./nta_map.rds")
saveRDS(zip_map, "./zip_map.rds")

