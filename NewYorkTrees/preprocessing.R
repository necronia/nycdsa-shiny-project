library(shiny)
library(dplyr)
library(tidyr)
library(tidyverse)

## load csv
tree_df = read.csv("../tree_data_raw.csv", stringsAsFactors = F)
#air_df = read.csv("../air_data_raw.csv", stringsAsFactors = F)
#air311_df = read.csv("../311_air_data_raw.csv", stringsAsFactors = F)
tree311_df = read.csv("../311_tree_data_raw.csv", stringsAsFactors = F)

## load maps
nta_map <- geojsonio::geojson_read("json/NTAmap.geojson", what = "sp")
zip_map <- geojsonio::geojson_read("json/ZIPmap.geojson", what = "sp")

## cleansing data
tree_df <- tree_df %>% 
  mutate(location = paste0(tree_df$latitude,':',tree_df$longitude)) 

tree311_df <- tree311_df %>% 
  mutate(location = paste0(tree311_df$Latitude,':',tree311_df$Longitude)) %>% 
  filter(!grepl('Street',Complaint.Type))

# count(tree) data add to NTA-map
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

# count(tree) data add to ZIP-map
df_count <- tree_df %>% 
  group_by(postcode) %>% 
  summarise(cnt = n())

df_temp = data.frame(postcode = as.numeric(as.character(zip_map$postalCode)), area = as.numeric(as.character(zip_map$Shape_Area)))
df_temp = df_temp %>% 
  group_by(postcode) %>% 
  mutate(area = sum(area))
df_temp = left_join(df_temp, df_count, by='postcode')
df_temp <- df_temp %>% 
  transmute(cntpa = ifelse(is.na(cnt),0,cnt/(area/1.076e7)), #feet->killometer
            cnt = ifelse(is.na(cnt),0,cnt)) 

zip_map$countperarea = df_temp$cntpa
zip_map$count = df_temp$cnt

# count(complaint) data add to ZIP-map
df_count <- tree311_df %>% 
  group_by(Incident.Zip) %>% 
  summarise(cnt = n())

df_temp = data.frame(Incident.Zip = as.numeric(as.character(zip_map$postalCode)), area = as.numeric(as.character(zip_map$Shape_Area)))
df_temp = df_temp %>% 
  group_by(Incident.Zip) %>% 
  mutate(area = sum(area))
df_temp = left_join(df_temp, df_count, by='Incident.Zip')
df_temp <- df_temp %>% 
  transmute(cntpa = ifelse(is.na(cnt),0,cnt/(area/1.076e7)), #feet->killometer
            cnt = ifelse(is.na(cnt),0,cnt)) 

zip_map$complaintperarea = df_temp$cntpa
zip_map$complaint = df_temp$cnt


## save datas to rds
saveRDS(tree_df, "./tree_data.rds")
#saveRDS(air_df, "./air_data.rds")
#saveRDS(air311_df, "./air_311_data.rds")
saveRDS(tree311_df, "./tree_311_data.rds")

#geojsonio::geojson_write(input = nta_map, file = "./NTAmap.geojson")
#geojsonio::geojson_write(input = zip_map, file = "./ZIPmap.geojson")
saveRDS(nta_map, "./nta_map.rds")
saveRDS(zip_map, "./zip_map.rds")




# colnames(as.data.frame(nta_map))
# as.data.frame(nta_map) %>% 
#   select(ntacode, countperarea, count) %>% 
#   arrange(desc(countperarea))
# 
# colnames(as.data.frame(zip_map))
# as.data.frame(zip_map) %>% 
#   select(postalCode, countperarea, count) %>% 
#   arrange(desc(countperarea))

tree311_df %>% 
  group_by(Incident.Zip) %>% 
  summarise(cnt = n()) %>% 
  arrange((cnt))

tree311_df %>% 
  group_by(Complaint.Type) %>% 
  summarise(cnt= n())


tree311_df %>% 
  group_by(Descriptor) %>% 
  summarise(cnt= n())

tree_df %>% 
  group_by(guards) %>% 
  summarise(cnt = n())
