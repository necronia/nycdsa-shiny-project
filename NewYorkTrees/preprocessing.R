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
boro_map <- geojsonio::geojson_read("json/BOROmap.geojson", what = "sp")

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

# count(tree) data add to BORO-map
df_count <- tree_df %>% 
  group_by(borough) %>% 
  summarise(cnt = n())

df_temp = data.frame(borough = boro_map$boro_name, area = as.numeric(as.character(boro_map$shape_area)))
df_temp = left_join(df_temp, df_count, by='borough')
df_temp <- df_temp %>% 
  transmute(cntpa = ifelse(is.na(cnt),0,cnt/(area/1.076e7)), #feet->killometer
            cnt = ifelse(is.na(cnt),0,cnt)) 

boro_map$countperarea = df_temp$cntpa
boro_map$count = df_temp$cnt

# count(complaint) data add to BORO-map
df_count_complaint <- tree311_df %>% 
  filter(Complaint.Type!='New Tree Request') %>% 
  group_by(Borough) %>% 
  summarise(comp_cnt = n())

df_count_tree <- tree_df %>% 
  group_by(borough) %>% 
  summarise(tree_cnt = n()) %>% 
  mutate(Borough = toupper(borough))

df_temp = data.frame(Borough = toupper(boro_map$boro_name), area = as.numeric(as.character(boro_map$shape_area)))
df_temp = left_join(df_temp, df_count_complaint, by='Borough')
df_temp = left_join(df_temp, df_count_tree, by='Borough')
df_temp <- df_temp %>% 
  transmute(cntpt = ifelse(is.na(comp_cnt),0,comp_cnt/tree_cnt), #feet->killometer
            cnt = ifelse(is.na(comp_cnt),0,comp_cnt)) 

boro_map$complaintpertree = df_temp$cntpt
boro_map$complaint = df_temp$cnt


## save datas to rds
saveRDS(tree_df, "./tree_data.rds")
#saveRDS(air_df, "./air_data.rds")
#saveRDS(air311_df, "./air_311_data.rds")
saveRDS(tree311_df, "./tree_311_data.rds")

#geojsonio::geojson_write(input = nta_map, file = "./NTAmap.geojson")
#geojsonio::geojson_write(input = zip_map, file = "./ZIPmap.geojson")
saveRDS(nta_map, "./nta_map.rds")
saveRDS(zip_map, "./zip_map.rds")
saveRDS(boro_map, "./boro_map.rds")




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


tt = tree311_df %>% 
  group_by(Complaint.Type,Descriptor) %>% 
  summarise(cnt= n())

tree_df %>% 
  filter(status=='Dead' & spc_common!='') %>% 
  group_by(spc_common, status) %>% 
  summarise(cnt = n())

temp_df <- tree_df %>% 
  # filter(spc_common %in% tree_df_type$spc_common) %>%
  group_by(spc_common) %>% 
  mutate(tot_cnt = n()) %>% 
  filter(health=='Good' & spc_common!='') %>% 
  group_by(spc_common) %>% 
  summarise(good_cnt = n(), tot_cnt = max(tot_cnt)) %>% 
  mutate(ratio = good_cnt/tot_cnt, 
         rank_ratio = dense_rank(desc(good_cnt/tot_cnt)), 
         rank_count = dense_rank(desc(tot_cnt))) %>% 
  arrange(desc(tot_cnt))

tree_df %>% 
  group_by(borough) %>% 
  summarise(m=mean(tree_dbh)) %>% 
  arrange((m))

tree311_df %>% 
  filter(Descriptor=='Dead/Dying Tree') %>% 
  group_by(Borough) %>% 
  summarise(cnt= n()) %>% 
  arrange(cnt)

tree_df %>% 
  group_by(borough) %>% 
  summarise(g=sum(health=='Good'), t = n()) %>% 
  mutate(a = g/t) %>% 
  arrange(a)

tree_df %>% 
  filter(spc_common %in% tree_df_type$spc_common) %>%
  group_by(borough) %>% 
  summarise(avg = mean(tree_dbh), sd = sd(tree_dbh), mx = max(tree_dbh)) %>% 
  arrange(desc(avg))

tree_df %>% 
  filter(spc_common == '') %>% 
  group_by(status) %>% 
  summarise(n())

tree_df %>% 
  filter(spc_common %in% tree_df_type$spc_common) %>%
  group_by(postcode, spc_common) %>% 
  summarise(n=n(), m=max(tree_dbh), a = mean(tree_dbh)) %>% 
  filter(n==max(n))


tree_df %>% 
  filter(status == 'Dead') %>% 
  group_by(postcode) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))


tree311_df %>% 
  filter(Descriptor=='Dead/Dying Tree') %>% 
  group_by(Borough) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))


tree <- tree_df %>% 
  transmute(borough = tolower(borough)) %>% 
  group_by(borough) %>% 
  mutate(tot_tree = n())
req <- tree311_df %>% 
  transmute(Borough = tolower(Borough)) %>% 
  group_by(Borough) %>% 
  summarise(tot_req = n()) %>% 
  mutate(borough = Borough)
temp = left_join(tree, req, by='borough')
temp %>% 
  group_by(borough, tot_tree, tot_req) %>%
  summarise(mx = max(tot_req/tot_tree)) %>% 
  arrange(desc(mx))
  

tree_df_rank <- tree_df %>% 
  filter(spc_common %in% tree_df_type$spc_common) %>%
  #filter(borough=='Queens') %>% 
  group_by(spc_common) %>% 
  mutate(tot_cnt = n()) %>% 
  filter(health!='Poor' & spc_common!='') %>% 
  group_by(spc_common) %>% 
  summarise(good_cnt = n(), tot_cnt = max(tot_cnt), m = mean(tree_dbh)) %>% 
  mutate(ratio = good_cnt/tot_cnt, 
         rank_ratio = dense_rank(desc(good_cnt/tot_cnt)), 
         rank_count = dense_rank(desc(tot_cnt)),
         rank_dbh = dense_rank(desc(m))) %>% 
  arrange(desc(tot_cnt))





tree_df_compare_complaint <- tree311_df %>%
  filter(Complaint.Type!='New Tree Request') %>% 
  group_by(Borough) %>% 
  summarise(comp_cnt = n()) %>% 
  mutate(rank_complaint_cnt = dense_rank(desc(comp_cnt))) %>% 
  arrange(comp_cnt)

tree_df_compare_request <- tree311_df %>%
  filter(Complaint.Type=='New Tree Request') %>% 
  group_by(Borough) %>% 
  summarise(requestnew__cnt = n()) %>% 
  mutate(rank_requestnew__cnt = dense_rank(desc(requestnew__cnt))) %>% 
  arrange(requestnew__cnt)

tree_df_compare_tree <- tree_df %>% 
  mutate(Borough = toupper(borough)) %>% 
  group_by(Borough) %>% 
  summarise(tree_cnt = n(), dbh_avg = mean(tree_dbh)) %>% 
  mutate(rank_tree_cnt = dense_rank(desc(tree_cnt)),
         rank_dbh_avg = dense_rank(desc(dbh_avg))) %>% 
  arrange(tree_cnt)

tree_df_dead_tree <- tree_df %>% 
  filter(status=='Dead') %>% 
  mutate(Borough = toupper(borough)) %>% 
  group_by(Borough) %>% 
  summarise(dead_cnt = n(), dead_dbh_avg = mean(tree_dbh)) %>% 
  mutate(rank_dead_cnt = dense_rank(desc(dead_cnt)),
         rank_dead_dbh_avg = dense_rank(desc(dead_dbh_avg))) %>% 
  arrange(dead_cnt)

tree_df_compare <- left_join(tree_df_compare_tree, tree_df_compare_complaint, by='Borough')
tree_df_compare <- left_join(tree_df_compare, tree_df_compare_request, by='Borough')
tree_df_compare <- left_join(tree_df_compare, tree_df_dead_tree, by='Borough')

tree_df_compare <- tree_df_compare %>% 
  mutate(dead_cnt_ratio = dead_cnt/tree_cnt)

temp = tree_df %>% 
  #filter(borough %in% c('Manhattan','Queens')) %>% 
  group_by(borough) %>% 
  summarise(s = sd(tree_dbh), mx = max(tree_dbh))


tree_df %>% 
  filter(spc_common=='London planetree') %>% 
  group_by(borough) %>% 
  summarise(n=n(), s=sd(tree_dbh), a=mean(tree_dbh))

tree_df %>% 
  filter(spc_common=='honeylocust') %>% 
  group_by(borough) %>% 
  summarise(n=n(), s=sd(tree_dbh), a=mean(tree_dbh))

tree_df %>% 
  filter(grepl('BranchLights',problems)) %>% 
  select(problems, tree_dbh) %>% 
  summarise(mean(tree_dbh))
         