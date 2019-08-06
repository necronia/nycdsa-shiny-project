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
tree311_df <- readRDS('./tree_311_data.rds')

nta_map <- readRDS('./nta_map.rds')
zip_map <- readRDS('./zip_map.rds')
boro_map <- readRDS('./boro_map.rds')

choice_dist <- c('Tree','Complaint','All')
choice_nta <- colnames(as.data.frame(nta_map))[9:8]
choice_zip <- colnames(as.data.frame(zip_map))[13:12]
choice_zip_meta <- c('Tree only','Complaint only','All')
choice_boro <- colnames(as.data.frame(boro_map))[c(6,8,5,7)]
choice_rank <- c('All','Top20')
choice_tree <- c('All','Manhattan','Bronx','Staten Island','Brooklyn','Queens')
choice_compl <- c('All','MANHATTAN','BRONX','STATEN ISLAND','BROOKLYN','QUEENS')
choice_diam <- c('curb_loc','status','health','spc_common','steward','guards','sidewalk','problems','root_stone','root_grate','root_other','trunk_wire','trnk_light','trnk_other','brch_light','brch_shoe','brch_other','zip_city','borough')
choice_column <- c('curb_loc','status','health','spc_common','steward','guards','sidewalk','problems','root_stone','root_grate','root_other','trunk_wire','trnk_light','trnk_other','brch_light','brch_shoe','brch_other','zip_city','borough')
choice_compair_1 <- c('comp_cnt','tree_cnt','dbh_avg','request_new_cnt','dead_cnt','dead_dbh_avg','dead_cnt_ratio','rank_tree_cnt','rank_dbh_avg','rank_complaint_cnt','rank_request_new_cnt','rank_dead_cnt','rank_dead_dbh_avg')
choice_compair_2 <- c('tree_cnt','comp_cnt','dbh_avg','request_new_cnt','dead_cnt','dead_dbh_avg','dead_cnt_ratio','rank_tree_cnt','rank_dbh_avg','rank_complaint_cnt','rank_request_new_cnt','rank_dead_cnt','rank_dead_dbh_avg')

nta_data <- as.data.frame(nta_map)

tree_df_type <- tree_df %>% 
  group_by(spc_common, borough) %>% 
  summarise(cnt = n()) %>% 
  filter(!is.na(spc_common)) 

tree_df_status <- tree_df %>% 
  group_by(status, borough) %>% 
  summarise(cnt = n()) %>% 
  filter(!is.na(status)) 

tree_df_health <- tree_df %>% 
  group_by(health, borough) %>% 
  summarise(cnt = n()) %>% 
  filter(!is.na(health) & health!='')

tree_df_dbh <- tree_df %>% 
  mutate(tree_dbh = ifelse(tree_dbh>50,50,tree_dbh)) %>% 
  group_by(tree_dbh, borough) %>% 
  summarise(cnt = n()) %>% 
  filter(!is.na(tree_dbh) & tree_dbh>0)

tree311_df_type <- tree311_df %>% 
  group_by(Complaint.Type, Borough) %>% 
  summarise(cnt = n()) 

tree311_df_desc <- tree311_df %>% 
  group_by(Descriptor, Borough) %>% 
  summarise(cnt = n()) 

tree_df_rank <- tree_df %>% 
  group_by(spc_common) %>% 
  mutate(tot_cnt = n()) %>% 
  filter(health=='Good' & spc_common!='') %>% 
  group_by(spc_common) %>% 
  summarise(good_cnt = n(), tot_cnt = max(tot_cnt)) %>% 
  mutate(ratio = good_cnt/tot_cnt, 
         rank_ratio = dense_rank(desc(good_cnt/tot_cnt)), 
         rank_count = dense_rank(desc(tot_cnt))) %>% 
  arrange(desc(tot_cnt))

tree_top_20 <- tree_df %>% 
  filter(spc_common != '') %>% 
  group_by(spc_common) %>% 
  summarise(n()) %>% 
  top_n(20)

tree_df_rank_20 <- tree_df %>% 
  filter(spc_common %in% tree_top_20$spc_common) %>%
  group_by(spc_common) %>% 
  mutate(tot_cnt = n()) %>% 
  filter(health=='Good' & spc_common!='') %>% 
  group_by(spc_common) %>% 
  summarise(good_cnt = n(), tot_cnt = max(tot_cnt)) %>% 
  mutate(ratio = good_cnt/tot_cnt, 
         rank_ratio = dense_rank(desc(good_cnt/tot_cnt)), 
         rank_count = dense_rank(desc(tot_cnt))) %>% 
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
  summarise(request_new_cnt = n()) %>% 
  mutate(rank_request_new_cnt = dense_rank(desc(request_new_cnt))) %>% 
  arrange(request_new_cnt)

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

# for adjust abnormal value
nta_map_view <- nta_map
nta_map_view$countperarea = ifelse(nta_map$countperarea > 2000, 2000, nta_map$countperarea)
zip_map_view <- zip_map 
zip_map_view$countperarea = ifelse(zip_map$countperarea > 2500, 2500, zip_map$countperarea)