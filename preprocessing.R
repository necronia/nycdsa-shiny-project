library(dplyr)

air_data = read.csv('./air_data.csv')
tree_data = read.csv('./tree_data.csv')

df = tree_data %>% 
  filter(!is.na(spc_common))
