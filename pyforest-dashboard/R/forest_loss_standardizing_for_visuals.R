
## Normalizing FOREST LOSS DATA 



#### Department
fl_dept <- py_fl_dept 

# 2000 - 2005 
years <- 2000:2004
fl_dpt_list_1 <- lapply(years, function(year) {
  fl_dept %>%
    filter(year_range == "2000_2005") %>%
    mutate(year = as.character(year),
           fl_area_ha_std = fl_area_ha / 5,
           percent_fl_std = percent_forest_loss / 5)
})

combined_df_1 <- bind_rows(fl_dpt_list_1)


# 2005 - 2011
years <- 2005:2010
fl_dpt_list_2 <- lapply(years, function(year) {
  fl_dept %>%
    filter(year_range == "2005_2011") %>%
    mutate(year = as.character(year),
           fl_area_ha_std = fl_area_ha / 6,
           percent_fl_std = percent_forest_loss / 6)
})

combined_df_2 <- bind_rows(fl_dpt_list_2)



#2011 - 2013
years <- 2011:2012
fl_dpt_list_3 <- lapply(years, function(year) {
  fl_dept %>%
    filter(year_range == "2011_2013") %>%
    mutate(year = as.character(year),
           fl_area_ha_std = fl_area_ha / 2,
           percent_fl_std = percent_forest_loss / 2)
})

combined_df_3 <- bind_rows(fl_dpt_list_3)



#2013-2015 
years <- 2013:2014

fl_dpt_list_4 <- lapply(years, function(year) {
  fl_dept %>%
    filter(year_range == "2013_2015") %>%
    mutate(year = as.character(year),
           fl_area_ha_std = fl_area_ha / 2,
           percent_fl_std = percent_forest_loss / 2)
})

combined_df_4 <- bind_rows(fl_dpt_list_4)

#2015-2017
years <- 2015:2016

fl_dpt_list_5 <- lapply(years, function(year) {
  fl_dept %>%
    filter(year_range == "2015_2017") %>%
    mutate(year = as.character(year),
           fl_area_ha_std = fl_area_ha / 2,
           percent_fl_std = percent_forest_loss / 2)
})

combined_df_5 <- bind_rows(fl_dpt_list_5)

#2017-2018
years <- 2017

fl_dpt_list_6 <- lapply(years, function(year) {
  fl_dept %>%
    filter(year_range == "2017_2018") %>%
    mutate(year = as.character(year),
           fl_area_ha_std = fl_area_ha / 1,
           percent_fl_std = percent_forest_loss / 1)
})

combined_df_6 <- bind_rows(fl_dpt_list_6)

#2018-2019
years <- 2018

fl_dpt_list_7 <- lapply(years, function(year) {
  fl_dept %>%
    filter(year_range == "2018_2019") %>%
    mutate(year = as.character(year),
           fl_area_ha_std = fl_area_ha / 1,
           percent_fl_std = percent_forest_loss / 1)
})

combined_df_7 <- bind_rows(fl_dpt_list_7)


#2019-2020
years <- 2019:2020
fl_dpt_list_8 <- lapply(years, function(year) {
  fl_dept %>%
    filter(year_range == "2019_2020") %>%
    mutate(year = as.character(year),
           fl_area_ha_std = fl_area_ha / 2,
           percent_fl_std = percent_forest_loss / 2)
})

combined_df_8 <- bind_rows(fl_dpt_list_8)



forest_loss_std_df <- 
  bind_rows(combined_df_1,
            combined_df_2,
            combined_df_3,
            combined_df_4,
            combined_df_5,
            combined_df_6,
            combined_df_7,
            combined_df_8)



forest_loss_std_df <- forest_loss_std_df %>% 
  mutate(fl_area_ha_std = round(fl_area_ha_std,2),
         percent_fl_std = round(percent_fl_std,2))


#### DISTRICT 



fl_dist <- py_fl_dist 

# 2000 - 2005 
years <- 2000:2004
fl_dist_list_1 <- lapply(years, function(year) {
  fl_dist %>%
    filter(year_range == "2000_2005") %>%
    mutate(year = as.character(year),
           fl_area_ha_std = fl_area_ha / 5,
           percent_fl_std = percent_forest_loss / 5)
})

district_combined_df_1 <- bind_rows(fl_dist_list_1)


# 2005 - 2011
years <- 2005:2010
fl_dist_list_2 <- lapply(years, function(year) {
  fl_dist %>%
    filter(year_range == "2005_2011") %>%
    mutate(year = as.character(year),
           fl_area_ha_std = fl_area_ha / 6,
           percent_fl_std = percent_forest_loss / 6)
})

district_combined_df_2 <- bind_rows(fl_dist_list_2)



#2011 - 2013
years <- 2011:2012
fl_dist_list_3 <- lapply(years, function(year) {
  fl_dist %>%
    filter(year_range == "2011_2013") %>%
    mutate(year = as.character(year),
           fl_area_ha_std = fl_area_ha / 2,
           percent_fl_std = percent_forest_loss / 2)
})

district_combined_df_3 <- bind_rows(fl_dist_list_3)



#2013-2015 
years <- 2013:2014

fl_dist_list_4 <- lapply(years, function(year) {
  fl_dist %>%
    filter(year_range == "2013_2015") %>%
    mutate(year = as.character(year),
           fl_area_ha_std = fl_area_ha / 2,
           percent_fl_std = percent_forest_loss / 2)
})

district_combined_df_4 <- bind_rows(fl_dist_list_4)

#2015-2017
years <- 2015:2016

fl_dist_list_5 <- lapply(years, function(year) {
  fl_dist %>%
    filter(year_range == "2015_2017") %>%
    mutate(year = as.character(year),
           fl_area_ha_std = fl_area_ha / 2,
           percent_fl_std = percent_forest_loss / 2)
})

district_combined_df_5 <- bind_rows(fl_dist_list_5)

#2017-2018
years <- 2017

fl_dist_list_6 <- lapply(years, function(year) {
  fl_dist %>%
    filter(year_range == "2017_2018") %>%
    mutate(year = as.character(year),
           fl_area_ha_std = fl_area_ha / 1,
           percent_fl_std = percent_forest_loss / 1)
})

district_combined_df_6 <- bind_rows(fl_dist_list_6)

#2018-2019
years <- 2018

fl_dist_list_7 <- lapply(years, function(year) {
  fl_dist %>%
    filter(year_range == "2018_2019") %>%
    mutate(year = as.character(year),
           fl_area_ha_std = fl_area_ha / 1,
           percent_fl_std = percent_forest_loss / 1)
})

district_combined_df_7 <- bind_rows(fl_dist_list_7)


#2019-2020
years <- 2019:2020
fl_dist_list_8 <- lapply(years, function(year) {
  fl_dist %>%
    filter(year_range == "2019_2020") %>%
    mutate(year = as.character(year),
           fl_area_ha_std = fl_area_ha / 2,
           percent_fl_std = percent_forest_loss / 2)
})

district_combined_df_8 <- bind_rows(fl_dist_list_8)



forest_loss_district_std_df <- 
  bind_rows(district_combined_df_1,
            district_combined_df_2,
            district_combined_df_3,
            district_combined_df_4,
            district_combined_df_5,
            district_combined_df_6,
            district_combined_df_7,
            district_combined_df_8)


forest_loss_district_std_df <- forest_loss_district_std_df %>% 
  mutate(fl_area_ha_std = round(fl_area_ha_std,2),
         percent_fl_std = round(percent_fl_std,2))

