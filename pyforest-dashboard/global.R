#global.R

# LOAD PACKAGES ----
# Data wrangling and analysis
library(tidyverse)
library(sf)
library(raster)
library(terra)
library(exactextractr)
library(units)
library(forcats)
library(here)
library(readr)

# Data visualization
library(ggplot2)
library(plotly)
library(tmap)
library(leaflet)
library(kableExtra)
library(ggiraph)
library(ggiraphExtra)
library(RColorBrewer)
library(htmltools)
library(maps)
library(maptools)
library(sp)
library(ggthemr)
library(flextable)
library(leafdown)
library(leaflet.extras)

# Interactive web apps
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
library(markdown)
library(rsconnect)
library(fresh)
library(DT)
library(shinythemes)

# READ IN DATA ----

#make sure you change directory to pyforest-dashboard

## ------------------------------------------land use plan assessment data ------------------------------------------
# Unauthorized deforestation
combined_illegal_df_by_dist <- st_read("data/combined_illegal_df_by_dist.gpkg")
combined_illegal_df_by_dist$normalized_value <- combined_illegal_df_by_dist$sum_df_ha / combined_illegal_df_by_dist$total_area_ha
combined_illegal_df_by_dist$year_range <- gsub("_", "-", combined_illegal_df_by_dist$year_range)


combined_illegal_df_by_dpto <- st_read("data/combined_illegal_df_by_dpto.gpkg")
combined_illegal_df_by_dpto$normalized_value <- combined_illegal_df_by_dpto$sum_df_ha / combined_illegal_df_by_dpto$total_area_ha
combined_illegal_df_by_dpto$year_range <- gsub("_", "-", combined_illegal_df_by_dpto$year_range)

# Authorized deforestation
combined_auth_df_by_dist <- st_read("data/combined_auth_df_by_dist.gpkg")
combined_auth_df_by_dist$normalized_value <- combined_auth_df_by_dist$sum_df_ha / combined_auth_df_by_dist$total_area_ha
combined_auth_df_by_dist$year_range <- gsub("_", "-", combined_auth_df_by_dist$year_range)

combined_auth_df_by_dpto <- st_read("data/combined_auth_df_by_dpto.gpkg")
combined_auth_df_by_dpto$normalized_value <- combined_auth_df_by_dpto$sum_df_ha / combined_auth_df_by_dpto$total_area_ha
combined_auth_df_by_dpto$year_range <- gsub("_", "-", combined_auth_df_by_dpto$year_range)

## ------------------------------------------ land use assessment sub tab ------------------------------------------
compliance <- st_read("data/compliance_updated.gpkg")
compliance <- st_transform(compliance, 4326)

## ------------------------------------------ deforestation and forest cover statistics data ------------------------------------------
py_fl_dept <- read_sf("data/department_forest_loss.gpkg")
py_fl_dist <- read_sf("data/district_forest_loss.gpkg")
chaco_fl <- read_sf("data/chaco_forest_loss.gpkg")
chaco_fc <- read_sf("data/chaco_forest_cover.gpkg")


source("R/forest_loss_standardizing_for_visuals.R") 
py_fc_dept <- read_sf("data/department_forest_cover.gpkg")
py_fc_dist <- read_sf("data/district_forest_cover.gpkg")

## ------------------------------------------ Land Use Plan Simulation & Deforestation Prediction Data ------------------------------------------
source("R/lup_sim_deforestation_prediction_histogram_data.R")
  
simulation_types <- unique(combined_data$simulation)

   

