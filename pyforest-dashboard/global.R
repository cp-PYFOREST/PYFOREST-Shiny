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

## ------------------------------------------land use assessment data ------------------------------------------
# Unauthorized deforestation
combined_illegal_df_by_dist <- read_rds("~/../../capstone/pyforest/lup_assessment_data/compliance_results/combined_illegal_df_by_dist.rds")
combined_illegal_df_by_dist$normalized_value <- combined_illegal_df_by_dist$sum_df_ha / combined_illegal_df_by_dist$total_area_ha
combined_illegal_df_by_dpto <- read_rds("~/../../capstone/pyforest/lup_assessment_data/compliance_results/combined_illegal_df_by_dpto.rds")
combined_illegal_df_by_dpto$normalized_value <- combined_illegal_df_by_dpto$sum_df_ha / combined_illegal_df_by_dpto$total_area_ha

# Authorized deforestation
combined_auth_df_by_dist <- read_rds("~/../../capstone/pyforest/lup_assessment_data/compliance_results/combined_auth_df_by_dist.rds")
combined_auth_df_by_dist$normalized_value <- combined_auth_df_by_dist$sum_df_ha / combined_auth_df_by_dist$total_area_ha
combined_auth_df_by_dpto <- read_rds("~/../../capstone/pyforest/lup_assessment_data/compliance_results/combined_auth_df_by_dpto.rds")
combined_auth_df_by_dpto$normalized_value <- combined_auth_df_by_dpto$sum_df_ha / combined_auth_df_by_dpto$total_area_ha

## ------------------------------------------ land use assessment sub tab ------------------------------------------
compliance <- st_read("~/../../capstone/pyforest/shinydata/lup_assessment/compliance_updated.gpkg")
compliance <- st_transform(compliance, 4326)

## ------------------------------------------ deforestation and forest cover statistics data ------------------------------------------
datadir <- path.expand("~/../../capstone/pyforest")
py_fl_dept <- read_sf(file.path(datadir, "lup_assessment_data/fc_fl_analysis_results/department_forest_loss.gpkg"))
py_fl_dist <- read_sf(file.path(datadir, "lup_assessment_data/fc_fl_analysis_results/district_forest_loss.gpkg"))
chaco_fl <- read_sf(file.path(datadir, "lup_assessment_data/fc_fl_analysis_results/chaco_forest_loss.gpkg"))
chaco_fc <- read_sf(file.path(datadir, "lup_assessment_data/fc_fl_analysis_results/chaco_forest_cover.gpkg"))

# Source in normalized deforestation data values 
source(here::here("pyforest-dashboard/R/forest_loss_standardizing_for_visuals.R"))
py_fc_dept <- read_sf(file.path(datadir, "lup_assessment_data/fc_fl_analysis_results/department_forest_cover.gpkg"))
py_fc_dist <- read_sf(file.path(datadir, "lup_assessment_data/fc_fl_analysis_results/district_forest_cover.gpkg"))

## ------------------------------------------ land use simulation data ------------------------------------------
area_by_department_land_use <- read_rds("~/../../capstone/pyforest/shinydata/simulation/bar_plot_datasets/area_by_department_land_use.rds")
area_by_district_land_use <- read_rds("~/../../capstone/pyforest/shinydata/simulation/bar_plot_datasets/area_by_district_land_use.rds")
area_pct_by_department_land_use <- read_rds("~/../../capstone/pyforest/shinydata/simulation/bar_plot_datasets/area_pct_by_department_land_use.rds")
area_pct_by_district_land_use <- read_rds("~/../../capstone/pyforest/shinydata/simulation/bar_plot_datasets/area_pct_by_district_land_use.rds")
  
## ------------------------------------------ Land Use Plan Simulation & Deforestation Prediction Data ------------------------------------------
source(here::here("pyforest-dashboard/R/lup_sim_deforestation_prediction_histogram_data.R"))
  
simulation_types <- unique(combined_data$simulation)
  
#   plotly::ggplotly(plot +
#                      theme(
#                        legend.position = "top",
#                        legend.direction = "horizontal",
#                        strip.background = element_blank(),
#                        strip.placement = "outside")) %>%
#     add_trace(showlegend = TRUE)
   

