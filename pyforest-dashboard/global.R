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

# NEW FILE PATHS ----

# Unauthorized deforestation
# combined_illegal_df_by_dist <- read_rds(here("data/combined_illegal_df_by_dist.rds"))
# combined_illegal_df_by_dist$normalized_value <- combined_illegal_df_by_dist$sum_df_ha / combined_illegal_df_by_dist$total_area_ha
# combined_illegal_df_by_dpto <- read_rds(here("data/combined_illegal_df_by_dpto.rds"))
# combined_illegal_df_by_dpto$normalized_value <- combined_illegal_df_by_dpto$sum_df_ha / combined_illegal_df_by_dpto$total_area_ha
# 
# Authorized deforestation
# combined_auth_df_by_dist <- read_rds(here("data/combined_auth_df_by_dist.rds"))
# combined_auth_df_by_dist$normalized_value <- combined_auth_df_by_dist$sum_df_ha / combined_auth_df_by_dist$total_area_ha
# combined_auth_df_by_dpto <- read_rds(here("data/combined_auth_df_by_dpto.rds"))
# combined_auth_df_by_dpto$normalized_value <- combined_auth_df_by_dpto$sum_df_ha / combined_auth_df_by_dpto$total_area_ha

# END -----

## ------------------------------------------ land use assessment sub tab ------------------------------------------
compliance <- st_read("~/../../capstone/pyforest/shinydata/lup_assessment/compliance_updated.gpkg")
compliance <- st_transform(compliance, 4326)

# NEW FILE PATHS FOR DATA -------------

#compliance <- st_read(here("data/compliance_updated.gpkg"))


# END FILE PATH FOR DATA -------------

## ------------------------------------------ deforestation and forest cover statistics data ------------------------------------------
datadir <- path.expand("~/../../capstone/pyforest")
py_fl_dept <- read_sf(file.path(datadir, "lup_assessment_data/fc_fl_analysis_results/department_forest_loss.gpkg"))
py_fl_dist <- read_sf(file.path(datadir, "lup_assessment_data/fc_fl_analysis_results/district_forest_loss.gpkg"))
chaco_fl <- read_sf(file.path(datadir, "lup_assessment_data/fc_fl_analysis_results/chaco_forest_loss.gpkg"))
chaco_fc <- read_sf(file.path(datadir, "lup_assessment_data/fc_fl_analysis_results/chaco_forest_cover.gpkg"))

# NEW FILE PATHS FOR DATA -------------

# py_fl_dept <- read_sf(here("data/department_forest_loss.gpkg"))
# py_fl_dist <- read_sf(here("data/district_forest_loss.gpkg"))
# chaco_fl <- read_sf(here("data/chaco_forest_loss.gpkg"))
# chaco_fc <- read_sf(here("data/chaco_forest_cover.gpkg"))

# END FILE PATH FOR DATA -------------

# Source in normalized deforestation data values 


source(here::here("pyforest-dashboard/R/forest_loss_standardizing_for_visuals.R"))
py_fc_dept <- read_sf(file.path(datadir, "lup_assessment_data/fc_fl_analysis_results/department_forest_cover.gpkg"))
py_fc_dist <- read_sf(file.path(datadir, "lup_assessment_data/fc_fl_analysis_results/district_forest_cover.gpkg"))

# NEW FILE PATHS FOR DATA -------------

# source(here::here("pyforest-dashboard/R/forest_loss_standardizing_for_visuals.R")) # STAYS SAME
# py_fc_dept <- read_sf(here("data/department_forest_cover.gpkg"))
# py_fc_dist <- read_sf(here("data/district_forest_cover.gpkg"))

# END FILE PATH FOR DATA -------------


## ------------------------------------------ land use simulation data ------------------------------------------
area_by_department_land_use <- read_rds("~/../../capstone/pyforest/shinydata/simulation/bar_plot_datasets/area_by_department_land_use.rds")
area_by_district_land_use <- read_rds("~/../../capstone/pyforest/shinydata/simulation/bar_plot_datasets/area_by_district_land_use.rds")
area_pct_by_department_land_use <- read_rds("~/../../capstone/pyforest/shinydata/simulation/bar_plot_datasets/area_pct_by_department_land_use.rds")
area_pct_by_district_land_use <- read_rds("~/../../capstone/pyforest/shinydata/simulation/bar_plot_datasets/area_pct_by_district_land_use.rds")

# NEW FILE PATHS FOR DATA -----

# DON'T NEED ANY MORE ---


# END FILE PATH FOR DATA -----


# FUNCTIONS?----
plot_land_use_type_stackedh <- function(dataset, name) {
  if (dataset == 'department') {
    data_to_plot <- area_by_department_land_use %>%
      filter(land_use_type != 'paddocks_area' & nom_dpto == name) %>%
      group_by(simulation, nom_dpto, land_use_type) %>%
      arrange(-total_area_lu) %>%
      mutate(simulation = factor(
        simulation,
        levels = c(
          "50% Forest Reserve",
          "25% Forest Reserve",
          "5% Forest Reserve",
          "Hedgerow incl. 25% Forest Reserve"
        )
      )) %>%
      ungroup()
    
    plot <-
      ggplot(data_to_plot,
             aes(x = total_area_lu, y = simulation, fill = land_use_type)) +
      geom_bar(
        stat = "identity",
        position = 'stack',
        orientation = 'y',
        color = "black",
        linewidth = 0.25
      ) +
      labs(
        title = paste("Total Area Conserved by Department and Simulation"),
        x = "Total Area",
        y = "Forest Law Simlulation"
      ) +
      facet_wrap(~ nom_dpto,
                 ncol = 1,
                 scales = "free_y",
                 dir = 'h')
    
  } else if (dataset == 'district') {
    data_to_plot <- area_by_district_land_use %>%
      filter(land_use_type != 'paddocks_area' & nom_dist == name) %>%
      group_by(simulation, nom_dist, land_use_type) %>%
      arrange(-total_area_lu) %>%
      mutate(simulation = factor(
        simulation,
        levels = c(
          "50% Forest Reserve",
          "25% Forest Reserve",
          "5% Forest Reserve",
          "Hedgerow incl. 25% Forest Reserve"
        )
      )) %>%
      ungroup()
    
    plot <- ggplot(data_to_plot, aes(x = total_area_lu, y = simulation, fill = land_use_type)) +
      geom_bar(
        stat = "identity",
        position = 'stack',
        orientation = 'y',
        color = "black",
        linewidth = 0.25
      ) +
      facet_wrap(~nom_dist, ncol = 5, scales = "free_x", dir = 'h') +
      labs(title = paste("Total Area Conserved by District and Simulation"),
           x = "Total Area",
           y = "Forest Law Simlulation") +
      theme(axis.text.x = element_blank())
    
    
  }}
  
  ## ------------------------------------------ Land Use Plan Simulation & Deforestation Prediction Data ------------------------------------------
# IN FILE UPDATED FILE PATHS ---------  
source(here::here("pyforest-dashboard/R/lup_sim_deforestation_prediction_histogram_data.R"))
  
  simulation_types <- unique(combined_data$simulation)
  
#   plotly::ggplotly(plot +
#                      theme(
#                        legend.position = "top",
#                        legend.direction = "horizontal",
#                        strip.background = element_blank(),
#                        strip.placement = "outside")) %>%
#     add_trace(showlegend = TRUE)
   

