#global.R

# LOAD PACKAGES ----
library(shiny)
library(shinydashboard)
library(shinydashboardPlus) 
library(tidyverse)
library(shinycssloaders)
library(leaflet)
library(markdown)
library(readxl)
library(tidygeocoder)
library(shinyWidgets)
library(sf)
library(shiny.i18n) #language 
library(fresh)
library(rsconnect)
<<<<<<< HEAD
=======
library(tmap)
library(raster)

>>>>>>> 514c9535b8855abe9a435e21d9cc7f4c6c33d9f5

# read in data ----

political_boundaries <- st_read("data/political_boundaries_dpt_dist.shp")
compliance1 <- readRDS("data/compliance1.rds")
fl_00_05 <- st_read("data/00_05_forest_loss.shp")



