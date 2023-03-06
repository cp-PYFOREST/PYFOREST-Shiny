# LOAD PACKAGES ----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinycssloaders)
library(leaflet)
library(markdown)
library(readxl)
library(tidygeocoder)
library(shinyWidgets)
library(sf)
library(shiny.i18n)


# read in data ----

political_boundaries <- st_read("data/political_boundaries_dpt_dist.shp")
