
# Land Use Plan Simulation & Deforestation Predictions Histogram Visual Data

  

# Packages 
library(tidyverse)
library(ggthemr)
library(plotly)
library(flextable)


# tsosie
# # Read the CSV files
# datadir <- path.expand("~/../../capstone/pyforest")
# 
# sim5 <- read_csv(file.path(datadir, "ml_data/output/predictions-log-lut-areas/sim-5-log-lut-area-hectares.csv"))
# sim25 <- read_csv(file.path(datadir, "ml_data/output/predictions-log-lut-areas/sim-25-log-lut-area-hectares.csv"))
# sim50 <- read_csv(file.path(datadir, "ml_data/output/predictions-log-lut-areas/sim-50-log-lut-area-hectares.csv"))
# simhedges <- read_csv(file.path(datadir, "ml_data/output/predictions-log-lut-areas/sim-hedges-log-lut-area-hectares.csv"))

#local data
sim5 <- read_csv("data/sim-5-log-lut-area-hectares.csv")
sim25 <- read_csv("data/sim-25-log-lut-area-hectares.csv")
sim50 <- read_csv("data/sim-50-log-lut-area-hectares.csv")
simhedges <- read_csv("data/sim-hedges-log-lut-area-hectares.csv")



# Combine the data frames
prediction_data <- bind_rows(
  sim5 %>% mutate(simulation = "Law Ambiguity"),
  sim25 %>% mutate(simulation = "Current Forest Law"),
  sim50 %>% mutate(simulation = "Promotes Forest Conservation"),
  simhedges %>% mutate(simulation = "Prioritize Cattle Production")) 

prediction_data$LandUseType <- factor(prediction_data$LandUseType, levels = c("Paddocks", "Hedgerow", "Riparian Corridor","Forest Reserve"))


levels(prediction_data$simulation) <- c("Current Forest Law", "Promotes Forest Conservation","Prioritize Cattle Production","Law Ambiguity") 

#prediction_data


simulation_total_area <-  prediction_data |>
  group_by(simulation, LandUseType) |>
  summarize(total_area = sum(TotalArea))
simulation_total_area



# Aesthetics
color_mapping_deforestation <- c(
  "Forest Reserve NonDeforested" = "#4B5F43",  # Hunter Green
  "Hedgerow NonDeforested" = "#AEBD93",  # Sage
  "Riparian Corridor NonDeforested" = "#A7C7D8",  # Columbia blue
  "Paddocks NonDeforested" = "#F6AE2D",  # Orange (Pantone)
  "Forest Reserve Deforested" = "#2F4858",  # Darker Hunter Green
  "Hedgerow Deforested" = "#6E7B5A",  # Darker Sage
  "Riparian Corridor Deforested" = "#33658A",  # Darker Columbia blue
  "Paddocks Deforested" = "#F26419"  # Darker Orange
)



# Reshape the data
prediction_data_long <- prediction_data %>%
  pivot_longer(cols = c(DeforestedArea, NonDeforestedArea),
               names_to = "DeforestationStatus",
               values_to = "Area") %>%
  mutate(LandUseTypeStatus = interaction(LandUseType, DeforestationStatus, sep = " ")) |>
  select(LandUseType, simulation, Area, LandUseTypeStatus)

# Set the levels of the new factor variable in the desired order
levels(prediction_data_long$LandUseTypeStatus) <- c(
  "Paddocks Deforested", "Hedgerow Deforested", "Riparian Corridor Deforested", "Forest Reserve Deforested",
  "Paddocks NonDeforested", "Hedgerow NonDeforested", "Riparian Corridor NonDeforested", "Forest Reserve NonDeforested"
)

#prediction_data_long



color_mapping <- c(
  "Forest Reserve" = "#4B5F43",  # Hunter Green
  "Hedgerow" = "#AEBD93",  # Sage
  "Riparian Corridor" = "#A7C7D8",  # Columbia blue
  "Paddocks" = "#F26419"  # Hunyadi yellow
)





simulation_total_area$type <- "Simulation"
prediction_data_long$type <- "Prediction"
# Assign LandUseTypeStatus in simulation_total_area
simulation_total_area$LandUseTypeStatus <- ifelse(simulation_total_area$LandUseType == "Paddocks", 
                                                  "Paddocks Deforested", 
                                                  paste(simulation_total_area$LandUseType, "NonDeforested"))
simulation_total_area$Area <- simulation_total_area$total_area
combined_data <- bind_rows(simulation_total_area, prediction_data_long)

combined_data$simulation_type <- paste(combined_data$simulation, combined_data$type)



# Reorder the factor levels
combined_data$LandUseTypeStatus <- factor(combined_data$LandUseTypeStatus, levels = c(
  "Paddocks Deforested", "Hedgerow Deforested", "Riparian Corridor Deforested", "Forest Reserve Deforested",
  "Paddocks NonDeforested", "Hedgerow NonDeforested", "Riparian Corridor NonDeforested", "Forest Reserve NonDeforested"
))


# Create the basic plot 

# ggplot(combined_data, aes(x = simulation_type, y = Area, fill = LandUseTypeStatus)) +
#   geom_bar(stat = 'identity', position = 'stack', color = "black", linewidth = 0.25) +
#   scale_fill_manual(values = color_mapping_deforestation) +
#   labs(x = "Type", y = "Area", fill = "LandUseTypeStatus") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5),
#         plot.background = element_rect(fill = "transparent"),
#         legend.position = "top") +
#   coord_flip()  # Flip the coordinates






