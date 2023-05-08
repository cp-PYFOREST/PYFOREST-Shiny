library(sf)
library(tmap)
fl_00_05 <- st_read("/Users/alexreed/Documents/MEDS/Capstone/pyforest-shiny/pyforest-dashboard/data/00_05_forest_loss/00_05_forest_loss.shp")


# Maps --------------------
#deforestation
  tmap_mode("view")
  tm_shape(fl_00_05) +
  tm_fill(col = "darkgreen") 
