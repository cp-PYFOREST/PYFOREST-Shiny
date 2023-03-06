## Political Boundaries Dataset
#reading in department and district shapefiles
department <- st_read("/capstone/pyforest/data/Political_Boundaries/departamento.shp")

district <- st_read("/capstone/pyforest/data/Political_Boundaries/distritos.shp") |>
  rename(dpto = cod_dpto)

#creating a new dataset by joining the districts and departments
political_boundaries <- st_join(district, department, by = "dpt")

#saving new dataset shapefiles in data folder
st_write(political_boundaries, "/capstone/pyforest/pyforest-shiny/data/political_boundaries_dpt_dist.shp")

##

