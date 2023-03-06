compliance <- readRDS("/capstone/pyforest/data/active_inactive/active_inactive.rds") 

political_boundaries_updated <- political_boundaries |>
  rename(cod_dpto = dpto_y)

compliance1 <- st_join(compliance, political_boundaries_updated, by = c("cod_dist", "cod_dpto"), left = FALSE) |>
  select(c(id, put_id, anho_capa, cod_dpto.x, cod_dist.x, nom_dpto, nom_dist, year_inactive)) |>
  st_drop_geometry()

write_rds(compliance1,"/capstone/pyforest/pyforest-shiny/data/compliance1.rds")

#visualization

compliance_output_plot <- ggplot(compliance1, aes(x = nom_dpto, y = year_inactive)) +
  geom_col(color = "#4b5f43") +
  labs(x = "Department Name", y = "Count", title = "Number of Compliant Properties per Year and Department") +
  theme_minimal()

compliance_output_plot



# write.csv(compliance, "/capstone/pyforest/pyforest-shiny/data/compliance.csv")
