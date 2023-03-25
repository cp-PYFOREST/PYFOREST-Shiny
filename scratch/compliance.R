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

compliance_fake <- read_excel("/Users/alexreed/Documents/MEDS/Capstone/fake_data/compliance.xlsx") |>
  dplyr::select(-...1)



# write.csv(compliance, "/capstone/pyforest/pyforest-shiny/data/compliance_fake.csv")


# Plot the compliance by department
ggplot(compliance_fake, aes(x = department, fill = compliance)) + 
  geom_bar() + 
  ggtitle("Compliance by Department")

# Plot the compliance by district
ggplot(compliance_fake, aes(x = district, fill = compliance)) + 
  geom_bar() + 
  ggtitle("Compliance by District")



# Group by department and calculate the count of compliance
df_by_dept <- compliance_fake |>
  group_by(department, compliance) |>
  summarize(count = n())
df_by_dept

# Define the colors
compliance_colors <- c("yes" = "#4b5f43", "no" = "#999999")

# Plot the compliance by department
ggplot(df_by_dept, aes(x = department, y = count, fill = compliance)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "Department", y = "Number of Properties", title = "Compliance by Department") +
  scale_fill_manual(values = compliance_colors) +
  theme_minimal()

# Create a table
kable(df_by_dept, "html") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)





compliance_filtered_df <- reactive({
  compliance_fake |>
    filter(department == input$department_compliance, district == input$district_compliance)
})

