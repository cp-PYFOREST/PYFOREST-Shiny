deforestation_fake <- read_excel("/Users/alexreed/Documents/MEDS/Capstone/fake_data/deforestation_fake.xlsx") 

# Summary statistics by department
df_dept <- aggregate(deforestation ~ department, deforestation_fake, sum)

df_dept
# Bar plot by department
ggplot(df_dept, aes(x = department, y = deforestation)) +
  geom_bar(stat = "identity") +
  xlab("Department") +
  ylab("Deforestation (hectares)") +
  ggtitle("Total Deforestation by Department")

# Summary statistics by district
df_dist <- aggregate(deforestation ~ department + district, df, sum)

# Stacked bar plot by district within department
ggplot(df_dist, aes(x = district, y = deforestation, fill = department)) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("District") +
  ylab("Deforestation (hectares)") +
  ggtitle("Total Deforestation by District within Department")


ggplot(deforestation_fake, aes(x = year, y = deforestation, group = interaction(department, district), color = department)) +
  geom_line() +
  facet_wrap(~district, nrow = 2) +
  labs(x = "Year", y = "Deforestation (hectares)", color = "Department") +
  theme_bw()


ggplot(deforestation_fake, aes(x = year, y = deforestation, color = district)) +
    geom_line() +
    facet_wrap(~department, scales = "free_y") +
  labs(x = "Year", y = "Deforestation (hectares loss)", title = "Deforestation by Department from 2002 to 2020") +
  theme_minimal()








ggplot(compliance_fake, aes(x = department, y = count, fill = compliance)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "Department", y = "Number of Properties", title = "Compliance by Department",
       caption = "yes: land use plan was accurately executed/ authorized deforestation, no: land use plan was not accurately executed/unauthorized deforestation") +
  scale_fill_manual(values = compliance_colors) +
  theme_minimal()



check <- deforestation_fake |>
  filter(department == department, district == district)


