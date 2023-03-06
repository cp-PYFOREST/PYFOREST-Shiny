# Load packages and data from global.R file

server <- function(input, output, session) {
  
  
  # Read in the data
  political_boundaries <- st_read("data/political_boundaries_dpt_dist.shp")
  
  # Create a reactive variable for filtered political boundaries
  filtered_boundaries <- eventReactive(input$nom_dist, {
    political_boundaries %>%
      filter(nom_dpto == input$nom_dpto, nom_dist == input$nom_dist)
  })
  
  # Update district choices based on selected department
  observeEvent(input$nom_dpto, {
    updateSelectInput(session, "nom_dist", 
                      choices = unique(political_boundaries$nom_dist[political_boundaries$nom_dpto == input$nom_dpto]))
  })
  
  # Generate table based on selected state and city
  output$table <- renderTable({
    filtered_boundaries() %>% filter(nom_dpto == input$nom_dpto, nom_dist == input$nom_dist)
  })
  
}





# # Create a reactive variable for filtered political boundaries
# political_boundaries <- eventReactive(input$nom_dist, {
#   political_boundaries %>%
#     filter(nom_dpto == input$nom_dpto, nom_dist == input$nom_dist)
# })
# 
# # Update district choices based on selected department
# observeEvent(input$nom_dpto, {
#   updateSelectInput(session, "nom_dist", 
#                     choices = unique(political_boundaries$nom_dist[political_boundaries$nom_dpto == input$nom_dpto]))
# })
# 
# # Generate table based on selected state and city
#  output$table <- renderTable({
#    political_boundaries %>% filter(nom_dpto == input$nom_dpto, nom_dist == input$nom_dist)
#  })}

# filter benzene data ----
# ladpw_filtered <- reactive({
#
#   ladpw_data |>
#     filter(Benzene >= input$benzene_slider_input[1] & Benzene <= input$benzene_slider_input[2])
#
# })

# # build leaflet map ----
# output$benzene_map <- renderLeaflet({
#
#   # leaflet map
#   leaflet() |>
#     addProviderTiles("Esri.WorldImagery") |>
#     setView(lng = -118.17, lat = 34.056, zoom = 16) |>
#     addMiniMap(toggleDisplay = TRUE, minimized = TRUE) |> # view to Alaska
#     addMarkers(data = ladpw_filtered(),
#                lng = ladpw_filtered()$longitude,
#                lat = ladpw_filtered()$latitude,
#                popup = paste("Sample ID:", ladpw_filtered()$Sample_ID, "<br>",
#                              "Benzene:", ladpw_filtered()$Benzene, "μg/m³", "<br>"))

#})

#   ladpw_filtered2 <- reactive({
#
#     ladpw_data |>
#       filter(Benzene >= input$benzene_slider_input2[1] & Benzene <= input$benzene_slider_input2[2])
#
#   })
#
# benzene plot
# output$benzene_plot <- renderPlot({
#
#   ggplot(ladpw_filtered2(), aes(x = Benzene, y = reorder(Sample_ID, desc(Sample_ID)))) +
#     geom_bar(stat = 'identity', fill = "#295673") +
#     labs(title = "Sample ID vs. Benzene Concentrations",
#          x = "Benzene",
#          y = "Sample ID")
#
#   })
