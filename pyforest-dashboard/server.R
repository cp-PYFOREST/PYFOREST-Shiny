#server.R
# Load packages and data from global.R file

server <- function(input, output, session) {
  
  
  # Read in the data
 # political_boundaries <- st_read("data/political_boundaries_dpt_dist.shp")
  
  # Create a reactive variable for filtered political boundaries
  filtered_boundaries <- eventReactive(input$nom_dist, {
    political_boundaries |>
      filter(nom_dpto == input$nom_dpto, nom_dist == input$nom_dist)
  })
  
  # Update district choices based on selected department
  observeEvent(input$nom_dpto, {
    updateSelectInput(session, "nom_dist", 
                      choices = unique(political_boundaries$nom_dist[political_boundaries$nom_dpto == input$nom_dpto]))
  })
  
  # filter for column desired
  dt_filtered_df <- reactive({
    compliance1 |>
      filter(nom_dpto == input$nom_dpto, nom_dist == input$nom_dist)
  })


  #compliance plot
  output$compliance_output_plot <- renderPlot({
    ggplot(compliance1, aes(x = nom_dpto, y = year_inactive)) +
      geom_col(color = "#4b5f43") +
      labs(x = "Department Name", y = "Count", title = "Number of Compliant Properties per Year and Department") +
      theme_minimal()
  })
  
  
  # filter data table
  dt_filtered_df <- reactive({
    compliance1 |>
      filter(nom_dpto == input$nom_dpto, nom_dist == input$nom_dist)
  })
  
  # Generate table based on selected dept and dist
  output$dt_output <- renderDataTable({
    dt_filtered_df()
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
