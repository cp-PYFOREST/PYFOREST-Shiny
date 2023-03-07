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
  
  # # filter map data
  # fl_00_05_filtered <- reactive({
  #   input_fid <- input$fid_input
  #   fl_00_05 %>%
  #     filter(FID == input_fid)
  # })
  
  
  output$map_output <- renderTmap({
    tmap_mode("view")
    tm_shape(fl_00_05) +
      tm_fill(col = "darkgreen") 
    
  })
  
}



