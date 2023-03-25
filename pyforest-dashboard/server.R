# server.R
# Load packages and data from global.R file

server <- function(input, output, session) {
  
  
  # # Create a reactive variable for filtered compliance_fake
  # compliance_filtered_df <- eventReactive(input$district, {
  #   compliance_fake |>
  #     filter(department == input$department_compliance, district == input$district_compliance)
  # })
  # 
  # Update district choices based on selected department
  # observeEvent(input$department_compliance, {
  #   updateSelectInput(session, "district",
  #                     choices = unique(compliance_fake$district[compliance_fake$department == input$department]),
  #                     selected = NULL)
  # })
    
    
    # update selectInput choices for district based on department
    observeEvent(input$department_compliance, {
      updateSelectInput(session, "district_compliance", choices = unique(compliance_fake$district[compliance_fake$department == input$department_compliance]))
    })
    
    observeEvent(input$department_deforestation, {
      updateSelectInput(session, "district_deforestation", choices = unique(deforestation_fake$district[deforestation_fake$department == input$department_deforestation]))
    })  
    
  
  # # filter for column desired
  # compliance_filtered_df <- reactive({
  #   compliance_fake |>
  #     filter(department == input$department, district == input$district)
  # })
  
  
  #compliance plot
  compliance_colors <- c("yes" = "#4b5f43", "no" = "#999999")
  # Group by department and calculate the count of compliance
  df_by_dept <- compliance_fake |>
    group_by(department, compliance) |>
    summarize(count = n())
  
  output$compliance_output_plot <- renderPlot({
    ggplot(df_by_dept, aes(x = department, y = count, fill = compliance)) + 
      geom_bar(stat = "identity", position = "dodge") + 
      labs(x = "Department", y = "Number of Properties", title = "Compliance by Department",
           caption = "yes: land use plan was accurately executed/ authorized deforestation, no: land use plan was not accurately executed/unauthorized deforestation") +
      scale_fill_manual(values = compliance_colors) +
      theme_minimal()
    
  })
  # 
  # # Download button for plot
  # output$download_compliance_plot <- downloadHandler(
  #   filename = function() {
  #     paste("compliance_plot_", Sys.Date(), ".png", sep = "")
  #   },
  #   content = function(file) {
  #     ggsave(file, plot = output$compliance_output_plot())
  #   })
  
  # filter data table
  compliance_filtered_df <- reactive({
    compliance_fake |>
      filter(department == input$department_compliance, district == input$district_compliance)
  })

  # Generate table based on selected dept and dist
  output$compliance_dt_output <- renderDataTable({
    compliance_filtered_df()
  })

  # # Create a download button for table
  # output$downloadTable <- downloadHandler(
  #   filename = function() {
  #     paste("compliance_data_", Sys.Date(), ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(compliance_filtered_df(), file)
  #   })

# 
  # filter for column desired
   deforestation_filtered_df <- reactive({
     deforestation_fake |>
       filter(department == input$department_deforestation, district == input$district_deforestation)
   })

# Deforestation plot
output$deforestation_output_plot <- renderPlot({
  ggplot(deforestation_filtered_df(), aes(x = year, y = deforestation, color = district)) +
    geom_line() +
    facet_wrap(~department, scales = "free_y") +
    labs(x = "Year", y = "Deforestation (hectares loss)", title = "Deforestation by Department from 2002 to 2020") +
    theme_minimal()
  
  })
  
  # # Download button for deforestation plot
  # output$download_deforestation_plot <- downloadHandler(
  #   filename = function() {
  #     paste("deforestation_plot_", Sys.Date(), ".png", sep = "")
  #   },
  #   content = function(file) {
  #     ggsave(file, plot = output$deforestation_output_plot())
  #   })
  #
  
  
  # Deforestation tmap output
  output$map_output <- renderTmap({
    tmap_mode("view")
    tm_shape(fl_00_05) +
      tm_fill(col = "darkgreen")
    
  })
  
  }





