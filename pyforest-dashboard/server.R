# server.R
# Load packages and data from global.R file

server <- function(input, output, session) {
  

  pyforest_palette <- c("#4B5F43", "#AEBD93", "#F6AE2D", "#F26419")  
  
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
    
    
    # # update selectInput choices for district based on department
    # observeEvent(input$department_compliance, {
    #   updateSelectInput(session, "nom_dist", choices = unique(pb_bosques_illegal_df$nom_dist[pb_bosques_illegal_df$nom_dpto == input$nom_dpto]))
    # })
    
    # observeEvent(input$department_deforestation, {
    #   updateSelectInput(session, "district_deforestation", choices = unique(deforestation_fake$district[deforestation_fake$department == input$department_deforestation]))
    # })  
    
  
  # # filter for column desired
  # compliance_filtered_df <- reactive({
  #   compliance_fake |>
  #     filter(department == input$department, district == input$district)
  # })
    
    # 
    # filtered_data <- reactive({
    #   pb_bosques_illegal_df %>%
    #     filter(nom_dpto == input$nom_dpto, nom_dist == input$nom_dist) %>%
    #     group_by(nom_dpto, nom_dist) %>%
    #     summarize(sum_df_ha = sum(df_area_ha),
    #               avg_df_ha = mean(df_area_ha),
    #               total_area_ha = mean(area_ha),
    #               df_percent = (sum_df_ha / total_area_ha) * 100,
    #               num_put_id = n_distinct(put_id))
    # })
    
  # # Update district choices based on department selection
  # observe({
  #   departments <- input$department
  #   if (departments != "Select a department") {
  #     districts <- unique(illegal_df %>%
  #                           filter(nom_dpto == departments) %>%
  #                           pull(nom_dist))
  #     updateSelectInput(session, "district", choices = c("Select a district", districts))
  #   } else {
  #     updateSelectInput(session, "district", choices = NULL)
  #   }
  # })
  # 
  # # Reactive object for the selected department
  # selected_dept <- reactive({
  #   if (input$department != "Select a department") {
  #     illegal_df %>% filter(nom_dpto == input$department)
  #   } else {
  #     NULL
  #   }
  # })
  # 
  # # Filter the data based on user inputs
  # filtered_data <- reactive({
  #   if (!is.null(selected_dept()) && input$district != "Select a district") {
  #     selected_dept() %>% filter(nom_dist == input$district)
  #   } else {
  #     NULL
  #   }
  # })
  # 
  # # Reactive object for the initial map with all departments
  # all_departments_map <- reactive({
  #   # Transform the data to WGS84 CRS
  #   illegal_df_transformed <- st_transform(illegal_df, crs = "+proj=longlat +datum=WGS84")
  #   
  #   # Create leaflet map
  #   leaflet(data = illegal_df_transformed) %>%
  #     addProviderTiles(providers$CartoDB.Positron) %>%
  #     addPolygons(fillColor = ~colorNumeric(palette = "YlGnBu", domain = illegal_df_transformed$sum_df_ha_dpto)(sum_df_ha_dpto),
  #                 fillOpacity = 0.8,
  #                 color = "#BDBDC3",
  #                 weight = 1,
  #                 opacity = 1,
  #                 label = paste0("Department: ", illegal_df_transformed$nom_dpto,
  #                                " Illegal Deforestation: ", round(illegal_df_transformed$sum_df_ha_dpto),
  #                                " Number of properties: ", illegal_df_transformed$num_put_id_dpto))
  # })
  # 
  # # Render the map
  # output$illegal_df_map <- renderLeaflet({
  #   # Display the map based on user input
  #   if (input$department == "Select a department") {
  #     all_departments_map()
  #   } else {
  #     if (is.null(filtered_data())) {
  #       return(NULL)
  #     }
  #     # Transform the data to WGS84 CRS
  #     filtered_data_transformed <- st_transform(filtered_data(), crs = "+proj=longlat +datum=WGS84")
  #     
  #     # Define color palette
  #     my_palette <- colorNumeric(palette = "YlGnBu", domain = filtered_data_transformed$sum_df_ha_dist)
  #     
  #     # Create leaflet map
  #     leaflet(data = filtered_data_transformed) %>%
  #       addProviderTiles(providers$CartoDB.Positron) %>%
  #       addPolygons(fillColor = ~my_palette(sum_df_ha_dist),
  #                   fillOpacity = 0.8,
  #                   color = "#BDBDC3",
  #                   weight = 1,
  #                   opacity = 1,
  #                   highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
  #                   label = paste0("District: ", filtered_data_transformed$nom_dist,
  #                                  " Illegal Deforestation: ", round(filtered_data_transformed$sum_df_ha_dist),
  #                                  " Number of properties: ", filtered_data_transformed$num_put_id_dist)) %>%
  #       addLegend(pal = my_palette,
  #                 values = filtered_data_transformed$sum_df_ha_dist,
  #                 title = "Illegal Deforestation (hectares)",
  #                 position = "bottomright")
  #   }
  # 
  # })
    
  
  combined_illegal_df_by_dist <- st_transform(combined_illegal_df_by_dist, crs = "+proj=longlat +datum=WGS84")
  combined_illegal_df_by_dpto  <- st_transform(combined_illegal_df_by_dpto, crs = "+proj=longlat +datum=WGS84")
  
  # Function to filter data by year range
  filter_data <- function(data) {
    data %>% filter(year_range == input$year_range)
  }
  
  data_reactive <- reactive({
    if (input$drill_down == 0 && input$drill_up == 0) {
      filter_data(combined_illegal_df_by_dpto)
    } else if (input$drill_down > 0) {
      filter_data(combined_illegal_df_by_dist)
    } else {
      filter_data(combined_illegal_df_by_dpto)
    }
  })
  
  output$leafdown <- renderLeaflet({
    data <- data_reactive()
    my_palette_dpto <- colorNumeric(palette = pyforest_palette, domain = data$sum_df_ha)
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = data,
        fillColor = ~my_palette_dpto(data$sum_df_ha),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1,
        opacity = 1,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste0(
          "Department: ", nom_dpto,
          "<br>Illegal Deforestation: ", round(sum_df_ha), " ha",
          "<br>Number of properties: ", num_put_id,
          "<br>Number of properties with illegal deforestation: ", num_illegal_props 
        ) %>% lapply(HTML)
      ) %>%
      addLegend(
        pal = my_palette_dpto,
        values = data$sum_df_ha,
        title = "Illegal Deforestation (ha)",
        position = "bottomright"
      )
  })
  
  observeEvent(input$drill_down, {
    data <- data_reactive()
    my_palette_dist <- colorNumeric(palette = pyforest_palette, domain = data$sum_df_ha)
    leafletProxy("leafdown") %>% clearShapes() %>%
      addPolygons(
        data = data,
        fillColor = ~my_palette_dist(data$sum_df_ha),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1,
        opacity = 1,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste0(
          "District: ", nom_dist,
          "<br>Illegal Deforestation: ", round(sum_df_ha), " ha",
          "<br>Number of properties: ", num_put_id,
          "<br>Number of properties with illegal deforestation: ", num_illegal_props 
        ) %>% lapply(HTML)
      )
  })
  
  observeEvent(input$drill_up, {
    data <- data_reactive()
    my_palette_dpto <- colorNumeric(palette = pyforest_palette, domain = data$sum_df_ha)
    leafletProxy("leafdown") %>% clearShapes() %>%
      addPolygons(
        data = data,
        fillColor = ~my_palette_dpto(data$sum_df_ha),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1,
        opacity = 1,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste0(
          "Department: ", nom_dpto,
          "<br>Illegal Deforestation: ", round(sum_df_ha), " ha",
          "<br>Number of properties: ", num_put_id,
          "<br>Number of properties with illegal deforestation: ", num_illegal_props 
        ) %>% lapply(HTML)
      )
  })
  
  
  output$illegalPlot <- renderPlotly({
    if (input$drill_down > 0) {
      data <- filter_data(combined_illegal_df_by_dist)
      p <- ggplot(data, aes(x = sum_df_ha, y = reorder(nom_dist, num_put_id), fill = sum_df_ha)) +
        geom_bar(stat = "identity", aes(text = paste("Number of properties:", num_put_id, "<br>Illegal deforestation (ha):", round(sum_df_ha)))) +
        scale_fill_gradientn(colors = pyforest_palette) +
        ggtitle("Illegal Deforestation by District") +
        xlab("Illegal Deforestation (hectares)") +
        ylab("District") +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "right") +
        labs(fill = "Illegal Deforestation")
    } else {
      data <- filter_data(combined_illegal_df_by_dpto)
      p <- ggplot(data, aes(x = sum_df_ha, y = reorder(nom_dpto, num_put_id), fill = sum_df_ha)) +
        geom_bar(stat = "identity", aes(text = paste("Number of properties:", num_put_id, "<br>Illegal deforestation (ha):", round(sum_df_ha)))) +
        scale_fill_gradientn(colors = pyforest_palette) +
        ggtitle("Illegal Deforestation by Department") +
        xlab("Illegal Deforestation (hectares)") +
        ylab("Department") +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "right") +
        labs(fill = "Illegal Deforestation")
    }
    p <- ggplotly(p, tooltip = "text")
    p <- layout(p, hoverlabel = list(bgcolor = "white"))  # Add this line to set the tooltip background to white
    return(p)
  })
  
  
  
  

    ####new  
  # #compliance plot
  # compliance_colors <- c("yes" = "#4b5f43", "no" = "#999999")
  # # Group by department and calculate the count of compliance
  # df_by_dept <- compliance_fake |>
  #   group_by(department, compliance) |>
  #   summarize(count = n())
  # 
  # output$compliance_output_plot <- renderPlot({
  #   ggplot(df_by_dept, aes(x = department, y = count, fill = compliance)) + 
  #     geom_bar(stat = "identity", position = "dodge") + 
  #     labs(x = "Department", y = "Number of Properties", title = "Compliance by Department",
  #          caption = "yes: land use plan was accurately executed/ authorized deforestation, no: land use plan was not accurately executed/unauthorized deforestation") +
  #     scale_fill_manual(values = compliance_colors) +
  #     theme_minimal()
  #   
  # })
  # 
  ####new
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
  illegal_filtered_df <- reactive({
    illegal_df |>
      filter(department == input$nom_dpto, district == input$nom_dist)
  })

  # Generate table based on selected dept and dist
  output$illegal_df_output <- renderDataTable({
    illegal_filtered_df()
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
#   # filter for column desired
#    deforestation_filtered_df <- reactive({
#      deforestation_fake |>
#        filter(department == input$department_deforestation, district == input$district_deforestation)
#    })
# 
# # Deforestation plot
# output$deforestation_output_plot <- renderPlot({
#   ggplot(deforestation_filtered_df(), aes(x = year, y = deforestation, color = district)) +
#     geom_line() +
#     facet_wrap(~department, scales = "free_y") +
#     labs(x = "Year", y = "Deforestation (hectares loss)", title = "Deforestation by Department from 2002 to 2020") +
#     theme_minimal()
#   
#   })
  
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
  
  
  # PUT ID
  
  compliance_filter<- reactive({
    if (!is.null(input$code) && input$code != "" && input$code != "0") {
      compliance %>%
        filter(put_id %in% input$code)
    } else {
      compliance
    }
  })
  
  observeEvent(input$reset_button, {
    updatePickerInput(session, "code", selected = "0")
  })
  
  
  compliance_map <- reactive({
    leaflet(data = compliance_filter()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        color = ifelse(compliance_filter()$df_status == "no illegal df", "#4b5f43", "#F26419"),
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1,
        fill = TRUE,
        fillColor = ifelse(compliance_filter()$df_status == "no illegal df", "#4b5f43", "#F26419"),
        popup = ~paste(
          "Put ID:", compliance_filter()$put_id, "<br>",
          "Deforested Area (ha):", compliance_filter()$df_area_ha, "<br>",
          "Status:", ifelse(compliance_filter()$df_status == "no illegal df", "Compliant", "Not Compliant")
        )
      )
  })
  # End of plot of map
  
  output$map <- renderLeaflet({ compliance_map() })
  
  
  # Create reactive expression for the table
  compliance_table <- reactive({
    compliance_filter() %>%
      st_drop_geometry() %>%
      select("put_id", "grupo", "cell_count", "df_area_ha", "land_use_type_area", "df_status") %>%
      datatable(options = list(pageLength = 10), rownames = FALSE) 
  })
  
  output$table <- renderDataTable({
    compliance_table()
  })
  
  output$download_report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = list(data = compliance_filter(),
                                      map = compliance_map(),
                                      table = compliance_table()))
    }
  ) # END PUT ID
  
  
  
  # Create a dynamic UI element in the server.R file using renderUI
  output$name_selection <- renderUI({
    if (input$dataset == "department") {
      selectInput("name",
                  label = "Choose department:",
                  choices = c("PDTE. HAYES", "BOQUERON", "ALTO PARAGUAY"))
    } else if (input$dataset == "district") {
      selectInput("name",
                  label = "Choose district:",
                  choices = c("BAHIA NEGRA", "BENJAMIN ACEVAL", "BOQUERON",
                              "CAMPO ACEVAL", "CARMELO PERALTA", "FILADELFIA",
                              "FUERTE OLIMPO", "GRAL JOSE MARIA BRUGUEZ",
                              "JOSE FALCON", "LOMA PLATA", "MCAL. ESTIGARRIBIA",
                              "NANAWA", "NUEVA ASUNCION", "PUERTO CASADO",
                              "PUERTO PINASCO", "TTE 1RO MANUEL IRALA FERNANDEZ",
                              "TTE. ESTEBAN MARTINEZ", "VILLA HAYES"))
    }
  })
  
  # Land use simulation plot
  output$landUsePlot <- renderPlotly({
    plot_land_use_type_stackedh(input$dataset, input$name)
  })
  
  
  
  
  
  
  
  
  
  
  }





