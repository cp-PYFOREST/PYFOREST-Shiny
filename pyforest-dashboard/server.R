# server.R
# Load packages and data from global.R file

server <- function(input, output, session) {
  

  pyforest_palette <- c("#4B5F43", "#AEBD93", "#F6AE2D", "#F26419")  
  
    
  # ------------------------------------------ LUP Assessment PB ------------------------------------------
  combined_illegal_df_by_dist <- st_transform(combined_illegal_df_by_dist, crs = "+proj=longlat +datum=WGS84")
  combined_illegal_df_by_dpto  <- st_transform(combined_illegal_df_by_dpto, crs = "+proj=longlat +datum=WGS84")
  
  current_view <- reactiveVal("department")  # Initialize as "department" by default
  
  observeEvent(input$drill_down, {
    current_view("district")  # Set the current view to "district"
  })
  
  observeEvent(input$drill_up, {
    current_view("department")  # Set the current view to "department"
  })
  
  # Function to filter data by year range
  filter_data <- function(data) {
    data %>% filter(year_range == input$year_range) %>%
      mutate(normalized_value = normalized_value * 10000)  # Multiply by 10,000
  }
  
  data_reactive <- reactive({
    if (current_view() == "district") {
      filter_data(combined_illegal_df_by_dist)
    } else {
      filter_data(combined_illegal_df_by_dpto)
    }
  })
  
  output$leafdown <- renderLeaflet({
    data <- data_reactive()
    max_sum_df_ha <- max(data$sum_df_ha)  # Get the maximum value of sum_df_ha
    my_palette_dpto <- colorNumeric(palette = pyforest_palette,
                                    domain = data$normalized_value,
                                    na.color = "transparent")
    
    selected_range <- reactive({
      data <- data_reactive()
      range_text <- paste0("Approximately ", round(min(data$normalized_value)), "-", round(max(data$normalized_value)), " hectares")
      range_text
    })
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = data,
        fillColor = ~my_palette_dpto(data$normalized_value),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1,
        opacity = 1,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste0(
          "Department: ", nom_dpto,
          "<br>Illegal Deforestation: ", round(sum_df_ha), " ha",
          "<br>Number of properties: ", num_put_id,
          "<br>Number of properties with unauthorized deforestation: ", num_illegal_props
        ) %>% lapply(HTML)
      ) %>%
      addLegend(
        pal = my_palette_dpto,
        values = data$normalized_value,
        #values = c(0, max_sum_df_ha),
        title = "Unauthorized Deforestation (per 10,000 ha)",
        position = "bottomright",
        labels = comma) %>%
      addControl(
        html = paste0('<div class="caption">', selected_range(), ' of unauthorized deforestation occurred per 10,000 hectares of total area</div>'),
        position = "bottomleft"
      )
  })
  
  observeEvent(input$drill_down, {
    data <- data_reactive()
    my_palette_dist <- colorNumeric(palette = pyforest_palette,
                                    domain = data$normalized_value)
    
    leafletProxy("leafdown") %>% clearShapes() %>%
      addPolygons(
        data = data,
        fillColor = ~my_palette_dist(data$normalized_value),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1,
        opacity = 1,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste0(
          "District: ", nom_dist,
          "<br>Illegal Deforestation: ", round(sum_df_ha * 10000), " ha",
          "<br>Number of properties: ", num_put_id,
          "<br>Number of properties with unauthorized deforestation: ", num_illegal_props
        ) %>% lapply(HTML)
      )
  })
  
  observeEvent(input$drill_up, {
    data <- data_reactive()
    my_palette_dpto <- colorNumeric(palette = pyforest_palette,
                                    domain = data$normalized_value)
    
    leafletProxy("leafdown") %>% clearShapes() %>%
      addPolygons(
        data = data,
        fillColor = ~my_palette_dpto(data$normalized_value),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1,
        opacity = 1,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste0(
          "Department: ", nom_dpto,
          "<br>Illegal Deforestation: ", round(sum_df_ha * 10000), " ha",
          "<br>Number of properties: ", num_put_id,
          "<br>Number of properties with unauthorized deforestation: ", num_illegal_props
        ) %>% lapply(HTML)
      )
  })

  output$illegalPlot <- renderPlotly({
    data <- data_reactive()
    if (current_view() == "district") {
      data <- filter_data(combined_illegal_df_by_dist)
      p <- ggplot(data, aes(x = sum_df_ha, y = reorder(nom_dist, num_put_id), fill = sum_df_ha)) +
        geom_bar(stat = "identity", tooltip = "text", aes(text = paste("Number of properties:", num_put_id, "<br>Unauthorized deforestation (ha):", round(sum_df_ha)))) +
        scale_fill_gradientn(colors = pyforest_palette) +
        #ggtitle("Unauthorized Deforestation by District") +
        xlab("Illegal Deforestation (hectares)") +
        ylab("District") +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "right") +
        labs(fill = "Unauthorized Deforestation")
    } else {
      data <- filter_data(combined_illegal_df_by_dpto)
      p <- ggplot(data, aes(x = sum_df_ha, y = reorder(nom_dpto, num_put_id), fill = sum_df_ha)) +
        geom_bar(stat = "identity", aes(text = paste("Number of properties:", num_put_id, "<br>Unauthorized deforestation (ha):", round(sum_df_ha)))) +
        scale_fill_gradientn(colors = pyforest_palette) +
        #ggtitle("Unauthorized Deforestation by Department") +
        xlab("Illegal Deforestation (ha)") +
        ylab("Department") +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "right") +
        labs(fill = "Unauthorized Deforestation")
    }
    p <- ggplotly(p, tooltip = "text")
    p <- layout(p, hoverlabel = list(bgcolor = "white"))  #tooltip background to white
    
    
    return(p)
  })
  
  output$areaPlot <- renderPlotly({
    if (current_view() == "department")  {
      combined_illegal_df_by_dpto$year_range <- factor(combined_illegal_df_by_dpto$year_range, levels = rev(unique(as.character(combined_illegal_df_by_dpto$year_range))))
      p <- ggplot(combined_illegal_df_by_dpto, aes(x = as.character(year_range), y = sum_df_ha, group = nom_dpto)) +
        geom_area(fill = "#33658A", alpha = 0.3) +
        geom_point(color = "#33658A", size = 1) +
        scale_x_discrete(limits = rev(levels(combined_illegal_df_by_dpto$year_range)), labels = rev(levels(combined_illegal_df_by_dpto$year_range))) +
        labs(x = "Time Frame", y = "Unauthorized Deforestation (ha)") +
        facet_wrap(~nom_dpto, ncol = 1, scales = "fixed") +
        theme_minimal() +
        theme(plot.margin = margin(t = 20, r = 20, b = 80, l = 20))
    } else {
      p <- ggplot(combined_illegal_df_by_dist, aes(x = year_range, y = sum_df_ha, group = 1)) +
        geom_area(fill = "#33658A", alpha = 0.3) +
        geom_point(color = "#33658A", size = 1) +
        scale_x_discrete(limits = rev(levels(combined_illegal_df_by_dist$year_range)), labels = rev(levels(combined_illegal_df_by_dist$year_range))) +
        labs(x = "Time Frame", y = "Unauthorized Deforestation (ha)") +
        facet_wrap(~ nom_dist, ncol = 2, scales = "free") +
        theme_minimal() +
        theme(plot.margin = margin(t = 20, r = 20, b = 40, l = 20),
              axis.text = element_text(size = 5),  # Adjust the font size of axis labels
              panel.spacing = unit(0.5, "lines"),  # Adjust the spacing between facets
              strip.text = element_text(size = 8))
      
    }
    ggplotly(p)
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
  
  # # filter data table
  # illegal_filtered_df <- reactive({
  #   illegal_df |>
  #     filter(department == input$nom_dpto, district == input$nom_dist)
  # })
  # 
  # # Generate table based on selected dept and dist
  # output$illegal_df_output <- renderDataTable({
  #   illegal_filtered_df()
  # })

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
  
  
  # # Deforestation tmap output
  # output$map_output <- renderTmap({
  #   tmap_mode("view")
  #   tm_shape(fl_00_05) +
  #     tm_fill(col = "darkgreen")
  #   
  # })
  
  
  # ------------------------------------------ LUP Assessment by PUT ID ------------------------------------------
  
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
  
  # ------------------------------------------ LUP Simulations ------------------------------------------
  
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
  
  # ------------------------------------------ XXXX ------------------------------------------
  
  

  
  
  
  
  }





