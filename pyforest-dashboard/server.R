# server.R

# Source
#source("R/forest_cover_module.R")
source("R/lup_sim_deforestation_prediction_histogram_data.R")

# Load packages and data from global.R file

server <- function(input, output, session) {
  

  pyforest_palette <- c("#4B5F43", "#AEBD93", "#F6AE2D", "#F26419")
  
    
  # ------------------------------------------ LUP Assessment Unauthorized ------------------------------------------
combined_illegal_df_by_dist <- st_transform(combined_illegal_df_by_dist, crs = "+proj=longlat +datum=WGS84")
combined_illegal_df_by_dpto  <- st_transform(combined_illegal_df_by_dpto, crs = "+proj=longlat +datum=WGS84")
  
  unauth_current_view <- reactiveVal("department")  # Initialize as "department" by default
  
  observeEvent(input$drill_down_unauthorized, {
    unauth_current_view("district")  # Set the current view to "district"
  })
  
  observeEvent(input$drill_up_unauthorized, {
    unauth_current_view("department")  # Set the current view to "department"
  })
  
  # Function to filter data by year range
  unauth_filter_data <- function(data) {
    data %>% filter(year_range == input$year_range_unauthorized) %>%
      mutate(normalized_value = normalized_value * 10000)  # Multiply by 10,000
  }
  
  unauth_data_reactive <- reactive({
    if (unauth_current_view() == "district") {
      unauth_filter_data(combined_illegal_df_by_dist)
    } else {
      unauth_filter_data(combined_illegal_df_by_dpto)
    }
  })
  
  # Define the fixed range for departments
  unauth_fixed_range_dpto <- c(0, 160)
  # Define the fixed range for districts
  unauth_fixed_range_dist <- c(0, 70)
  
  output$leafdown_unauthorized <- renderLeaflet({
    data <- unauth_data_reactive()
    max_sum_df_ha <- max(data$sum_df_ha)  # Get the maximum value of sum_df_ha
    
    if(unauth_current_view() == "district") {
      my_palette <- colorNumeric(palette = pyforest_palette,
                                 domain = unauth_fixed_range_dist,
                                 na.color = "transparent")
      legend_values = unauth_fixed_range_dist
      legend_layerId = "district-legend"
    } else {
      my_palette <- colorNumeric(palette = pyforest_palette,
                                 domain = unauth_fixed_range_dpto,
                                 na.color = "transparent")
      legend_values = unauth_fixed_range_dpto
      legend_layerId = "department-legend"
    }
    
    selected_range <- reactive({
      data <- unauth_data_reactive()
      range_text <- paste0("Approximately ", round(min(data$normalized_value)), "-", round(max(data$normalized_value)), " hectares")
      range_text
    })
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = data,
        fillColor = ~my_palette(data$normalized_value),
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
      addControl(
        html = paste0('<div class="caption">', selected_range(), ' of unauthorized deforestation occurred per 10,000 hectares of total area</div>'),
        position = "bottomleft"
      ) %>%
      addLegend(
        pal = my_palette,
        values = legend_values,
        title = "Unauthorized Deforestation (per 10,000 ha)",
        position = "bottomright",
        labels = comma,
        layerId = legend_layerId
      )
  })
  
  observeEvent(input$drill_down_unauthorized, {
    data <- unauth_data_reactive()
    
    my_palette <- colorNumeric(palette = pyforest_palette,
                               domain = unauth_fixed_range_dist,
                               na.color = "transparent")
    legend_values = unauth_fixed_range_dist
    legend_layerId = "district-legend"
    
    leafletProxy("leafdown_unauthorized") %>% 
      clearShapes() %>%
      removeControl("department-legend") %>%
      addPolygons(
        data = data,
        fillColor = ~my_palette(data$normalized_value),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1,
        opacity = 1,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste0(
          "District: ", nom_dist,
          "<br>Illegal Deforestation: ", round(sum_df_ha), " ha",
          "<br>Number of properties: ", num_put_id,
          "<br>Number of properties with unauthorized deforestation: ", num_illegal_props
        ) %>% lapply(HTML)
      ) %>%
      addLegend(
        pal = my_palette,
        values = legend_values,
        title = "Unauthorized Deforestation (per 10,000 ha)",
        position = "bottomright",
        labels = comma,
        layerId = legend_layerId
      )
  })
  
  observeEvent(input$drill_up_unauthorized, {
    data <- unauth_data_reactive()
    
    my_palette <- colorNumeric(palette = pyforest_palette,
                               domain = unauth_fixed_range_dpto,
                               na.color = "transparent")
    legend_values = unauth_fixed_range_dpto
    legend_layerId = "department-legend"
    
    leafletProxy("leafdown_unauthorized") %>% 
      clearShapes() %>%
      removeControl("district-legend") %>%
      addPolygons(
        data = data,
        fillColor = ~my_palette(data$normalized_value),
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
        pal = my_palette,
        values = legend_values,
        title = "Unauthorized Deforestation (per 10,000 ha)",
        position = "bottomright",
        labels = comma,
        layerId = legend_layerId
      )
  })
  
  # # render valueBox for combined_illegal_df_by_dist ----
  # output$unauth_prop_valuebox_dist <- renderValueBox({
  #   total_sum_by_dist <- sum(unauth_data_reactive()$sum_df_ha)
  #   valueBox(
  #     "Total Sum by District",
  #     value = total_sum_by_dist,
  #     color = "orange",
  #     icon = icon("bar-chart", lib = "font-awesome")
  #   )
  # })
  # 
  # # render valueBox for combined_illegal_df_by_dpt ----
  # output$unauth_prop_valuebox_dpt <- renderValueBox({
  #   total_sum_by_dpt <- sum(unauth_data_reactive()$sum_df_ha)
  #   valueBox(
  #     "Total Sum by Department",
  #     value = total_sum_by_dpt,
  #     color = "orange",
  #     icon = icon("bar-chart", lib = "font-awesome")
  #   )
  # })
  
  output$illegalPlot <- renderPlotly({
    data <- unauth_data_reactive()
    if (unauth_current_view() == "district") {
      data <- unauth_filter_data(combined_illegal_df_by_dist)
      p <- ggplot(data, aes(x = sum_df_ha, y = reorder(nom_dist, num_put_id), fill = sum_df_ha)) +
        geom_bar(stat = "identity", tooltip = "text", aes(text = paste("Number of properties:", num_put_id, "<br>Unauthorized deforestation (ha):", round(sum_df_ha)))) +
        scale_fill_gradientn(colors = pyforest_palette) +
        #ggtitle("Unauthorized Deforestation by District") +
        xlab("Unauthorized Deforestation (ha)") +
        ylab("District") +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "right") +
        labs(fill = "Unauthorized Deforestation")
    } else {
      data <- unauth_filter_data(combined_illegal_df_by_dpto)
      p <- ggplot(data, aes(x = sum_df_ha, y = reorder(nom_dpto, num_put_id), fill = sum_df_ha)) +
        geom_bar(stat = "identity", aes(text = paste("Number of properties:", num_put_id, "<br>Unauthorized deforestation (ha):", round(sum_df_ha)))) +
        scale_fill_gradientn(colors = pyforest_palette) +
        #ggtitle("Unauthorized Deforestation by Department") +
        xlab("Unauthorized Deforestation (ha)") +
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
    if (unauth_current_view() == "department")  {
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
        theme(plot.margin = margin(t = 10, r = 10, b = 30, l = 10),
              axis.text = element_text(size = 5),  # Adjust the font size of axis labels
              panel.spacing = unit(1, "lines"),  # Adjust the spacing between facets
              strip.text = element_text(size = 8))
      
    }
    ggplotly(p)
  })
  
  # ------------------------------------------ LUP Assessment Authorized ------------------------------------------
  
  combined_auth_df_by_dist <- st_transform(combined_auth_df_by_dist, crs = "+proj=longlat +datum=WGS84")
  combined_auth_df_by_dpto  <- st_transform(combined_auth_df_by_dpto, crs = "+proj=longlat +datum=WGS84")
  
  auth_current_view <- reactiveVal("department")  # Initialize as "department" by default
  
  observeEvent(input$drill_down_authorized, {
    auth_current_view("district")  # Set the current view to "district"
  })
  
  observeEvent(input$drill_up_authorized, {
    auth_current_view("department")  # Set the current view to "department"
  })
  
  # Function to filter data by year range
  auth_filter_data <- function(data) {
    data %>% filter(year_range == input$year_range_authorized) %>%
      mutate(normalized_value = normalized_value * 10000)  # Multiply by 10,000
  }
  
  auth_data_reactive <- reactive({
    if (auth_current_view() == "district") {
      auth_filter_data(combined_auth_df_by_dist)
    } else {
      auth_filter_data(combined_auth_df_by_dpto)
    }
  })
  
  # Define the fixed range for departments
  fixed_range_dpto_auth <- c(0, 500)
  # Define the fixed range for districts
  fixed_range_dist_auth <- c(0, 240)
  
  output$leafdown_authorized <- renderLeaflet({
    data <- auth_data_reactive()
    max_sum_df_ha <- max(data$sum_df_ha)  # Get the maximum value of sum_df_ha
    
    if(auth_current_view() == "district") {
      my_palette <- colorNumeric(palette = pyforest_palette,
                                 domain = fixed_range_dist_auth,
                                 na.color = "transparent")
      legend_values = fixed_range_dist_auth
      legend_layerId = "district-legend"
    } else {
      my_palette <- colorNumeric(palette = pyforest_palette,
                                 domain = fixed_range_dpto_auth,
                                 na.color = "transparent")
      legend_values = fixed_range_dpto_auth
      legend_layerId = "department-legend"
    }
    
    selected_range <- reactive({
      data <- auth_data_reactive()
      range_text <- paste0("Approximately ", round(min(data$normalized_value)), "-", round(max(data$normalized_value)), " hectares")
      range_text
    })
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = data,
        fillColor = ~my_palette(data$normalized_value),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1,
        opacity = 1,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste0(
          "Department: ", nom_dpto,
          "<br>Authorized Deforestation: ", round(sum_df_ha), " ha",
          "<br>Remaining hectares authorized to be deforested: ", round(sum_remaining_df_area_ha), " ha",
          "<br>Number of properties: ", num_put_id
        ) %>% lapply(HTML)
      ) %>%
      addControl(
        html = paste0('<div class="caption">', selected_range(), ' of authorized deforestation per 10,000 hectares of total area</div>'),
        position = "bottomleft"
      ) %>%
      addLegend(
        pal = my_palette,
        values = legend_values,
        title = "Authorized Deforestation (per 10,000 ha)",
        position = "bottomright",
        labels = comma,
        layerId = legend_layerId
      )
  })
  
  observeEvent(input$drill_down_authorized, {
    data <- auth_data_reactive()
    
    my_palette <- colorNumeric(palette = pyforest_palette,
                               domain = fixed_range_dist_auth,
                               na.color = "transparent")
    legend_values = fixed_range_dist_auth
    legend_layerId = "district-legend"
    
    leafletProxy("leafdown") %>% 
      clearShapes() %>%
      removeControl("department-legend") %>%
      addPolygons(
        data = data,
        fillColor = ~my_palette(data$normalized_value),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1,
        opacity = 1,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste0(
          "District: ", nom_dist,
          "<br>Authorized Deforestation: ", round(sum_df_ha), " ha",
          "<br>Remaining hectares authorized to be deforested: ", round(sum_remaining_df_area_ha), " ha",
          "<br>Number of properties: ", num_put_id
        ) %>% lapply(HTML)
      ) %>%
      addLegend(
        pal = my_palette,
        values = legend_values,
        title = "Aauthorized Deforestation (per 10,000 ha)",
        position = "bottomright",
        labels = comma,
        layerId = legend_layerId
      )
  })
  
  observeEvent(input$drill_up_authorized, {
    data <- auth_data_reactive()
    
    my_palette <- colorNumeric(palette = pyforest_palette,
                               domain = fixed_range_dpto_auth,
                               na.color = "transparent")
    legend_values = fixed_range_dpto_auth
    legend_layerId = "department-legend"
    
    leafletProxy("leafdown") %>% 
      clearShapes() %>%
      removeControl("district-legend") %>%
      addPolygons(
        data = data,
        fillColor = ~my_palette(data$normalized_value),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1,
        opacity = 1,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste0(
          "Department: ", nom_dpto,
          "<br>Authorized Deforestation: ", round(sum_df_ha), " ha",
          "<br>Remaining hectares authorized to be deforested: ", round(sum_remaining_df_area_ha), " ha",
          "<br>Number of properties: ", num_put_id
        ) %>% lapply(HTML)
      ) %>%
      addLegend(
        pal = my_palette,
        values = legend_values,
        title = "Authorized Deforestation (per 10,000 ha)",
        position = "bottomright",
        labels = comma,
        layerId = legend_layerId
      )
  })
  
  # Authorized Plot
  output$authorizedPlot <- renderPlotly({
    data <- auth_data_reactive()
    if (auth_current_view() == "district") {
      data <- auth_filter_data(combined_auth_df_by_dist)
      p <- ggplot(data, aes(x = sum_df_ha, y = reorder(nom_dist, num_put_id), fill = sum_df_ha)) +
        geom_bar(stat = "identity", tooltip = "text", aes(text = paste("Number of properties:", num_put_id, "<br>Authorized deforestation (ha):", round(sum_df_ha)))) +
        scale_fill_gradientn(colors = pyforest_palette) +
        xlab("Authorized Deforestation (ha)") +
        ylab("District") +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "right") +
        labs(fill = "Authorized Deforestation")
    } else {
      data <- auth_filter_data(combined_auth_df_by_dpto)
      p <- ggplot(data, aes(x = sum_df_ha, y = reorder(nom_dpto, num_put_id), fill = sum_df_ha)) +
        geom_bar(stat = "identity", aes(text = paste("Number of properties:", num_put_id, "<br>Authorized deforestation (ha):", round(sum_df_ha)))) +
        scale_fill_gradientn(colors = pyforest_palette) +
        xlab("Authorized Deforestation (ha)") +
        ylab("Department") +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "right") +
        labs(fill = "Authorized Deforestation")
    }
    p <- ggplotly(p, tooltip = "text")
    p <- layout(p, hoverlabel = list(bgcolor = "white"))  #tooltip background to white


    return(p)
  })

  output$area_authorized_Plot <- renderPlotly({
    if (auth_current_view() == "department")  {
      combined_auth_df_by_dpto$year_range <- factor(combined_auth_df_by_dpto$year_range, levels = rev(unique(as.character(combined_auth_df_by_dpto$year_range))))
      p <- ggplot(combined_auth_df_by_dpto, aes(x = as.character(year_range), y = sum_df_ha, group = nom_dpto)) +
        geom_area(fill = "#33658A", alpha = 0.3) +
        geom_point(color = "#33658A", size = 1) +
        scale_x_discrete(limits = rev(levels(combined_auth_df_by_dpto$year_range)), labels = rev(levels(combined_auth_df_by_dpto$year_range))) +
        labs(x = "Time Frame", y = "Authorized Deforestation (ha)") +
        facet_wrap(~nom_dpto, ncol = 1, scales = "fixed") +
        theme_minimal() +
        theme(plot.margin = margin(t = 20, r = 20, b = 80, l = 20))
    } else {
      p <- ggplot(combined_auth_df_by_dist, aes(x = year_range, y = sum_df_ha, group = 1)) +
        geom_area(fill = "#33658A", alpha = 0.3) +
        geom_point(color = "#33658A", size = 1) +
        scale_x_discrete(limits = rev(levels(combined_auth_df_by_dist$year_range)), labels = rev(levels(combined_auth_df_by_dist$year_range))) +
        labs(x = "Time Frame", y = "Authorized Deforestation (ha)") +
        facet_wrap(~ nom_dist, ncol = 2, scales = "free") +
        theme_minimal() +
        theme(plot.margin = margin(t = 10, r = 10, b = 30, l = 10),
              axis.text = element_text(size = 5),  # Adjust the font size of axis labels
              panel.spacing = unit(1, "lines"),  # Adjust the spacing between facets
              strip.text = element_text(size = 8))

    }
    ggplotly(p)
  })

  
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
  
  
  
  # ----------------------------------------- Deforestation Statistics -----------------------------------

  output$deforestation_data_disclaimer_txt <- renderText({
    "All deforestation data was provided in year ranges 
    (2000-2005, 2005-2011, 2011-2013, 2013-2015, 2015-2017, 2017-2018, 2018-2019, 2019-2020).
    To standardize the data, the deforestation percentage and area values within each range were divided 
    by the corresponding number of years to obtain a per-year value."
  }) 
  
  py_fl_dept <- st_transform(py_fl_dept, crs = "+proj=longlat +datum=WGS84")
  py_fl_dist <- st_transform(py_fl_dist, crs = "+proj=longlat +datum=WGS84")
  #pyforest_palette_df <- c("#F26419", "#F6AE2D", "#AEBD93", "#4B5F43")
  pyforest_palette_df <- c("#4B5F43", "#AEBD93", "#F6AE2D", "#F26419")
  forest_loss_std_df <- st_transform(forest_loss_std_df, crs = "+proj=longlat +datum=WGS84")
  forest_loss_district_std_df <- st_transform(forest_loss_district_std_df, crs = "+proj=longlat +datum=WGS84")
  
  
  filter_data_deforestation <- function(data) {
    data %>% filter(year == input$year_deforestation_percent_map)
  }
  
  
  data_dept_deforestation <- reactive({
    if (input$drill_down == 0 && input$drill_up == 0) {
      filter_data_deforestation(forest_loss_std_df)
    } else if (input$drill_up > 0) {
      filter_data_deforestation(forest_loss_std_df)
    } else {
      filter_data_deforestation(forest_loss_std_df)
    }
  })
  
  data_dist_deforestation <- reactive({
    if (input$drill_down > 0) {
      filter_data_deforestation(forest_loss_district_std_df)
    }  else {
      filter_data_deforestation(forest_loss_std_df)
    }
  })
  
  output$leafdown_forest_loss <- renderLeaflet({
    data <- data_dept_deforestation()
    my_palette_dpto <- colorNumeric(palette = pyforest_palette_df, domain = data$percent_fl_std)
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = data,
        fillColor = ~my_palette_dpto(data$percent_fl_std),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1,
        opacity = 1,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste0(
          "<b>Year: </b>", year,
          "<br><b>Department: </b>", nom_dpto,
          "<br><b>Deforestation Percent: </b>", data$percent_fl_std, " %",
          "<br><b>Deforestation Area: </b>", data$fl_area_ha_std, " ha"
        ) %>% lapply(HTML)
      ) %>%
      addLegend(
        pal = my_palette_dpto,
        values = data$percent_fl_std,
        title = "Deforestation Percent",
        position = "bottomright"
      )
  })
  
  observeEvent(input$drill_down, {
    data <- data_dist_deforestation()
    my_palette_dist <- colorNumeric(palette = pyforest_palette_df, domain = data$percent_fl_std)
    leafletProxy("leafdown_forest_loss") %>% clearShapes() %>%
      addPolygons(
        data = data,
        fillColor = ~my_palette_dist(data$percent_fl_std),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1,
        opacity = 1,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste0(
          "<b>Year: </b>", year,
          "<br><b>District: </b>", nom_dist,
          "<br><b>Department: </b>", nom_dpto,
          "<br><b>Deforestation Percent: </b>", data$percent_fl_std, " %",
          "<br><b>Deforestation Area: </b>", data$fl_area_ha_std, " ha"
        ) %>% lapply(HTML)
      )
  })
  
  
  observeEvent(input$drill_up, {
    data <- data_dept_deforestation()
    my_palette_dept <- colorNumeric(palette = pyforest_palette_df, domain = data$percent_fl_std)
    leafletProxy("leafdown_forest_loss") %>% clearShapes() %>%
      addPolygons(
        data = data,
        fillColor = ~my_palette_dept(data$percent_fl_std),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1,
        opacity = 1,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste0(
          "<b>Year: </b>", year,
          "<br><b>Department: </b>", nom_dpto,
          "<br><b> Deforestation Percent </b>", data$percent_fl_std, " %",
          "<br><b>Deforestation Area: </b>", data$fl_area_ha_std, " ha"
        ) %>% lapply(HTML)
      )
  })
  
  
  output$forest_loss_area_ha_plot <- renderPlotly({
    if (input$drill_down > 0) {
      data <- forest_loss_district_std_df %>% st_drop_geometry()
      
      fl_district_plot <-
        ggplot(data = data, aes(x = year, y = fl_area_ha_std, group = nom_dist, color = nom_dpto)) +
        geom_line(size = 1) +
        geom_point(aes(text = paste("<b>Year: </b>", year, "<br>",
                                    "<b>District: </b>", nom_dist, "<br>",
                                    "<b>Department: </b>", nom_dpto, "<br>",
                                    "<b>Deforestation Area: </b>", fl_area_ha_std, "ha", "<br>",
                                    "<b>Deforestation Percent: </b>", percent_fl_std, "%")),color = "transparent") +
        labs(#title = "<b>Deforestation Area (ha) over the Years by District</b>",
          x = "Year",
          y = "Deforestation Area (ha)",
          color = "Department") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_y_continuous(labels = scales::comma_format()) +
        scale_color_manual(values = c("#4B5F43", "#AEBD93", "#F26419")) +
        scale_x_discrete(breaks = seq(min(data$year), max(data$year), by = 2))
      fl_district_plot <- ggplotly(fl_district_plot, tooltip = "text")
      fl_district_plot <- layout(fl_district_plot, hoverlabel = list(bgcolor = "white"))
      
      
      
    } else if (input$drill_up > 0) {
      data <- forest_loss_std_df %>% st_drop_geometry()
      fl_department_plot <-
        ggplot(data = data, aes(x = year, y = fl_area_ha_std, group = nom_dpto, color = nom_dpto)) +
        geom_line(size = 1) +
        geom_point(aes(text = paste("<b>Year: </b>", year, "<br>",
                                    "<b>Department: </b>", nom_dpto, "<br>",
                                    "<b>Deforestation Area: </b>", fl_area_ha_std, "ha", "<br>",
                                    "<b>Deforestation Percent: </b>", percent_fl_std, "%")),color = "transparent") +
        labs(#title = "<b>Departments Area (ha) over the Years by Department</b>",
          x = "Year",
          y = "Deforestation Area (ha)",
          color = "Department") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_y_continuous(labels = scales::comma_format()) +
        scale_color_manual(values = c("#4B5F43", "#AEBD93", "#F26419")) +
        scale_x_discrete(breaks = seq(min(data$year), max(data$year), by = 2))
      fl_department_plot <- ggplotly(fl_department_plot, tooltip = "text")
      fl_department_plot <- layout(fl_department_plot, hoverlabel = list(bgcolor = "white"))
      
      
      
    } else {
      data <- forest_loss_std_df %>% st_drop_geometry()
      fl_department_plot <-
        ggplot(data = data, aes(x = year, y = fl_area_ha_std, group = nom_dpto, color = nom_dpto)) +
        geom_line(size = 1) +
        geom_point(aes(text = paste("<b>Year: </b>", year, "<br>",
                                    "<b>Department: </b>", nom_dpto, "<br>",
                                    "<b>Deforestation Area: </b>", fl_area_ha_std, "ha", "<br>",
                                    "<b>Deforestation Percent: </b>", percent_fl_std, "%")),color = "transparent") +
        labs(#title = "<b>Departments Area (ha) over the Years by Department</b>",
          x = "Year",
          y = "Deforestation Area (ha)",
          color = "Department") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_y_continuous(labels = scales::comma_format()) +
        scale_color_manual(values = c("#4B5F43", "#AEBD93", "#F26419")) +
        scale_x_discrete(breaks = seq(min(data$year), max(data$year), by = 2))
      fl_department_plot <- ggplotly(fl_department_plot, tooltip = "text")
      fl_department_plot <- layout(fl_department_plot, hoverlabel = list(bgcolor = "white"))
    }
  })
  
  
  
  
  output$forest_loss_area_percent_plot <- renderPlotly({
    if (input$drill_down > 0) {
      data <- forest_loss_district_std_df %>% st_drop_geometry()
      fl_district_plot <-
        ggplot(data = data, aes(x = year, y = percent_fl_std, group = nom_dist, color = nom_dpto)) +
        geom_line(size = 1) +
        geom_point(aes(text = paste("<b>Year: </b>", year, "<br>",
                                    "<b>District: </b>", nom_dist, "<br>",
                                    "<b>Department: </b>", nom_dpto, "<br>",
                                    "<b>Deforestation Percent: </b>", percent_forest_loss, "%", "<br>",
                                    "<b>Deforestation Area: </b>", fl_area_ha_std, "ha")),color = "transparent") +
        labs(#title = "<b>Deforestation Percent (%) over the Years by District</b>",
          x = "Year Range",
          y = "Deforestation Percent (%)",
          color = "Department") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_color_manual(values = c("#4B5F43", "#AEBD93", "#F26419")) +
        scale_x_discrete(breaks = seq(min(data$year), max(data$year), by = 2))
      
      fl_district_plot <- ggplotly(fl_district_plot, tooltip = "text")
      fl_district_plot <- layout(fl_district_plot, hoverlabel = list(bgcolor = "white"))
      
      
      
    } else if (input$drill_up > 0) {
      data <- forest_loss_std_df %>% st_drop_geometry()
      fl_department_plot <-
        ggplot(data = data, aes(x = year, y = percent_fl_std, group = nom_dpto, color = nom_dpto)) +
        geom_line(size = 1) +
        geom_point(aes(text = paste("<b>Year: </b>", year, "<br>",
                                    "<b>Department: </b>", nom_dpto, "<br>",
                                    "<b>Deforestation Percent: </b>", percent_fl_std, "%", "<br>",
                                    "<b>Deforestation Area: </b>", fl_area_ha_std, "ha")),color = "transparent") +
        labs(#title = "<b>Deforestation Percent (%) over the Years by Department</b>",
          x = "Year",
          y = "Deforestation Percent (%)",
          color = "Department") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_color_manual(values = c("#4B5F43", "#AEBD93", "#F26419")) +
        scale_x_discrete(breaks = seq(min(data$year), max(data$year), by = 2))
      fl_department_plot <- ggplotly(fl_department_plot, tooltip = "text")
      fl_department_plot <- layout(fl_department_plot, hoverlabel = list(bgcolor = "white"))
      
      
      
    } else {
      data <- forest_loss_std_df %>% st_drop_geometry()
      fl_department_plot <-
        ggplot(data = data, aes(x = year, y = percent_fl_std, group = nom_dpto, color = nom_dpto)) +
        geom_line(size = 1) +
        geom_point(aes(text = paste("<b>Year Range: </b>", year, "<br>",
                                    "<b>Department: </b>", nom_dpto, "<br>",
                                    "<b>Deforestation Percent: </b>", percent_fl_std, "%", "<br>",
                                    "<b>Deforestation Area: </b>", fl_area_ha_std, "ha")),color = "transparent") +
        labs(#title = "<b>Deforestation Percent (%) over the Years by Department</b>",
          x = "Year",
          y = "Deforestation Percent (%)",
          color = "Department") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_color_manual(values = c("#4B5F43", "#AEBD93", "#F26419")) +
        scale_x_discrete(breaks = seq(min(data$year), max(data$year), by = 2))
      fl_department_plot <- ggplotly(fl_department_plot, tooltip = "text")
      fl_department_plot <- layout(fl_department_plot, hoverlabel = list(bgcolor = "white"))
    }
  }) #END plotly

  # -----------------------------------------  Forest Cover Statistics -----------------------------------
 
  #  # Call the forest cover module
  # callModule(forestCoverModule, "forest_cover_module", 
  #            py_fc_dept = py_fc_dept, 
  #            py_fc_dist = py_fc_dist)
  

  py_fc_dept <- st_transform(py_fc_dept, crs = "+proj=longlat +datum=WGS84")
  py_fc_dist <- st_transform(py_fc_dist, crs = "+proj=longlat +datum=WGS84")
  pyforest_palette_fc <- c("#F26419", "#F6AE2D", "#AEBD93", "#4B5F43")


  filter_data <- function(data) { data %>% filter(year == input$years_selected_var) }


  # Create reactive data for department and district levels
  data_dept_forest_cover <- reactive({
    if (input$drill_downward == 0 && input$drill_upward == 0) {
      filter_data(py_fc_dept)
    } else if (input$drill_upward > 0) {
      filter_data(py_fc_dept)
    } else {
      filter_data(py_fc_dept)
    }
  })

  data_dist_forest_cover <- reactive({
    if (input$drill_downward > 0) {
      filter(py_fc_dist)
    }  else {
      filter(py_fc_dept)
    }
  })

  output$leafdown_forest_cover <- renderLeaflet({
    data_filtered <- data_dept_forest_cover()
    my_palette_dpto <- colorNumeric(palette = pyforest_palette_fc, domain = data_filtered$percent_fc)
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = data_filtered,
        fillColor = ~my_palette_dpto(data_filtered$percent_fc),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1,
        opacity = 1,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste0(
          "<b>Year: </b>", year,
          "<br><b>Department: </b>", nom_dpto,
          "<br><b>Forest Cover Percent: </b>", data_filtered$percent_fc, " %",
          "<br><b>Forest Cover Area: </b>", data_filtered$fc_area_ha, " ha"
        ) %>% lapply(HTML)
      ) %>%
      addLegend(
        pal = my_palette_dpto,
        values = data_filtered$percent_fc,
        title = "Forest Cover Percent",
        position = "bottomright"
      )
  })

  observeEvent(input$drill_downward, {
    data_filtered <- data_dist_forest_cover()
    my_palette_dist <- colorNumeric(palette = pyforest_palette_fc, domain = data_filtered$percent_fc)
    leafletProxy("leafdown_forest_cover") %>% clearShapes() %>%
      addPolygons(
        data = data_filtered,
        fillColor = ~my_palette_dist(data_filtered$percent_fc),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1,
        opacity = 1,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste0(
          "<b>Year: </b>", year,
          "<br><b>District: </b>", nom_dist,
          "<br><b>Department: </b>", nom_dpto,
          "<br><b>Forest Cover Percent: </b>", data_filtered$percent_fc, " %",
          "<br><b>Forest Cover Area: </b>", data_filtered$fc_area_ha, " ha"
        ) %>% lapply(HTML)
      )
  })


  observeEvent(input$drill_upward, {
    data_filtered <- data_dept_forest_cover()
    my_palette_dept <- colorNumeric(palette = pyforest_palette_fc, domain = data_filtered$percent_fc)
    leafletProxy("leafdown_forest_cover") %>% clearShapes() %>%
      addPolygons(
        data = data_filtered,
        fillColor = ~my_palette_dept(data_filtered$percent_fc),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1,
        opacity = 1,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste0(
          "<b>Year: </b>", year,
          "<br><b>Department: </b>", nom_dpto,
          "<br><b>Forest Cover Percent </b>", data_filtered$percent_fc, " %",
          "<br><b>Forest Cover Area: </b>", data_filtered$fc_area_ha, " ha"
        ) %>% lapply(HTML)
      )
  })


  output$forest_cover_area_ha_plot <- renderPlotly({

    if (input$drill_downward > 0) {
      data_filtered <- py_fc_dist %>% st_drop_geometry()
      data_filtered$year <- as.Date(paste(data_filtered$year, "-01-01", sep = ""))
      fc_district_plot <-
        ggplot(data = data_filtered, aes(x = year, y = fc_area_ha, group = nom_dist, color = nom_dpto)) +
        geom_line(size = 1) +
        geom_point(aes(text = paste("<b>Year: </b>", format(year, "%Y"), "<br>",
                                    "<b>District: </b>", nom_dist, "<br>",
                                    "<b>Department: </b>", nom_dpto, "<br>",
                                    "<b>Forest Cover Area: </b>", fc_area_ha, "ha", "<br>",
                                    "<b>Forest Cover Percent: </b>", percent_fc, "%")),color = "transparent") +
        labs(#title = "Forest Cover Over the Years by Department",
             x = "Year",
             y = "Forest Cover Area (ha)",
             color = "Department") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_y_continuous(labels = scales::comma_format()) +
        scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
        scale_color_manual(values = c("#4B5F43", "#AEBD93", "#F26419"))
      fc_district_plot <- ggplotly(fc_district_plot, tooltip = "text")
      fc_district_plot <- layout(fc_district_plot, hoverlabel = list(bgcolor = "white"))


    } else if (input$drill_upward > 0) {
      data_filtered <- py_fc_dept %>% st_drop_geometry()
      data_filtered$year <- as.Date(paste(data_filtered$year, "-01-01", sep = ""))
      fc_department_plot <-
        ggplot(data = data_filtered, aes(x = year, y = fc_area_ha, group = nom_dpto, color = nom_dpto)) +
        geom_line(size = 1) +
        geom_point(aes(text = paste("<b>Year: </b>", format(year, "%Y"), "<br>",
                                    "<b>Department: </b>", nom_dpto, "<br>",
                                    "<b>Forest Cover Area: </b>", fc_area_ha, "ha", "<br>",
                                    "<b>Forest Cover Percent: </b>", percent_fc, "%")),color = "transparent") +
        labs(#title = "Chaco Departments Forest Cover Over the Years",
             x = "Year",
             y = "Forest Cover Area (ha)",
             color = "Department") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
        scale_y_continuous(labels = scales::comma_format()) +
        scale_color_manual(values = c("#4B5F43", "#AEBD93", "#F26419"))
      fc_department_plot <- ggplotly(fc_department_plot, tooltip = "text")
      fc_department_plot <- layout(fc_department_plot, hoverlabel = list(bgcolor = "white"))


    } else {
      data_filtered <- py_fc_dept %>% st_drop_geometry()
      data_filtered$year <- as.Date(paste(data_filtered$year, "-01-01", sep = ""))
      fc_department_plot <-
        ggplot(data = data_filtered, aes(x = year, y = fc_area_ha, group = nom_dpto, color = nom_dpto)) +
        geom_line(size = 1) +
        geom_point(aes(text = paste("<b>Year: </b>", format(year, "%Y"), "<br>",
                                    "<b>Department: </b>", nom_dpto, "<br>",
                                    "<b>Forest Cover Area: </b>", fc_area_ha, "ha", "<br>",
                                    "<b>Forest Cover Percent: </b>", percent_fc, "%")),color = "transparent") +
        labs(#title = "Chaco Departments Forest Cover Over the Years",
             x = "Year",
             y = "Forest Cover Area (ha)",
             color = "Department") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
        scale_y_continuous(labels = scales::comma_format()) +
        scale_color_manual(values = c("#4B5F43", "#AEBD93", "#F26419"))
      fc_department_plot <- ggplotly(fc_department_plot, tooltip = "text")
      fc_department_plot <- layout(fc_department_plot, hoverlabel = list(bgcolor = "white"))
    }
  })

  output$forest_cover_area_percent_plot <- renderPlotly({

    if (input$drill_downward > 0) {
      data_filtered <- py_fc_dist %>% st_drop_geometry()
      data_filtered$year <- as.Date(paste(data_filtered$year, "-01-01", sep = ""))
      fc_district_plot <-
        ggplot(data = data_filtered, aes(x = year, y = percent_fc, group = nom_dist, color = nom_dpto)) +
        geom_line(size = 1) +
        geom_point(aes(text = paste("<b>Year: </b>", format(year, "%Y"), "<br>",
                                    "<b>District: </b>", nom_dist, "<br>",
                                    "<b>Department: </b>", nom_dpto, "<br>",
                                    "<b>Forest Cover Percent: </b>", percent_fc, "%", "<br>",
                                    "<b>Forest Cover Area: </b>", fc_area_ha, "ha")),color = "transparent") +
        labs(#title = "Chaco Districts Forest Cover Over the Years",
             x = "Year",
             y = "Forest Cover Percent (%)",
             color = "Department") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
        scale_color_manual(values = c("#4B5F43", "#AEBD93", "#F26419"))
      fc_district_plot <- ggplotly(fc_district_plot, tooltip = "text")
      fc_district_plot <- layout(fc_district_plot, hoverlabel = list(bgcolor = "white"))



    } else if (input$drill_upward > 0) {
      data_filtered <- py_fc_dept %>% st_drop_geometry()
      data_filtered$year <- as.Date(paste(data_filtered$year, "-01-01", sep = ""))
      fc_department_plot <-
        ggplot(data = data_filtered, aes(x = year, y = percent_fc, group = nom_dpto, color = nom_dpto)) +
        geom_line(size = 1) +
        geom_point(aes(text = paste("<b>Year: </b>", format(year, "%Y"), "<br>",
                                    "<b>Department: </b>", nom_dpto, "<br>",
                                    "<b>Forest Cover Percent: </b>", percent_fc, "%", "<br>",
                                    "<b>Forest Cover Area: </b>", fc_area_ha, "ha")),color = "transparent") +
        labs(#title = "Chaco Departments Forest Cover Over the Years",
             x = "Year",
             y = "Forest Cover Percent (%)",
             color = "Department") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
        scale_color_manual(values = c("#4B5F43", "#AEBD93", "#F26419"))
      fc_department_plot <- ggplotly(fc_department_plot, tooltip = "text")
      fc_department_plot <- layout(fc_department_plot, hoverlabel = list(bgcolor = "white"))



    } else {
      data_filtered <- py_fc_dept %>% st_drop_geometry()
      data_filtered$year <- as.Date(paste(data_filtered$year, "-01-01", sep = ""))
      fc_department_plot <-
        ggplot(data = data_filtered, aes(x = year, y = percent_fc, group = nom_dpto, color = nom_dpto)) +
        geom_line(size = 1) +
        geom_point(aes(text = paste("<b>Year: </b>", format(year, "%Y"), "<br>",
                                    "<b>Department: </b>", nom_dpto, "<br>",
                                    "<b>Forest Cover Percent: </b>", percent_fc, "%", "<br>",
                                    "<b>Forest Cover Area: </b>", fc_area_ha, "ha")),color = "transparent") +
        labs(#title = "Chaco Departments Forest Cover Over the Years",
             x = "Year",
             y = "Forest Cover Percent (%)",
             color = "Department") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
        scale_color_manual(values = c("#4B5F43", "#AEBD93", "#F26419"))


      fc_department_plot <- ggplotly(fc_department_plot, tooltip = "text")
      fc_department_plot <- layout(fc_department_plot, hoverlabel = list(bgcolor = "white"))
    }
  })
  
  
  # ------------------------------------------ LUP Simulations ------------------------------------------
  
  # # Create a dynamic UI element in the server.R file using renderUI
  # output$name_selection <- renderUI({
  #   if (input$dataset == "department") {
  #     selectInput("name",
  #                 label = "Choose department:",
  #                 choices = c("PDTE. HAYES", "BOQUERON", "ALTO PARAGUAY"))
  #   } else if (input$dataset == "district") {
  #     selectInput("name",
  #                 label = "Choose district:",
  #                 choices = c("BAHIA NEGRA", "BENJAMIN ACEVAL", "BOQUERON",
  #                             "CAMPO ACEVAL", "CARMELO PERALTA", "FILADELFIA",
  #                             "FUERTE OLIMPO", "GRAL JOSE MARIA BRUGUEZ",
  #                             "JOSE FALCON", "LOMA PLATA", "MCAL. ESTIGARRIBIA",
  #                             "NANAWA", "NUEVA ASUNCION", "PUERTO CASADO",
  #                             "PUERTO PINASCO", "TTE 1RO MANUEL IRALA FERNANDEZ",
  #                             "TTE. ESTEBAN MARTINEZ", "VILLA HAYES"))
  #   }
  # })
  # 
  # # Land use simulation plot
  # output$landUsePlot <- renderPlotly({
  #   plot_land_use_type_stackedh(input$dataset, input$name)
  # })
  
  # ------------------------------------------ Land Use Plan Simulation & Deforestation Predictions ------------------------------------------
  simulation_types <- unique(combined_data$simulation)
  
  output$lup_simulation_example <- renderUI({
    # Display images based on the selected simulation type
    if (input$simulation_type == "Current Forest Law") {
      tagList(
        tags$h4("Current Forest Law"),
        tags$img(src = "current_forest_law_lup_example.png", width = "100%")
      )
    } else if (input$simulation_type == "Law Ambiguity") {
      tagList(
        tags$h4("Law Ambiguity"),
        tags$img(src = "law_ambiguity_simulation_example.png", width = "100%")
      )
    } else if (input$simulation_type == "Prioritize Economic Development") {
      tagList(
        tags$h4("Prioritize Economic Development"),
        tags$img(src = "prioritize_econ_development_lup_example.png", width = "100%")
      )
    } else if (input$simulation_type == "Promotes Forest Conservation") {
      tagList(
        tags$h4("Promotes Forest Conservation"),
        tags$img(src = "forest_conservation_lup_example.png", width = "100%")
      )
    } else {
      NULL
      print("May only view one LUP simulation at a time.")
    }
  })
  
  
  #histogram_sim_pred_land_use
  output$histogram_sim_pred_land_use <- renderPlotly({
    # Filter the data based on the selected simulation type
    filtered_data <- if (input$simulation_type == "All") {
      combined_data
    } else {
      combined_data[combined_data$simulation == input$simulation_type, ]
    }
    
    # Create the ggplot object
    ggplot_obj <- ggplot(filtered_data, aes(x = simulation_type, y = Area, fill = LandUseTypeStatus)) +
      geom_bar(stat = 'identity', position = 'stack', color = "NA", linewidth = 0.25) +
      scale_fill_manual(values = color_mapping_deforestation) +
      labs(x = "", y = "Area (ha)", fill = "Land Use Type Status") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.background = element_rect(fill = "transparent"),
            legend.position = "top") +
      coord_flip()
    
    # Convert ggplot to Plotly object
    ggplotly(ggplot_obj)
  })
  
  
  
  output$lup_simulation_images <- renderUI({
    # Display images based on the selected simulation type
    if (input$simulation_type == "Current Forest Law") {
      tagList(
        tags$h4("Current Forest Law"),
        tags$img(src = "current_forest_law_lup_simulation.png", width = "74%")
      )
    } else if (input$simulation_type == "Law Ambiguity") {
      tagList(
        tags$h4("Law Ambiguity"),
        tags$img(src = "law_ambiguity_lup_simulation.png", width = "74%")
      )
    } else if (input$simulation_type == "Prioritize Economic Development") {
      tagList(
        tags$h4("Prioritize Economic Development"),
        tags$img(src = "prioritize_cattle_production_lup_simulation.png", width = "74%")
      )
    } else if (input$simulation_type == "Promotes Forest Conservation") {
      tagList(
        tags$h4("Promotes Forest Conservation"),
        tags$img(src = "promotes_forest_conservation_lup_simulation.png", width = "74%")
      )
    } else {
      NULL
      print("Select only one scenario to compare simulated vs predicted land use.")
    }
  })
  
  output$prediction_images <- renderUI({
    # Display images based on the selected simulation type
    if (input$simulation_type == "Current Forest Law") {
      tagList(
        tags$h4("Current Forest Law"),
        tags$img(src = "current_forest_law_deforestation_pred.png", width = "100%")
      )
    } else if (input$simulation_type == "Law Ambiguity") {
      tagList(
        tags$h4("Law Ambiguity"),
        tags$img(src = "law_ambiguity_pred.png", width = "100%")
      )
    } else if (input$simulation_type == "Prioritize Economic Development") {
      tagList(
        tags$h4("Prioritize Economic Development"),
        #tags$img(src = "prioritize_cattle_production_deforestation_pred.png", width = "100%")
        print("Need this image.")
      )
    } else if (input$simulation_type == "Promotes Forest Conservation") {
      tagList(
        tags$h4("Promotes Forest Conservation"),
        tags$img(src = "promotes_forest_conservation_pred.png", width = "100%")
      )
    } else {
      NULL
      print("Select only one scenario to compare simulated vs predicted land use.")
    }
  })
  
  
  
  
  
  
  
  
  
} # END SERVER





