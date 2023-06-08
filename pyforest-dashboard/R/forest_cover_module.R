library(ggplot2)
library(plotly)
library(tidyverse)
library(leaflet)
library(shiny)
library(shinydashboard)

# Module UI function
forestCoverModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Define the UI components for the forest cover module
    fluidRow(
      box(width = 12,
          title = tags$strong("Forest Cover by Political Boundaries"),
          column(width = 12, actionButton("drill_upward", "View Departments"),
                 actionButton("drill_downward", "View Districts")),
          column(width =12,
                 print(tags$strong("Switch between political boundaries by selecting departments or districts to observe forest cover statistics.")))
      ),
      box(
        width = 12,
        column(width = 2, selectInput("years_selected_var", "Select a Year", unique(py_fc_dept$year))),
        withSpinner(leafletOutput("leafdown_forest_cover")),
        tags$style(HTML(".leaflet-container {background: #ffffff;}"))
      ),
      box(width = 6,
          title = tags$strong("Forest Cover Area (ha):"),
          withSpinner(plotly::plotlyOutput(outputId = "forest_cover_area_ha_plot"))
      ),
      box(width = 6,
          title = tags$strong("Forest Cover Percent (%):"),
          withSpinner(plotly::plotlyOutput(outputId = "forest_cover_area_percent_plot"))
      )
    )
  )
}

# Server function
forestCoverModule <- function(input, output, session, py_fc_dept, py_fc_dist) {
  
  # Perform necessary transformations on the input datasets
  py_fc_dept <- st_transform(py_fc_dept, crs = "+proj=longlat +datum=WGS84")
  py_fc_dist <- st_transform(py_fc_dist, crs = "+proj=longlat +datum=WGS84")
  pyforest_palette_fc <- c("#F26419", "#F6AE2D", "#AEBD93", "#4B5F43")
  
  filter_data <- function(data) {
    data %>% filter(year == input$years_selected_var)
  }
  
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
      filter_data(py_fc_dist)
    } else {
      filter_data(py_fc_dept)
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
          "<br><b>Forest Cover Percent: </b>", percent_fc, " %",
          "<br><b>Forest Cover Area: </b>", fc_area_ha, " ha"
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
          "<br><b>Forest Cover Percent: </b>", percent_fc, " %",
          "<br><b>Forest Cover Area: </b>", fc_area_ha, " ha"
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
          "<br><b>Forest Cover Percent </b>", percent_fc, " %",
          "<br><b>Forest Cover Area: </b>", fc_area_ha, " ha"
        ) %>% lapply(HTML)
      )
  })
  
  output$forest_cover_area_ha_plot <- renderPlotly({
    data_filtered <- data_dept_forest_cover()
    data_filtered$year <- as.Date(paste(data_filtered$year, "-01-01", sep = ""))
    
    if (input$drill_downward > 0) {
      data_filtered <- filter_data(py_fc_dist)
      plot_title <- "Chaco Districts Forest Cover Over the Years"
    } else if (input$drill_upward > 0) {
      data_filtered <- filter_data(py_fc_dept)
      plot_title <- "Chaco Departments Forest Cover Over the Years"
    } else {
      data_filtered <- filter_data(py_fc_dept)
      plot_title <- "Chaco Departments Forest Cover Over the Years"
    }
    
    fc_plot <-
      ggplot(data = data_filtered, aes(x = year, y = fc_area_ha, group = nom_dist, color = nom_dpto)) +
      geom_line(size = 1) +
      geom_point(aes(text = paste("<b>Year: </b>", format(year, "%Y"), "<br>",
                                  "<b>District: </b>", nom_dist, "<br>",
                                  "<b>Department: </b>", nom_dpto, "<br>",
                                  "<b>Forest Cover Area: </b>", fc_area_ha, " ha", "<br>",
                                  "<b>Forest Cover Percent: </b>", percent_fc, "%")), color = "transparent") +
      labs(
        title = plot_title,
        x = "Year",
        y = "Forest Cover Area (ha)",
        color = "Department"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_y_continuous(labels = scales::comma_format()) +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      scale_color_manual(values = c("#4B5F43", "#AEBD93", "#F26419"))
    
    ggplotly(fc_plot, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })
  
  output$forest_cover_area_percent_plot <- renderPlotly({
    data_filtered <- data_dept_forest_cover()
    data_filtered$year <- as.Date(paste(data_filtered$year, "-01-01", sep = ""))
    
    if (input$drill_downward > 0) {
      data_filtered <- filter_data(py_fc_dist)
      plot_title <- "Chaco Districts Forest Cover Over the Years"
    } else if (input$drill_upward > 0) {
      data_filtered <- filter_data(py_fc_dept)
      plot_title <- "Chaco Departments Forest Cover Over the Years"
    } else {
      data_filtered <- filter_data(py_fc_dept)
      plot_title <- "Chaco Departments Forest Cover Over the Years"
    }
    
    fc_plot <-
      ggplot(data = data_filtered, aes(x = year, y = percent_fc, group = nom_dist, color = nom_dpto)) +
      geom_line(size = 1) +
      geom_point(aes(text = paste("<b>Year: </b>", format(year, "%Y"), "<br>",
                                  "<b>District: </b>", nom_dist, "<br>",
                                  "<b>Department: </b>", nom_dpto, "<br>",
                                  "<b>Forest Cover Percent: </b>", percent_fc, "%", "<br>",
                                  "<b>Forest Cover Area: </b>", fc_area_ha, " ha")), color = "transparent") +
      labs(
        title = plot_title,
        x = "Year",
        y = "Forest Cover Percent (%)",
        color = "Department"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      scale_color_manual(values = c("#4B5F43", "#AEBD93", "#F26419"))
    
    ggplotly(fc_plot, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })
}
