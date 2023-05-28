# Create the theme
pytheme <- create_theme(
  adminlte_color(
    light_blue = "#2F4858" ##3c4d36
  ),
  adminlte_sidebar(
    width = "350px", # Set the width of the sidebar
    dark_bg = "#3c4d36", # Set the dark background color of the sidebar #4d5c47
    dark_hover_bg = "#868f82" # Set the dark hover background color of the sidebar
  ),
  adminlte_global(
    box_bg = "white" # Set the background color of the global elements
  )
)

# ------------------------------------------ header ------------------------------------------
# Create the header section of the dashboard
header <- dashboardHeader(
  title = HTML('<span style="color: white;">INFONA Policy Decision-Making Tool</span>'),
  disable = FALSE,
  titleWidth = 500,
  dropdownMenu(
    type = 'messages',
    headerText = "Share it",
    icon = icon("share-alt"),
    messageItem(
      from = 'Twitter',
      message = "",
      icon = icon("twitter"),
      href = "https://twitter.com/intent/tweet?url=https%3A%2F%2Fgithub.com%2Fcp-PYFOREST%2FPYFOREST-Shiny&text=Check%20out%20the%20repository%20for%20the%20Paraguayan%20%20Chaco%20Interactive%20Tool%20https%3A%2F%2Fgithub.com%2Fcp-PYFOREST%2FPYFOREST-Shiny&hashtags=TNC%20%23RShiny"
    ),
    messageItem(
      from = 'LinkedIn',
      message = "",
      icon = icon("linkedin"),
      href = "https://www.linkedin.com/sharing/share-offsite/?url=https%3A%2F%2Fgithub.com%2Fcp-PYFOREST%2FPYFOREST-Shiny"
    ) # END messageItem
  ) # END dropdownMenu
) # END dashboardHeader

# ------------------------------------------ sidebar ------------------------------------------
# Create the sidebar section of the dashboard
sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem(
      text = "Home",
      tabName = "home",
      icon = icon("house"),
      startExpanded = TRUE,
      menuSubItem('About the app', tabName = 'about_the_app', icon = icon('circle-info')),
      menuSubItem('Data Source', tabName = 'data_source', icon = icon('server')),
      menuSubItem('Project Information', tabName = 'project_information', icon = icon('github'))
    ),
    menuItem(
      text = "Land Use Plan Assessment",
      tabName = "assessment",
      icon = icon("clipboard-check"),
      startExpanded = FALSE,
      menuSubItem('By Political Boundary', tabName = 'political_boundary', icon = icon('earth-americas')),
      menuSubItem('By PUT ID', tabName = 'put_id', icon = icon('draw-polygon'))
    ),
    menuItem(
      text = "Deforestation and Forest Cover Statistics",
      tabName = "deforestation_statistics",
      icon = icon("chart-line"),
      menuSubItem('Deforestation Statistics', tabName = 'deforestation_stats', icon = icon('chart-line')),
      menuSubItem('Forest Cover Statistics', tabName = 'forest_cover_stats', icon = icon('chart-line'))
    ),
    menuItem(
      text = "Land Use Plan Simulations",
      tabName = 'simulations',
      icon = icon('chart-simple')
    ),
    menuItem(
      text = "Deforestation Predictions",
      tabName = 'predictions',
      icon = icon('globe')
    )
  ) # END sidebarMenu
) # END dashboardSidebar

# ------------------------------------------ body ------------------------------------------
# Create the body section of the dashboard
body <- dashboardBody(
  
  use_theme(pytheme),
  
  tabItems(
    # about the app tabItem
    tabItem(tabName = "about_the_app",
            fluidPage(
              tabsetPanel(
                id = "dashboard_tabsetPanel",
                tabPanel(title = "About the app",
                         fluidRow(
                           tags$img(
                             class = "banner",
                             src = "chaco.jpg",
                             width = "90%",
                             alt = "A landscape photo of the Paraguayan Chaco. A river is in the foreground with trees in the background."
                           ),
                           title = tags$strong("About the app"),
                           includeMarkdown("text/about_the_app.md")
                         ),
                         fluidRow(
                           includeMarkdown("text/home_page_footer.md")
                         )
                ),
                tabPanel(title = "Land Use Plan Assessment",
                         fluidRow(
                           includeMarkdown("text/land_use_assessment.md")
                         )
                ),
                tabPanel(title = "Deforestation and Forest Cover Statistics",
                         fluidRow(
                           includeMarkdown("text/df_fc_stats.md")
                         )
                ),
                tabPanel(title = "Land Use Plan Simulations",
                         fluidRow(
                           includeMarkdown("text/land_use_simulation.md")
                         )
                ),
                tabPanel(title = "Deforestation Predictions",
                         fluidRow(
                           includeMarkdown("text/df_prediction.md")
                         )
                )
              ) # END tabsetPanel
            ) # END fluidPage
    ),
    
    # data source tabItem
    tabItem(tabName = "data_source",
            includeMarkdown("text/data_source.md")
    ),
    
    # project information tabItem
    tabItem(tabName = "project_information",
            includeMarkdown("text/project_information.md")
    ),
    
    # political boundary tabItem
    tabItem(tabName = "political_boundary",
            tabsetPanel(id = "deforestation_tabsetPanel",
                        tabPanel(title = "Unauthorized Deforestation",
                                 
                                 fluidRow(
                                   div(
                                     style = "border: 1px solid #ddd; margin-bottom: 10px; padding: 10px;",
                                     fluidRow(
                                       column(width = 12,
                                              tags$style(HTML(".leaflet-container {background: #ffffff;}")),
                                              h4(tags$strong("Unauthorized Deforestation by Political Boundaries")),
                                              actionButton("drill_up", "View Departments"),
                                              actionButton("drill_down", "View Districts"),
                                              selectInput("year_range", "Select Year Range", unique(combined_illegal_df_by_dpto$year_range)),
                                              leafletOutput("leafdown", height = "350px")
                                       ),
                                       
                                       box(title = tagList(tags$strong("Unauthorized Deforestation")),
                                           width = 6,
                                           plotlyOutput("illegalPlot", height = "400px")
                                       ),
                                       box(title = tagList(tags$strong("Change in Unauthorized Deforestation Over Time")),
                                           width = 6,
                                           plotlyOutput("areaPlot", height = "400px")
                                       )
                                     )))
                        ),
                        tabPanel(title = "Authorized Deforestation",
                                 fluidRow(
                                   # box(
                                   #   width = 6,
                                   #   tags$style(HTML(".leaflet-container {background: #ffffff;}")),
                                   #   actionButton("drill_up", "View Departments"),
                                   #   actionButton("drill_down", "View Districts"),
                                   #   selectInput("year_range", "Select Year Range", unique(combined_illegal_df_by_dpto$year_range)),
                                   #   leafletOutput("leafdown", height = "600px")
                                   # ),
                                   # box(
                                   #   title = tagList(tags$strong("Illegal Deforestation Bar Plot")),
                                   #   width = 6,
                                   #   plotlyOutput("illegalPlot")
                                   # )
                                 )
                        )
            )
    ),
    # put_id tabItem
    tabItem(tabName = "put_id",
            fluidPage(
              #theme = bslib::bs_theme(bootswatch = "morph"),
              titlePanel("Property Compliance"),
              sidebarLayout(
                sidebarPanel(
                  width = 4,
                  # input -----
                  pickerInput(inputId = "code", label = "Select PUT ID",
                              choices = unique(compliance$put_id),
                              options = pickerOptions(
                                liveSearch= TRUE,
                                title = "PUT0000",
                                limit = 5)
                  ),
                  actionButton(inputId = "reset_button", label = "Reset"),
                  
                  # button for download report
                  downloadButton("download_report", "Download Report")
                ),
                mainPanel(
                  width = 8,
                  fluidRow(
                    column(
                      width = 12,
                      br(),
                      div(
                        style = 'overflow-x: scroll; height: auto;',
                        # output 1 map ----
                        leafletOutput(outputId = "map")
                      )
                    )
                  )
                  ,
                  fluidRow(
                    # output 2 table ----
                    DT::dataTableOutput("table")
                  )
                  
                )
              )
            )
    ),
    
    # deforestation_stats tabItem
    tabItem(tabName = "deforestation_stats",
            # put_id content here
    ),
    
    # forest_cover_statstabItem
    tabItem(tabName = "forest_cover_stats",
            # put_id content here
    ),
    
    # simulations tabItem
    tabItem(tabName = "simulations",
            fluidPage(
              titlePanel("Land Use Stacked Bar Chart"),
              
              sidebarLayout(
                sidebarPanel(
                  selectInput("dataset",
                              label = "Political Boundary:",
                              choices = c("department", "district"),
                              selected = "department"),
                  
                  uiOutput("name_selection")
                ),
                
                mainPanel(
                  plotlyOutput("landUsePlot")
                )
              )
            ) # END fluidPage
    ), # END simulations tabItem
    
    
    # put_id tabItem
    tabItem(tabName = "predictions",
            # predictions content here
    ) # END predictions tabItem
    
  ), # END tabItems
  
) # END dashboardBody

# ------------------------------------------ app ------------------------------------------
# Create the dashboard page by combining the header, sidebar, and body sections
dashboardPage(header, sidebar, body)
