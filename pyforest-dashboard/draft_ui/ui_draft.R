#ui.R

# # Create the theme
pytheme <- create_theme(
  adminlte_color(
    light_blue = "#3c4d36"
  ),
  adminlte_sidebar(
    width = "350px",
    dark_bg = "#3c4d36",
    dark_hover_bg = "#586e4f"
    #dark_color = "white" #text color in sidebar
  ),
   adminlte_global(
  #   content_bg = "#FFF",
     box_bg = "white"
  #   info_box_bg = "#EDF3ED" #light green
   )
)# END pyforest theme 

# ------------------------------------------ header ------------------------------------------
header <- dashboardHeader(
  # title = span(img(src="pyforest_logo.png", width = 40,
  #              href = "https://www.infona.gov.py/", target = "_blank"), # target = "_blank" opens link in new tab
  #              span("PYFOREST Interactive Dashboard", style = "font-size: 18px;"))
  title = HTML('<span style="color: white;">INFONA Policy Decision-Making Tool</span>'),
  disable = FALSE,
  titleWidth = 500,
  dropdownMenu(type = 'messages',
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
               )
  
) # END dropdownMenu
) # END dashboardHeader

# ------------------------------------------ sidebar ------------------------------------------
sidebar <- dashboardSidebar(
  # sidebarMenu ----
  sidebarMenu(
    menuItem(text = "Home", tabName = "home", icon = icon("house"), startExpanded = TRUE,
             menuSubItem('About the app', tabName = 'about_the_app', icon = icon('circle-info')),
             menuSubItem('Data Source', tabName = 'data_source', icon = icon('server')),
             menuSubItem('Project Information',tabName = 'project_information',icon = icon('github'))),
    menuItem(text = "Land Use Plan Assessment", tabName = "assessment", icon = icon("clipboard-check"), startExpanded = FALSE,
             menuSubItem('By Political Boundary', tabName = 'political_boundary', icon = icon('earth-americas')),
             menuSubItem('By PUT ID', tabName = 'put_id', icon = icon('draw-polygon'))),
    menuItem(text = "Deforestation Statistics", tabName = "deforestation_statistics", icon = icon("chart-line")),
    menuItem(text = "Forest Cover Statistics", tabName = "fc_statistics", icon = icon("chart-line")),
    menuItem(text = "Land Use Plan Simulations", tabName = 'simulations', icon = icon('chart-simple')),
    menuItem(text = "Deforestation Predictions", tabName = 'predictions', icon = icon('globe'))
    
  ) # END sidebarMenu
) # END dashboardSidebar

# ------------------------------------------ body ------------------------------------------
body <- dashboardBody(
  
  #use the pytheme
  use_theme(pytheme), 
  
  # tabItems
  tabItems(
    
    # about the app tabItem ----
    tabItem(tabName = "about_the_app",
            
            #fluidPage  ----
            fluidPage(
              #titlePanel("Forest Conservation Interactive Dashboard"),
              
              # dashboard tabsetPanel ----
              tabsetPanel(id = "dashboard_tabsetPanel",
                          
                          # about the app tabPanel ----
                          tabPanel(title = "About the app",
                                   
                                   
                                   # fluidRow with intro & getting data text boxes ----
                                    fluidRow(
                                      
                                      tags$img(class = "banner", src = "chaco.jpg",
                                               width = "90%",
                                               alt = "A landscape photo of the Paraguayan Chaco. A river is in the foreground with trees in the background."),
                                      
                                   
                                      title = tags$strong("About the app"),
                                             includeMarkdown("text/intro.md")
                                      
                                    ), # END fluidRow
                                   
                                 
                                   # fluidRow with footer ----
                                   fluidRow(
                                     
                                     includeMarkdown("text/home_page_footer.md")
                                     
                                   ), # END fluidRow
                                   
                          ), # END about the app tabPanel
                          
                          # Land Use Plan Assessment tabPanel ----
                          tabPanel(title = "Land Use Plan Assessment",
                                   
                                   # fluidRow with footer ----
                                   fluidRow(
                                     
                                     includeMarkdown("text/land_use_assessment.md")
                                     
                                   ), # END fluidRow
                                   
                          ), # END Land Use Plan Assessment tabPanel
                          
                          # Deforestation and Forest Cover Statistics tabPanel ----
                          tabPanel(title = "Deforestation and Forest Cover Statistics",
                                   
                                   "df and fc info"
                                   
                          ), # END Deforestation and Forest Cover Statistics tabPanel
                          
                          # Land Use Plan Simulations tabPanel ----
                          tabPanel(title = "Land Use Plan Simulations",
                                   
                                   "lup simulations info"
                                   
                          ), # END Land Use Plan Simulations tabPanel
                          
                          # Deforestation Predictions tabPanel ----
                          tabPanel(title = "Deforestation Predictions",
                                   
                                   "df prediction info"
                                   
                          ), # END Deforestation Predictions tabPanel
                          
              ), # END about the app tabsetPanels
            
            ), # END fluidPage
            
    ), # END about the app tabItem
    
    
    # data source tabItem ----
    tabItem(tabName = "data_source",
            
            includeMarkdown("text/data_source.md")
            
    ), # END source tabItem
    
    
    # project information tabItem ----
    tabItem(tabName = "project_information",
            
            "Insert PYFOREST info here"
            
    ), # END project information tabItem
    
   ### Alex
   
   # tabItems
   tabItems(
     
     # political boundary tabItem ----
     tabItem(tabName = "politcal_boundary",
   
    # #assessment tabItem ----
    # tabItem(tabName = "assessment",

            # fluidRow ----
            fluidRow(
              
              # # input box ----
              box(title = tagList(tags$strong("Illegal Deforestation Choropleth Map")),
                  width = 6, 

              tags$style(HTML(".leaflet-container {background: #ffffff;}")),
              actionButton("drill_up", "View Departments"),
              actionButton("drill_down", "View Districts"),
              selectInput("year_range", "Select Year Range", unique(combined_illegal_df_by_dpto$year_range)),
              leafletOutput("leafdown", height = "600px")
              #     
               ), # END input box
              
              
              # plot box ----
              box(title = tagList(tags$strong("Illegal Deforestation Bar Plot")),
                  width = 6, 
              plotlyOutput("illegalPlot")
              ), # END plot box

              # # input box ----
              # box(width = 4, 
              #     title = tags$strong("Political Boundaries:"),
              #     
              #     # selectInput for PB----
              #     selectInput("department", "Select a department:", choices = unique(illegal_df$nom_dpto)),
              #     selectInput("district", "Select a district:", choices = NULL)
              #     
              # ), # END input box
              # 
              # # plot and table box ----
              # box(width = 8,
              #     
              #     title = tags$strong("Illegal Deforestation by Political Boundary:"),
              #     
              #     # map output ----
              #     leafletOutput(outputId = "illegal_df_map") |>
              #       withSpinner(type = 1,
              #                   color = "#4b5f43"),
              #     
              #     #plot output ----
              #     plotOutput(outputId = "illegal_df_output_plot") |>
              #       withSpinner(type = 1,
              #                   color = "#4b5f43"),
              #     #Add a download button
              #     #downloadButton("downloadplot", "Download Plot"),
              #     
              #     # table output ____
              #     dataTableOutput(outputId = "illegal_df_output") |>
              #       withSpinner(type = 1,
              #                   color = "#4b5f43")
              #     # Add a download button
              #     #downloadButton("downloadTable", "Download Table")
              #     
              # ) # END map, plot, and table box
              
            ), # END fluidRow
            
    ), # END political boundary tabItem
    
    # put_id tabItem ----
    tabItem(tabName = "put_id",
            
    ), # END put_id tabItem
    
   ),

  
   
   ### Dalila 
    # deforestation statistics tabItem ----
    tabItem(tabName = "deforestation_statistics",
            
            # fluidRow ----
            fluidRow(
              
              # input box ----
              box(width = 4,
                  title = tags$strong("Deforestation by Political Boundary:")
                  
                  # # selectInput for PB----
                  # selectInput("department_deforestation", "Select a department:", choices = unique(deforestation_fake$department)),
                  # selectInput("district_deforestation", "Select a district:", choices = NULL)
                  
              ), # END input box
              
              
              # deforestation plot box ----
              box(width = 8,
                  
                  title = tags$strong("Deforestation by Political Boundary:"),
                  
                  # plot output ----
                  plotOutput(outputId = "deforestation_output_plot") |>
                    withSpinner(type = 1,
                                color = "#4b5f43")
                  # Add a download button
                  #downloadButton("downloadplot", "Download Plot")
                  
              ), # END deforestation plot box
              
              
              # tmap box ----
              box(width = 8,
                  
                  title = tags$strong("Deforestation by Political Boundary:"),
                  
                  #tmap output ----
                  tmapOutput(outputId = "map_output") |>
                    withSpinner(type = 1,
                                color = "#4b5f43")
                  
              ) # END tmap box
              
              
            ) # END fluidRow
            
    ), # END deforestation statistics tabItem
    
    
    # fc_statistics tabItem ----
    tabItem(tabName = "fc_statistics",
            
            # fluidRow ----
            fluidRow(
              
              # input box ----
              box(width = 4,
                  title = tags$strong("Forest Cover by Political Boundary:")
                  
                  # # selectInput for PB----
                  # selectInput("department_deforestation", "Select a department:", choices = unique(deforestation_fake$department)),
                  # selectInput("district_deforestation", "Select a district:", choices = NULL)
                  
              ), # END input box
              
              
              # fc_stats plot box ----
              box(width = 8,
                  
                  title = tags$strong("Forest Cover by Political Boundary:"),
                  
                  # plot output ----
                  plotOutput(outputId = "fc_output_plot") |>
                    withSpinner(type = 1,
                                color = "#4b5f43")
                  
              ), # END fc_stats plot box
              
              
              # tmap box ----
              box(width = 8,
                  
                  title = tags$strong("Forest Cover by Political Boundary:")
                  
                  # #tmap output ----
                  # tmapOutput(outputId = "map_output") |>
                  #   withSpinner(type = 1,
                  #               color = "#4b5f43")
                  
              ) # END tmap box
              
              
            ) # END fluidRow
            
    ), # END fc_statistics tabItem
    
   
    # land use simulation tabItem ----
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
            )
            
    ), # END land use simulation tabItem
    
    
    # deforestation predictions tabItem ----
    tabItem(tabName = "predictions",
            
            "Insert deforestation predictions info here"
            
    ) # END deforestation predictions tabItem
    
    
  ), # END tabItems
  
) # END dashboardBody

# combine all -----------------------
dashboardPage(header, sidebar, body)


