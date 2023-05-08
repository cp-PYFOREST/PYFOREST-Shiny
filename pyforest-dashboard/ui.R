#ui.R

# # Create the theme
# pytheme <- create_theme(
#   adminlte_color(
#     light_blue = "#4b5f43" #dark green
#   ),
#   adminlte_sidebar(
#     width = "400px",
#     dark_bg = "#EDF3ED", #light green
#     dark_hover_bg = "#778570", #green from canva background
#     dark_color = "#2E3440" #color not being used?
#   ),
#   adminlte_global(
#     content_bg = "#FFF",
#     box_bg = "#EDF3ED", #light green
#     info_box_bg = "#EDF3ED" #light green
#   )
# )


# END pyforest theme 

# dashboard header -----------------------
header <- dashboardHeader(
  title = span(img(src="infona_logo.png", width = 40,
               href = "https://www.infona.gov.py/", target = "_blank"), # target = "_blank" opens link in new tab
               span("PYFOREST Interactive Dashboard", style = "font-size: 18px;"))

) # END dashboardHeader

# dashboard sidebar -----------------------
sidebar <- dashboardSidebar(
  # sidebarMenu ----
  sidebarMenu(
    menuItem(text = "Home", tabName = "home", icon = icon("house")),
    menuItem(text = "Land Use Plan Assessment", tabName = "assessment", icon = icon("clipboard-check")),
    menuItem(text = "Deforestation Statistics", tabName = "deforestation_statistics", icon = icon("chart-line")),
    menuItem(text = "Forest Cover Statistics", tabName = "fc_statistics", icon = icon("chart-line")),
    menuItem(text = "Land Use Simulations", tabName = 'simulations', icon = icon('chart-simple')),
    menuItem(text = "Deforestation Predictions", tabName = 'predictions', icon = icon('globe'))
    
  ) # END sidebarMenu
) # END dashboardSidebar

# dashboard body -----------------------
body <- dashboardBody(
  
  #use_theme(pytheme), # <-- use the pytheme
  
  # tabItems
  tabItems(
    
    # home tabItem ----
    tabItem(tabName = "home",
            tags$img(class = "banner", src = "river_trees.jpeg",
                     width = "100%",
                     alt = "A landscape photo of the Paraguayan Chaco. A river is in the foreground with trees in the background."),
            
            # fluidRow with intro & getting data text boxes ----
            fluidRow(
              
              # intro box ----
              box(width = 6,
                  title = tagList(icon("file"), tags$strong("Background")),
                  includeMarkdown("text/intro.md")
              ), # END intro box
              
              # database box ----
              box(width = 6,
                  title = tagList(icon("database"), tags$strong("About the data")),
                  includeMarkdown("text/citation.md")
              ), # END database box
              
            ), # END fluidRow
            
            # fluidRow with disclaimer box ----
            fluidRow(
              
              box(width = 12,
                  title = tagList(icon("triangle-exclamation"), tags$strong("Disclaimer")),
                  includeMarkdown("text/disclaimer.md")
              ) # END disclaimer box
              
            ), # END fluidRow
            
            # fluidRow with footer ----
            fluidRow(
              
              includeMarkdown("text/home_page_footer.md")
              
            ) # END fluidRow
            
    ), # END home tabItem
          
    #assessment tabItem ----
    tabItem(tabName = "assessment",

            # fluidRow ----
            fluidRow(
              
              # input box ----
              box(width = 4, 
                  title = tags$strong("Political Boundaries:"),
                  
                  # selectInput for PB----
                  selectInput("department", "Select a department:", choices = unique(illegal_df$nom_dpto)),
                  selectInput("district", "Select a district:", choices = NULL)
                  
              ), # END input box
              
              # plot and table box ----
              box(width = 8,
                  
                  title = tags$strong("Illegal Deforestation by Political Boundary:"),
                  
                  # map output ----
                  leafletOutput(outputId = "illegal_df_map") |>
                    withSpinner(type = 1,
                                color = "#4b5f43"),
                  
                  # plot output ----
                  # plotOutput(outputId = "illegal_df_output_plot") |>
                  #   withSpinner(type = 1,
                  #               color = "#4b5f43"),
                  #Add a download button
                  #downloadButton("downloadplot", "Download Plot"),
                  
                  # table output ____
                  dataTableOutput(outputId = "illegal_df_output") |>
                    withSpinner(type = 1,
                                color = "#4b5f43")
                  # Add a download button
                  #downloadButton("downloadTable", "Download Table")
                  
              ) # END map, plot, and table box
              
            ), # END fluidRow
            
    ), # END assessment tabItem
    
    # deforestation statistics tabItem ----
    tabItem(tabName = "deforestation_statistics",
            
            # fluidRow ----
            fluidRow(
              
              # input box ----
              box(width = 4,
                  title = tags$strong("Deforestation by Political Boundary:"),
                  
                  # selectInput for PB----
                  selectInput("department_deforestation", "Select a department:", choices = unique(deforestation_fake$department)),
                  selectInput("district_deforestation", "Select a district:", choices = NULL)
                  
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
                  
                  title = tags$strong("Forest COver by Political Boundary:"),
                  
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
    
    
  ) # END tabItems
  
) # END dashboardBody

# combine all -----------------------
dashboardPage(header, sidebar, body)