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


# dashboard header -----------------------
header <- dashboardHeader(
  title = "PYFOREST",
  titleWidth = 250
) # END dashboardHeader


# dashboard sidebar -----------------------
sidebar <- dashboardSidebar(
  # sidebarMenu ----
  sidebarMenu(
    menuItem(text = "Home", tabName = "home", icon = icon("house")),
    menuItem(text = "Land Use Plan Compliance", tabName = "compliance", icon = icon("clipboard-check")),
    menuItem(text = "Land Cover Statistics", tabName = "land cover statistics", icon = icon("chart-line"), startExpanded = TRUE,
             menuSubItem('Historic Deforestation',
                         tabName = 'deforestation',
                         icon = icon('tree')),
             menuSubItem('Projections',
                         tabName = 'projections',
                         icon = icon('chart-simple')),
             menuSubItem('Predictions',
                         tabName = 'predictions',
                         icon = icon('globe')))
  ) # END sidebarMenu
) # END dashbaordSidebar

# dashboard body -----------------------
body <- dashboardBody(
  
  #use_theme(pytheme), # <-- use the pytheme 
  
  # tabItems
  tabItems(
    
    # home tabItem ----
    tabItem(tabName = "home",
            
            # left-hand column ----
            column(width = 6,
                   
                   #input left-box ----
                   box(width = NULL,
                       
                       title = tagList(icon("file"), tags$strong("Background")),
                       includeMarkdown("text/intro.md"),
                       tags$img(src = "image.jpeg", 
                                alt = "A map of Paraguay, showing the Chaco ecoregion.",
                                height = "350px", width = "auto") 
                       
                   ) # END left-box
                   
            ), # END left-hand column
            
            # right-hand column ----
            column(width = 6,
                   # top fluid row ----
                   fluidRow(
                     # input box
                     box(width = NULL,
                         
                         title = tagList(icon("database"), tags$strong("About the data")),
                         includeMarkdown("text/citation.md")
                         
                     ) # END box
                     
                   ), # END top fluid row
                   
                   # bottom fluidRow ----
                   fluidRow(
                     box(width = NULL,
                         
                         title = tagList(icon("triangle-exclamation"), tags$strong("Disclaimer")),
                         includeMarkdown("text/disclaimer.md")
                         
                     ) # END box
                     
                   ) #END bottom fluidRow
                   
            ) # END right-column
            
    ), #END home tabItem
    
    #compliance tabItem ----
    tabItem(tabName = "compliance",
            
            # fluidRow ----
            fluidRow(
              
              # input box ----
              box(width = 4, 
                  title = tags$strong("Political Boundaries:"),
                  
                  # selectInput for PB----
                  selectInput("nom_dpto", "Select a department:", choices = unique(political_boundaries$nom_dpto)),
                  selectInput("nom_dist", "Select a district:", choices = NULL)
                  
              ), # END input box
              
              # plot and table box ----
              box(width = 8,
                  
                  title = tags$strong("Compliance by Political Boundary:"),
                  
                  # plot and table output ----
                  plotOutput(outputId = "compliance_output_plot") |>
                    withSpinner(type = 1,
                                color = "#4b5f43"),
                  dataTableOutput(outputId = "dt_output") |>
                    withSpinner(type = 1,
                                color = "#4b5f43")
                  
              ) # END plot and table box
              
            ), # END fluidRow
            
    ), # END compliance tabItem
    
    #deforestation statistics tabItem ----
    tabItem(tabName = "deforestation",
            
            # fluidRow ----
            fluidRow(
              
              # input box ----
              box(width = 4, 
                  title = tags$strong("Political Boundaries:"),
                  
                  # selectInput for PB----
                  selectInput("nom_dpto", "Select a department:", choices = unique(political_boundaries$nom_dpto)),
                  selectInput("nom_dist", "Select a district:", choices = NULL)
                  
              ), # END input box
              
              # table box ----
              box(width = 8,
                  
                  title = tags$strong("Deforestation by Political Boundary:"),
                  
                  # table output ----
                  tableOutput(outputId = "table") |>
                    withSpinner(type = 1,
                                color = "#4b5f43")
                  
              ), # END table box
              
              
            ) # END fluidRow
            
    ) # END deforestation statistics tabItem
    
    
  ) # END tabItems
  
) # END dashboardBody

# combine all -----------------------
dashboardPage(header, sidebar, body)