library(fresh)
# Create the theme
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#4b5f43"
  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#EDF3ED",
    dark_hover_bg = "#778570",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#EDF3ED", 
    info_box_bg = "#EDF3ED"
  )
)


# dashboard header -----------------------
header <- dashboardHeader(
  title = "PYFOREST Mockup",
  titleWidth = 300
) # END dashboardHeader


# dashboard sidebar -----------------------
sidebar <- dashboardSidebar(
  # sidebarMenu ----
  sidebarMenu(
    menuItem(text = "Home", tabName = "home", icon = icon("house")),
    menuItem(text = "Compliance", tabName = "compliance", icon = icon("clipboard-check")),
    menuItem(text = "Predictions", tabName = "predictions", icon = icon("code-compare"))
  ) # END sidebarMenu
) # END dashbaordSidebar

# dashboard body -----------------------
body <- dashboardBody(
  
  use_theme(mytheme), # <-- use the theme
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  # ),
  # 
  
  # # change color of sidebar
  # tags$head(
  #   tags$style(HTML("
  #     /* Change background color of sidebar */
  #     .sidebar {
  #       background-color: #778570  !important;
  #     }
  #     
  #     /* Change color of sidebar menu items */
  #     .sidebar-menu li a {
  #       color: #F8F7F1 !important;
  #     }
  #     
  #     /* Change background color of active sidebar menu item */
  #     .sidebar-menu li.active a {
  #       background-color: #778570 !important;
  #     }
  #     
  #     /* Change color of text in active sidebar menu item */
  #     .sidebar-menu li.active a {
  #       color: #4b5f43 !important;
  #     }
  #   "))
  # ),
  
  # set theme ----
  #fresh::use_theme("fresh_theme.css"),
  
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
                   
            ), # END left-column
            
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
            
    ), #END welcome tabItem
    
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
              
              # table box ----
              box(width = 8,
                  
                  title = tags$strong("Forest Coverage by Political Boundary:"),
                  
                  # table output ----
                  tableOutput(outputId = "table") |>
                    withSpinner(type = 1,
                                color = "blue")
                  
              ) # END table box
              
              
            ) # END fluidRow
            
    ), # END compliance tabItem
    
    
    #predictions tabItem ----
    tabItem(tabName = "predictions",
            
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
                  
                  # title = tags$strong("Forest Coverage by Political Boundary:"),
                  
                  # # table output ----
                  # tableOutput(outputId = "table") |>
                  #   withSpinner(type = 1,
                  #               color = "blue")
                  
              ) # END table box
              
              
            ) # END fluidRow
            
    ) # END compliance tabItem
    
  ) # END tabItem
  
) # END dashboardBody

# combine all -----------------------
dashboardPage(header, sidebar, body)
