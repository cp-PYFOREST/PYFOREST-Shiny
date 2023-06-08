# Source
#source("R/forest_cover_module.R")

# Create the theme
pytheme <- create_theme(
  adminlte_color(light_blue = "#2F4858"), ##3c4d36
  adminlte_sidebar(width = "350px", # Set the width of the sidebar
                   dark_bg = "#3c4d36", # Set the background color of the sidebar 
                   dark_hover_bg = "#868f82"), # Set the hover background color of the sidebar
  adminlte_global(box_bg = "white") # Set the background color of the global elements
)

# ------------------------------------------ header ------------------------------------------
# Create the header section of the dashboard
header <- dashboardHeader(
  title = tags$a(href = "https://www.infona.gov.py/",
                 class = "navbar-brand",
                 tags$img(src = "infona_logo.png", height = "30px", style = "float: left; margin-right: 5px;"),
                 "INFONA Interactive Tool"
  ),
  tags$li(class = "dropdown",
          dropdownMenu(type = 'messages',
                       headerText = "Share it",
                       icon = icon("share-alt"),
                       messageItem(from = 'Twitter',
                                   message = "",
                                   icon = icon("twitter"),
                                   href = "https://twitter.com/intent/tweet?url=https%3A%2F%2Fgithub.com%2Fcp-PYFOREST%2FPYFOREST-Shiny&text=Check%20out%20the%20repository%20for%20the%20Paraguayan%20%20Chaco%20Interactive%20Tool%20https%3A%2F%2Fgithub.com%2Fcp-PYFOREST%2FPYFOREST-Shiny&hashtags=TNC%20%23RShiny"
                       ),
                       messageItem(from = 'LinkedIn',
                                   message = "",
                                   icon = icon("linkedin"),
                                   href = "https://www.linkedin.com/sharing/share-offsite/?url=https%3A%2F%2Fgithub.com%2Fcp-PYFOREST%2FPYFOREST-Shiny"
                       ) # END messageItem
          ), # END dropdownMenu
          tags$a(href = "https://github.com/cp-PYFOREST", # Replace this with the website you want to redirect to
                 tags$img(src = "pyforest_hex_sticker.png", 
                          id = "right-logo", 
                          height = "30px", 
                          style = "float: right; margin-right: 15px; margin-top: 10px;"),
          ) # END tags$a
  ), # END tags$li
  disable = FALSE,
  titleWidth = 400
) # END dashboardHeader

# ------------------------------------------ sidebar ------------------------------------------
# Create the sidebar section of the dashboard
sidebar <- dashboardSidebar(
  
  #sidebarMenu ----
  sidebarMenu(
    menuItem(text = "Home",
             tabName = "home",
             icon = icon("house")
    ), # END Home menuItem
    menuItem(text = "Land Use Plan Assessment",
             tabName = "assessment",
             icon = icon("clipboard-check"),
             startExpanded = FALSE,
             menuSubItem('By Political Boundary', tabName = 'political_boundary', icon = icon('earth-americas')),
             menuSubItem('By PUT ID', tabName = 'put_id', icon = icon('draw-polygon'))
    ), # END Land Use Plan Assessment menuItem
    menuItem(text = "Deforestation and Forest Cover Statistics",
             tabName = "deforestation_statistics",
             icon = icon("chart-line"),
             menuSubItem('Deforestation Statistics', tabName = 'deforestation_stats', icon = icon('chart-line')),
             menuSubItem('Forest Cover Statistics', tabName = 'forest_cover_stats', icon = icon('chart-line'))
    ), # END df and fc Statistics menuItem
    menuItem(text = "Simulation and Deforestation Comparisons",
             tabName = 'sim_pred',
             icon = icon('globe')
    ) # END Deforestation Predictions menuItem
  ) # END sidebarMenu
) # END dashboardSidebar

# ------------------------------------------ body ------------------------------------------
# Create the body section of the dashboard
body <- dashboardBody(
  
  #fluidPage(theme = shinytheme("simplex")),
  #shinythemes::themeSelector(),
  
  #theme ----
  tags$style(HTML("
      /* Change color of active tab */
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        background-color: #2F4858;
        color: white;
      }
      
      /* Change color of inactive tabs */
      .nav-tabs > li > a,
      .nav-tabs > li > a:focus,
      .nav-tabs > li > a:hover {
        background-color: #868f82;
        color: white;
      }
      
      /* Sticky footer */
        html, body {
          height: 100%;
          margin: 0;
          padding: 0;
          position: relative;
        }
        
        .content {
          padding-bottom: 60px; /* Height of the footer */
        }
        
        .footer {
          position: fixed;
          left: 0;
          bottom: 0;
          width: 100%;
          background-color: #f5f5f5;
          padding: 10px;
          text-align: center;
        }
    ")),
  
  use_theme(pytheme),
  
  # ------------------------------------------ Home ------------------------------------------ 
  
  tabItems(
    tabItem(tabName = "home",
            fluidPage(
              fluidRow(
                width = 6,
                column(
                  width = 6,
                  box(width = 12,
                      title = "About the dashboard",
                      tags$strong(tags$h3("Welcome!")),
                      br(),
                      "Our dashboard aims to provide valuable insights of the Paraguayan Chaco region. We have conducted  geospatial analysis, land use simulations, and deforestation predictions to help stakeholders make informed decisions regarding forest management.
                                 Through this interactive platform, you can explore the results of our research and analysis. Gain a deeper understanding of the current land use patterns, identify areas at risk of deforestation, and visualize the potential impacts of different land use scenarios.",
                      br(),
                      "Explore this page to gain a deeper understanding of how to navigate the app.",
                      br(),
                      br(),
                      tags$h4("Langagues (Lenguajes)"),
                      tags$h5(tags$b("Translation Instructions")),
                      "In order to translate this web application to another language. Utlize Google Chrome to access it. On the right of the address bar, click Translate. Click on your preferred language. Chrome will then translate the web application.",
                      br(),
                      br(),
                      tags$h5(tags$b("Instrucciones para traducir la applicación")),
                      
                      # UPDATES START -----------
                      "Para traducir esta applicación, utilize la applicación en Google Chrome, a la derecha de la barra, haz clic en traducir, luego, elige Español."
                      # UPDATES END -------------
                      ) 
                ),
                column(
                  width = 6,
                  fluidRow(box(width = 12,
                               title = "General Information",
                               bsCollapse(
                                 id = "collapseExample",
                                 open = "Panel_data",
                                 bsCollapsePanel(
                                   title = HTML(paste0("Data Source <span class='arrow'>&#x25BE;</span>")),
                                   style = "info",
                                   "The data used in this app is sourced from the National Forestry Institute of Paraguay (INFONA). INFONA provides land use plan shapefiles and deforestation raster files, which serve as the foundation for our analysis. To access the data used in our study, please visit our Zenodo repository [insert Zenodo link]. For the most up-to-date data, we recommend visiting INFONA’s official website [insert INFONA website link] and submitting a request for the desired information."
                                 ),
                                 bsCollapsePanel(
                                   title = HTML(paste0("Project Information <span class='arrow'>&#x25BE;</span>")),
                                   style = "info",
                                   "PYFOREST is an open-source project aimed at developing an interactive tool for forest conservation analysis in Paraguay. The project is built using R, Shiny, and other technologies to provide a comprehensive and user-friendly platform for exploring deforestation rates, land use plans, and potential conservation policies. For more information on the project background and to access the source code, please visit our GitHub repository [insert GitHub link]. Technical documentation for the PyForest project can be found on the Bren School of Environmental Science & Management Capstone page [insert Capstone page link]."
                                 ),
                                 bsCollapsePanel(
                                   title = HTML(paste0("Technical Documentation <span class='arrow'>&#x25BE;</span>")),
                                   style = "info",
                                   "Technical documentation for the PyForest project can be found on the Bren School of Environmental Science & Management Capstone page [insert Capstone page link]."
                                 )
                               )
                  )),
                  
                  fluidRow(box(width = 12,
                               title = "Tabs Information",
                               bsCollapse(
                                 id = "collapseExample",
                                 open = "Panel_data",
                                 bsCollapsePanel(
                                   title = HTML(paste0("Land Use Assessment <span class='arrow'>&#x25BE;</span>")),
                                   style = "info",
                                   tags$h3(tags$b("By Political Boundary")),
                                   fluidRow(
                                     tags$h4("Unauthorized Deforestation"),
                                     tags$div(
                                       style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 100%;",
                                       tags$img(
                                         src = "unauth_deforestation_yr_dpt.gif",
                                         alt = "Unauthorized Deforestation by department and year",
                                         style = "width: 80%; height: 80%; margin-bottom: 10px;"
                                       ),
                                       tags$figcaption(
                                         style = "text-align: center; font-style: italic; margin-bottom: 10px;",
                                         "Unauthorized deforestation rates over the years by department"
                                       ),
                                       tags$p(
                                         style = "margin-top: 10px;",
                                         "This page displays a map and a table of illegal deforestation rates by political boundary (departments and districts). To view the data, select a department and a district from the drop-down menus. The map will show the illegal deforestation rates for the selected area, while the table provides additional details on the deforestation rates."
                                       )
                                     )
                                   ),
                                   
                                   fluidRow(
                                     tags$h4("Authorized Deforestation"),
                                     tags$div(
                                       style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 100%;",
                                       tags$img(
                                         src = "auth_deforestation_yr_dpt.gif",
                                         alt = "Authorized Deforestation by department and year",
                                         style = "width: 80%; height: 80%; margin-bottom: 10px;"
                                       ),
                                       tags$figcaption(
                                         style = "text-align: center; font-style: italic; margin-bottom: 10px;",
                                         "Authorized deforestation rates over the years by department"
                                       ),
                                       tags$p(
                                         style = "margin-top: 10px;",
                                         "This page presents authorized deforestation rates by political boundary (departments and districts). Use the dropdown menus to select a department and a district to view the data. The map will display authorized deforestation rates for the chosen area, and the table will provide additional information on the deforestation rates."
                                       )
                                     )
                                   ),
                                   
                                   tags$h3(tags$b("By PUT ID")),
                                   tags$p(style = "margin-top: 10px;",
                                          "This tab allows the users to explore properties complaince by PUT ID. Explore the map visually or search by PUT ID to view complaince in the table and map.")
                                   
                                   
                                 ), # END COLLAPSE 1
                                 bsCollapsePanel(
                                   title = HTML(paste0("Deforestation and Forest Cover Statistics <span class='arrow'>&#x25BE;</span>")),
                                   style = "info",
                                   
                                   fluidRow(
                                     tags$h4("Deforestation Statistics"),
                                     tags$div(
                                       style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 100%;",
                                       tags$img(
                                         src = "forest_loss_yr_dpt.gif",
                                         alt = "Deforestation gif by department and year",
                                         style = "width: 80%; height: 80%; margin-bottom: 10px;"
                                       ),
                                       tags$figcaption(
                                         style = "text-align: center; font-style: italic; margin-bottom: 10px;",
                                         "Deforestation percentage rates over the years by department"
                                       ),
                                       tags$p(
                                         style = "margin-top: 10px;",
                                         "This page shows deforestation statistics by political boundary. The plot displays deforestation by selected political boundary (department of district). Data can be filtered by selecting a specific year and plots may also be filtered by selecting on and off policial boundaries names on the right."
                                       )
                                     )
                                   ),
                                   fluidRow(
                                     tags$h4("Forest Cover Statistics"),
                                     tags$div(
                                       style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 100%;",
                                       tags$img(
                                         src = "forest_cover_yr_dpt.gif",
                                         alt = "Forst Cover gif by department and year",
                                         style = "width: 80%; height: 80%; margin-bottom: 10px;"
                                       ),
                                       tags$figcaption(
                                         style = "text-align: center; font-style: italic; margin-bottom: 10px;",
                                         "Forest Cover percentage rates over the years by department"
                                       ),
                                       tags$p(
                                         style = "margin-top: 10px;",
                                         "This page displays forest cover statistics by political boundary. The plot displays forest cover by selected political boundary (department of district). Data can be filtered by selecting a specific year and plots may also be filtered by selecting on and off policial boundaries names on the right."
                                       )
                                     )
                                   )
                                 ), # END COLLAPSE 2
                                 
      # UPDATES START -------------
                                 bsCollapsePanel(
                                   HTML(paste0("Land Use Plan Simulation & Deforestation Predictions <span class='arrow'>&#x25BE;</span>")),
                                   style = "info",
                                   tags$h4("Land Use Plan Simulation"),
                                   tags$p("On this tab you can find the Land Use Plan Simulations; land-use results following the current forest law and alternate forest laws scenarios."),
                                   br(),
                                   tags$h4("Deforestation Predictions"),
                                   tags$p("Deforestation Predictions with land-use type results may also be found here follwoing the current and alternate forest laws."),
                                   br(),
                                   tags$h4(tags$b("Scenarios")),
                                   br(),
                                   tags$b("Current Forest Law"),
                                   tags$p("This scenario follows the current policy and legal requirements for LUPs enforced by INFONA. It includes a 25% forest reserve, a 100-meter hedgerow buffer, a 100-meter riparian forest, and paddocks of less than 100 ha for authorized deforestation. The purpose of this scenario is to simulate the continuation of existing practices and policies without any significant changes."),
                                   tags$img(
                                     src = "current_forest_law_lup_example.png",
                                     alt = "Simulated LUP following current forest law",
                                     style = "display: block; margin: 0 auto; width: 80%; height: 80%; margin-bottom: 10px;",
                                     tags$figcaption( 
                                       style = "text-align: center; font-style: italic; margin-bottom: 10px;",
                                       "LUP under Current Forest Law")
                                   ),
                                   br(),
                                   tags$b("Promotes Forest Conservation"),
                                   tags$p("This scenario aims to enhance forest conservation efforts. It proposes increasing the forest reserve requirement to 50%, along with maintaining a 100-meter hedgerow buffer, a 100-meter riparian forest, and paddocks of less than 100 ha. The objective is to simulate the potential outcomes of a policy that prioritizes the preservation and protection of forests."),
                                   tags$img(
                                     src = "forest_conservation_lup_example.png",
                                     alt = "Promotes Forest Conservation simulated LUP",
                                     style = "display: block; margin: 0 auto; width: 80%; height: 80%; margin-bottom: 10px;",
                                     tags$figcaption( 
                                       style = "text-align: center; font-style: italic; margin-bottom: 10px;",
                                       "Promotes Forest Conservation LUP")
                                   ),
                                   br(),
                                   tags$b("Prioritize Cattle Ranching"),
                                   tags$p("This scenario aims to find a balance between cattle production and forest conservation. It proposes a 25% total forest cover, which includes the combined area of the 100-meter riparian forest and 100-meter hedgerow buffer. Any additional forest area required to reach the 25% target would be designated as forest reserve. This policy includes paddocks of less than 100 ha. The intention is to simulate potential effects of a policy goal that prioritizes land use for economic purposes, while maintaining a 25% forest cover goal."),
                                   tags$img(
                                     src = "prioritize_econ_development_lup_example.png",
                                     alt = "Prioritize Cattle Ranching simulated LUP scenario",
                                     style = "display: block; margin: 0 auto; width: 80%; height: 80%; margin-bottom: 10px;",
                                     tags$figcaption( 
                                       style = "text-align: center; font-style: italic; margin-bottom: 10px;",
                                       "Proritize Cattle Ranching LUP")
                                   ),
                                   br(),
                                   tags$b("Law Ambiguity"),
                                   tags$p("This scenario addresses a potential ambiguity in the law's interpretation. It suggests that if a property has been deforested beyond the approved amount, an immediate reforestation of 5% of the property is required in the areas of regrowth. This is in addition to maintaining the 100-meter hedgerow buffer, the 100-meter riparian forest, and paddocks of less than 100 ha. However, some might interpret this policy as allowing them to deforest their entire property and only replant 5%. This misinterpretation could lead to substantial deforestation, undermining the policy's intent."),
                                   tags$img(
                                     src = "law_ambiguity_simulation_example.png",
                                     alt = "LUP under law ambiguity",
                                     style = "display: block; margin: 0 auto; width: 80%; height: 80%; margin-bottom: 10px;",
                                     tags$figcaption( 
                                       style = "text-align: center; font-style: italic; margin-bottom: 10px;",
                                       "Law Ambiguity LUP")
                                   )
      # UPDATES END ----------------
                                   )
                               )
                  )) 
                )
              ),
              
              fluidRow(
                tags$img(
                  class = "banner",
                  src = "chaco.jpg",
                  width = "100%",
                  alt = "A landscape photo of the Paraguayan Chaco. A river is in the foreground with trees in the background."
                ),
                title = tags$strong("About the dashboard"),
                includeMarkdown("text/disclaimer.md")
              )
            )),
    
    # ------------------------------------------ Land Use Plan Assessment ------------------------------------------ 
    # political boundary tabItem
    tabItem(
      tabName = "political_boundary",
      tabsetPanel(
        id = "deforestation_tabsetPanel",
        tabPanel(
          title = "Unauthorized Deforestation",
          fluidRow(
            div(
              style = "border: 1px solid #ddd; margin-bottom: 10px; padding: 10px;",
              fluidRow(
                column(
                  width = 12,
                  tags$style(HTML(".leaflet-container {background: #ffffff;}")),
                  h4(tags$strong("Unauthorized Deforestation by Political Boundaries")),
                  actionButton("drill_up_unauthorized", "View Departments"),
                  actionButton("drill_down_unauthorized", "View Districts"),
                  selectInput(
                    "year_range_unauthorized",
                    "Select Year Range",
                    choices = unique(combined_illegal_df_by_dpto$year_range)
                  ),
                  leafletOutput("leafdown_unauthorized", height = "350px")
                )
              )),
            #   fluidRow(
            #     column(
            #       width = 6,
            #       valueBoxOutput("unauth_prop_valuebox_dist")  # Output for the district value box
            #     ),
            #     column(
            #       width = 6,
            #       valueBoxOutput("unauth_prop_valuebox_dpt")  # Output for the department value box
            #     )
            #   )
            # ),
            box(
              title = tagList(tags$strong("Unauthorized Deforestation")),
              width = 6,
              plotlyOutput("illegalPlot", height = "400px")
            ),
            box(
              title = tagList(tags$strong("Change in Unauthorized Deforestation Over Time")),
              width = 6,
              plotlyOutput("areaPlot", height = "400px")
            )
          )
        ),
        tabPanel(title = "Authorized Deforestation",
                 fluidRow(
                   div(
                     style = "border: 1px solid #ddd; margin-bottom: 10px; padding: 10px;",
                     fluidRow(
                       column(width = 12,
                              tags$style(HTML(".leaflet-container {background: #ffffff;}")),
                              h4(tags$strong("Authorized Deforestation by Political Boundaries")),
                              actionButton("drill_up_authorized", "View Departments"),
                              actionButton("drill_down_authorized", "View Districts"),
                              selectInput("year_range_authorized", "Select Year Range", unique(combined_auth_df_by_dpto$year_range)),
                              leafletOutput("leafdown_authorized", height = "350px")
                       ),
                       
                       box(title = tagList(tags$strong("Authorized Deforestation")),
                           width = 6,
                           plotlyOutput("authorizedPlot", height = "400px")
                       ),
                       box(title = tagList(tags$strong("Change in Authorized Deforestation Over Time")),
                           width = 6,
                           plotlyOutput("area_authorized_Plot", height = "400px")
                       )
                     )))
        ) # END Authorized Deforestation tabPanel 
      )
    ),
    
    # put_id tabItem ----
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
                ), # END sidebarPanel
                mainPanel(width = 8,
                          fluidRow(
                            column(
                              width = 12,
                              br(),
                              div(style = 'overflow-x: scroll; height: auto;',
                                  # output 1 map ----
                                  withSpinner(leafletOutput(outputId = "map")) 
                              )
                            )
                          )
                          ,
                          fluidRow(
                            # output 2 table ----
                            withSpinner(DT::dataTableOutput("table")) 
                          )
                          
                ) # END mainPanel
              ) # END sidebarLayout
            ) # END fluidPage
    ),
    
    # ------------------------------------------ Deforestation and Forest Cover Statistics ------------------------------------------ 
    # deforestation_stats tabItem ----
    tabItem(tabName = "deforestation_stats",
            
            fluidRow(
              box(width = 12,
                  title = tags$strong("Deforestation by Political Boundaries"),
                  column(width = 12,
                         print(tags$strong("Switch between political boundaries by selecting departments or districts to observe deforestation statistics."))),
                  column(width = 12,
                         actionButton("drill_up", "View Departments"),
                         actionButton("drill_down", "View Districts"))
              ),
              box(width = 12,
                  column(width = 3,
                         selectInput("year_deforestation_percent_map", "Select a year", unique(forest_loss_district_std_df$year)),
                         box(width = 12,
                             print(tags$strong("Visualizations Disclaimer:")),
                             HTML("<br>"),
                             textOutput("deforestation_data_disclaimer_txt"))
                  ),
                  column(width = 9,
                         withSpinner(leafletOutput("leafdown_forest_loss", width = "100%", height = "550px")),
                         tags$style(HTML(".leaflet-container {background: #ffffff;}")))
              ),
              box(width = 6,
                  title = tags$strong("Deforestation Area (ha):"),
                  withSpinner(plotly::plotlyOutput(outputId = "forest_loss_area_ha_plot", width = "100%", height = "400px"))
              ),
              box(width = 6,
                  title = tags$strong("Deforestation Percent (%):"),
                  withSpinner(plotly::plotlyOutput(outputId = "forest_loss_area_percent_plot", width = "100%", height = "400px"))
              )
            )
    ),
    
    
    # tabItem(tabName = "forest_cover_stats",
    #         forestCoverModuleUI("forest_cover_module")),
    
    # forest_cover_stats tabItem ----
    tabItem(tabName = "forest_cover_stats",
            fluidRow(
              box(width = 12,
                  title = tags$strong("Forest Cover by Political Boundaries"),
                  column(width = 12, actionButton("drill_upward", "View Departments"),
                         actionButton("drill_downward", "View Districts")),
                  column(width =12,
                         print(tags$strong("Switch between political boundaries by selecting departments or districts to observe forest cover statistics.")))#,
                  #column(width = 2, selectInput("years_selected_var", "Select a Year", unique(py_fc_dept$year)))
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
            
    ),
    
    # ------------------------------------------ Simulation and Deforestation Comparisons ------------------------------------------ 
    tabItem(tabName = "sim_pred",
            # predictions content here
            fluidRow(
              
# UPDATES HERE -----
              box(width = 12,
                  title = tags$strong("Scenarios:"),
                  selectInput("simulation_type", "Select one:", choices = c("All", simulation_types), selected = "Current Forest Law"))
            ),
# UPDATES END -----
            fluidRow(
              column(width = 4,
                     box(width = 12,
                         style = "height: 470px;",  # Adjust the height as needed
                         title = "Deforestation Predictions",
                         withSpinner(uiOutput("maps_predictions_by_scenario"))) #, ######### --- UPDATE HERE ---------
                     # box(width = 12,
                     #     align = "center",
                     #     title = "Land Use Plan Simulations Example",
                     #     uiOutput("lup_simulation_example"))
              ),
              column(width = 8,
                     box(width = 12,
                         title = "Comparing Land Use Plan Simulations & Deforestation Predictions Land-Use Totals",
                         # plotlyOutput("histogram_sim_pred_land_use")
                         withSpinner(plotlyOutput("histogram_sim_pred_land_use", height = "450px", width = "100%"))  ######### --- UPDATE HERE ---------
                     ),
                     
                     
# MOVING THIS --------                     
                     # fluidRow(
                     #   
                     #   box(width = 6,
                     #       align = "center",
                     #       title = "Comparing Results: Land Use Plan Simulations",
                     #       uiOutput("lup_simulation_images") #simulation scenarios
                     #   ),
                     #   box(width = 6,
                     #       title = "Comparing Results: Deforestation Predictions",
                     #       column(
                     #         width = 9,
                     #         align = "center",
                     #         uiOutput("prediction_images")),
                     #       column(width = 3,
                     #              tags$img(src = "pred_scale.png", width = "80%")))
                     # )
# END MOVING THIS ------                    
                     
              )
            ),
            
            # fluidRow(
            # 
            #   box(width = 6,
            #       align = "center",
            #       title = "Comparing Results: Land Use Plan Simulations",
            #       uiOutput("lup_simulation_images") #simulation scenarios
            #   ),
            #   box(width = 6,
            #       title = "Comparing Results: Deforestation Predictions",
            #       column(
            #         width = 9,
            #         align = "center",
            #         uiOutput("prediction_images")),
            #   column(width = 3,
            #          tags$img(src = "pred_scale.png", width = "80%")))
            # ),
            

# UPDATE MOVED HERE ---------
fluidRow(
  
  box(width = 6,
      style = "height: 460px;",  # Adjust the height as needed
      align = "center",
      title = "Comparing Results: Land Use Plan Simulations",
      withSpinner(uiOutput("lup_simulation_images")) #simulation scenarios  ######### --- UPDATE HERE ---------
  ),
  box(width = 6,
      style = "height: 461.5px;",  # Adjust the height as needed
      title = "Comparing Results: Deforestation Predictions",
      column(
        width = 9,
        align = "center",
        withSpinner(uiOutput("prediction_images"))), #  ######### --- UPDATE HERE ---------
      column(width = 3,
             tags$img(src = "pred_scale.png", width = "80%")))
),

# END UPDATE -------------
        
            fluidRow(
              box(width = 12,
                  title = "txt explaning how you compare them."
              )
            )
            
    ) # END predictions tabItem
    
  ), # END tabItems
  
  # Footer text ----
  div(class = "footer",
      includeMarkdown("text/home_page_footer.md"))
  
) # END dashboardBody

# ------------------------------------------ app ------------------------------------------
# Create the dashboard page by combining the header, sidebar, and body sections
dashboardPage(header, sidebar, body)
