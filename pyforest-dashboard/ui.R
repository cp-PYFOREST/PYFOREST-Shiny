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
  tags$style(HTML
#              (
# #     "
# # .value-box-custom-green { background-color: #4B5F43 !important; }
# # .value-box-custom-orange { background-color: #F26419 !important; }
# # .value-box-custom-yellow { background-color: #F6AE2D !important; }
# # "
#   ),
    
    ("
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
    ")
    ),
  
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
                      title = tagList(tags$strong("About the dashboard")),
                      includeMarkdown("text/about_the_dashboard.md")
                  ) 
                ),
                column(
                  width = 6,
                  fluidRow(box(width = 12,
                               title = tagList(tags$strong("General Information")),
                               bsCollapse(
                                 id = "collapseExample1",
                                 open = "Panel_data",
                                 #accordion = TRUE,
                                 bsCollapsePanel(
                                   title = HTML(paste0("Data Source <span class='arrow'>&#x25BE;</span>")),
                                   style = "info",
                                   includeMarkdown("text/data_source.md")
                                 ),
                                 bsCollapsePanel(
                                   title = HTML(paste0("Project Information <span class='arrow'>&#x25BE;</span>")),
                                   style = "info",
                                   includeMarkdown("text/project_information.md")
                                 ),
                                 bsCollapsePanel(
                                   title = HTML(paste0("Technical Documentation <span class='arrow'>&#x25BE;</span>")),
                                   style = "info",
                                   includeMarkdown("text/technical_documentation.md")
                                 )
                               )
                  )
                  ),
                  
                  fluidRow(
                    box(
                      width = 12,
                      title = tagList(tags$strong("Tabs Information")),
                      bsCollapse(
                        id = "collapseExample2",
                        bsCollapsePanel(
                          title = HTML(paste0("Land Use Plan Assessment <span class='arrow'>&#x25BE;</span>")),
                          style = "info",
                          fluidRow(
                            tags$h3(tags$b("By Political Boundary"))
                            ),
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
                                   
                                   fluidRow(
                                     tags$h3(tags$b("By PUT ID"))
                                            ),
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
                                   tags$b("Prioritize Cattle Production"),
                                   tags$p("This scenario aims to find a balance between cattle production and forest conservation. It proposes a 25% total forest cover, which includes the combined area of the 100-meter riparian forest and 100-meter hedgerow buffer. Any additional forest area required to reach the 25% target would be designated as forest reserve. This policy includes paddocks of less than 100 ha. The intention is to simulate potential effects of a policy goal that prioritizes land use for economic purposes, while maintaining a 25% forest cover goal."),
                                   tags$img(
                                     src = "prioritize_econ_development_lup_example.png",
                                     alt = "Prioritize Cattle Production simulated LUP scenario",
                                     style = "display: block; margin: 0 auto; width: 80%; height: 80%; margin-bottom: 10px;",
                                     tags$figcaption( 
                                       style = "text-align: center; font-style: italic; margin-bottom: 10px;",
                                       "Proritize Cattle Production LUP")
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
                )
                # ,
                # title = tags$strong("About the dashboard"),
                # includeMarkdown("text/disclaimer.md")
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
              fluidRow(
                column(
                  width = 12,
                  box(
                    width = 12,
                    title = tagList(tags$strong("Compliance by Political Boundary")),
                    tags$style(HTML(".leaflet-container {background: #ffffff;}")),
                    actionButton("drill_up_unauthorized", "View Departments"),
                    actionButton("drill_down_unauthorized", "View Districts"),
                    selectInput(
                      "year_range_unauthorized",
                      "Select Year Range",
                      choices = unique(combined_illegal_df_by_dpto$year_range)
                    ),
                    tags$p("Unauthorized deforestation refers to the illegal clearing of forested areas within land use plans categorized under 'BOSQUES'. This category encompasses forest reserves, hedgerows, and riparian forests.")
                    
                  ), # END box
                  
                  # Value Boxes ----
                  fluidRow(
                    column(
                      width = 12,
                      box(
                        width = 4,
                        title = tagList(tags$strong("Bosque Area")),
                        collapsible = TRUE, # this adds a button to collapse the box
                        uiOutput("total_area_valuebox") |> 
                          withSpinner(color = "#4B5F43")
                      ),
                      box(
                        width = 4,
                        title = tagList(tags$strong("Unauthorized Deforestation Sum")),
                        collapsible = TRUE,
                        uiOutput("unauth_prop_valuebox") |> 
                          withSpinner(color = "#4B5F43")
                      ),
                      box(
                        width = 4,
                        title = tagList(tags$strong("Illegal Properties")),
                        collapsible = TRUE,
                        uiOutput("num_illegal_valuebox") |> 
                          withSpinner(color = "#4B5F43")
                      )
                    )
                  ), # END fluidRow for Value Boxes
                  
                  box(
                    width = 12,
                    title = tagList(tags$strong("Unauthorized Deforestation Map")),
                    leafletOutput("leafdown_unauthorized", height = "350px",) |> 
                      withSpinner(color = "#4B5F43"),
                    collapsible = TRUE
                  )
                )
              )
            ),
            
            box(
              title = tagList(tags$strong("Unauthorized Deforestation Bar Plot")),
              width = 12,
              plotlyOutput("illegalPlot", height = "400px") |> 
                withSpinner(color = "#4B5F43"),
              collapsible = TRUE
            ),
            box(
              title = tagList(tags$strong("Change in Unauthorized Deforestation Over Time")),
              width = 12,
              plotlyOutput("areaPlot", height = "400px") |> 
                withSpinner(color = "#4B5F43"),
              collapsible = TRUE
            )
          )
        ),
        
        tabPanel(title = "Authorized Deforestation",
                 fluidRow(
                   div(
                     fluidRow(
                       column(width = 12,
                              box(
                                width = 12,
                              title = tagList(tags$strong("Compliance by Political Boundary")),
                              tags$style(HTML(".leaflet-container {background: #ffffff;}")),
                              actionButton("drill_up_authorized", "View Departments"),
                              actionButton("drill_down_authorized", "View Districts"),
                              selectInput(
                                "year_range_authorized", 
                                "Select Year Range", 
                                choices = unique(combined_auth_df_by_dpto$year_range)
                                ),
                              tags$p("Authorized deforestation refers to the legal clearing of forested areas within land use plans categorized under 'AREA_AUTORIZADA'. This category encompasses authorized areas that will become paddocks.")
                       ),
                       
                       
                       # Authorized Value Boxes ----
                       fluidRow(
                         column(
                           width = 12,
                           box(
                             width = 4,
                             title = tagList(tags$strong("Authorized Area")),
                             collapsible = TRUE, 
                             uiOutput("total_auth_area_valuebox") |> 
                               withSpinner(color = "#4B5F43")
                           ),
                           box(
                             width = 4,
                             title = tagList(tags$strong("Authorized Deforestation Sum")),
                             collapsible = TRUE,
                             uiOutput("auth_prop_valuebox") |> 
                               withSpinner(color = "#4B5F43")
                           ),
                           box(
                             width = 4,
                             title = tagList(tags$strong("Remaining Authorized Area")),
                             collapsible = TRUE,
                             uiOutput("remaining_area_valuebox") |> 
                               withSpinner(color = "#4B5F43")
                           )
                         )
                       ), # END fluidRow for Authorized Value Boxes
                       
                       box(
                         width = 12,
                         title = tagList(tags$strong("Authorized Deforestation Map")),
                         leafletOutput("leafdown_authorized", height = "350px",) |> 
                           withSpinner(color = "#4B5F43"),
                         collapsible = TRUE
                       ),

                       box(title = tagList(tags$strong("Authorized Deforestation")),
                           width = 12,
                           plotlyOutput("authorizedPlot", height = "400px") |> 
                             withSpinner(color = "#4B5F43"),
                           collapsible = TRUE
                       ),
                       box(title = tagList(tags$strong("Change in Authorized Deforestation Over Time")),
                           width = 12,
                           plotlyOutput("area_authorized_Plot", height = "400px") |> 
                             withSpinner(color = "#4B5F43"),
                           collapsible = TRUE
                       )
                     )
                     )
                     )
        ) 
      ) # END Authorized Deforestation tabPanel
    )
    ), # END tabItem
  

# put_id tabItem ----
tabItem(tabName = "put_id",
        fluidPage(
          # sidebarLayout(
          #   sidebarPanel(
          fluidRow(
            column(
              width = 12,
              br(),
              # input -----
              box(
                width = 12,
                title = tagList(tags$strong("Compliance by PUT ID")),
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
              )
            ,

            ), # END sidebarPanel
            mainPanel(width = 12,
                      fluidRow(
                        column(
                          width = 12,
                          br(),
                          box(tags$p("This map only displays active properties between 2019 and 2020."),
                              title = tagList(tags$strong("Property Compliance Map")),
                              width = 12,
                              div(style = 'overflow-x: scroll; height: auto;',
                                  # output 1 map ----
                                  withSpinner(leafletOutput(outputId = "map"), 
                                              color = "#4B5F43")
                              )
                          )
                        )
                      ),
                      fluidRow(
                        # output 2 table ----
                        withSpinner(DT::dataTableOutput("table"), color = "#4B5F43")
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
                  title = tags$strong("Deforestation by Political Boundary"),
                  column(width = 12,
                         actionButton("drill_up", "View Departments"),
                         actionButton("drill_down", "View Districts"))
              ),
              box(width = 12,
                  collapsible = TRUE,
                  column(width = 3,
                         selectInput("year_deforestation_percent_map", "Select a year", unique(forest_loss_district_std_df$year)),
                         box(width = 12,
                             print(tags$strong("Visualizations Disclaimer:")),
                             HTML("<br>"),
                             textOutput("deforestation_data_disclaimer_txt"))
                  ),
                  column(width = 9,
                         withSpinner(leafletOutput("leafdown_forest_loss", 
                                                   width = "100%", 
                                                   height = "550px"),
                                     color = "#4B5F43"), 
                         tags$style(HTML(".leaflet-container {background: #ffffff;}")))
              ),
              box(width = 6,
                  collapsible = TRUE,
                  title = tags$strong("Deforestation Area (ha):"),
                  withSpinner(plotly::plotlyOutput(outputId = "forest_loss_area_ha_plot", 
                                                   width = "100%", 
                                                   height = "400px"),
                              color = "#4B5F43")
              ),
              box(width = 6,
                  collapsible = TRUE,
                  title = tags$strong("Deforestation Percent (%):"),
                  withSpinner(plotly::plotlyOutput(outputId = "forest_loss_area_percent_plot", 
                                                   width = "100%", 
                                                   height = "400px"),
                              color = "#4B5F43")
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
                         actionButton("drill_downward", "View Districts")) 
                 #,
                  #column(width = 2, selectInput("years_selected_var", "Select a Year", unique(py_fc_dept$year)))
              ),
              box(
                width = 12,
                collapsible = TRUE,
                column(width = 2, selectInput("years_selected_var", "Select a Year", unique(py_fc_dept$year))),
                withSpinner(leafletOutput("leafdown_forest_cover"),
                            color = "#4B5F43"),
                tags$style(HTML(".leaflet-container {background: #ffffff;}"))
              ),
              box(width = 6,
                  collapsible = TRUE,
                  title = tags$strong("Forest Cover Area (ha):"),
                  withSpinner(plotly::plotlyOutput(outputId = "forest_cover_area_ha_plot"),
                              color = "#4B5F43")
              ),
              box(width = 6,
                  collapsible = TRUE,
                  title = tags$strong("Forest Cover Percent (%):"),
                  withSpinner(plotly::plotlyOutput(outputId = "forest_cover_area_percent_plot"),
                              color = "#4B5F43")
              )
            )
            
    ),
    
    # ------------------------------------------ Simulation and Deforestation Comparisons ------------------------------------------ 
    tabItem(tabName = "sim_pred",
            # predictions content here
            fluidRow(
              box(width = 12,
                  title = tags$strong("Scenarios:"),
                  selectInput("simulation_type", "Select one:", choices = c("All", simulation_types), selected = "Current Forest Law"))
            ),
            fluidRow(
              column(
                width = 4,
                box(
                  width = 12,
                  collapsible = TRUE,
                  style = "height: 550px; display: flex; align-items: center; justify-content: center;",  # Adjust the height as needed
                  title = "Deforestation Predictions",
                  withSpinner(
                    div(
                      style = "max-height: 100%; max-width: 100%; display: flex; align-items: center; justify-content: center;",
                      uiOutput("maps_predictions_by_scenario")
                    )
                  )
                )
              ),
              column(width = 8,
                     box(width = 12,
                         collapsible = TRUE,
                         title = "Comparison of Deforestation and Areas under Different Policy Scenarios by Land Use Types",
                         withSpinner(plotlyOutput("histogram_sim_pred_land_use", 
                                                  height = "525px", 
                                                  width = "100%"),
                                     color = "#4B5F43"),
                         tags$p("Riparian Corridor Deforestation is insignificant, Non-deforested Riparian Corridor remains relatively constant.")
                     ),
                     
              )
            ),

            fluidRow(

              box(width = 6,
                  collapsible = TRUE,
                  style = "height: 460px;",  # Adjust the height as needed
                  align = "center",
                  title = "Land Use Plan Simulations",
                  withSpinner(uiOutput("lup_simulation_images"),
                              color = "#4B5F43") #simulation scenarios
              ),
              box(width = 6,
                  collapsible = TRUE,
                  style = "height: 460px;",  # Adjust the height as needed
                  title = "Deforestation Predictions",
                  column(
                    width = 9,
                    align = "center",
                    withSpinner(uiOutput("prediction_images"),
                                color = "#4B5F43")),
                  column(width = 3,
                         tags$img(src = "pred_scale.png", width = "80%")))
            ),
            
            fluidRow(
              box(width = 12,
                  collapsible = TRUE,
                  title = "Comparing Simulations & Predictions",
                  includeMarkdown("text/pred_sim.md")
              )
            )
            
    ) # END predictions tabItem
    
  ), # END tabItems
  
  # Footer text ----
div(class = "footer",
    fluidRow(
      column(12,
             align = "center", # this will center your content
             includeMarkdown("text/home_page_footer.md")
      )
    )
)
  
) # END dashboardBody

# ------------------------------------------ app ------------------------------------------
# Create the dashboard page by combining the header, sidebar, and body sections
dashboardPage(header, sidebar, body)
