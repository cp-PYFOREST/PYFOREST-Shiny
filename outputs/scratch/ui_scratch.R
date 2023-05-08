# dashboard sidebar -----------------------
sidebar <- dashboardSidebar(
  # sidebarMenu ----
  sidebarMenu(
    menuItem(text = "Home", tabName = "home", icon = icon("house")),
    menuItem(text = "Land Use Plan Compliance", tabName = "compliance", icon = icon("clipboard-check")),
    menuItem(text = "Land Cover Statistics", tabName = "land cover statistics", icon = icon("chart-line"), startExpanded = FALSE,
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