shinyUI(dashboardPage(
  dashboardHeader(title = 'NYC Tree Dashboard'),
  dashboardSidebar(
    #sidebarUserPanel(name = 'aaa'),
    sidebarMenu(
      menuItem(text = 'Distribution',tabName = 'distribution', icon = icon('map')),
      menuItem(text = 'Map',tabName = 'map', icon = icon('map'))
      #menuItem(text = 'Graph',tabName = 'graph', icon = icon('graph')),
      #menuItem(text = 'Data',tabName = 'data', icon = icon('database'))
    ),
    selectizeInput('selected','Select Item to Display', choice)
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'distribution',
              #fluidRow(infoBoxOutput("treemap"))),
              #          infoBoxOutput("minBox"),
              #          infoBoxOutput("avgBox")),
              fluidRow(box(leafletOutput('distributionmap'),width=9))),
              #         box(htmlOutput('hist'), height=300))),
      tabItem(tabName = 'map',
              fluidRow(box(leafletOutput('treemap'),width=9)))
              #fluidRow(infoBoxOutput("treemap"))
              #luidRow(box(DT::dataTableOutput('table'), width=12))
      )
    )
  )
)