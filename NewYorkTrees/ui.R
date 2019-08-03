shinyUI(dashboardPage(
  dashboardHeader(title = 'NYC Tree Dashboard'),
  dashboardSidebar(
    #sidebarUserPanel(name = 'aaa'),
    sidebarMenu(
      menuItem(text = 'Distribution',tabName = 'distribution', icon = icon('map')),
      menuItem(text = 'NTA Map',tabName = 'ntamap', icon = icon('map')),
      menuItem(text = 'ZIP Map',tabName = 'zipmap', icon = icon('map')),
      menuItem(text = 'Tree Graph',tabName = 'treegraphs', icon = icon('chart-line')),
      menuItem(text = 'Complaint Graph',tabName = 'complaintgraphs', icon = icon('chart-line'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'distribution',
              #fluidRow(infoBoxOutput("treemap"))),
              #          infoBoxOutput("minBox"),
              #          infoBoxOutput("avgBox")),
              fluidRow(box(leafletOutput('distributionmap',height = 500),width = 12)),
              selectizeInput('selected_dist','Select Item to Display', choice_dist)),
              #         box(htmlOutput('hist'), height=300))),
      tabItem(tabName = 'ntamap',
              fluidRow(box(leafletOutput('ntatreemap'),width=9)),
              selectizeInput('selected_nta','Select Item to Display', choice_nta)),
              #fluidRow(infoBoxOutput("treemap"))
              #fluidRow(box(DT::dataTableOutput('table'), width=12))
      tabItem(tabName = 'zipmap',
              fluidRow(box(leafletOutput('ziptreemap'),width=9)),
              selectizeInput('selected_zip_meta','Select Item to Display meta', choice_zip_meta),
              selectizeInput('selected_zip','Select Item to Display', choice_zip)),
      tabItem(tabName = 'treegraphs',
              fluidRow(box(htmlOutput('treeType'),width=6),
                       box(htmlOutput('treeStatus'),width=6)),
              fluidRow(box(htmlOutput('treeHealth'),width=6),
                       box(htmlOutput('treeDbh'),width=6))),
      tabItem(tabName = 'complaintgraphs',
              fluidRow(box(htmlOutput('complatintType'),width=6),
                       box(htmlOutput('complaintDesc'),width=6)))
    )
  )
))