shinyUI(dashboardPage(
  dashboardHeader(title = 'NYC Tree Dashboard'),
  dashboardSidebar(
    #sidebarUserPanel(name = 'aaa'),
    sidebarMenu(
      menuItem(text = 'Distribution',tabName = 'distribution', icon = icon('map')),
      menuItem(text = 'NTA Map',tabName = 'ntamap', icon = icon('map')),
      menuItem(text = 'POST Map',tabName = 'zipmap', icon = icon('map')),
      menuItem(text = 'BORO Map',tabName = 'boromap', icon = icon('map')),
      menuItem(text = 'Tree Graph',tabName = 'treegraphs', icon = icon('chart-bar')),
      menuItem(text = 'Complaint Graph',tabName = 'complaintgraphs', icon = icon('chart-bar')),
      menuItem(text = 'Rank Graph',tabName = 'rankgraph', icon = icon('chart-line')),
      menuItem(text = 'Diameter Graph',tabName = 'diametergraph', icon = icon('chart-bar')),
      menuItem(text = 'Total Graph',tabName = 'totalgraph', icon = icon('chart-pie')),
      menuItem(text = 'Compare Graph',tabName = 'comparegraph', icon = icon('chart-line'))
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
              fluidRow(box(leafletOutput('ntatreemap'),width=6),
                       column(htmlOutput('ntatreedata'),width=6)),
              selectizeInput('selected_nta','Select Item to Display', choice_nta)),
              #fluidRow(infoBoxOutput("treemap"))
              #fluidRow(box(DT::dataTableOutput('table'), width=12))
      tabItem(tabName = 'zipmap',
              fluidRow(box(leafletOutput('ziptreemap'),width=12)),
              selectizeInput('selected_zip_meta','Select Item to Display meta', choice_zip_meta),
              selectizeInput('selected_zip','Select Item to Display', choice_zip)),
      tabItem(tabName = 'boromap',
              fluidRow(box(leafletOutput('borotreemap'),width=12)),
              selectizeInput('selected_boro','Select Item to Display', choice_boro)),
              # selectizeInput('selected_zip','Select Item to Display', choice_zip)),
      tabItem(tabName = 'treegraphs',
              fluidRow(box(htmlOutput('treeType'),width=6),
                       box(htmlOutput('treeStatus'),width=6)),
              fluidRow(box(htmlOutput('treeHealth'),width=6),
                       box(htmlOutput('treeDbh'),width=6)),
              selectizeInput('selected_tree','Select Item to Display', choice_tree)),
      tabItem(tabName = 'complaintgraphs',
              fluidRow(box(htmlOutput('complatintType'),width=6),
                       box(htmlOutput('complaintDesc'),width=6)),
              selectizeInput('selected_compl','Select Item to Display', choice_compl)),
      tabItem(tabName = 'rankgraph',
              fluidRow(column(htmlOutput('treeRank'),width=6))),
      tabItem(tabName = 'diametergraph',
              fluidRow(column(htmlOutput('treeDiam'),width=6)),
              selectizeInput('selected_diam','Select Item to Display', choice_diam)),
      tabItem(tabName = 'totalgraph',
              fluidRow(column(htmlOutput('treeTotal'),width=6)),
              selectizeInput('selected_column','Select Item to Display', choice_column)),
      tabItem(tabName = 'comparegraph',
              fluidRow(column(htmlOutput('treeCompare'),width=6)),
              fluidRow(column(selectizeInput('selected_compare_1','Select Item to Compair left side', choice_compair_1),width=6),
                       column(selectizeInput('selected_compare_2','Select Item to Compair right side', choice_compair_2),width=6)))
    )
  )
))