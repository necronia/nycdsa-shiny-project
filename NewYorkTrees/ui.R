shinyUI(dashboardPage(
  dashboardHeader(title = 'NYC Tree Dashboard'),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = 'Distribution',tabName = 'distribution', icon = icon('map')),
      menuItem(text = 'NTA Map',tabName = 'ntamap', icon = icon('map')),
      menuItem(text = 'POST Map',tabName = 'zipmap', icon = icon('map')),
      menuItem(text = 'BORO Map',tabName = 'boromap', icon = icon('map')),
      menuItem(text = 'Complaint Graph',tabName = 'complaintgraphs', icon = icon('chart-bar')),
      menuItem(text = 'Tree Graph',tabName = 'treegraphs', icon = icon('chart-bar')),
      menuItem(text = 'Rank Graph',tabName = 'rankgraph', icon = icon('chart-line')),
      menuItem(text = 'Total Graph',tabName = 'totalgraph', icon = icon('chart-pie')),
      menuItem(text = 'Diameter Graph',tabName = 'diametergraph', icon = icon('chart-bar')),
      menuItem(text = 'Compare Graph',tabName = 'comparegraph', icon = icon('chart-line'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'distribution',
              h2("Distribution"),
              fluidRow(box(leafletOutput('distributionmap',height = 500),width = 12)),
              selectizeInput('selected_dist','Select Item to Display', choice_dist)),
      tabItem(tabName = 'ntamap',
              h2("Distribution by NTA"),
              fluidRow(box(leafletOutput('ntatreemap'),width=6),
                       column(htmlOutput('ntatreedata'),width=6)),
              selectizeInput('selected_nta','Select Item to Display', choice_nta)),
      tabItem(tabName = 'zipmap',
              h2("Distribution by Postcode"),
              fluidRow(box(leafletOutput('ziptreemap'),width=12)),
              selectizeInput('selected_zip_meta','Select Item to Display meta', choice_zip_meta),
              selectizeInput('selected_zip','Select Item to Display', choice_zip)),
      tabItem(tabName = 'boromap',
              h2("Distribution by Borough"),
              fluidRow(box(leafletOutput('borotreemap'),width=12)),
              selectizeInput('selected_boro','Select Item to Display', choice_boro)),
      tabItem(tabName = 'complaintgraphs',
              h2("Complaints about street tree"),
              fluidRow(box(htmlOutput('complatintType'),width=6),
                       box(htmlOutput('complaintDesc'),width=6)),
              selectizeInput('selected_compl','Select Item to Display', choice_compl)),
      tabItem(tabName = 'treegraphs',
              h2("Statistics for street trees"),
              fluidRow(box(htmlOutput('treeType'),width=6),
                       box(htmlOutput('treeDbh'),width=6)),
              fluidRow(box(htmlOutput('treeHealth'),width=6),
                       box(htmlOutput('treeStatus'),width=6)),
              selectizeInput('selected_tree','Select Item to Display', choice_tree)),
      tabItem(tabName = 'rankgraph',
              h2("Rank about street tree’s quantity and health ratio"),
              fluidRow(column(htmlOutput('treeRank'),width=6)),
              radioButtons('selected_rank','Select Item to Display', choice_rank)),
      tabItem(tabName = 'diametergraph',
              h2("Average size of street for each data"),
              fluidRow(column(htmlOutput('treeDiam'),width=6)),
              selectizeInput('selected_diam','Select Item to Display', choice_diam)),
      tabItem(tabName = 'totalgraph',
              h2("Ratio by number of tree"),
              fluidRow(column(htmlOutput('treeTotal'),width=6)),
              selectizeInput('selected_column','Select Item to Display', choice_column)),
      tabItem(tabName = 'comparegraph',
              h2("Compare data by each borough"),
              fluidRow(column(htmlOutput('treeCompare'),width=6)),
              fluidRow(column(selectizeInput('selected_compare_1','Select Item to Compair left side', choice_compair_1),width=6),
                       column(selectizeInput('selected_compare_2','Select Item to Compair right side', choice_compair_2),width=6)))
    )
  )
))