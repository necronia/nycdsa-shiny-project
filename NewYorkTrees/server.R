server <- function(input, output, session) {
    
    tree_data <- reactive({
        if(input$selected_dist == 'Tree'){
            tree_df_sample
        }else if(input$selected_dist == 'Complaint'){
            tree311_df
        }
    })
    
    dot_color <- reactive({
        if(input$selected_dist == 'Tree'){
            "#77AA77"
        }else if(input$selected_dist == 'Complaint'){
            "#AA7777"
        }
    })
    
    draw_tree <- reactive({
        if(input$selected_dist == 'All'){
            leaflet() %>%
                addTiles('Trees of NY') %>%
                setView(-74.00, 40.71, zoom = 10) %>%
                addProviderTiles("CartoDB.Positron") %>% 
                addCircles(data = tree_df_sample, radius = 10, weight = 0, color = "#77AA77",
                           fillColor = "#77AA77", fillOpacity = 0.3) %>% 
                addCircles(data = tree311_df, radius = 10, weight = 0, color = "#AA7777",
                           fillColor = "#AA7777", fillOpacity = 0.3) %>% 
                addLegend(labels = c("Num.of Trees","Num.of Complaints"),
                          colors = c("#77AA77","#AA7777"),
                          opacity = 0.7,
                          title = NULL,
                          position = "topleft")
        }else {
            leaflet(tree_data()) %>%
                addTiles('Trees of NY') %>%
                setView(-74.00, 40.71, zoom = 10) %>%
                addProviderTiles("CartoDB.Positron") %>% 
                addCircles(radius = 10, weight = 0, color = dot_color(),
                           fillColor = dot_color(), fillOpacity = 0.3)
        }
    })

    output$distributionmap <- renderLeaflet({
        draw_tree()
    })
    
    output$ntatreemap <- renderLeaflet({
        pal <- colorNumeric("BuGn", NULL)
        pas_data = nta_map_view[[input$selected_nta]]
        
        leaflet(nta_map) %>%
            setView(-74.00, 40.71, zoom = 10) %>%
            addProviderTiles("CartoDB.Positron") %>% 
            addPolygons(label=~ntaname,
                        fillColor = ~pal(pas_data),
                        color = 'white',
                        weight = 1,
                        smoothFactor = 0.5,
                        opacity = 0.5,
                        fillOpacity = 0.7,
                        popup = paste('NTA name : ', nta_map$ntaname, '<br>',
                                      'Count of tree : ', nta_map$count, '<br>',
                                      'Density(tree per km2) : ', nta_map$countperarea),
                        highlightOptions = highlightOptions(color = "black",
                                                            weight = 1,
                                                            bringToFront = T))  %>% 
           addLegend(label='density(per km2)',
              pal = pal,
              values = ~pas_data,
              opacity = 0.7,
              title = NULL,
              position = "topleft")
    })
    
    nta_data_reord <- reactive({
        nta_data %>% 
            arrange(desc(nta_data[[input$selected_nta]]))
    })
    
    output$ntatreedata <- renderGvis({
        gvisBarChart(nta_data_reord(), xvar='ntaname', yvar=input$selected_nta,
                        options=list(title='NYC Tree Count by NTA',
                                     colors="['#abe8bd']",
                                     vAxes="[{title:'count'},{title:'Tree Type'}]",
                                     legend='none',height=420)
        )
    })
    
    draw_single_zip <- reactive({
        if(input$selected_zip_meta == 'Tree only'){
            pal <- colorNumeric("Blues", NULL)
            pas_data <- zip_map_view[[input$selected_zip]]
        }else if(input$selected_zip_meta == 'Complaint only'){
            pal <- colorNumeric("Reds", NULL)
            pas_data <- zip_map_view[[gsub('count','complaint',input$selected_zip)]]
        }
        leaflet(zip_map) %>%
            setView(-74.00, 40.71, zoom = 10) %>%
            addProviderTiles("CartoDB.Positron") %>% 
            addPolygons(label=~PO_NAME,
                        fillColor = ~pal(pas_data),
                        color = 'white',
                        weight = 1,
                        smoothFactor = 0.5,
                        opacity = 0.5,
                        fillOpacity = 0.7,
                        popup = paste('NTA name : ', zip_map$PO_NAME, '<br>',
                                      'Count of tree : ', zip_map$count, '<br>',
                                      'Density(tree per km2) : ', zip_map$countperarea,'<br>'),
                        highlightOptions = highlightOptions(color = "black",
                                                            weight = 1,
                                                            bringToFront = T))  %>% 
            addLegend(label='density(per km2)',
                      pal = pal,
                      values = ~pas_data,
                      opacity = 0.7,
                      title = NULL,
                      position = "topleft")
    })
    
    draw_zip <- reactive({
        if (input$selected_zip_meta == 'All'){
            pal_g <- colorNumeric("Blues", NULL)
            pal_r <- colorNumeric("Reds", NULL)
            pas_data_tree = zip_map_view[[input$selected_zip]]
            pas_data_complaint = zip_map_view[[gsub('count','complaint',input$selected_zip)]]
            leaflet(zip_map) %>%
                setView(-74.00, 40.71, zoom = 10) %>%
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(label=~PO_NAME,
                            fillColor = ~pal_g(pas_data_tree),
                            color = 'white',
                            weight = 1,
                            smoothFactor = 0.5,
                            opacity = 1,
                            fillOpacity = 1,
                            highlightOptions = highlightOptions(color = "black",
                                                                weight = 1,
                                                                bringToFront = T))  %>% 
                addPolygons(label=~PO_NAME,
                            fillColor = ~pal_r(pas_data_complaint),
                            color = 'white',
                            weight = 1,
                            smoothFactor = 0.5,
                            opacity = 0.5,
                            fillOpacity = 0.5,
                            highlightOptions = highlightOptions(color = "black",
                                                                weight = 1,
                                                                bringToFront = T))
        }else {
            draw_single_zip()
        }
    })
    
    output$ziptreemap <- renderLeaflet({
        draw_zip()
    })
    
    get_map_color <- reactive({
        if(input$selected_boro %in% c('countperarea','count')){
            return('Greens')
        }else if(input$selected_boro %in% c('complaintpertree','complaint')){
            return('Reds')
        }
    })
    
    output$borotreemap <- renderLeaflet({
        pal <- colorNumeric(get_map_color(), NULL)
        pas_data <- boro_map[[input$selected_boro]]
       
        leaflet(boro_map) %>%
            setView(-74.00, 40.71, zoom = 10) %>%
            addProviderTiles("CartoDB.Positron") %>% 
            addPolygons(label=~boro_name,
                        fillColor = ~pal(pas_data),
                        color = 'white',
                        weight = 1,
                        smoothFactor = 0.5,
                        opacity = 0.5,
                        fillOpacity = 0.7,
                        popup = paste('NTA name : ', boro_map$boro_name, '<br>',
                                      'Count of tree : ', boro_map$count, '<br>',
                                      'Density(tree per km2) : ', boro_map$countperarea,'<br>'),
                        highlightOptions = highlightOptions(color = "black",
                                                            weight = 1,
                                                            bringToFront = T))  %>% 
            addLegend(label='density(per km2)',
                      pal = pal,
                      values = ~pas_data,
                      opacity = 0.7,
                      title = NULL,
                      position = "topleft")
    })
    
    draw_treeType <- reactive({
        if(input$selected_tree == 'All'){
            tree_df_type %>% 
                group_by(spc_common) %>% 
                summarise(cnt = sum(cnt)) %>% 
                arrange(desc(cnt)) %>% 
                top_n(20)
        }else{
            tree_df_type %>% 
                filter(borough==input$selected_tree) %>% 
                group_by(spc_common) %>% 
                summarise(cnt = sum(cnt)) %>% 
                arrange(desc(cnt)) %>% 
                top_n(20)
        }
    })
    
    output$treeType <- renderGvis({
        gvisBarChart(draw_treeType(), xvar='spc_common', yvar='cnt',
                        options=list(title='NYC Tree Type',
                                     colors="['#77AA77']",
                                     vAxes="[{title:'count'},{title:'Tree Type'}]",
                                     legend='none',
                                     height=300))
    })
    
    draw_treeStatus <- reactive({
        if(input$selected_tree == 'All'){
            tree_df_status %>% 
                group_by(status) %>% 
                summarise(cnt = sum(cnt)) %>% 
                arrange(desc(cnt)) 
        }else{
            tree_df_status %>% 
                filter(borough==input$selected_tree) %>% 
                arrange(desc(cnt))
        }
    })
    
    output$treeStatus <- renderGvis({
        gvisColumnChart(draw_treeStatus(), xvar='status', yvar='cnt',
                        options=list(title='NYC Tree Status',
                                     colors="['#cbb69d']",
                                     vAxes="[{title:'count'},{title:'Tree Type'}]",
                                     legend='none'))
    })
    
    draw_treeHealth <- reactive({
        if(input$selected_tree == 'All'){
            tree_df_health %>% 
                group_by(health) %>% 
                summarise(cnt = sum(cnt)) %>% 
                arrange(desc(cnt)) 
        }else{
            tree_df_health %>% 
                filter(borough==input$selected_tree) %>% 
                arrange(desc(cnt))
        }
    })
    
    output$treeHealth <- renderGvis({
        gvisColumnChart(draw_treeHealth(), xvar='health', yvar='cnt',
                        options=list(title='NYC Tree Health',
                                     colors="['#cb688d']",
                                     vAxes="[{title:'count'},{title:'Tree Type'}]",
                                     legend='none'))
    })
    
    draw_treeDbh <- reactive({
        if(input$selected_tree == 'All'){
            tree_df_dbh %>% 
                group_by(tree_dbh) %>% 
                summarise(cnt = sum(cnt)) %>% 
                arrange(desc(cnt)) 
        }else{
            tree_df_dbh %>% 
                filter(borough==input$selected_tree) %>% 
                arrange(desc(cnt))
        }
    })
    
    output$treeDbh <- renderGvis({
        gvisBarChart(draw_treeDbh(), xvar='tree_dbh', yvar='cnt',
                        options=list(title='NYC Tree diameter',
                                     colors="['#8b96cd']",
                                     vAxes="[{title:'count'},{title:'Tree Type'}]",
                                     legend='none',
                                     height=300))
    })
    
    draw_complatintType <- reactive({
        if(input$selected_compl == 'All'){
            tree311_df_type %>% 
                group_by(Complaint.Type) %>% 
                summarise(cnt = sum(cnt)) %>% 
                arrange(desc(cnt)) 
        }else{
            tree311_df_type %>% 
                filter(Borough==input$selected_compl) %>% 
                arrange(desc(cnt))
        }
    })
    
    output$complatintType <- renderGvis({
        gvisColumnChart(draw_complatintType(), xvar='Complaint.Type', yvar='cnt',
                        options=list(title='NYC Tree Complaint by type',
                                     colors="['#FF7043']",
                                     vAxes="[{title:'count'},{title:'Tree Type'}]",
                                     legend='none'))
    })
    
    draw_complaintDesc <- reactive({
        if(input$selected_compl == 'All'){
            tree311_df_desc %>% 
                group_by(Descriptor) %>% 
                summarise(cnt = sum(cnt)) %>% 
                arrange(desc(cnt)) 
        }else{
            tree311_df_desc %>% 
                filter(Borough==input$selected_compl) %>% 
                arrange(desc(cnt))
        }
    })
    
    output$complaintDesc <- renderGvis({
        gvisColumnChart(draw_complaintDesc(), xvar='Descriptor', yvar='cnt',
                        options=list(title='NYC Tree Complaint by descriptor',
                                     colors="['#8D6E63']",
                                     vAxes="[{title:'count'},{title:'Tree Type'}]",
                                     legend='none'))
    })
    
    draw_tree_rank <- reactive({
        if(input$selected_rank =='All'){
            tree_df_rank
        }else{
            tree_df_rank_20
        }
    })
    
    output$treeRank <- renderGvis({
        gvisLineChart(draw_tree_rank(), xvar='spc_common', yvar=c('rank_count','rank_ratio'),
                        options=list(title='NYC Tree rank by count and good health ratio',
                                     colors="['#cbc8dd','#7aa3d9']",
                                     vAxes="[{title:'rank'},{title:'Tree Type'}]",
                                     width=700,height=420))
    })
    
    group_diam_tree <- reactive({
        tree_df %>% 
            group_by_(input$selected_diam) %>% 
            summarise(avg = mean(tree_dbh)) %>% 
            arrange(desc(avg))
    })
    
    output$treeDiam <- renderGvis({
        gvisColumnChart(group_diam_tree(), xvar=input$selected_diam, yvar='avg',
                        options=list(title='NYC Tree average diameter by various column',
                                     colors="['#42A5F5','#26C6DA','#26A69A','#29B6F6','#66BB6A','#FFEE58','#FF7043','#8D6E63']",
                                     vAxes="[{title:'AVG(diameter)'},{title:'Tree Type'}]",
                                     legend='none',
                                     width=700,height=420))
    })
    
    group_count_tree <- reactive({
        tree_df %>% 
            group_by_(input$selected_column) %>% 
            summarise(cnt = n()) %>% 
            arrange(desc(cnt))
    })
    
    output$treeTotal <- renderGvis({
        gvisPieChart(group_count_tree(), labelvar=input$selected_column, numvar='cnt',
                      options=list(title='NYC Tree count by various column',
                                   colors="['#EF5350','#5C6BC0','#26C6DA','#D4E157','#FFEE58','#FFA726','#FF7043','#BDBDBD','#26A69A','#78909C','#7E57C2','#42A5F5','#26C6DA','#26A69A','#29B6F6','#66BB6A','#FFEE58','#FF7043','#8D6E63']",
                                   vAxes="[{title:'count'},{title:'Tree Type'}]",
                                   width=700,height=420))
    })
    
    group_compare_tree <- reactive({
        tree_df_compare
    })
    
    output$treeCompare <- renderGvis({
        gvisLineChart(group_compare_tree(), xvar='Borough', yvar=c(input$selected_compare_1,input$selected_compare_2),
                        options=list(title='NYC Tree data comparision',
                                     series="[{targetAxisIndex: 0}, {targetAxisIndex:1}]",
                                     colors="['#EF5350','#5C6BC0','#26C6DA','#D4E157','#FFEE58','#FFA726','#FF7043','#BDBDBD','#26A69A','#78909C','#7E57C2','#42A5F5','#26C6DA','#26A69A','#29B6F6','#66BB6A','#FFEE58','#FF7043','#8D6E63']",
                                     vAxes="[{title:'Red line'},{title:'Blue line'}]",
                                     width=700,height=420))
    })
    
    
}