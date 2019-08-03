server <- function(input, output, session) {
    
    tree_data <- reactive({
        if(input$selected_dist == 'Tree'){
            tree_df_sample
        }else if(input$selected_dist == 'Complaint'){
            tree_311_df
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
                addCircles(data = tree_311_df, radius = 10, weight = 0, color = "#AA7777",
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
        #bins <- c(0,30, 60, 90, 120, 150, 180, Inf)
        #pal <- colorBin("BuGn", domain = NULL, bins = bins)
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
    
    draw_single_zip <- reactive({
        if(input$selected_zip_meta == 'Tree only'){
            pal <- colorNumeric("BuGn", NULL)
            pas_data <- zip_map_view[[input$selected_zip]]
        }else if(input$selected_zip_meta == 'Complaint only'){
            pal <- colorNumeric("OrRd", NULL)
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
            pal_g <- colorNumeric("Greens", NULL)
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
                            opacity = 0.5,
                            fillOpacity = 0.7,
                            highlightOptions = highlightOptions(color = "black",
                                                                weight = 1,
                                                                bringToFront = T))  %>% 
                addPolygons(label=~PO_NAME,
                            fillColor = ~pal_r(pas_data_complaint),
                            color = 'white',
                            weight = 1,
                            smoothFactor = 0.5,
                            opacity = 0.5,
                            fillOpacity = 0.7,
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
    
    output$treeType <- renderGvis({
        # gvisHistogram(tree_df_type[,input$selected, drop=FALSE])
        gvisColumnChart(tree_df_type, xvar='spc_common', yvar='cnt',
                        options=list(title='NYC Tree Type',
                                     colors="['#77AA77']",
                                     vAxes="[{title:'count'},{title:'Tree Type'}]"))
    })
    
    output$treeStatus <- renderGvis({
        gvisColumnChart(tree_df_status, xvar='status', yvar='cnt',
                        options=list(title='NYC Tree Status',
                                     colors="['#cbb69d']",
                                     vAxes="[{title:'count'},{title:'Tree Type'}]"))
    })
    
    output$treeHealth <- renderGvis({
        gvisColumnChart(tree_df_health, xvar='health', yvar='cnt',
                        options=list(title='NYC Tree Health',
                                     colors="['#cb688d']",
                                     vAxes="[{title:'count'},{title:'Tree Type'}]"))
    })
    
    output$treeDbh <- renderGvis({
        gvisColumnChart(tree_df_dbh, xvar='tree_dbh', yvar='cnt',
                        options=list(title='NYC Tree diameter',
                                     colors="['#8b96cd']",
                                     vAxes="[{title:'count'},{title:'Tree Type'}]"))
    })
    
    output$complatintType <- renderGvis({
        gvisColumnChart(tree311_df_type, xvar='Complaint.Type', yvar='cnt',
                        options=list(title='NYC Tree Complaint by type',
                                     colors="['#db588d']",
                                     vAxes="[{title:'count'},{title:'Tree Type'}]"))
    })
    
    output$complaintDesc <- renderGvis({
        gvisColumnChart(tree311_df_desc, xvar='Descriptor', yvar='cnt',
                        options=list(title='NYC Tree Complaint by descriptor',
                                     colors="['#cbc8dd']",
                                     vAxes="[{title:'count'},{title:'Tree Type'}]"))
    })
}