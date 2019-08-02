server <- function(input, output, session) {

    output$distributionmap <- renderLeaflet({
        
        leaflet(tree_df_sample) %>%
            addTiles('Trees of NY') %>%
            setView(-74.00, 40.71, zoom = 10) %>%
            addProviderTiles("CartoDB.Positron") %>% 
            addCircles(radius = 10, weight = 0, color = "#77AA77",
                       fillColor = '#77AA77', fillOpacity = 0.3)
    })
    
    output$treemap <- renderLeaflet({
        #bins <- c(0,30, 60, 90, 120, 150, 180, Inf)
        #pal <- colorBin("BuGn", domain = NULL, bins = bins)
        pal <- colorNumeric("BuGn", NULL)
        pas_data = nta_map[[input$selected]]
        
        leaflet(nta_map) %>%
            setView(-74.00, 40.71, zoom = 10) %>%
            addProviderTiles("CartoDB.Positron") %>% 
            addPolygons(label=~ntaname,
                        fillColor = ~pal(pas_data),
                        color = 'white',
                        weight = 1,
                        smoothFactor = 0.5,
                        opacity = 0.5,
                        fillOpacity = 0.5,
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
    
    
}