server <- function(input, output, session) {

    #data <- reactive({
    #     x <- df %>% 
    #         mutate(location, paste0(df$latitude,':',df$longitude))
    #    nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71), maptype = "terrain", zoom = 11)
    #})
       
    output$distributionmap <- renderLeaflet({
        
        leaflet(df) %>%
            addTiles('Trees of NY') %>%
            setView(-74.00, 40.71, zoom = 10) %>%
            addProviderTiles("CartoDB.Positron") %>% 
            addCircles(radius = 30, weight = 1, color = "#77AA77",
                       fillColor = '#77AA77', fillOpacity = 0.5)
        
        # leaflet(Andrew) %>%
        #     addProviderTiles("Esri.WorldStreetMap") %>%
        #     addPolylines(~Long, ~Lat)
        
    })
    
    output$treemap <- renderLeaflet({
        neStates <- subset(df, df$nta %in% c(
            "QN17","QN49","BK90"
        ))
        
        leaflet(df) %>%
            addTiles('Trees of NY') %>%
            setView(-74.00, 40.71, zoom = 10) %>%
            addProviderTiles("CartoDB.Positron") %>% 
            addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5,
                        fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE))
            
        # leaflet(Andrew) %>%
        #     addProviderTiles("Esri.WorldStreetMap") %>%
        #     addPolylines(~Long, ~Lat)
        
       
            
        
    })
    
    
        
        #ggmap(nyc_map) + 
        #geom_polygon(data=df, aes(x=long, y=lat, group=group), color="blue", fill=NA)
        
        # renderGvis({
        # gvisGeoChart(data = df, locationvar = 'location',
        #              options=list(region="US-NY", displayMode="marker",
        #                           resolution="metros",markerOpacity=0.5,
        #                           width="500", height="500")
        # )
        #gvisHistogram(df[, input$selected, drop=F])
        
    # })
        # renderLeaflet({
        # df <- data()
        # 
        # leaflet(data = df) %>%
        #     addTiles() %>%
        #     addMarkers(lng = ~longitude,
        #                lat = ~latitude,
        #                popup = paste("NTA name", df$nta_name, "<br>",
        #                              "SPEC:", df$spc_common))
        # })
}