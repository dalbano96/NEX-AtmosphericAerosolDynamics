#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# map_functions.R
# Desc - Loads map data with site information for EPA and AERONET
#--------------------------------------------------------------#

#--------------------------------------------------------------#
# Map all site locations from 2014-2017
# Used to determine site locations to analyze
#--------------------------------------------------------------#
load.map <- function() {
  leaflet() %>%
    # AERONET Sites
    addCircles(data = unique(select(subset(all.aod, DateTime.GMT >= "2014-01-01" & DateTime.GMT <= "2017-12-31"), c(Site_Longitude.Degrees., Site_Latitude.Degrees., AERONET_Site))),
               lng = ~Site_Longitude.Degrees.,
               lat = ~Site_Latitude.Degrees.,
               color = "red",
               radius = 5000,
               label = ~paste("[AERONET] Site Name: ", AERONET_Site)) %>%
    # EPA Sites
    addCircles(data = unique(select(hourly.pm25.FRM.14_17, c(Longitude, Latitude, Site.Num, County.Name))), 
               lng = ~Longitude, 
               lat = ~Latitude,
               label = ~paste("[EPA] Site Num: ", Site.Num, ", ",
                              "County Name: ", County.Name)) %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addScaleBar()
}


# draft-01 mapview
latitude <- c(35.94077, 35.83770, 35.84545, 35.81584, 35.79387, 36.05600)
longitude <- c(-78.58010, -78.78084, -78.72444, -78.62568, -78.64262, -78.67600)
radius<-c(15, 12, 12, 12, 12, 15)
ids<-c("a", "b", "c", "d", "e", "f")

shinyApp(
  ui = fluidPage(
    fluidRow(
      leafletMap(
        "map", "100%", 400,
        initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
        options=list(
          center = c(37.45, -93.85),
          zoom = 4,
          maxBounds = list(list(17, -180), list(59, 180))))),
    fluidRow(verbatimTextOutput("Click_text"))),
  server = function(input, output, session){
    map = createLeafletMap(session, 'map')
    session$onFlushed(once=T, function(){
      
      map$addCircleMarker(lat = latitude, 
                          lng = longitude, 
                          radius = radius, 
                          layerId=ids)
    })        
    
    observe({
      click<-input$map_marker_click
      if(is.null(click))
        return()
      text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
      text2<-paste("You've selected point ", click$id)
      map$clearPopups()
      map$showPopup( click$lat, click$lng, text)
      output$Click_text<-renderText({
        text2
      })
      
    })
    
  }
)


# draft-02 mapview
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                            value = range(quakes$mag), step = 0.1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(quakes) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                 fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = quakes)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~mag
      )
    }
  })
}

shinyApp(ui, server)
