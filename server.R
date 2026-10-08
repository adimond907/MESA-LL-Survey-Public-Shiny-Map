server <- function(input, output, session) {
  
  # --- 1. REACTIVE FILTERING FOR MAP ---
  filtered_map_data <- reactive({
    req(input$map_year, input$species, input$map_metric)
    
    station_cpue %>%
      filter(Year == as.numeric(input$map_year), Species == input$species) %>%
      filter(!is.na(.data[[input$map_metric]]))
  })
  
  # --- 2. BASE LEAFLET MAP ---
  output$station_map <- renderLeaflet({
    valid_lng <- station_cpue$Longitude[!is.na(station_cpue$Longitude)]
    valid_lat <- station_cpue$Latitude[!is.na(station_cpue$Latitude)]
    
    map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$Esri.OceanBasemap)
    
    if (length(valid_lng) > 0 && length(valid_lat) > 0) {
      map <- map %>% fitBounds(
        min(valid_lng), min(valid_lat),
        max(valid_lng), max(valid_lat)
      )
    }
    
    map
  })
  
  # --- 3. DYNAMIC LEAFLET PROXY OBSERVER ---
  observe({
    proxy <- leafletProxy("station_map")
    proxy %>% clearMarkers() %>% clearShapes() %>% clearControls()
    
    req(input$map_metric)
    
    data <- filtered_map_data()
    req(nrow(data) > 0)
    
    metric_name <- input$map_metric
    metric_values <- data[[metric_name]]
    valid_values <- metric_values[!is.na(metric_values)]
    req(length(valid_values) > 0)
    
    pal <- colorNumeric(palette = "YlOrRd", domain = valid_values)
    
    proxy %>%
      addCircleMarkers(
        data = data,
        lng = ~Longitude, 
        lat = ~Latitude,
        radius = 7,
        color = "#222222",
        weight = 1,
        fillColor = ~pal(metric_values),
        fillOpacity = 0.85,
        popup = ~paste0("<strong>Station:</strong> ", Station, "<br>",
                        "<strong>Area:</strong> ", Area, "<br>",
                        "<strong>Value:</strong> ", round(metric_values, 2))
      ) %>%
      addLegend(
        pal = pal, 
        values = valid_values, 
        title = ifelse(metric_name == "TotalCatch", "Catch", metric_name), 
        position = "bottomright"
      )
  })
  
  # --- 4. EXPORT HANDLER ---
  filtered_export_data <- reactive({
    req(input$exp_years, input$exp_species)
    
    df <- station_cpue %>% 
      filter(
        Year %in% input$exp_years,
        Species == input$exp_species
      ) %>% 
      select(-any_of("X"))
    
    return(df)
  })
  
  output$preview_table <- renderTable({
    head(filtered_export_data(), 10)
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("fisheries_query_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_export_data(), file, row.names = FALSE)
    }
  )
}