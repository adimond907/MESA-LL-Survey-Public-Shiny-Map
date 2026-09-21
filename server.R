server <- function(input, output, session) {
  
  # --- 0. DYNAMIC UI DROPDOWN UPDATE ---
  observeEvent(input$map_viz_type, {
    current_selected <- input$map_metric
    
    if (input$map_viz_type == "regions") {
      new_choices <- c(
        "Average Catch / Station" = "TotalCatch",
        "Mean Length (cm)" = "MeanLength",
        "Mean Weight (kg)" = "MeanWeight"
      )
    } else {
      new_choices <- c(
        "Catch" = "TotalCatch",
        "Mean Length (cm)" = "MeanLength",
        "Mean Weight (kg)" = "MeanWeight"
      )
    }
    
    updateSelectInput(
      session, 
      "map_metric", 
      choices = new_choices, 
      selected = current_selected
    )
  })
  
  # --- 1. REACTIVE FILTERING FOR MAP ---
  filtered_map_data <- reactive({
    station_cpue %>%
      filter(Year == as.numeric(input$map_year), Species == input$species) %>%
      filter(!is.na(.data[[input$map_metric]]))
  })
  
  region_map_data <- reactive({
    agg_df <- station_cpue %>%
      filter(Year == as.numeric(input$map_year), Species == input$species) %>%
      group_by(Area) %>%
      summarise(
        AvgCatchPerStation = mean(TotalCatch, na.rm = TRUE),
        MeanLength = mean(MeanLength, na.rm = TRUE),
        MeanWeight = mean(MeanWeight, na.rm = TRUE),
        StationCount = n(),
        .groups = "drop"
      )
    
    regions_sf %>%
      left_join(agg_df, by = c("Area" = "Area"))
  })
  
  # --- 2. BASE LEAFLET MAP ---
  output$station_map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      fitBounds(
        min(station_cpue$Longitude, na.rm = TRUE), min(station_cpue$Latitude, na.rm = TRUE),
        max(station_cpue$Longitude, na.rm = TRUE), max(station_cpue$Latitude, na.rm = TRUE)
      )
  })
  
  # --- 3. DYNAMIC LEAFLET PROXY OBSERVER ---
  observe({
    proxy <- leafletProxy("station_map")
    proxy %>% clearMarkers() %>% clearShapes() %>% clearControls()
    
    metric_name <- input$map_metric
    
    if (input$map_viz_type == "stations") {
      data <- filtered_map_data()
      req(nrow(data) > 0)
      
      metric_values <- data[[metric_name]]
      pal <- colorNumeric(palette = "YlOrRd", domain = metric_values)
      
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
                          "<strong>Catch / Value:</strong> ", round(metric_values, 2))
        ) %>%
        addLegend(
          pal = pal, 
          values = metric_values, 
          title = ifelse(metric_name == "TotalCatch", "Catch", metric_name), 
          position = "bottomright"
        )
      
    } else {
      sf_data <- region_map_data()
      req(nrow(sf_data) > 0)
      
      if (metric_name == "TotalCatch") {
        plot_col <- "AvgCatchPerStation"
        legend_label <- "Avg Catch / Station"
      } else if (metric_name == "MeanLength") {
        plot_col <- "MeanLength"
        legend_label <- "Mean Length (cm)"
      } else {
        plot_col <- "MeanWeight"
        legend_label <- "Mean Weight (kg)"
      }
      
      metric_values <- sf_data[[plot_col]]
      pal <- colorNumeric(palette = "YlOrRd", domain = metric_values, na.color = "#CCCCCC")
      
      proxy %>%
        addPolygons(
          data = sf_data,
          color = "#222222",
          weight = 2,
          fillColor = ~pal(metric_values),
          fillOpacity = 0.65,
          highlightOptions = highlightOptions(
            weight = 3, color = "#000000", fillOpacity = 0.85, bringToFront = TRUE
          ),
          popup = ~paste0("<strong>Region:</strong> ", Area, "<br>",
                          "<strong>Stations Surveyed:</strong> ", ifelse(is.na(StationCount), 0, StationCount), "<br>",
                          "<strong>Average Catch / Station:</strong> ", ifelse(is.na(AvgCatchPerStation), "N/A", round(AvgCatchPerStation, 2)), "<br>",
                          "<strong>Mean Length (cm):</strong> ", ifelse(is.na(MeanLength), "N/A", round(MeanLength, 2)), "<br>",
                          "<strong>Mean Weight (kg):</strong> ", ifelse(is.na(MeanWeight), "N/A", round(MeanWeight, 2)))
        ) %>%
        addLegend(
          pal = pal, 
          values = metric_values, 
          title = legend_label, 
          position = "bottomright"
        )
    }
  })
  
  # --- 4. EXPORT HANDLER ---
  filtered_export_data <- reactive({
    req(input$exp_years, input$exp_species)
    
    df <- station_cpue %>% 
      filter(
        Year %in% input$exp_years,
        Species == input$exp_species
      )
    
    if (input$exp_agg == "region") {
      df <- df %>%
        group_by(Year, Survey, Area_ID, Area, Species, species_code) %>%
        summarise(
          StationCount = n(),
          TotalCatch = sum(TotalCatch, na.rm = TRUE),
          MeanLength = mean(MeanLength, na.rm = TRUE),
          MeanWeight = mean(MeanWeight, na.rm = TRUE),
          .groups = 'drop'
        )
    }
    
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