library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(sf)
library(here)

setwd(here())

# --- LOAD DATA & STATIC SHAPEFILES ---
station_cpue <- read.csv("data/station_cpue.csv")

# Load static management regions shapefile
regions_sf <- st_read("shapefiles/sablefish_management_regions.shp") %>%
  st_transform(4326) # Leaflet requires EPSG:4326

# --- UI SIDE ---
ui <- dashboardPage(
  dashboardHeader(title = "Fisheries Station Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Station Map", tabName = "maps_tab", icon = icon("map")),
      menuItem("Data Export", tabName = "export_tab", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Tab 1: Map View
      tabItem(tabName = "maps_tab",
              fluidRow(
                box(title = "Map Controls", width = 4, status = "primary", solidHeader = TRUE,
                    
                    # Visualization Mode Selector
                    radioButtons("map_viz_type", "Visualization Mode:",
                                 choices = c("Station Markers" = "stations",
                                             "Region Choropleth" = "regions"),
                                 selected = "stations"),
                    hr(),
                    
                    selectInput("species", "Select Species:",
                                choices = unique(station_cpue$Species),
                                selected = "Sablefish"),
                    
                    # Year Selector
                    selectInput("map_year", "Select Year:", 
                                choices = unique(station_cpue$Year), 
                                selected = max(station_cpue$Year)),
                    
                    # Dynamic Metric Selector
                    selectInput("map_metric", "Select Metric to Display:",
                                choices = c("Catch" = "TotalCatch",
                                            "Mean Length (cm)" = "MeanLength",
                                            "Mean Weight (kg)" = "MeanWeight"),
                                selected = "TotalCatch")
                ),
                
                box(title = "Station Map", width = 8, status = "success", solidHeader = TRUE,
                    leafletOutput("station_map", height = "600px")
                )
              )
      ),
      
      # Tab 2: Export View
      tabItem(tabName = "export_tab",
              fluidRow(
                box(title = "Query Options", width = 4, status = "primary", solidHeader = TRUE,
                    checkboxGroupInput("exp_years", "Select Years:", 
                                       choices = unique(station_cpue$Year), 
                                       selected = max(station_cpue$Year)),
                    radioButtons("exp_agg", "Aggregation Level:", 
                                 choices = c("Station" = "station", "Region/Area" = "region")),
                    radioButtons("exp_metric", "Output Metrics:", 
                                 choices = c("Catch Numbers" = "TotalCatch", "Length Frequency" = "MeanLength")),
                    br(),
                    downloadButton("download_data", "Export Data (CSV)", class = "btn-block btn-success")
                ),
                box(title = "Data Preview", width = 8, status = "success", solidHeader = TRUE,
                    tableOutput("preview_table")
                )
              )
      )
    )
  )
)

# --- SERVER SIDE ---
server <- function(input, output, session) {
  
  # 0. Dynamic UI Observer: Change dropdown label based on Station vs Region mode
  observeEvent(input$map_viz_type, {
    current_selected <- input$map_metric
    
    if (input$map_viz_type == "regions") {
      new_choices <- c("Average Catch / Station" = "TotalCatch",
                       "Mean Length (cm)" = "MeanLength",
                       "Mean Weight (kg)" = "MeanWeight")
    } else {
      new_choices <- c("Catch" = "TotalCatch",
                       "Mean Length (cm)" = "MeanLength",
                       "Mean Weight (kg)" = "MeanWeight")
    }
    
    updateSelectInput(
      session, 
      "map_metric", 
      choices = new_choices, 
      selected = current_selected
    )
  })
  
  # 1. Filtered Station Level Data
  filtered_map_data <- reactive({
    station_cpue %>%
      filter(Year == as.numeric(input$map_year),
             Species == input$species) %>%
      filter(!is.na(.data[[input$map_metric]]))
  })
  
  # 2. Aggregated Regional Data (Normalized per Station)
  region_map_data <- reactive({
    agg_df <- station_cpue %>%
      filter(Year == as.numeric(input$map_year),
             Species == input$species) %>%
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
  
  # 3. Render Base Map
  output$station_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      fitBounds(min(station_cpue$Longitude, na.rm = TRUE), min(station_cpue$Latitude, na.rm = TRUE),
                max(station_cpue$Longitude, na.rm = TRUE), max(station_cpue$Latitude, na.rm = TRUE))
  })
  
  # 4. Observer: Dynamically toggle elements on Leaflet Proxy
  observe({
    proxy <- leafletProxy("station_map")
    
    # Wipe all vector layers, markers, and legends before rendering chosen view
    proxy %>% clearMarkers() %>% clearShapes() %>% clearControls()
    
    metric_name <- input$map_metric
    
    if (input$map_viz_type == "stations") {
      # --- STATION MARKERS ONLY MODE ---
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
      # --- REGION CHOROPLETH POLYGONS ONLY MODE ---
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
            weight = 3,
            color = "#000000",
            fillOpacity = 0.85,
            bringToFront = TRUE
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
  
  # 5. Export Data Logic
  filtered_export_data <- reactive({
    req(input$exp_years)
    
    df <- station_cpue %>% filter(Year %in% input$exp_years)
    
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
    
    if (input$exp_metric == "TotalCatch") {
      df <- df %>% select(-any_of(c("MeanLength", "MeanWeight")))
    } else {
      df <- df %>% select(-any_of("TotalCatch"))
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

shinyApp(ui, server)