ui <- dashboardPage(
  dashboardHeader(title = "NOAA AFSC Longline Survey"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Station Map", tabName = "maps_tab", icon = icon("map")),
      menuItem("Data Export", tabName = "export_tab", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Tab 1: Map View
      tabItem(
        tabName = "maps_tab",
        fluidRow(
          box(
            title = "Map Controls", width = 4, status = "primary", solidHeader = TRUE,
            radioButtons(
              "map_viz_type", "Visualization Mode:",
              choices = c("Station Markers" = "stations", "Region" = "regions"),
              selected = "stations"
            ),
            hr(),
            selectInput(
              "species", "Select Species:",
              choices = unique(station_cpue$Species),
              selected = "Sablefish"
            ),
            selectInput(
              "map_year", "Select Year:", 
              choices = unique(station_cpue$Year), 
              selected = max(station_cpue$Year)
            ),
            selectInput(
              "map_metric", "Select Metric to Display:",
              choices = c("Catch" = "TotalCatch",
                          "Mean Length (cm)" = "MeanLength",
                          "Mean Weight (kg)" = "MeanWeight"),
              selected = "TotalCatch"
            )
          ),
          box(
            title = "Station Map", width = 8, status = "success", solidHeader = TRUE,
            leafletOutput("station_map", height = "600px")
          )
        )
      ),
      
      # Tab 2: Export View
      tabItem(
        tabName = "export_tab",
        fluidRow(
          box(
            title = "Query Options", width = 4, status = "primary", solidHeader = TRUE,
            
            # --- SINGLE SPECIES SELECTOR ---
            selectInput(
              "exp_species", "Select Species:",
              choices = unique(station_cpue$Species),
              selected = "Sablefish",
              multiple = FALSE
            ),
            
            checkboxGroupInput(
              "exp_years", "Select Years:", 
              choices = unique(station_cpue$Year), 
              selected = max(station_cpue$Year)
            ),
            
            radioButtons(
              "exp_agg", "Aggregation Level:", 
              choices = c("Station" = "station", "Region/Area" = "region")
            ),
            
            br(),
            downloadButton("download_data", "Export Data (CSV)", class = "btn-block btn-success")
          ),
          box(
            title = "Data Preview", width = 8, status = "success", solidHeader = TRUE,
            tableOutput("preview_table")
          )
        )
      )
    )
  )
)