ui <- dashboardPage(
  dashboardHeader(
    title = "NOAA AFSC Longline Survey Data Portal", 
    titleWidth = 480,
    
    # --- CUSTOM NOAA LOGO AT TOP RIGHT ---
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://www.noaa.gov/", 
        target = "_blank",
        style = "padding: 5px 15px 5px 5px; background-color: transparent !important;",
        tags$img(
          src = "noaa_logo.png", 
          height = "40px", 
          alt = "NOAA Logo"
        )
      )
    )
  ),
  
  dashboardSidebar(
    width = 480,
    sidebarMenu(
      menuItem("Station Map", tabName = "maps_tab", icon = icon("map")),
      menuItem("Data Export", tabName = "export_tab", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    # --- DOM MANIPULATION & CSS ---
    tags$head(
      tags$script(HTML("
        $(document).ready(function() {
          // Prepend the hamburger icon element directly into the logo element
          $('.sidebar-toggle').prependTo('.main-header .logo');
        });
      ")),
      tags$style(HTML("
        /* Style the moved toggle inside the logo */
        .main-header .logo .sidebar-toggle {
          float: left !important;
          padding: 0 12px 0 0 !important;
          line-height: 50px !important;
          color: #ffffff !important;
          background-color: transparent !important;
        }
        .main-header .logo .sidebar-toggle:hover {
          background-color: rgba(0, 0, 0, 0.1) !important;
        }
        /* Ensure the title text aligns horizontally next to the toggle icon */
        .main-header .logo {
          text-align: left !important;
          padding-left: 15px !important;
          display: flex !important;
          align-items: center !important;
        }
        /* Style the right-hand header navbar item container */
        .main-header .navbar-custom-menu {
          float: right !important;
        }
      "))
    ),
    
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