library(tidyverse)
library(scales)
library(shiny)
library(sf)
library(janitor)
library(ggthemes)
library(colorspace)
library(bslib)
library(bsicons)
library(gt)

# Import data ----------------------------------------------------------------
# climate risk
climate_risk <- read_rds(file = "data/climate-risk.rds")

# import state and county boundaries
state_sf <- st_read(dsn = "data/states.geojson")
county_sf <- st_read(dsn = "data/counties.geojson")

# combine climate risk with county_sf
climate_sf <- left_join(
  x = county_sf,
  y = climate_risk,
  by = join_by(GEOID == state_county_fips_code)
) |>
  as_tibble() |>
  st_as_sf()

# Define UI -----------------------------------------------------------------
#ui <- page_fluid()


ui <- page_navbar( 
  title = "National Risk Index Counties", 
  
  nav_panel(
    title = "National Risk Index",
    layout_sidebar(
      sidebar = sidebar(
        selectizeInput(
          "Select Risk index", 
          "Risk index", 
          list(
            "National Risk Index", 
            "Expected Annual Loss", 
            "Social Vulnerability",
            "Community Resilience"
            )
        )
      ),
      
      #main content - US map
      card(
        card_header("National Risk Map"),
        plotOutput(outputId = "national_map")
      )
      
    )
  ),
    
    
  nav_panel(
    title = "County Details",
    layout_sidebar(
      sidebar = sidebar(
        selectizeInput(
          "Select County", 
          "Selected County", 
          list("county name")
        ),
        checkboxGroupInput( 
          "County Checkbox", 
          "Hazard types", 
          c( 
            "Avalanche",
            "Coastal Flooding",
            "Cold Wave",
            "Drought",
            "Earthquake",
            "Hail",
            "Heat Wave",
            "Hurricane",
            "Ice Storm",
            "Landslide",
            "Lightning",
            "Riverine Flooding",
            "Strong Wind",
            "Tornado",
            "Tsunami",
            "Volcanic Activity",
            "Wildfire",
            "Winter Weather"
          ) 
        ), 
      ),
      
      
      # state map + plot
      layout_column_wrap("Content1"),
      #value boxes
      layout_column_wrap("Content2")
    )
  ), 
  
  
  nav_panel(
    title = "Data",
    climate_risk |>
      gt() |>
      opt_interactive()
    )

  )



# Server function for the National Risk Index Counties Shiny app
server <- function(input, output) {}

# Run the app
shinyApp(ui = ui, server = server)
