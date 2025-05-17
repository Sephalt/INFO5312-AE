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

# get county names
county_names <- climate_sf |>
  arrange(STATEFP) |>
  pull(county)

# define hazard types
hazard_types <- climate_risk |>
  select(contains("hazard")) |>
  colnames()

# create human-readable labels
hazard_types_labels <- hazard_types |>
  str_remove(pattern = "_hazard_type_risk_index_score") |>
  make_clean_names(case = "title")

# create a named character vector for the input
names(hazard_types) <- hazard_types_labels

# Define UI -----------------------------------------------------------------
ui <- page_navbar(
  title = "National Risk Index Counties",
  theme = bs_theme(version = 5, preset = "minty"),

  # National Risk Index page
  nav_panel(
    title = "National Risk Index",
    layout_sidebar(
      sidebar = sidebar(
        # select between the four risk ratings
        varSelectInput(
          inputId = "risk_var",
          label = "Risk index",
          # select specific columns of data to populate select options
          data = climate_risk |>
            select(`National Risk Index`, `Expected Annual Loss`, `Social Vulnerability`, `Community Resilience`)
        )
      ),
      # Main content
      card(
        card_header("National Risk Map"),
        plotOutput(outputId = "national_map")
      )
    )
  ),

  # County Details page
  nav_panel(
    title = "County Details",
    layout_sidebar(
      sidebar = sidebar(
        # extract county/state labels as character vector
        selectizeInput(
          inputId = "county",
          label = "Selected county",
          choices = county_names,
          selected = NULL,
          # custom selectize.js options
          options = list(
            # placeholder text
            placeholder = "Select a county",
            # limit to one county at a time
            maxItems = 1
          )
        ),

        # identify columns with hazard risks and extract column names
        checkboxGroupInput(
          inputId = "hazard_types",
          label = "Hazard types",
          # all possible choices
          choices = hazard_types,
          # initialize plot with all individual hazards
          selected = hazard_types
        )
      ),
      # Main content
      layout_column_wrap(
        width = "400px", # This width makes two columns when screen is > 800px, one column when narrower
        style = htmltools::css(gap = "10px", margin_bottom = "10px"),
        # Row 1 - Maps side by side on wider screens
        card(
          card_header("County Map"),
          plotOutput(outputId = "county_map")
        ),
        card(
          card_header("County Hazards"),
          plotOutput(outputId = "county_hazards")
        )
      ),
      layout_column_wrap(
        width = 1/4,
        height = "auto",
        style = htmltools::css(gap = "10px"),
        # Row 2 - Value boxes
        value_box(
          title = "Overall risk score",
          value = textOutput("county_risk"),
          showcase = bs_icon("radioactive")
        ),
        value_box(
          title = "Expected annual loss",
          value = textOutput("county_loss"),
          showcase = bs_icon("trash")
        ),
        value_box(
          title = "Social vulnerability",
          value = textOutput("county_vulnerability"),
          showcase = bs_icon("cone-striped")
        ),
        value_box(
          title = "Community resilience",
          value = textOutput("county_resilience"),
          showcase = bs_icon("emoji-sunglasses")
        )
      )
    )
  ),

  # Data page
  nav_panel(
    title = "Data",
    card(
      card_header("National Risk Index Data"),
      climate_risk |>
        gt() |>
        opt_interactive()
    )
  )
)

# Server function for the National Risk Index Counties Shiny app
server <- function(input, output) {

  # National Map
  # developer message
  message("The national map is rendering.")
  
  output$national_map <- renderPlot({
    ggplot(data = climate_sf) +
      # layer for each county's risk
      geom_sf(mapping = aes(fill = !!input$risk_var)) +
      # layer for state boundaries to better locate regions
      geom_sf(data = state_sf, fill = NA, color = "white") +
      # appropriate color palette
      scale_fill_discrete_diverging(rev = TRUE) +
      # don't label the legend
      labs(
        fill = NULL
      ) +
      # map theme
      theme_map(base_size = 18) +
      # position the legend on the right
      theme(legend.position = "right")
  })
  
  
  # County Map
  # developer message
  message("The county map is rendering.")
  
  output$county_map <- renderPlot({
    
    validate(
      need(input$county, "Please select a county.")
    )
    
    # get selected county's state
    county_state <- climate_sf |>
      filter(county == input$county) |>
      pull(STATE_NAME)
    
    climate_sf |>
      # filter for counties in specific state
      filter(STATE_NAME == county_state) |>
      # variable to highlight selected county
      mutate(selected = county == input$county) |>
      # draw the map
      ggplot() +
      geom_sf(mapping = aes(fill = selected, color = selected)) +
      scale_fill_manual(values = c(NA, "orange")) +
      scale_color_manual(values = c("white", "orange")) +
      theme_map() +
      theme(legend.position = "none")
    
  })
  
  # filter data frame for specific county
  selected_county <- reactive({
    
    validate(
      need(input$county, "Please select a county.")
    )
    
    climate_risk |>
      filter(county == input$county)
    
  })
  
  # County risk scores
  output$county_risk <- renderText({
    
    # get selected county's overall risk score
    val <- selected_county() |>
      pull(national_risk_index_score_composite)
    
    # format using scales function
    label_number(accuracy = 1)(val)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
