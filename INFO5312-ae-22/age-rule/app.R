library(shiny)
library(dplyr)
library(ggplot2)
library(bslib)
library(markdown)

# function to determine minimum date-able age
age_min <- function(you) {
  age <- (you / 2) + 7
  age[age > you] <- you[age > you]
  return(age)
}

# function to determine maximum date-able age
age_max <- function(you) {
  age <- 2 * (you - 7)
  age[age < you] <- you[age < you]
  return(age)
}

# function to check if permissible (returns boolean)
is_permissible <- function(you, partner) {
  return(age_min(you) <= partner && age_max(you) >= partner)
}

# function to check if you-partner ages are permissible
age_check <- function(you, partner) {
  if (is_permissible(you, partner)) {
    "Yes, this is permissible"
  } else if (age_min(you) > partner) {
    "No, you are too old to date this person"
  } else if (age_max(you) < partner) {
    "No, you are too young to date this person"
  }
}

# generate example points
age_min_data <- 14
age_max_data <- 123

# generate grid of age combinations
ages <- tibble(
  you = age_min_data:age_max_data,
  age_min = age_min(you),
  age_max = age_max(you)
)

# age plot
cushion <- 15 # amount of extra axis space to draw when rendering plot

# define base plot
age_plot <- ggplot(data = ages) +
  geom_line(mapping = aes(x = you, y = age_min), linetype = 2) +
  geom_line(mapping = aes(x = you, y = age_max), linetype = 2) +
  geom_ribbon(mapping = aes(x = you, ymin = age_min, ymax = age_max, fill = "range"), alpha = .25) +
  scale_fill_manual(name = NULL, values = c("pink"), labels = c("Acceptable")) +
  labs(
    x = "Your age",
    y = "Partner's age"
  ) +
  theme_bw(base_size = 18) +
  theme(legend.position = "none")

# Define UI with bslib components
ui <- page_sidebar(
  # dashboard title
  title = "Can You Date This Person? The Half Plus Seven Rule",
  # use the flatly Bootswatch theme
  theme = bs_theme(bootswatch = "flatly"),
  
  # define sidebar content
  sidebar = sidebar(
    width = 350,
    # include support for math equations
    withMathJax(),
    # include Markdown formatted text
    includeMarkdown("describe.md"),
    # age input for user's age
    numericInput(
      inputId = "age_you",
      label = "Your age:",
      min = 14,
      max = 123,
      value = 25
    ),
    # age input for partner's age
    numericInput(
      inputId = "age_partner",
      label = "Partner's age:",
      min = 14,
      max = 123,
      value = 25
    ),
    # result box if relationship is permissible
    uiOutput("result_box")
  ),
  
  # Main panel with the plot in a card
  card(
    full_screen = TRUE,
    card_header("Zone of permissibility"),
    card_body(
      # plot output goes here
      plotOutput("age_plot", height = "700px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # generate age plot based on user input
  output$age_plot <- renderPlot({
    age_plot +
      geom_point(
        # draw point on plot based on input ages
        data = tibble(
          you = input$age_you,
          partner = input$age_partner
        ),
        mapping = aes(you, partner), size = 4, color = "blue"
      ) +
      # adjust the range of the plot to focus on the specific age combinations
      coord_cartesian(
        xlim = c(
          max(age_min_data, input$age_you - cushion),
          min(age_max_data, input$age_you + cushion)
        ),
        ylim = c(
          max(age_min_data, input$age_partner - cushion),
          min(age_max_data, input$age_partner + cushion)
        )
      )
  })
  
  # Generate dynamic value box based on permissibility
  output$result_box <- renderUI({
    # check if relationship is permissible
    permissible <- is_permissible(input$age_you, input$age_partner)
    
    # if so, display a success message
    # otherwise, display a failure message
    if (permissible) {
      value_box(
        title = "Result",
        value = "Yes, this is permissible",
        showcase = bsicons::bs_icon("check-circle-fill"),
        theme = "success",
        full_width = TRUE
      )
    } else {
      value_box(
        title = "Result",
        value = age_check(input$age_you, input$age_partner),
        showcase = bsicons::bs_icon("x-circle-fill"),
        theme = "danger",
        full_width = TRUE
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
