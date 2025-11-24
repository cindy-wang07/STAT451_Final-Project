library(shiny)

shinyUI(
  navbarPage(
    "Climate Dashboard",
    

    tabPanel(
      "Energy & Emissions",
      
      plotly::plotlyOutput("plot1", height = 500),
      
      fluidRow(
        column(
          width = 12,
          div(style = "margin-top: 20px;",
              uiOutput("year_selector"),
              checkboxInput("show_legend1", "Show Legend", TRUE),
              helpText("Explore whether higher renewable energy use corresponds to lower CO₂ emissions.")
          )
        )
      )
    ),
    

    tabPanel(
      "Temperature & Extreme Weather",
      
      plotly::plotlyOutput("plot2", height = 500),

      fluidRow(
        column(
          width = 12,
          div(style = "margin-top: 20px;",
              uiOutput("country_selector"),
              checkboxInput("show_points2", "Show Data Points", TRUE),
              checkboxInput("show_legend2", "Show Legend", TRUE),
              helpText("Up to 5 countries can be selected.")
          )
        )
      )
    )
  )
)


