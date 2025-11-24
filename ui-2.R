library(shiny)

shinyUI(
  fluidPage(
    titlePanel("European Unemployment, 2013–2023"),

    sidebarLayout(
      sidebarPanel(
        # dynamic because choices depend on the data
        uiOutput("country_ui"),

        sliderInput(
          "year_range",
          "Year range:",
          min = 2013, max = 2023,
          value = c(2013, 2023),
          step = 1, sep = ""
        ),

        radioButtons(
          "view",
          "Choose visualization:",
          choices = c(
            "Trends over time"          = "trend",
            "Latest-year comparison"    = "latest"
          ),
          selected = "trend"
        )
      ),

      mainPanel(
        conditionalPanel(
          "input.view == 'trend'",
          h3("Unemployment trends over time"),
          plotOutput("trend_plot")
        ),
        conditionalPanel(
          "input.view == 'latest'",
          h3("Cross-country comparison (latest year)"),
          plotOutput("latest_plot")
        )
      )
    )
  )
)