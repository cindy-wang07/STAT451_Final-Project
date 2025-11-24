library(shiny)
library(DT)

DATA_PATH <- "hourly_gas_2024.csv"

ui <- navbarPage(
  title = "Air Quality Dashboard",
  id = "nav",

  tabPanel(
    "Overview",
    fluidRow(
      column(
        6,
        h3("Project Question"),
        p("How do hourly concentrations of pollutants differ between weekdays and weekends?")
      ),
      column(
        6,
        h3("What this project shows"),
        tags$ul(
          tags$li("Classify each data point as a weekday or weekend based on its date."),
          tags$li("Keep only valid measurements using the VALUE_F quality flag."),
          tags$li("Remove extreme outliers by trimming a small percentage of very high and very low values."),
          tags$li("Let users choose any pollutant in the dataset to explore."),
          tags$li("Summarize all pollutants for the full year (mean, median, standard deviation, quartiles, etc.)."),
          tags$li("Compare weekday and weekend hourly patterns across the day."),
          tags$li("Plot the full-year distribution of the selected pollutant, optionally split by weekday/weekend.")
        )
      )
    ),
    hr(),
    h4("Cleaning Notes"),
    tags$ul(
      tags$li("Keep records where VALUE_F is blank or NULL. Records flagged as calibration, power failure, missing data, etc. are removed."),
      tags$li("Per-hour trimming: remove values outside the chosen percentile band (default 1%–99%)."),
      tags$li("Hourly summaries can use either the mean or the median.")
    )
  ),
  
  tabPanel(
    "Data & Controls",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("param", "Pollutant:", choices = NULL),
        checkboxInput("drop23", "Exclude hour 23 (often calibration hour)", FALSE)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Cleaning Summary",
            verbatimTextOutput("txt_summary")
          ),
          tabPanel(
            "Yearly Distribution",
            # 先放 density 图
            helpText("Density plot for the pollutant selected in \"Pollutant:\" above."),
            checkboxInput("dist_split", "Show Weekday vs Weekend separately", FALSE),
            plotOutput("plot_dist", height = 300),
            br(),
            # 再放 summary 表
            helpText("Summary statistics for all pollutants in the cleaned dataset."),
            DTOutput("tbl_dist_all")
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Weekday vs Weekend",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("param_ww", "Pollutant:", choices = NULL),
        selectInput("stat", "Statistic:", c("Mean", "Median"), selected = "Mean"),
        sliderInput(
          "trim", "Percentile trimming (% on each tail):",
          min = 0, max = 10, value = 1, step = 0.5
        ),
        checkboxGroupInput(
          "daytype", "Day Type to show:",
          choices = c("Weekday", "Weekend"),
          selected = c("Weekday", "Weekend")
        )
      ),
      mainPanel(
        plotOutput("plot_ww", height = 420),
        br(),
        DTOutput("tbl_ww")))))
