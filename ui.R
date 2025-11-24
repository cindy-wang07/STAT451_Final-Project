library(shiny)
library(DT)

ui <- navbarPage(
  title = "Education Spending & Outcomes Dashboard",
  
  tabPanel("Overview",
           sidebarLayout(
             sidebarPanel(
               sliderInput("yr_range", "Years", min = 1970, max = 2020,
                           value = c(2000, 2015), sep = ""),
               checkboxInput("show_lm", "Add linear trend", TRUE)),
             mainPanel(plotOutput("scatter_overview", height = 500)))),

  tabPanel("Top Spenders",
           sidebarLayout(
             sidebarPanel(
               numericInput("n_top", "Number of countries:", value = 15, min = 5, max = 50, step = 5),
               checkboxInput("label_bars", "Show values on bars", TRUE)),
             mainPanel(plotOutput("bar_top", height = 520)))),
  
  tabPanel("Country Trends",
           sidebarLayout(
             sidebarPanel(
               selectizeInput("country", "Select Country", choices = NULL),
               checkboxInput("smooth_cty", "Add smooth curve", FALSE)),
             mainPanel(
               fluidRow(
                 column(6, plotOutput("line_spend", height = 320)),
                 column(6, plotOutput("line_tertiary", height = 320)))))),
  
  tabPanel("Data",
           sidebarLayout(
             sidebarPanel(downloadButton("dl_data", "Download data (CSV)")),
             mainPanel(DTOutput("tbl")))))
