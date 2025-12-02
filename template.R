library(shiny)
library(bslib)
library(gapminder)
library(tidyverse)
library(janitor)
library(stringr)
library(xml2)
library(DT)
library(readr)
library(plotly)
library(lubridate)
library(tidyr)

# =========================================================
# 1. Your data prep (EU unemployment)
# =========================================================
raw_df <- readr::read_csv("une.csv", show_col_types = FALSE) %>%
  clean_names()

pick_col <- function(nms, candidates) {
  for (c in candidates) if (c %in% nms) return(c)
  stop("Could not find: ", paste(candidates, collapse = ", "))
}

nms      <- names(raw_df)
col_time <- pick_col(nms, c("time_period", "time", "year"))
col_geo  <- pick_col(nms, c("geopolitical_entity_reporting", "geo", "country", "location", "geo_name"))
col_val  <- pick_col(nms, c("obs_value", "value", "values", "observation_value", "rate"))
col_unit <- if ("unit" %in% nms) "unit" else NA_character_

year_from <- function(x) as.integer(substr(as.character(x), 1, 4))

une <- raw_df %>%
  transmute(
    year    = year_from(.data[[col_time]]),
    country = as.character(.data[[col_geo]]),
    value   = suppressWarnings(as.numeric(.data[[col_val]])),
    unit0   = if (!is.na(col_unit)) str_to_lower(as.character(.data[[col_unit]])) else NA_character_
  ) %>%
  filter(!is.na(year), !is.na(country), !is.na(value), is.finite(value))

une <- une %>%
  mutate(
    value = case_when(
      !is.na(unit0) & str_detect(unit0, "per\\s*1000|‰") ~ value / 10,
      value > 100 ~ value / 10,
      TRUE ~ value
    )
  ) %>%
  filter(year >= 2013) %>%
  mutate(
    country_plot = case_when(
      country %in% c(
        "European Union - 27 countries (from 2020)",
        "EU27_2020", "EU27"
      ) ~ "EU27",
      TRUE ~ country
    ),
    value = case_when(
      unit0 == "pc_act"  ~ value,
      unit0 == "pc_pop"  ~ value * 2,
      unit0 == "ths_per" ~ value / 2000,
      TRUE ~ value
    ),
    unit0 = "Percentage of active population"
  )

countries_all <- sort(unique(une$country_plot))
year_min <- min(une$year, na.rm = TRUE)
year_max <- max(une$year, na.rm = TRUE)

# =========================================================
# 2. Cindy's data prep (education spending & tertiary)
# =========================================================
read_un_xml <- function(path) {
  xml <- read_xml(path)
  recs <- xml_find_all(xml, ".//record")
  
  data.frame(
    country = xml_text(xml_find_all(recs, "./field[@name='Reference Area']")),
    year    = as.integer(xml_text(xml_find_all(recs, "./field[@name='Time Period']"))),
    value   = as.numeric(xml_text(xml_find_all(recs, "./field[@name='Observation Value']"))),
    stringsAsFactors = FALSE
  ) |>
    filter(!is.na(year), !is.na(value))
}

edu_spending <- read_un_xml("UNdata_Export_20251102_112030274.xml")
tertiary_enroll <- read_un_xml("UNdata_Export_20251110_013432849.xml")

names(edu_spending)[3] <- "spend_pct_gdp"
names(tertiary_enroll)[3] <- "tertiary_enroll"

joined <- full_join(edu_spending, tertiary_enroll, by = c("country", "year"))

# =========================================================
# 3. Yijing globals (air quality)
# =========================================================
DATA_PATH <- "hourly_gas_2024.csv"
VALID_WHITELIST <- c(NA, "", "NULL")

trim_vec <- function(x, lo = 0.01, hi = 0.99, min_n = 10) {
  x <- as.numeric(x)
  if (sum(!is.na(x)) >= min_n) {
    ql <- quantile(x, lo, na.rm = TRUE)
    qh <- quantile(x, hi, na.rm = TRUE)
    x[x < ql | x > qh] <- NA
  }
  x
}

# =========================================================
# 4. UI: navbar with 6 panels
# =========================================================
ui <- page_navbar(
  title = "Group Shiny Application",
  

  # Panel 3 - YOUR PART: EU Unemployment
  nav_panel("EU Unemployment",
            sidebarLayout(
              sidebarPanel(
                h4("Filters"),
                uiOutput("country_ui"),
                uiOutput("year_ui"),
                radioButtons(
                  "view",
                  "Choose visualization:",
                  choices = c(
                    "Trends over time"        = "trend",
                    "Latest-year comparison"  = "latest"
                  ),
                  selected = "trend"
                )
              ),
              mainPanel(
                conditionalPanel(
                  "input.view == 'trend'",
                  h3("Unemployment Trends Over Time"),
                  plotOutput("trend_plot", height = "450px")
                ),
                conditionalPanel(
                  "input.view == 'latest'",
                  h3("Cross-Country Comparison (Latest Year)"),
                  plotOutput("latest_plot", height = "450px")
                )
              )
            )
  ),
  
  # Panel 4 - Cindy: Education Spending
  nav_panel("Education Spending (Cindy)",
            sidebarLayout(
              sidebarPanel(
                h4("Filters"),
                sliderInput("yr_range", "Years", min = 1970, max = 2020,
                            value = c(2000, 2015), sep = ""),
                checkboxInput("show_lm", "Add linear trend", TRUE),
                hr(),
                h4("Top Spenders"),
                numericInput("n_top", "Number of countries:", 
                             value = 15, min = 5, max = 50, step = 5),
                checkboxInput("label_bars", "Show values on bars", TRUE),
                hr(),
                h4("Country Trends"),
                selectizeInput("country", "Select Country", choices = NULL),
                checkboxInput("smooth_cty", "Add smooth curve", FALSE),
                hr(),
                downloadButton("dl_data", "Download data (CSV)")
              ),
              mainPanel(
                tabsetPanel(
                  tabPanel("Overview",    plotOutput("scatter_overview", height = 450)),
                  tabPanel("Top Spenders", plotOutput("bar_top", height = 480)),
                  tabPanel("Trends",
                           fluidRow(
                             column(6, plotOutput("line_spend",    height = 300)),
                             column(6, plotOutput("line_tertiary", height = 300))
                           )
                  ),
                  tabPanel("Raw Data", DTOutput("tbl"))
                )
              )
            )
  ),
  
  # Panel 5 - Tony: Climate Dashboard
  nav_panel("Climate Dashboard (Tony)",
            sidebarLayout(
              sidebarPanel(
                h4("Energy & Emissions"),
                uiOutput("t_year_selector"),
                checkboxInput("t_show_legend1", "Show Legend (plot 1)", TRUE),
                hr(),
                h4("Temperature & Extreme Weather"),
                uiOutput("t_country_selector"),
                checkboxInput("t_show_points2", "Show Data Points (plot 2)", TRUE),
                helpText("Up to 5 countries can be selected.")
              ),
              mainPanel(
                tabsetPanel(
                  tabPanel(
                    "Energy & Emissions",
                    plotlyOutput("t_plot1", height = 500),
                    helpText("Explore whether higher renewable energy use corresponds to lower CO₂ emissions.")
                  ),
                  tabPanel(
                    "Temperature & Extreme Weather",
                    plotlyOutput("t_plot2", height = 500),
                    helpText("Each line shows how temperature and extreme events co-move over time.")
                  )
                )
              )
            )
  ),
  
  # Panel 6 - Yijing: Air Quality Dashboard
  nav_panel("Air Quality (Yijing)",
            tabsetPanel(
              # Overview
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
              
              # Data & Controls
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
                        helpText("Density plot for the pollutant selected in \"Pollutant:\" above."),
                        checkboxInput("dist_split", "Show Weekday vs Weekend separately", FALSE),
                        plotOutput("plot_dist", height = 300),
                        br(),
                        helpText("Summary statistics for all pollutants in the cleaned dataset."),
                        DTOutput("tbl_dist_all")
                      )
                    )
                  )
                )
              ),
              
              # Weekday vs Weekend
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
                    DTOutput("tbl_ww")
                  )
                )
              )
            )
  )
)

# =========================================================
# 5. SERVER
# =========================================================
server <- function(input, output, session) {
  
  # ---- Panel 1: gapminder ----
  output$plot1 <- renderPlot({
    thePlot <- gapminder %>%
      filter(year > 1960) %>%
      group_by(continent, year) %>%
      summarise(meanLife = mean(lifeExp), .groups = "drop") %>%
      ggplot(aes(x = year, y = meanLife, group = continent, colour = continent)) +
      geom_line()
    
    if (input$linear1) {
      thePlot <- thePlot + geom_smooth(method = "lm")
    }
    thePlot
  })
  
  # ---- Panel 2: gapminder ----
  output$plot2 <- renderPlot({
    thePlot <- gapminder %>%
      filter(year > 1960) %>%
      group_by(continent, year) %>%
      summarise(meanLife = mean(lifeExp), .groups = "drop") %>%
      ggplot(aes(x = year, y = meanLife, group = continent, colour = continent)) +
      geom_line()
    
    if (input$linear2) {
      thePlot <- thePlot + geom_smooth(method = "lm")
    }
    thePlot
  })
  
  # ---- Panel 3: YOUR PART (EU Unemployment) ----
  
  # country selector
  output$country_ui <- renderUI({
    selectInput(
      "countries",
      "Select countries:",
      choices  = countries_all,
      selected = c("EU27", "Spain", "Greece", "Germany")[
        c("EU27", "Spain", "Greece", "Germany") %in% countries_all
      ],
      multiple = TRUE
    )
  })
  
  # year slider
  output$year_ui <- renderUI({
    sliderInput(
      "year_range",
      "Year range:",
      min   = year_min,
      max   = year_max,
      value = c(year_min, year_max),
      step  = 1,
      sep   = ""
    )
  })
  
  # filtered data
  une_filtered <- reactive({
    req(input$year_range)
    dat <- une %>%
      filter(
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
    if (!is.null(input$countries) && length(input$countries) > 0) {
      dat <- dat %>% filter(country_plot %in% input$countries)
    }
    dat
  })
  
  # trend plot
  output$trend_plot <- renderPlot({
    dat <- une_filtered() %>%
      group_by(country_plot, year) %>%
      summarise(rate = mean(value, na.rm = TRUE), .groups = "drop") %>%
      arrange(country_plot, year)
    
    req(nrow(dat) > 0)
    min_year <- min(dat$year, na.rm = TRUE)
    max_year <- max(dat$year, na.rm = TRUE)
    
    ggplot(dat, aes(x = year, y = rate)) +
      geom_line(linewidth = 1) +
      facet_wrap(~ country_plot, ncol = 3) +
      scale_x_continuous(
        breaks = seq(min_year, max_year, 1),
        labels = function(x) as.integer(x)
      ) +
      labs(
        x = "Year",
        y = "Unemployment rate (%)",
        title = "Unemployment rate over time",
        subtitle = "Percentage of active population"
      ) +
      theme_minimal(base_size = 13)
  })
  
  # latest-year comparison plot
  output$latest_plot <- renderPlot({
    dat <- une_filtered()
    req(nrow(dat) > 0)
    
    latest_year <- max(dat$year, na.rm = TRUE)
    
    dat2 <- dat %>%
      filter(year == latest_year) %>%
      group_by(country_plot) %>%
      summarise(rate = mean(value, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(rate)) %>%
      mutate(country_plot = factor(country_plot, levels = country_plot))
    
    ggplot(dat2, aes(x = country_plot, y = rate)) +
      geom_col(width = 0.7) +
      labs(
        x = "Country / Region",
        y = "Unemployment rate (%)",
        title = paste0("Unemployment rate by country (", latest_year, ")"),
        subtitle = "Percentage of active population"
      ) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # ---- Panel 4: Cindy – Education Spending ----
  
  observe({
    updateSelectizeInput(session, "country",
                         choices = sort(unique(joined$country)), server = TRUE)
  })
  
  data_filtered <- reactive({
    joined |>
      filter(year >= input$yr_range[1], year <= input$yr_range[2])
  })
  
  output$scatter_overview <- renderPlot({
    df <- data_filtered() |> filter(!is.na(spend_pct_gdp), !is.na(tertiary_enroll))
    p <- ggplot(df, aes(x = spend_pct_gdp, y = tertiary_enroll)) +
      geom_point(alpha = 0.7, color = "steelblue") +
      labs(
        x = "Education Spending (% of GDP)",
        y = "Tertiary Enrollment (%)",
        title = "Relationship Between Education Spending and Enrollment",
        caption = "Source: UNdata"
      ) +
      theme_minimal(base_size = 13)
    if (input$show_lm) p <- p + geom_smooth(method = "lm", se = TRUE, color = "darkred")
    p
  })
  
  output$bar_top <- renderPlot({
    df <- data_filtered() |>
      group_by(country) |>
      summarise(avg_spend = mean(spend_pct_gdp, na.rm = TRUE)) |>
      arrange(desc(avg_spend)) |>
      slice_head(n = input$n_top)
    p <- ggplot(df, aes(x = reorder(country, avg_spend), y = avg_spend)) +
      geom_col(fill = "gray40") +
      coord_flip() +
      labs(
        x = NULL, y = "% of GDP",
        title = "Top Countries by Education Spending"
      ) +
      theme_minimal(base_size = 13)
    if (input$label_bars)
      p <- p + geom_text(aes(label = round(avg_spend, 1)), hjust = -0.2)
    p
  })
  
  output$line_spend <- renderPlot({
    req(input$country)
    df <- edu_spending |> filter(country == input$country)
    p <- ggplot(df, aes(x = year, y = spend_pct_gdp)) +
      geom_line(color = "darkgreen") +
      labs(
        y = "Spending (% of GDP)", x = NULL,
        title = paste("Education Spending -", input$country)
      ) +
      theme_minimal(base_size = 13)
    if (input$smooth_cty) p <- p + geom_smooth(se = FALSE, color = "red")
    p
  })
  
  output$line_tertiary <- renderPlot({
    req(input$country)
    df <- tertiary_enroll |> filter(country == input$country)
    p <- ggplot(df, aes(x = year, y = tertiary_enroll)) +
      geom_line(color = "darkblue") +
      labs(
        y = "Tertiary Enrollment (%)", x = NULL,
        title = paste("Tertiary Enrollment -", input$country)
      ) +
      theme_minimal(base_size = 13)
    if (input$smooth_cty) p <- p + geom_smooth(se = FALSE, color = "red")
    p
  })
  
  output$tbl <- renderDT({
    data_filtered()
  }, options = list(pageLength = 10, lengthChange = FALSE), rownames = FALSE)
  
  output$dl_data <- downloadHandler(
    filename = function() {
      sprintf("education_dashboard_%s-%s.csv",
              input$yr_range[1], input$yr_range[2])
    },
    content = function(file) {
      write_csv(data_filtered(), file)
    }
  )
  
  # ---- Panel 5: Tony – Climate Dashboard ----
  
  t_data_all <- reactive({
    validate(need(file.exists("update_temperature.csv"),
                  "File 'update_temperature.csv' not found in the app folder."))
    read.csv("update_temperature.csv", stringsAsFactors = FALSE) %>%
      distinct(Year, Country, .keep_all = TRUE)
  })
  
  output$t_year_selector <- renderUI({
    df <- t_data_all()
    year_list <- sort(unique(df$Year))
    selectInput(
      "t_year",
      "Select Year:",
      choices = year_list,
      selected = max(year_list)
    )
  })
  
  output$t_plot1 <- renderPlotly({
    validate(
      need(input$t_year, "Please select a year.")
    )
    df <- t_data_all()
    year_sel <- input$t_year
    df_year <- df %>% filter(Year == year_sel)
    
    p <- ggplot(df_year, aes(
      x = Renewable_Energy_pct,
      y = CO2_Emissions_tons_per_capita,
      color = Country,
      text = paste0(
        "Country: ", Country, "<br>",
        "Renewables: ", round(Renewable_Energy_pct, 1), "%<br>",
        "CO₂ per capita: ", round(CO2_Emissions_tons_per_capita, 2), " t"
      )
    )) +
      geom_point(size = 3, alpha = 0.9) +
      labs(
        title = paste0("CO₂ Emissions vs Renewable Energy Usage (", year_sel, ")"),
        x = "Renewable Energy Share (%)",
        y = "CO₂ Emissions (tons per capita)",
        color = "Country"
      ) +
      scale_x_continuous(labels = function(z) paste0(z, "%")) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = if (isTRUE(input$t_show_legend1)) "bottom" else "none",
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8)
      ) +
      guides(color = guide_legend(nrow = 3, byrow = TRUE))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$t_country_selector <- renderUI({
    df <- t_data_all()
    selectizeInput(
      "t_countries", "Select up to 5 countries:",
      choices = sort(unique(df$Country)),
      selected = head(sort(unique(df$Country)), 5),
      multiple = TRUE,
      options = list(maxItems = 5)
    )
  })
  
  output$t_plot2 <- renderPlotly({
    df <- t_data_all()
    req(input$t_countries)
    
    df_sel <- df %>%
      filter(Country %in% input$t_countries) %>%
      arrange(Country, Year)
    
    p2 <- ggplot(df_sel, aes(
      x = Avg_Temperature_degC,
      y = Extreme_Weather_Events,
      color = Country,
      group = Country,
      text = paste0(
        "Country: ", Country, "<br>",
        "Year: ", Year, "<br>",
        "Avg Temp: ", round(Avg_Temperature_degC, 2), " °C<br>",
        "Extreme Events: ", Extreme_Weather_Events
      )
    )) +
      geom_line(size = 1) +
      { if (isTRUE(input$t_show_points2)) geom_point(size = 2, alpha = 0.85) } +
      labs(
        title = "Temperature vs Extreme Weather Events (Up to 5 Countries)",
        x = "Average Temperature (°C)",
        y = "Number of Extreme Weather Events"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "none"
      )
    
    fig <- ggplotly(p2, tooltip = "text")
    
    fig <- fig %>% layout(
      annotations = list(
        x = 0.98, y = 0.95,
        xref = "paper", yref = "paper",
        text = paste0(
          "Lines show changes over 2000–2024.<br>",
          "Each point = one year's avg temperature & extreme events."
        ),
        showarrow = FALSE,
        font = list(size = 12),
        align = "right",
        bgcolor = "white",
        bordercolor = "black",
        borderwidth = 1,
        opacity = 0.85
      )
    )
    
    fig
  })
  
  # ---- Panel 6: Yijing – Air Quality Dashboard ----
  
  raw <- reactive({
    validate(need(file.exists(DATA_PATH), paste("File not found:", DATA_PATH)))
    readr::read_csv(DATA_PATH, show_col_types = FALSE, progress = FALSE)
  })
  
  prepared <- reactive({
    raw() %>%
      mutate(
        DATE_TIME = gsub(" UTC$", "", DATE_TIME),
        dt = ymd_hms(DATE_TIME, quiet = TRUE),
        hour = hour(dt),
        wday_num = wday(dt, week_start = 1),
        weekend = if_else(wday_num >= 6, "Weekend", "Weekday")
      ) %>%
      filter(!is.na(dt)) %>%
      mutate(VALUE = as.numeric(VALUE))
  })
  
  observe({
    df <- prepared()
    params <- sort(unique(df$PARAMETER))
    if (length(params) > 0) {
      updateSelectInput(session, "param",
                        choices = params, selected = params[1])
      updateSelectInput(session, "param_ww",
                        choices = params, selected = params[1])
    }
  })
  
  cleaned <- reactive({
    prepared() %>%
      filter(VALUE_F %in% VALID_WHITELIST)
  })
  
  filtered <- reactive({
    req(input$param)
    df <- cleaned() %>% filter(PARAMETER == input$param)
    if (isTRUE(input$drop23)) {
      df <- df %>% filter(hour != 23)
    }
    df
  })
  
  filtered_ww <- reactive({
    req(input$param_ww)
    df <- cleaned() %>% filter(PARAMETER == input$param_ww)
    if (isTRUE(input$drop23)) {
      df <- df %>% filter(hour != 23)
    }
    df
  })
  
  output$txt_summary <- renderPrint({
    df0 <- prepared()
    df1 <- cleaned()
    cat("Rows total:", nrow(df0), "\n")
    cat("Rows after VALUE_F whitelist:", nrow(df1), "\n")
    cat("Distinct parameters:", paste(sort(unique(df0$PARAMETER)), collapse = ", "), "\n")
  })
  
  yearly_dist_all <- reactive({
    df <- cleaned()
    
    if (isTRUE(input$dist_split)) {
      df <- df %>% mutate(daygroup = weekend)   # Weekday / Weekend
    } else {
      df <- df %>% mutate(daygroup = "All Days")
    }
    
    df %>%
      group_by(PARAMETER, daygroup) %>%
      summarise(
        n      = sum(!is.na(VALUE)),
        mean   = mean(VALUE, na.rm = TRUE),
        sd     = sd(VALUE, na.rm = TRUE),
        median = median(VALUE, na.rm = TRUE),
        q25    = quantile(VALUE, 0.25, na.rm = TRUE),
        q75    = quantile(VALUE, 0.75, na.rm = TRUE),
        IQR    = q75 - q25,
        .groups = "drop"
      ) %>%
      arrange(PARAMETER, daygroup)
  })
  
  output$tbl_dist_all <- renderDT({
    yearly_dist_all()
  },
  options = list(
    pageLength = 10,
    lengthChange = FALSE
  ))
  
  output$plot_dist <- renderPlot({
    req(input$param)
    df <- cleaned() %>% filter(PARAMETER == input$param)
    if (nrow(df) == 0) return(NULL)
    
    base_plot <- ggplot(df, aes(x = VALUE)) +
      labs(
        title = paste("Distribution of", input$param, "throughout 2024"),
        x = paste(input$param, "(ppb)")
      ) +
      theme_minimal()
    
    if (isTRUE(input$dist_split)) {
      base_plot +
        geom_density(aes(fill = weekend), alpha = 0.4) +
        labs(fill = "Day Type")
    } else {
      base_plot +
        geom_density(alpha = 0.4)
    }
  })
  
  ww_hourly <- reactive({
    req(length(input$daytype) > 0)
    lo <- input$trim / 100
    hi <- 1 - lo
    
    filtered_ww() %>%
      filter(weekend %in% input$daytype) %>%
      group_by(weekend, hour) %>%
      mutate(VALUE_trim = trim_vec(VALUE, lo, hi, min_n = 10)) %>%
      summarise(
        y = if (input$stat == "Mean") mean(VALUE_trim, na.rm = TRUE)
        else median(VALUE_trim, na.rm = TRUE),
        n = sum(!is.na(VALUE_trim)),
        .groups = "drop"
      ) %>%
      complete(weekend, hour = 0:23)
  })
  
  output$plot_ww <- renderPlot({
    df <- ww_hourly()
    req(nrow(df) > 0)
    
    ylab_txt <- paste(
      ifelse(input$stat == "Mean", "Mean", "Median"),
      input$param_ww, "Concentration (ppb)"
    )
    
    ggplot(df, aes(hour, y, color = weekend)) +
      geom_line(linewidth = 1.2, na.rm = FALSE) +
      geom_point(size = 1.6, na.rm = TRUE) +
      scale_x_continuous(breaks = 0:23) +
      scale_y_continuous(limits = c(0, NA), name = ylab_txt) +
      scale_color_manual(values = c("Weekday" = "#1B9E77", "Weekend" = "#D95F02")) +
      labs(
        title = paste0(
          "How Do Hourly ", input$param_ww,
          " Concentrations Differ: Weekday vs Weekend (2024)?"
        ),
        x = "Hour of Day", color = ""
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")
  })
  
  output$tbl_ww <- renderDT({
    ww_hourly() %>% arrange(weekend, hour)
  },
  options = list(
    pageLength = 10,
    lengthChange = FALSE
  ))
}

shinyApp(ui = ui, server = server)





