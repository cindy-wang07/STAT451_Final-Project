library(shiny)
library(tidyverse)
library(janitor)
library(stringr)

df <- readr::read_csv("une.csv", show_col_types = FALSE) %>%
  clean_names()

pick_col <- function(nms, candidates) {
  for (c in candidates) if (c %in% nms) return(c)
  NA_character_
}

nms      <- names(df)
col_time <- pick_col(nms, c("time_period","time","year"))
col_geo  <- pick_col(nms, c("geopolitical_entity_reporting","geo","country","location","geo_name"))
col_val  <- pick_col(nms, c("obs_value","value","values","observation_value","rate"))
col_unit <- pick_col(nms, c("unit","unit_of_measure","measure"))

stopifnot(!is.na(col_time), !is.na(col_geo), !is.na(col_val))

year_from <- function(x) as.integer(substr(as.character(x), 1, 4))

une <- df %>%
  transmute(
    year    = year_from(.data[[col_time]]),
    country = as.character(.data[[col_geo]]),
    value   = suppressWarnings(as.numeric(.data[[col_val]])),
    unit0   = if (!is.na(col_unit))
                str_to_lower(as.character(.data[[col_unit]]))
              else NA_character_
  ) %>%
  filter(!is.na(year), !is.na(country), !is.na(value), is.finite(value))

une <- une %>%
  mutate(value = case_when(
    !is.na(unit0) & str_detect(unit0, "per\\s*1000|‰") ~ value / 10,
    value > 100 ~ value / 10,   # fallback guard
    TRUE ~ value
  ))

une <- une %>% filter(year >= 2013, year <= 2023)

une <- une %>%
  mutate(country_plot = case_when(
    country %in% c("European Union - 27 countries (from 2020)",
                   "EU27_2020", "EU27") ~ "EU27",
    TRUE ~ country
  ))

une <- une %>%
  mutate(
    value = case_when(
      unit0 == "pc_act"  ~ value,          # already correct
      unit0 == "pc_pop"  ~ value * 2,      # approximate rescale
      unit0 == "ths_per" ~ value / 2000,   # thousands of persons → %
      TRUE ~ value
    ),
    unit0 = "Percentage of active population"
  )

countries_all <- sort(unique(une$country_plot))
year_min <- min(une$year)
year_max <- max(une$year)

shinyServer(function(input, output, session) {

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

  une_filtered <- reactive({
    req(input$year_range)
    une %>%
      filter(
        year >= input$year_range[1],
        year <= input$year_range[2],
        if (!is.null(input$countries)) country_plot %in% input$countries else TRUE
      )
  })

  output$trend_plot <- renderPlot({
    data_trend <- une_filtered() %>%
      group_by(country_plot, year) %>%
      summarise(rate = mean(value, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(rate))

    req(nrow(data_trend) > 0)

    ggplot(data_trend, aes(x = year, y = rate, group = country_plot)) +
      geom_line(linewidth = 1) +
      facet_wrap(~ country_plot, ncol = 3) +
      labs(
        title = "Unemployment rate over time",
        subtitle = "Percentage of active population",
        x = "Year",
        y = "Unemployment rate (%)"
      ) +
      theme_minimal(base_size = 13)
  })

  output$latest_plot <- renderPlot({
    dat <- une_filtered()
    req(nrow(dat) > 0)

    latest_year <- max(dat$year, na.rm = TRUE)

    figB_df <- dat %>%
      filter(year == latest_year) %>%
      group_by(country_plot) %>%
      summarise(rate = mean(value, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(rate)) %>%
      mutate(country_plot = factor(country_plot, levels = country_plot))

    ggplot(figB_df, aes(x = country_plot, y = rate)) +
      geom_col(width = 0.7) +
      labs(
        title    = paste0("Unemployment rate by country (", latest_year, ")"),
        subtitle = "Percentage of active population",
        x = "Country/Region",
        y = "Unemployment rate (%)"
      ) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
})