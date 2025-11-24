library(shiny)
library(DT)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(readr)

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

server <- function(input, output, session) {
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
  ))}
