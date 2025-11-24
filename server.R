library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

shinyServer(function(input, output, session) {

  data_all <- reactive({
    validate(need(file.exists("update_temperature.csv"),
                  "File 'update_temperature.csv' not found in the app folder."))
    read.csv("update_temperature.csv", stringsAsFactors = FALSE) %>%
      distinct(Year, Country, .keep_all = TRUE)
  })
  
  output$year_selector <- renderUI({
    df <- data_all()
    year_list <- sort(unique(df$Year))   # automatically: 2000, 2005, 2010, 2015, 2020, 2024
    selectInput(
      "year",
      "Select Year:",
      choices = year_list,
      selected = max(year_list)
    )
  })
  
  output$plot1 <- renderPlotly({
    validate(
      need(input$year, "Please select a year.")
    )
    df <- data_all()
    year_sel <- input$year
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
        legend.position = if (isTRUE(input$show_legend1)) "bottom" else "none",
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8)
      ) +
      guides(color = guide_legend(nrow = 3, byrow = TRUE))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$country_selector <- renderUI({
    df <- data_all()
    selectizeInput(
      "countries", "Select up to 5 countries:",
      choices = sort(unique(df$Country)),
      selected = head(sort(unique(df$Country)), 5),
      multiple = TRUE,
      options = list(maxItems = 5)
    )
  })
  

  output$plot2 <- renderPlotly({
    df <- data_all()
    req(input$countries)
    
    df_sel <- df %>%
      filter(Country %in% input$countries) %>%
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
      { if (isTRUE(input$show_points2)) geom_point(size = 2, alpha = 0.85) } +
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
        text = "Lines show changes over 2000–2024.<br>"
        %>% paste0("Each point = 1 year’s avg temperature & extreme events."),
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
  
    
})
