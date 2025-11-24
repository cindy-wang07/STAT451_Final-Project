library(shiny)
library(xml2)
library(dplyr)
library(ggplot2)
library(readr)

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

edu_spending <- read_un_xml("/Users/macbook/Desktop/Stat 451/UNdata_Export_20251102_112030274.xml")
tertiary_enroll <- read_un_xml("/Users/macbook/Desktop/Stat 451/UNdata_Export_20251110_013432849.xml")

names(edu_spending)[3] <- "spend_pct_gdp"
names(tertiary_enroll)[3] <- "tertiary_enroll"

joined <- full_join(edu_spending, tertiary_enroll, by = c("country", "year"))

server <- function(input, output, session) {
  observe({
    updateSelectizeInput(session, "country",
                         choices = sort(unique(joined$country)), server = TRUE)
  })
  
  data_filtered <- reactive({
    joined |> filter(year >= input$yr_range[1], year <= input$yr_range[2])
  })
  
  output$scatter_overview <- renderPlot({
    df <- data_filtered() |> filter(!is.na(spend_pct_gdp), !is.na(tertiary_enroll))
    p <- ggplot(df, aes(x = spend_pct_gdp, y = tertiary_enroll)) +
      geom_point(alpha = 0.7, color = "steelblue") +
      labs(x = "Education Spending (% of GDP)",
           y = "Tertiary Enrollment (%)",
           title = "Relationship Between Education Spending and Enrollment",
           caption = "Source: UNdata") +
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
      labs(x = NULL, y = "% of GDP",
           title = "Top Countries by Education Spending") +
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
      labs(y = "Spending (% of GDP)", x = NULL, title = paste("Education Spending -", input$country)) +
      theme_minimal(base_size = 13)
    if (input$smooth_cty) p <- p + geom_smooth(se = FALSE, color = "red")
    p
  })
  
  output$line_tertiary <- renderPlot({
    req(input$country)
    df <- tertiary_enroll |> filter(country == input$country)
    p <- ggplot(df, aes(x = year, y = tertiary_enroll)) +
      geom_line(color = "darkblue") +
      labs(y = "Tertiary Enrollment (%)", x = NULL, title = paste("Tertiary Enrollment -", input$country)) +
      theme_minimal(base_size = 13)
    if (input$smooth_cty) p <- p + geom_smooth(se = FALSE, color = "red")
    p
  })

  output$tbl <- renderDT({
    data_filtered()
  }, options = list(pageLength = 10), rownames = FALSE)
  
  output$dl_data <- downloadHandler(
    filename = function() {
      sprintf("education_dashboard_%s-%s.csv",
              input$yr_range[1], input$yr_range[2])
    },
    content = function(file) {
      write_csv(data_filtered(), file)
    }
  )
}
