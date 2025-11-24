# STAT451_Final-Project

European Unemployment Dashboard (2013–2023)

This Shiny application provides an interactive visualization of unemployment trends across European countries between 2013 and 2023.
It is built as one tab of a four-member group project, with each member contributing a separate dataset and visualization module.

Features
-Interactive filtering

Select multiple countries

Adjust year range using a dynamic slider

Switch between:

Trend View: unemployment rate over time (facet plot)

Latest-Year View: cross-country comparison for the most recent available year

-Automatic data cleaning and harmonization

This app:

Standardizes multiple unit types (e.g., per 1000 people → % active population)

Normalizes inconsistent EU region names (e.g., “EU27_2020” → “EU27”)

Filters dataset to 2013–2023

Handles missing or inconsistent values

-Dynamic UI

All controls (countries, year slider) are created based on the dataset, so the interface adapts automatically.

Repository Structure
/my-unemployment-app/
   ├── ui.R
   ├── server.R
   ├── une.csv
   └── README.md  (this file)


ui.R — controls the layout and input/output structure

server.R — performs data cleaning, filtering, and visualization

une.csv — dataset containing unemployment data for the EU

README.md — instructions for running and editing the module





This application uses the European Unemployment Dataset from Eurostat.

Dataset source:
Eurostat – Unemployment by sex, age and region (monthly & annual)
(Official EU labour market statistics)

Download link:
https://ec.europa.eu/eurostat/api/discoveries/unemployment-rate

How to download

Open the link above in a browser.

Select Download → CSV.

Save the file as:

une.csv


Place une.csv in the same folder as ui.R and server.R.



Running the Application

In RStudio:

shiny::runApp()
