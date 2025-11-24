# STAT451_Final-Project

This Shiny module visualizes unemployment trends across European countries from 2013–2023 using data downloaded from Eurostat. The app allows users to select countries, adjust the year range, and switch between a multi-panel trend view and a latest-year comparison bar chart. All data are automatically cleaned, standardized, and harmonized (e.g., unit conversions and EU27 name normalization). To run the app, download the unemployment dataset from Eurostat at https://ec.europa.eu/eurostat/api/discoveries/unemployment-rate
, save it as une.csv, and place it in the same folder as ui.R and server.R. Running shiny::runApp() will launch the interactive dashboard, which forms the unemployment-analysis tab of a four-member group project.
