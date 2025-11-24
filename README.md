# Climate Dashboard — STAT 424 Final Project

This Shiny dashboard investigates two major climate-related questions across multiple countries:

1. **Does higher renewable energy usage correspond to lower CO₂ emissions per capita?**  
2. **Do higher average temperatures correlate with more frequent extreme weather events?**

The dashboard contains two interactive panels that allow users to explore these questions using real-world climate and energy data.

---

## Repository Structure

```bash
STAT 424 Final Project Tony Wan/
├── ui.R
├── server.R
└── update_temperature.csv
```

---

## Running the Application

1. Clone this repository:

```bash
git clone https://github.com/<your-team>/<your-repo>.git
```

Open the project folder in RStudio.

Run the Shiny application:

```bash
shiny::runApp(".")
```

---

Panel 1 — Energy & Emissions

This panel visualizes the relationship between renewable energy usage and CO₂ emissions.

Users can:

Select a specific year (2000, 2005, 2010, 2015, 2020, 2024)

Explore each country’s CO₂ emissions per capita

See how CO₂ levels vary with renewable energy share

Hover on points to reveal country-level details

Show or hide the legend

Main question addressed:
Do countries that rely more on renewable energy tend to emit less CO₂ per capita?


Panel 2 — Temperature & Extreme Weather

This panel examines how temperature levels correspond to extreme weather events over time.

Users can:

Select up to 5 countries

Compare temperature and extreme weather trends

Observe year-to-year changes

Hover on points and lines to view detailed information

Toggle visibility of points and legend

Main question addressed:
Do higher temperatures lead to more extreme weather events?

---

Dataset Description

update_temperature.csv includes the following variables:

Country

Year

Avg_Temperature_degC — Average annual temperature (°C)

CO2_Emissions_tons_per_capita — CO₂ emissions per capita

Renewable_Energy_pct — Share of renewable energy (%)

Extreme_Weather_Events — Number of extreme weather events

---

Dataset retrieved from Kaggle:
https://www.kaggle.com/datasets/adilshamim8/temperature
