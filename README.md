# STAT451_Final-Project/Yijing

# Air Quality Dashboard  
*A Shiny Web Application for Analyzing Hourly Pollutant Data (2024)*

## 📌 Overview

This Shiny application explores how **hourly air pollutant concentrations** change between **weekdays and weekends** across the year 2024.

The app:
- Classifies each timestamp as **Weekday** or **Weekend**
- Cleans the dataset using the official **VALUE_F** quality flag
- Removes extreme outliers with **percentile trimming**
- Allows users to select pollutants interactively
- Compares hourly pollutant patterns between weekday and weekend
- Displays full-year distributions and summary statistics

---

## 📂 Dataset

**File:** `hourly_gas_2024.csv`  
Place this file in the same folder as `ui.R` and `server.R`.

### Required Columns
| Column | Description |
|--------|-------------|
| `DATE_TIME` | Timestamp in UTC |
| `PARAMETER` | Pollutant type (e.g., CO, NO, NH3) |
| `VALUE` | Pollutant concentration |
| `VALUE_F` | Data quality flag |

---

## 🚀 Runnin
