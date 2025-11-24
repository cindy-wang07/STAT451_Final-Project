# Air Quality Dashboard  
*A Shiny Application for Analyzing Weekday vs Weekend Differences in Air Pollutant Levels (2024)*

## Research Question

**How do hourly concentrations of key atmospheric pollutants (CO, NO, NH₃) differ between weekdays and weekends during 2024?**

This application allows users to interactively explore these patterns, compare hourly profiles, and examine full-year distributions for each pollutant.

## Data Source

This project uses publicly available air-quality data from the **U.S. EPA CASTNET (Clean Air Status and Trends Network)**.

### **Hourly Gas Data (2024)**  
- **Download link:**  
  https://gaftp.epa.gov/castnet/CASTNET_Outgoing/data/hourly_gas_2024.zip  
- After downloading the ZIP file, extract `hourly_gas_2024.csv` （place it **in the same directory** as your `ui.R` and `server.R`）.  

This dataset includes hourly pollutant measurements across multiple U.S. monitoring stations.

## Methodology Summary

The app performs the following processing steps:

### **1. Classification**
- Each timestamp is converted into date-time format  
- Day of week computed using `lubridate`  
- Days are tagged as **Weekday** (Mon–Fri) or **Weekend** (Sat–Sun)

### **2. Cleaning**
- Keep only valid measurements based on **VALUE_F whitelist**  
- Drop rows flagged as “calibration”, “power failure”, “below RL”, or missing  
- Convert all pollutant values to numeric  
- Optionally drop **hour 23** (often calibration hour)

### **3. Outlier Removal**
- Use **percentile trimming** (default: top 1% and bottom 1%)  
- Trimming occurs **within each hour of day** and **within each pollutant**

### **4. Aggregation**
- Summaries computed using **Mean** or **Median**  
- Hourly patterns generated separately for Weekday and Weekend  
- Full-year distributions plotted for each pollutant  
- Users may optionally compare Weekday vs Weekend distributions

---

## Running the App
Place the following files together:

ui.R
server.R
hourly_gas_2024.csv


Run:

```r
library(shiny)
runApp()

