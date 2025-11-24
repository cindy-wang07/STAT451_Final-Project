# STAT451_Final-Project
# Education Spending & Outcomes Dashboard

## Overview
This Shiny dashboard explores how government spending on education relates to tertiary enrollment rates across countries.
It uses official data from the United Nations (UNdata) via the UNESCO Institute for Statistics (UIS).
Users can interactively compare spending patterns, visualize top-spending countries, and track long-term trends for each nation.

## Research Question
**Do countries that spend a larger share of their GDP on education achieve higher participation in higher education?**
This project aims to show whether financial investment in education helps increase access to learning and reduce inequality between nations.

## Repository Contents

```bash
├── ui.R                  
├── server.R          
├── data/
│   ├── UNdata_Export_20251102_112030274.xml  
│   └── UNdata_Export_20251110_013432849.xml   
└── README.md
```

## Datasets and Download Links
This app uses two datasets from **UNdata (UNESCO Institute for Statistics)**:

1. **Government Expenditure on Education as % of GDP**  
   https://data.un.org/Data.aspx?q=Government+Expenditure+on+Education&d=UNESCO&f=series%3aXGDP_FSGOV  

2. **Gross Enrollment Ratio, Tertiary (%)**  
   https://data.un.org/Data.aspx?q=Gross+Enrollment+Ratio+Tertiary&d=UNESCO&f=series%3aGER_56  

### How to Download
1. Open each link above.  
2. Click **“Download data”** on the right side of the page.  
3. Choose **XML format**.  
4. Save the files and keep the names as:
   - `UNdata_Export_20251102_112030274.xml`
   - `UNdata_Export_20251110_013432849.xml`
5. Put both XML files into a folder named **data/** in the same directory as `ui.R` and `server.R`.  
The app reads XML directly, so no CSV conversion is needed.

## How to Run the App
1. Install required packages:
```r
install.packages(c("shiny", "xml2", "dplyr", "ggplot2", "readr", "DT"))
```

2. Run the Shiny app in RStudio:
```r
shiny::runApp(".")
```
