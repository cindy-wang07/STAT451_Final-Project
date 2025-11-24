# STAT451_Final-Project
# Education Spending & Outcomes Dashboard

## Overview
This Shiny dashboard looks at how government spending on education relates to tertiary (college-level) enrollment across countries. The goal is to help users explore whether higher public investment in education leads to higher participation in higher education.

## Research Question
**Do countries that spend a larger share of GDP on education have higher tertiary enrollment rates?**

## Repository Contents
.
├── ui.R
├── server.R
├── data/
│ ├── UNdata_Export_20251102_112030274.xml # Education spending (% of GDP)
│ └── UNdata_Export_20251110_013432849.xml # Tertiary enrollment ratio (%)
└── README.md


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

