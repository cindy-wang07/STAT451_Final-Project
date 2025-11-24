# STAT451_Final-Project

This is the final project for Chenyi Wang, Yijing Chen, Tony Wan and Yao Yao's STAT 451 class in the Fall 2025 semester.
Each team member developed own **Shiny dashboard**, exploring unique data questions using statistical graphics and real-world datasets.  
The five folders in this repository correspond to the five project branches, each with its own code, data files, and README.
The **main** folder contains shared documentation and serves as the landing page for the project, while each of the other folders holds a complete Shiny app built by an individual group member.

---
Repository Structure

```bash
STAT 451 Final Project/
├── main
├── Yao-Yao
├── Tony-Wan
├── yijing
└── Chenyi-Wang
```

## Dashboard Modules

### **1. Chenyi-Wang — Education Spending & Outcomes Dashboard**
**Research Question:**  
Do countries that spend a larger share of their GDP on education achieve higher participation in higher education?

**Summary:**  
This dashboard uses UNdata XML datasets to visualize global education spending and higher education participation.  
Users can explore the spending–enrollment relationship, view top spenders, and analyze country-level trends.

➡️ See full README in `Chenyi-Wang/`

---

### **2. Tony-Wan — Climate Dashboard**
**Research Questions:**  
- Does renewable energy usage reduce CO₂ emissions?  
- Do higher temperatures correspond to more extreme weather events?

**Summary:**  
This dashboard visualizes climate, energy, and emissions trends using real-world temperature and renewable energy datasets.

➡️ See full README in `Tony-Wan/`

---

### **3.  Yao-Yao — European Unemployment Dashboard**
**Research Question:**  
How have unemployment rates changed across European countries from 2013–2023?

**Summary:**  
This dashboard uses Eurostat unemployment data, performs automatic data cleaning, and provides country-specific and regional visualizations.

➡️ See full README in `Yao-Yao/`

---

### **4. Yijing Chen — Air Quality Dashboard**
**Research Question:**  
How do pollutant concentrations (CO, NO, NH₃...) differ between weekdays and weekends during 2024?

**Summary:**  
This dashboard analyzes hourly U.S. EPA CASTNET air quality data to compare weekday vs weekend patterns.

➡️ See full README in `yijing/`

---

## How to Run Any Dashboard

To run any module:

1. Enter the corresponding folder (e.g., `Chenyi-Wang/`).
2. Make sure the data files (CSV/XML) are placed in the same directory as `ui.R` and `server.R`.
3. In RStudio or R console, run:

```r
shiny::runApp(".")
```
