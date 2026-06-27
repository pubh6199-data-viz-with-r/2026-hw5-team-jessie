[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/jEmP5upM)
# Final Project: Final Project: Plastic Chemical Facilities and Maternal Health Explorer

Authors: Salem Yohannes and Megan Liu  
Course: PUBH 6199 – Visualizing Data with R  
Date: 2026-06-25

## 🔍 Project Overview

This project explores the relationship between plastic production chemical facilities and maternal health outcomes across the United States through three interactive visualizations: a line graph comparing state trends with national averages, a scatterplot examining the relationship between chemical facility counts and maternal health outcomes, and a bivariate choropleth map highlighting geographic patterns across states. The dashboard combines maternal health data from the CDC with plastics production chemical facility data from the EPA to help users identify trends and geographic patterns that may inform future public health research and policy.

## 📊 Final Write-up

The final write-up, including code and interpretation of the visualizations, is available here:

👉 [**View the write-up website**](https://pubh6199-data-viz-with-r.github.io/hw5-YOUR-TEAM-NAME/)
(https://pubh6199-data-viz-with-r.github.io/2026-hw5-team-jessie/)

## 📂 Repository Structure

```plaintext
.
├── _quarto.yml          # Quarto website configuration
├── .gitignore           # Git ignore file
├── data/                # Cleaned maternal health and chemical facility datasets
├── docs/                # Rendered Quarto website
├── images/              # Figures used in the write-up (if applicable)
├── index.qmd            # Main Quarto write-up
├── references.bib       # Bibliography
├── shiny-app/
│   ├── app.R            # Interactive Shiny dashboard
│   ├── app-data/        # Data used by the dashboard
│   └── www/             # Static assets (images, CSS, etc.)
├── scratch/             # Exploratory analyses and development files
└── README.md            # Project overview and instructions

```

## 🛠 How to Run the Code

### To render the write-up:

1. Open the `.Rproj` file in RStudio.
2. Open `index.qmd`.
3. Click **Render**. The updated html will be saved in the `docs/` folder.

### To run the Shiny app (if applicable):

```r
shiny::runApp("shiny-app")
```

> ⚠️ Make sure any necessary data files are in `shiny-app/app-data/`.

## 🔗 Shiny App Link

If your project includes a Shiny app, you can access it here:

👉 [https://yourusername.shinyapps.io/your-app-name](https://n4s5zp-megan-liu.shinyapps.io/shiny-app/)

## 📦 Packages Used

- `tidyverse`
- `dplyr`
- `ggplot2`
- `shiny`
- `plotly`
- `bslib`
- `leaflet`
- `sf`
- `tigris`
- `tmap`
- `tidycensus`
- `classInt`
- `ggrepel`
- `readxl`
- `writexl`
- `zipcodeR`

## ✅ To-Do or Known Issues

[Optional section for you to note improvements or bugs.]
