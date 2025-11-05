# Seoul Bike Sharing App

## About

This is a Shiny app made for my ST 558 class project.\
It uses the Seoul Bike Sharing dataset to look at how weather and seasons affect bike rentals in Seoul.

The app lets you pick filters, see tables, and make plots about the data.

------------------------------------------------------------------------

## Data

**Data Source:** [Seoul Bike Sharing Demand - UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Seoul+Bike+Sharing+Demand)

The data has daily and hourly bike rental counts in Seoul.\
It also includes temperature, humidity, wind speed, rainfall, snowfall, and season information.

------------------------------------------------------------------------

## How to Use the App

-   Pick two **categorical variables** (like Season or Holiday).\
-   Pick two **numeric variables** (like Temperature or Humidity) and move the sliders.\
-   Click **Apply Filters** to update everything.\
-   The filtered data will show in all the tabs.

### Tabs

1.  **About Tab**\
    Tells what the app does and shows a picture.

2.  **Data Tab**\
    Shows the filtered data in a table.\
    You can also download it as a CSV file.

3.  **Explore Tab**

    -   **Tables:** Makes one-way and two-way tables.\
    -   **Summaries:** Shows averages, medians, and other summary numbers.\
    -   **Plots:** You can make bar, box, scatter, and line plots.

------------------------------------------------------------------------

## What I Used

-   `shiny`\
-   `dplyr`\
-   `tidyr`\
-   `ggplot2`\
-   `DT`

The app uses reactive filters so the data only changes when you click **Apply Filters**.\
It should work fine when uploaded to [shinyapps.io](https://www.shinyapps.io/).

------------------------------------------------------------------------

## Files

-   `app7.R` – main app file\
-   `SeoulBikeData.csv` – data used in the app\
-   `www/app-cover.jpg` – image for About tab\
-   `README.md` – this file\
-   `exploration.qmd` – file with static plots and summaries

------------------------------------------------------------------------

## Made By

**Saurabh Gupta**\
ST 558 — NC State University
