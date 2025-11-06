# app.R — Main startup file for Seoul Bike App
options(stringsAsFactors = FALSE)

# ---- Load libraries ----
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)

# ---- Read and preprocess dataset ----
file_path <- "/Users/saurabhgupta/projects/github/ST558_Project2_ShinyApp/SeoulBikeData.csv"
text_lines <- readLines(file_path, encoding = "UTF-8", warn = FALSE)
text_lines <- iconv(text_lines, from = "", to = "UTF-8", sub = "")
bikeData <- read.csv(textConnection(text_lines), check.names = FALSE)
closeAllConnections()

# ---- Data formatting ----
bikeData$Date <- as.Date(bikeData$Date, "%d/%m/%Y")
bikeData$Hour <- as.integer(bikeData$Hour)
bikeData$Holiday <- as.factor(bikeData$Holiday)
bikeData$`Functioning Day` <- as.factor(bikeData$`Functioning Day`)
bikeData$Seasons <- factor(bikeData$Seasons, levels = c("Winter", "Spring", "Summer", "Autumn"))
bikeData$Month <- format(bikeData$Date, "%b")

# ---- Variable lists ----
cat_vars_all <- c("Seasons", "Holiday", "Functioning Day")
num_vars_all <- c("Rented Bike Count","Temperature(°C)","Temperature(C)","Humidity(%)",
                  "Wind speed (m/s)","Visibility (10m)","Dew point temperature(C)",
                  "Solar Radiation (MJ/m2)","Rainfall(mm)","Snowfall (cm)","Hour")

cat_vars <- intersect(cat_vars_all, names(bikeData))
num_vars <- intersect(num_vars_all, names(bikeData))

# ---- Source UI and Server AFTER defining variables ----
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

# ---- Run the app ----
shinyApp(ui = ui, server = server)
