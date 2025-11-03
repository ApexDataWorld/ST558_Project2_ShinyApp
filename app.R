

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---- Load CSV exactly  ----
df <- read.csv("seoul_bike_sharing.csv", check.names = FALSE)


if ("Date" %in% names(df)) df$Date <- suppressWarnings(as.Date(df$Date, "%d/%m/%Y"))
if ("Hour" %in% names(df)) df$Hour <- suppressWarnings(as.integer(df$Hour))
for (nm in c("Holiday","Functioning Day","Seasons")) if (nm %in% names(df)) df[[nm]] <- as.factor(df[[nm]])
if ("Seasons" %in% names(df)) {
  lev <- c("Winter","Spring","Summer","Autumn")
  df$Seasons <- factor(df$Seasons, levels = intersect(lev, levels(df$Seasons)))
}

# ---- Choices used by UI  ----
preferred_cats <- c("Seasons","Holiday","Functioning Day")
is_cat <- vapply(df, function(x) is.factor(x) || is.character(x), logical(1))
cats_all <- names(df)[is_cat]
cat_choices <- intersect(preferred_cats, names(df))
if (length(cat_choices) == 0) cat_choices <- cats_all

num_choices <- names(df)[vapply(df, is.numeric, logical(1))]
num_choices <- unique(c("Rented Bike Count", setdiff(num_choices, "Rented Bike Count")))

# ---- Source UI and Server after choices made ----
source("ui.R")
source("server.R")

shinyApp(ui, server)
