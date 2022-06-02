# load packages -----------------------------------------------------
library(tidyverse)
library(leaflet)
library(shiny)
library(shinythemes)
# load data ---------------------------------------------------------
datafest <- read_csv("data/datafest.csv")

# set colors --------------------------------------------------------
href_color <- "#9966CC"
marker_color <- "darkseagreen"
part_color <- "#CC9966"
bins <- c(0, 10, 20, 40, 50, 100, 200, 300, 400, Inf)
pal <- colorBin("Blues", domain = states$num_par, bins = bins)
# set map bounds ----------------------------------------------------
left <- floor(min(datafest$lon))
right <- ceiling(max(datafest$lon))
bottom <- floor(min(datafest$lat))
top <- ceiling(max(datafest$lat))

# calculate total participants for each year ------------------------
part_count <- datafest %>%
  group_by(year) %>%
  summarise(tot_part = sum(num_part, na.rm = TRUE))

min_tot_part <- min(part_count$tot_part)
max_tot_part <- max(part_count$tot_part)