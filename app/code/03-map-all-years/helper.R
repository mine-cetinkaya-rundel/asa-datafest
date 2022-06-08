# load packages -----------------------------------------------------
library(tidyverse)
library(leaflet)
library(shiny)
library(shinythemes)
library(here)
library(praise)
library(usethis)
library(wordcloud)
library(shinyWidgets)
library(shinydashboard)


# load data ---------------------------------------------------------

datafest <- read_csv(here::here("app/data/datafest.csv"))
datafest <- datafest %>%
  mutate(Awards = "", Title = "", Team = "", Presentation = "")
major_df <- read_csv(here::here("app/data/majors.csv"))

# get data for universities page
universities_df <- datafest %>%
  select(host, year, num_part)

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

#make main and max according to min and max of inputted college, change year scale?
#min_tot_part <- min(part_count$tot_part)
#max_tot_part <- max(part_count$tot_part)


# calculate total countries participating for each year ------------------------
country_count <- datafest %>%
  group_by(year) %>%
  summarise(tot_country = length(unique(datafest$country)))

# calculate total hosts participating for each year ------------------------
host_count <- datafest %>%
  group_by(year) %>%
  summarise(tot_host = length(unique(datafest$host)))

# calculate DataSource list for each year ----------------------
# year <- c("2011","2012","2013","2014","2015","2016","2017")
# source_data <- c("LAPD","Kiva.com","eHarmony","GridPoint","Edmunds.com","Ticketmaster", "Expedia")
# datasource <- data.frame(year, source_data)

# Tiles
# creating tiles for dashboard -------------------------
year_today <- as.double(c(format(Sys.Date(), "%Y")))
year_tile <- (year_today - min(datafest$year))+1

participants_tile <- sum(datafest$num_part,na.rm = TRUE)

unique_host <- unique(datafest$host)
hosts_tile <- length(unique_host)

unique_country <- unique(datafest$country)
country_tile <- length(unique_country)

# Tiles
library(htmltools)
library(TileMaker)

b1 <- solo_box(value = year_tile, txt = "Years", size = "lg", color = "info")
b2 <- solo_box(value = country_tile, txt = "Countries", size = "lg", color = "danger" )
b3 <- solo_box(value = hosts_tile, txt = "Institutions", size = "lg", color = "warning")
b4 <- solo_box(value = participants_tile, txt = "Participants", size = "lg", color = "success")

d1 <- div_maker(subtitle = "DataFest by the Numbers",
                textModifier = "h4", b1, b2, b3, b4)