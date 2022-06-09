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



# Map

left <- floor(min(datafest$lon))
right <- ceiling(max(datafest$lon))
bottom <- floor(min(datafest$lat))
top <- ceiling(max(datafest$lat))

href_color <- "#9966CC"
marker_color <- "darkseagreen"
part_color <- "#CC9966"

states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
countries <- geojsonio::geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", what = "sp")
country <- countries[countries$name %in% datafest$country,]

country$density = NA
states <- rbind(states,country)
states$num_par=0
for (i in 1:nrow(states)) {
  for (j in 1:nrow(participants)) {
    if (states$name[i] == participants$name[j]) {
      if (!is.na(participants$num_part[j])) {
        states$num_par[i] = states$num_par[i] + participants$num_part[j]
      }
    }
  }
}

bins <- c(0, 10, 20, 40, 100, 150, 300, 400, 600, 1000, max(states$num_par))
pal <- colorBin("Blues", domain = states$num_par, bins = bins)

labels <- sprintf(
  "<strong>%s</strong>",
  states$name
) %>% lapply(htmltools::HTML)

host_text <- paste0(
  "<b><a href='", datafest$url, "' style='color:", href_color, "'>", datafest$host, "</a></b>"
)

other_inst_text <- paste0(
  ifelse(is.na(datafest$other_inst),
         "",
         paste0("<br>", "with participation from ", datafest$other_inst))
)

part_text <- paste0(
  "<font color=", part_color,">", datafest$num_part, " participants</font>"
)

popups <- paste0(
  host_text, other_inst_text, "<br>" , part_text
)



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
