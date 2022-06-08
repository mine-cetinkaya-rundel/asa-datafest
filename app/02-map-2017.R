# load packages -----------------------------------------------------
library(tidyverse)
library(leaflet)

# load data ---------------------------------------------------------
datafest <- read_csv("app/data/datafest.csv")
participants <- datafest %>%
  mutate(state = ifelse(country == "Germany", "Germany", state)) %>%
  mutate(state = ifelse(country == "Canada", "Canada", state)) %>%
  select(state, num_part) %>%
  rename(name = state)

# filter for 2017 events --------------------------------------------
datafest_2017 <- filter(datafest, year == 2017 & df == "Yes")


# set map bounds ----------------------------------------------------
left <- floor(min(datafest$lon))
right <- ceiling(max(datafest$lon))
bottom <- floor(min(datafest$lat))
top <- ceiling(max(datafest$lat))


# set colors --------------------------------------------------------
href_color <- "#9966CC"
marker_color <- "darkseagreen"
part_color <- "#CC9966"


# define popups -----------------------------------------------------
host_text <- paste0(
  "<b><a href='", datafest_2017$url, "' style='color:", href_color, "'>", datafest_2017$host, "</a></b>"
)

other_inst_text <- paste0(
  ifelse(is.na(datafest_2017$other_inst),
         "",
         paste0("<br>", "with participation from ", datafest_2017$other_inst))
)

part_text <- paste0(
  "<font color=", part_color,">", datafest_2017$num_part, " participants</font>"
)

popups <- paste0(
  host_text, other_inst_text, "<br>" , part_text
)

# plot map ----------------------------------------------------------
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




map <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

map %>% addPolygons()

bins <- c(0, 10, 20, 40, 50, 100, 200, 300, 400, Inf)

pal <- colorBin("Blues", domain = states$num_par, bins = bins)




library(RColorBrewer)
display.brewer.all()

map1 <- map %>% addPolygons(
  fillColor = ~pal(num_par),
  weight = 1,
  opacity = 1,
  color = "#969696",
  dashArray = "1",
  fillOpacity = 0.7)
unique(datafest$country)



labels <- sprintf(
  "<strong>%s</strong>",
  states$name
) %>% lapply(htmltools::HTML)


leaflet() %>%
  # addPolygons(
  #   fillColor = "yellow",
  #   weight = 2,
  #   opacity = 1,
  #   color = "white",
  #   dashArray = "",
  #   fillOpacity = 0.7
  # ) %>%
  addPolygons(
    data = states,
    fillColor = ~pal(num_par),
    weight = 3,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 1,
      color = "blanchedalmond",
      dashArray = "",
      fillOpacity = 0.5,
      bringToFront = FALSE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal",
                   padding = "3px 8px",
                   "color" = "#999999"),
      textsize = "10px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = states$num_par, opacity = 0.5, title = NULL,
            position = "bottomright") %>%
  addTiles() %>%
  fitBounds(lng1 = left, lat1 = bottom, lng2 = right, lat2 = top) %>%
  addCircleMarkers(
    lng = datafest_2017$lon, lat = datafest_2017$lat,
    radius = log(datafest_2017$num_part) * 1.2,
    fillColor = marker_color,
    color = marker_color,
    weight = 1,
    fillOpacity = 0.5,
    popup = popups)

