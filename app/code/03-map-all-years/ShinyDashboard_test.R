# load helpers ------------------------------------------------------
source("/Users/Aarushi/Duke/MIDS/DataFest/asa-datafest/app/code/03-map-all-years/helper.R", local = TRUE)
# -------------------------------------------------------------------
library(shiny)
library(shinydashboard)

header <- dashboardHeader(title = "ASA DataFest over the years", titleWidth = "350px")

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(
  p("This app is designed to demonstrate the growth and spread of",
    tags$a(href = "http://www.amstat.org/education/datafest/", "ASA DataFest"),
    "over the years. Click on the points to find out more about each event.",
    "If your institution does not appear on the list, email",
    tags$a(href = "mailto:mine@stat.duke.edu", "mine@stat.duke.edu.")),
  fluidPage(
    tabBox(
      title = NULL, width = 12,
      id = "tabset1",height = "250px",
      tabPanel("Geographically", "GeoTab"),
      tabPanel("Chronologically", "ChronTab"),
      fluidRow(box(sliderInput("year", "Year",value = 2011,
                   min = 2011, max = 2017, step = 1,
                   width = "100%",
                   animate = animationOptions(interval = 1500),
                   sep = ""))),
    fluidRow(
    infoBoxOutput("ParticipantsTile", width = 3),
    infoBoxOutput("HostsTile", width = 3),
    infoBoxOutput("CountryTile", width = 3),
    infoBoxOutput("DataTile", width = 3)
    ),
    br(),
    fluidRow(
      leafletOutput("map",width = "870px", height = "696px"), align="center"),
    br(),
    fluidRow(
      wordcloud2Output("wordcloud", width = "80%", height = "200px"),align ="center")
    )
  )
)

#dashboardPage(header, sidebar, body)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

# Preview the UI in the console
server <- function(input, output, session) {
  
  d <- reactive({
    filter(datafest, year == input$year & df == "Yes")
  })
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(left, bottom, right, top)
  })
  
  observeEvent(d(), {
    
    mapProxy <- leafletProxy("map", session)
    
    # clear previous controls and markers each time input$year changes
    clearControls(mapProxy)
    clearMarkers(mapProxy)
    
    # define popups
    host_text <- paste0(
      "<b><a href='", d()$url, "' style='color:", href_color, "'>", d()$host, "</a></b>"
    )
    
    other_inst_text <- paste0(
      ifelse(is.na(d()$other_inst),
             "",
             paste0("<br>", "with participation from ", d()$other_inst))
    )
    
    part_text <- paste0(
      "<font color=", part_color,">", d()$num_part, " participants</font>"
    )
    
    popups <- paste0(
      host_text, other_inst_text, "<br>" , part_text
    )
    
    mapProxy %>%
      addControl(h1(input$year), position = "topright") %>%
      addCircleMarkers(lng = d()$lon, lat = d()$lat,
                       radius = log(d()$num_part),
                       fillColor = marker_color,
                       color = marker_color,
                       weight = 1,
                       fillOpacity = 0.5,
                       popup = popups)
  })
  
  ## Add Tile graphics (to be changed to become dynamic)
  output$ParticipantsTile <- renderInfoBox({
    infoBox(
    "Participants", paste0("4956"), icon = icon("group"),
    color = "purple", fill = TRUE, width = 3
    )
    })
  output$HostsTile <- renderInfoBox({
    infoBox(
      "Instituitions", paste0("24"), icon = icon("building"),
      color = "yellow", fill = TRUE, width = 3
    )
  })
  output$CountryTile <- renderInfoBox({
    infoBox(
      "Countries", paste0("3"), icon = icon("globe"),
      color = "blue", fill = TRUE, width = 3
    )
  })
  output$DataTile <- renderInfoBox({
    infoBox(
      "Source Data", paste0("Expedia"), icon = icon("table"),
      color = "green", fill = TRUE, width = 3
    )
  })
  
  #adding wordcloud
  output$wordcloud <- renderWordcloud2({
    Major <- c("Stats", "Computer Science", "Pure Math", "Applied Math","A","B","C")
    Freq <- c(23, 41, 32, 58,3,2,1)
    
    df <- data.frame(Major,Freq)
    wordcloud2(data=df)
  })
}

############
shinyApp(ui = ui, server = server)



