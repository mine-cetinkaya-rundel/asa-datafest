# load helpers ------------------------------------------------------
source("/Users/Aarushi/Duke/MIDS/DataFest/asa-datafest/app/code/03-map-all-years/helper.R", local = TRUE)
# -------------------------------------------------------------------

# create a table with just winning titles ---------------------------

datafest_titles <- datafest %>%
  select(Awards, host, year, Title, Team, Presentation)

#--------------------------------------------------------------------
library(shiny)
library(shinydashboard)

header <- dashboardHeader(title = "ASA DataFest over the years", titleWidth = "350px")

sidebar <- dashboardSidebar(width = 140,collapsed = FALSE, 
                            sidebarMenuOutput("home"),
                            sidebarMenuOutput("host"),
                            sidebarMenuOutput("winner"))

body <- dashboardBody(
  h4("This app is designed to demonstrate the growth and spread of",
    tags$a(href = "http://www.amstat.org/education/datafest/", "ASA DataFest"),
    "over the years. Click on the points to find out more about each event.",
    "If your institution does not appear on the list, email",
    tags$a(href = "mailto:mine@stat.duke.edu", "mine@stat.duke.edu.")),
  br(),
  
  fluidPage(
      title = NULL, width = 12,
      id = "tabset1",height = "250px",
      tabItems(
        tabItem(tabName = "home",
                fluidRow(box(sliderInput("year", "Year",value = 2011,
                   min = 2011, max = 2017, step = 1,
                   width = "200%",
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
                ),
        
        tabItem(tabName = "host",
                fluidRow(
                  box(width = 4,
                      selectInput("college", "College", choices = unique(pull(datafest, "host"))),
                      sliderInput("uni_year", "Year", value = 2017,
                                  min = 2011, max = 2017, step = 1,
                                  animate = animationOptions(interval = 1500),
                                  sep = "")
                      ),
                  plotOutput("line", height = "200px"),
                  #p("histogram for majors"),
                  plotOutput("major_histogram")
                  )
                ),
        
        tabItem(tabName = "winner",
                fluidRow(
                  box(width = 4,
                  pickerInput("year_choice",
                              "Year",
                              choices = unique(pull(datafest, "year")),
                              selected = c(datafest$year),
                              options = list(`actions-box` = TRUE),
                              multiple = TRUE),
                  
                  pickerInput("host_choice",
                              "Host University",
                              choices = unique(pull(datafest, "host")),
                              selected = c(datafest$host),
                              options = list(`actions-box` = TRUE),
                              multiple = TRUE),
                  
                  pickerInput("award_choice",
                              "Award",
                              choices = c("Best Insight", "Best Visualization", "Best Use of External Data"),
                              selected = c("Best Insight", "Best Visualization", "Best Use of External Data"),
                              options = list(`actions-box` = TRUE),
                              multiple = TRUE)),
                  tableOutput("titles"))
                )
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
  
  # Create tab items in sidebar
  output$home <- renderMenu({
    sidebarMenu(
      menuItem("Homepage", tabName = "home",icon = icon("home")))})
  
  output$host <- renderMenu({
    sidebarMenu(
      menuItem("Host University", tabName = "host",icon = icon("university")))})
  
  output$winner <- renderMenu({
    sidebarMenu(
      menuItem("Past Winners", tabName = "winner",icon = icon("award")))})
  
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
  
  ## Add Tile graphics
  output$ParticipantsTile <- renderInfoBox({
    participants <- part_count[part_count$year == input$year, ]$tot_part
    infoBox(
    "Participants", paste0(participants), icon = icon("users"),
    color = "red", fill = TRUE, width = 2.5
    )
    })
  
  output$HostsTile <- renderInfoBox({
    hosts <- host_count[host_count$year == input$year, ]$tot_host
    infoBox(
      "Instituitions", paste0(hosts), icon = icon("university"),
      color = "yellow", fill = TRUE, width = 2.5
    )
  })
  
  output$CountryTile <- renderInfoBox({
    countries <- country_count[host_count$year == input$year, ]$tot_country
    infoBox(
      "Countries", paste0(countries), icon = icon("globe"),
      color = "blue", fill = TRUE, width = 2.5
    )
  })
  
  output$DataTile <- renderInfoBox({
    company <-datasource[datasource$year == input$year, ]$source_data
    infoBox(
      "Source Data", paste0(company), icon = icon("file-upload", library = "font-awesome"),
      color = "green", fill = TRUE, width = 2.5
    )
  })
  
  #use df of individual university
  output$line <- renderPlot({
    
    sel_part_count <- filter(part_count, year <= input$uni_year)
    
    ggplot(sel_part_count, aes(x = year, y = tot_part)) +
      geom_line(color = "blue") +
      geom_point(size = 3) +
      scale_x_continuous("Year",
                         limits = c(2011, 2017),
                         breaks = c(2011:2017)) +
      scale_y_continuous("",
                         limits = c(0, max_tot_part)) +
      labs(title = "DataFest participants over time",
           subtitle = "Total number of participants for each year")
  })
  
  titles_subset <- reactive({
    req(input$year_choice)
    req(input$host_choice)
    req(input$award_choice)
    filter(
      datafest_titles,
      Awards %in% input$award_choice,
      year %in% input$year_choice,
      host %in% input$host_choice)
  })
  
  output$titles <- renderTable(
    titles_subset(),
    hover = TRUE,
    striped = TRUE,
    digits = 0
  )
  
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



