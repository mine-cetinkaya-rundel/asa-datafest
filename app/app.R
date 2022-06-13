# load helpers ------------------------------------------------------
source("helper.R", local = TRUE)
# -------------------------------------------------------------------

# create a table with just winning titles ---------------------------

datafest_titles <- datafest %>%
  select(Awards, host, year, Title, Team, Presentation)

datafest_titles[nrow(datafest_titles)+1,] = list("Best Insight", "Duke University", 2022, "Reordering minigames with personalized Recommendation System", '"Chill Chill"', "https://www2.stat.duke.edu/datafest/winning-projects/team-chili-chill-presentation.pdf")

datafest_titles <- datafest_titles %>%
  mutate(
    Presentation = paste0("<a href='", Presentation, "'>", Presentation, "</a>"
    )
  )

names(datafest_titles) <- tools::toTitleCase(names(datafest_titles))
major_only <- major_df$Major_Breakdown


#--------------------------------------------------------------------
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
                fluidRow(
                  infoBoxOutput("ParticipantsTile", width = 3),
                  infoBoxOutput("HostsTile", width = 3),
                  infoBoxOutput("CountryTile", width = 3),
                  infoBoxOutput("DataTile", width = 3)
                  ),
                fluidRow(box(width = 12,
                           sliderInput("year",
                                       "Year",value = 2011,
                                       min = 2011, max = 2017, step = 1,
                                       width = "100%",
                                       animate = animationOptions(interval = 1500),
                                       sep = ""))),
                br(),
                fluidRow(leafletOutput("map")),
                br(),
                fluidRow(plotOutput("wordcloud", width = "100%", height = "400px"))
                ),

        tabItem(tabName = "host",
                fluidRow(
                  box(width = 3,
                      selectInput("college", "College", choices = unique(pull(datafest, "host")))),
                  box(width = 9,
                      sliderInput("uni_year", "Year", value = 2017,
                                  min = 2011, max = 2017, step = 1,
                                  animate = animationOptions(interval = 1500),
                                  sep = "")
                      )
                  ),
                fluidRow(
                  plotOutput("line", height = "200px"),
                  p("major distribution"),
                  #textOutput("major_distribution")
                  )
                ),

        tabItem(tabName = "winner",
                fluidRow(
                  box(
                  pickerInput("year_choice",
                              "Year",
                              choices = c(unique(pull(datafest, "year")), "2022"),
                              options = list(`actions-box` = TRUE),
                              multiple = TRUE),

                  pickerInput("host_choice",
                              "Host University",
                              "Host University",
                              choices = unique(pull(datafest, "host")),
                              options = list(`actions-box` = TRUE),
                              multiple = TRUE),

                  pickerInput("award_choice",
                              "Award",
                              choices = c("Best Insight", "Best Visualization", "Best Use of External Data"),
                              options = list(`actions-box` = TRUE),
                              multiple = TRUE),
                  actionButton(inputId = "search", label = "Search"),
                  width = 3
                  ),
                  tableOutput("titles"), width = 9)
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
    leaflet()
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

    participants <- d() %>%
      mutate(state = case_when(country == "Germany"~ "Germany",
                               country == "Canada"~ "Canada",
                               state == "Minnessota"~ "Minnesota",
                               TRUE ~ state)) %>%
      select(state, num_part) %>%
      rename(name = state)

    # calculate total participants in each state
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

    mapProxy %>%
      addControl(h1(input$year), position = "topright") %>%
      addTiles() %>%
      fitBounds(lng1 = left, lat1 = bottom, lng2 = right, lat2 = top) %>%
      addPolygons(
        data = states,
        fillColor = ~pal(num_par),
        weight = 1,
        opacity = 1,
        color = "lightgray",
        dashArray = "",
        fillOpacity = 1,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "white",
          dashArray = "2",
          fillOpacity = 0.7,
          bringToFront = FALSE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal",
                       padding = "3px 8px",
                       "color" = "#999999"),
          textsize = "10px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = states$num_par, opacity = 0.7, title = "Number of Participants",
                position = "bottomright") %>%
      addCircleMarkers(lng = d()$lon, lat = d()$lat,
                       radius = log(d()$num_part),
                       fillColor = marker_color,
                       color = marker_color,
                       weight = ,
                       fillOpacity = 0.5,
                       popup = popups)



  })

  ## Add Tile graphics
  output$ParticipantsTile <- renderInfoBox({
    participant_tile <- part_count[part_count$year == input$year, ]$tot_part
    infoBox(
    "Participants", paste0(participant_tile), icon = icon("users"),
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
    sel_part_count <- filter(universities_df, year <= input$uni_year, host == input$college)
    min_tot_part <- min(sel_part_count$num_part)
    uni_max <- filter(universities_df, host == input$college)$num_part
    max_tot_part <- max(uni_max)

    ggplot(sel_part_count, aes(x = year, y = num_part)) +
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

  titles_subset <- eventReactive(input$search, {
    if(is.null(input$year_choice)&is.null(input$host_choice)&is.null(input$award_choice))
    {return(datafest_titles)}

    else{

    ifelse(is.null(input$year_choice), {year_choice <- c(unique(pull(datafest, "year")))},{year_choice <- input$year_choice})
    ifelse(is.null(input$host_choice), {host_choice <- c(unique(pull(datafest, "host")))},{host_choice <- input$host_choice})
    ifelse(is.null(input$award_choice), {award_choice <- c(unique(pull(datafest, "Awards")))},{award_choice <- input$award_choice})
    print(year_choice)
      filter(
        datafest_titles,
        Awards %in% award_choice,
        year %in% year_choice,
        host %in% host_choice)
    }})


  output$titles <- renderTable(
    {titles_subset()}, sanitize.text.function = function(x) x,
    hover = TRUE,
    striped = TRUE,
    digits = 0
  )
  ## Adding Word Cloud

  output$wordcloud <- renderPlot({
    Major <- c("Stats", "Computer Science", "Pure Math", "Applied Math","Business")
    Freq <- c(23, 41, 32, 58,10,3,2)

    #dev.new(width = 10000, height = 10000, unit = "px")
    #DF <- as.data.frame(YourList)
    wordcloud(words = major_only, freq = Freq, rot.per=0, fixed.asp = FALSE, colors=brewer.pal(8, "Spectral"),scale = c(6,0.5))
  })


  output $major_distribution <- renderText({
    distr <- filter(major_df, Institution == input$college)
    distr
  })

}

############
shinyApp(ui = ui, server = server)



