# load packages
library(shinydashboard)
library(dashboardthemes)
library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)
library(leaflet)
library(sf)
library(tidycensus)
library(rnaturalearth)
library(geojsonio)
library(conflicted)

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflict_prefer("box", "shinydashboard")

# setwd("~/Desktop/Final")
tl <- read_excel("data/TL2019.xlsx")

# convert data from wide to long format at state level, i.e. combine drop & pick
tl_state_all <- tl %>%
  select(`Pick State`, `Ship Month`, `Shipment Count`) %>%
  rename(State = `Pick State`) %>%
  rbind(
    tl %>%
      select(`Drop State`, `Ship Month`, `Shipment Count`) %>%
      rename(State = `Drop State`)
  )

# convert data from wide to long format at city level, i.e. combine drop & pick
tl_city_all <- tl %>%
  select(`PickCity`, `Ship Month`, `Shipment Count`) %>%
  rename(City = `PickCity`) %>%
  rbind(tl %>%
          select(`Drop City`, `Ship Month`, `Shipment Count`) %>%
          rename(City = `Drop City`))

# generate states level data for each plot
# 1
states1 <-
  geojsonio::geojson_read("data/us-states.geojson", what = "sp")
states1$state_abb <- state.abb[match(states1@data$name, state.name)]
states1@data <- tl_state_all %>%
  group_by(State) %>%
  summarize(shipment = sum(`Shipment Count`)) %>%
  right_join(states1@data,
             by = c("State" = "state_abb"),
             multiple = "all")

# 2
states2 <-
  geojsonio::geojson_read("data/us-states.geojson", what = "sp")
states2$state_abb <- state.abb[match(states2@data$name, state.name)]
states2@data <- tl_state_all %>%
  group_by(State, `Ship Month`) %>%
  summarize(shipment = sum(`Shipment Count`)) %>%
  right_join(states2@data,
             by = c("State" = "state_abb"),
             multiple = "all") %>%
  distinct()


# generate Chicago data
states3 <-
  geojsonio::geojson_read("data/us-states.geojson", what = "sp")
states3$state_abb <- state.abb[match(states3@data$name, state.name)]
states3@data <- tl %>%
  filter(`Pick State` == "IL") %>%
  group_by(`Drop State`) %>%
  summarize(shipment = sum(`Shipment Count`)) %>%
  right_join(states3@data,
             by = c("Drop State" = "state_abb"),
             multiple = "all")


# generate city level data for each plot
cities <-
  read_sf("data/ne_10m_populated_places/ne_10m_populated_places.shp",
          layer = "ne_10m_populated_places") %>%
  filter(SOV0NAME == "United States", ADM0_A3 == "USA") %>%
  mutate(CITY = toupper(NAME)) %>%
  select(CITY, LONGITUDE, LATITUDE, POP_MAX)

# cities data, months combined
cities1 <- cities %>%
  inner_join(tl_city_all %>%
               group_by(City) %>%
               summarize(shipment = sum(`Shipment Count`)),
             by = c("CITY" = "City"))

# cities data, months separated
cities2 <- cities %>%
  inner_join(
    tl_city_all %>%
      group_by(City, `Ship Month`) %>%
      summarize(shipment = sum(`Shipment Count`)),
    by = c("CITY" = "City"),
    multiple = "all"
  )


#### #### YEAR 2020 ########
ibtl <- read_excel("data/IB TL Data.xlsx")
ibtl_2020 <- ibtl %>% filter(`Ship Year` == 2020)

# select pairs that have data each month
ibtl_2020 %>%
  group_by(`Pick State`, `Drop State`) %>%
  summarize(dist_month = n_distinct(`Ship Month`)) %>%
  filter(dist_month == 12) -> df_dist_month

# select top 5 origins
ibtl_2020 %>%
  filter(
    `Pick State` %in% df_dist_month$`Pick State`,
    `Drop State` %in% df_dist_month$`Drop State`
  ) %>%
  group_by(`Pick State`) %>%
  summarize(total_n = sum(`Shipment Count`)) %>%
  arrange(desc(total_n)) %>%
  head(10) -> top5_origin
top5_origin # So the top five states are IA, IL, WI, TN, GA

# look for the top five destinations for each of the five origins
ibtl_2020 %>%
  filter(`Pick State` %in% top5_origin$`Pick State`) %>%
  group_by(`Pick State`, `Drop State`) %>%
  summarize(total_n = sum(`Shipment Count`)) %>%
  top_n(10, total_n) %>%
  arrange(desc(total_n), .by_group = TRUE) -> final_comb

# get the final dataset that has origin, destination, monthly shipment
ibtl_2020 %>%
  filter(`Pick State` %in% final_comb$`Pick State`,
         `Drop State` %in% final_comb$`Drop State`) %>%
  group_by(`Pick State`, `Drop State`, `Ship Month`) %>%
  summarize(total_n = sum(`Shipment Count`)) %>%
  mutate(`Ship Month` = as.factor(`Ship Month`)) -> monthly_count


########  YEAR 2021 ########
ibtl_2021 <- ibtl %>% filter(`Ship Year` == 2021)

# select pairs that have data each month
ibtl_2021 %>%
  group_by(`Pick State`, `Drop State`) %>%
  summarize(dist_month = n_distinct(`Ship Month`)) %>%
  filter(dist_month == 12) -> df21_dist_month

# select top 5 origins
ibtl_2021 %>%
  filter(
    `Pick State` %in% df_dist_month$`Pick State`,
    `Drop State` %in% df_dist_month$`Drop State`
  ) %>%
  group_by(`Pick State`) %>%
  summarize(total_n = sum(`Shipment Count`)) %>%
  arrange(desc(total_n)) %>%
  head(5) -> top5_origin_21
# Top 5 pick states: IA, IL, WI, TN, GA


# look for the top five destinations for each of the five origins
ibtl_2021 %>%
  filter(`Pick State` %in% top5_origin_21$`Pick State`) %>%
  group_by(`Pick State`, `Drop State`) %>%
  summarize(total_n = sum(`Shipment Count`)) %>%
  top_n(5, total_n) %>%
  arrange(desc(total_n), .by_group = TRUE) -> final_comb_21

# get the final dataset that has origin, destination, monthly shipment
ibtl_2021 %>%
  filter(
    `Pick State` %in% final_comb_21$`Pick State`,
    `Drop State` %in% final_comb_21$`Drop State`
  ) %>%
  group_by(`Pick State`, `Drop State`, `Ship Month`) %>%
  summarize(total_n = sum(`Shipment Count`)) %>%
  mutate(`Ship Month` = as.factor(`Ship Month`)) -> monthly_count_21


# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Truckload Freight Shipping Dashboard",
                  titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "tab0", icon = icon("truck")),
      menuItem(
        "2019WI Coverage",
        tabName = "tab1",
        icon = icon("location-pin")
      ),
      menuItem(
        "2019WI Shipment by Month",
        tabName = "tab2",
        icon = icon("chart-line")
      ),
      menuItem("2019WI Chicago", tabName = "tab3", icon = icon("city")),
      menuItem("2020 OD-Pair", tabName = "tab4", icon = icon("route"))
    )
  ),
  dashboardBody(
    shinyDashboardThemes(theme = "onenote"),
    tabItems(
      tabItem(tabName = "tab0",
              box(
                h4(
                  "This easy-to-use Truckload (TL) Shipping Dashboard provides direct insights into the overall shipping performance of truckload shipping operations and helps you make informed decisions, increase efficiency, and reduce costs to optimize supply chain management."
                ),
                width = 10,
                status = "primary"
              )),
      tabItem(tabName = "tab1",
              fluidRow(box(
                h3("State-wide Transportation & Logistics Services"),
                width = 10
              )),
              fluidRow(
                box(
                  title = div(h4('TL Service at State Level', style = "margin: 0;")),
                  status = "primary",
                  width = 10,
                  leafletOutput("map_state1")
                )
              ),
              fluidRow(
                box(
                  title = div(h4("TL Service at City Level", style = "margin: 0;")),
                  status = "primary",
                  width = 10,
                  leafletOutput("map_city1"),
                  footer = "The points represent the cities covered and the radius is proportional to the shipment from and to the city."
                )
              )),
      tabItem(
        tabName = "tab2",
        fluidRow(column(
          2,
          selectInput("month", "Month",
                      choices = list(
                        "November" = 11, "December" = 12
                      ))
        ),
        column(
          10,
          box(
            leafletOutput("map_state2"),
            status = "primary",
            width = 10,
            title = "TL at State Level by Month"
          )
        )),
        fluidRow(column(
          2,
          selectInput("month_city", "Month",
                      choices = list(
                        "November" = 11, "December" = 12
                      ))
        ),
        column(
          10,
          box(
            leafletOutput("map_city2"),
            status = "primary",
            width = 10,
            title = "TL at City Level by Month",
            footer = "The points represent the cities covered and the radius is proportional to the shipment from and to the city."
          )
        ))
      ),
      tabItem(tabName = "tab3",
              fluidRow(
                box(
                  title = "Destination and Shipment Count of Freight Flow out of Chicago in Winter 2019",
                  status = "primary",
                  width = 10,
                  leafletOutput("chicago")
                )
                
              )),
      tabItem(tabName = "tab4",
              fluidRow(
                column(4,
                       selectInput(
                         "origin", "Origin",
                         choices = list("IA", "IL", "WI", "TN", "GA")
                       )),
                column(
                  3,
                  checkboxGroupInput(
                    "show",
                    "Show",
                    choices = list("Bar" = 1,
                                   "Line" = 2),
                    selected = 1
                  )
                )
              ),
              fluidRow(
                column(
                  8,
                  conditionalPanel(
                    condition = "input.origin == 'IA'",
                    selectInput(
                      "destIA",
                      "Destination",
                      choices = list(
                        "IA" = "IA",
                        "IL" = "IL",
                        "WI",
                        "MN",
                        "TX" = "TX"
                      )
                    ),
                    plotOutput("plotIA")
                  ),
                  conditionalPanel(
                    condition = "input.origin == 'IL'",
                    selectInput(
                      "destIL",
                      "Destination",
                      choices = list(
                        "IA" = "IA",
                        "IL" = "IL",
                        "WI",
                        "TX" = "TX",
                        "IN"
                      )
                    ),
                    plotOutput("plotIL")
                  ),
                  conditionalPanel(
                    condition = "input.origin == 'WI'",
                    selectInput(
                      "destWI",
                      "Destination",
                      choices = list("IA" =
                                       "IA", "WI", "IL" = "IL", "TN", "NC")
                    ),
                    plotOutput("plotWI")
                  ),
                  conditionalPanel(
                    condition = "input.origin == 'TN'",
                    selectInput(
                      "destTN",
                      "Destination",
                      choices = list("TN", "IL", "IA" =
                                       "IA", "NC", "GA")
                    ),
                    plotOutput("plotTN")
                  ),
                  conditionalPanel(
                    condition = "input.origin == 'GA'",
                    selectInput(
                      "destGA",
                      "Destination",
                      choices = list("GA", "SC", "IL" =
                                       "IL", "TX" = "TX", "TN")
                    ),
                    plotOutput("plotGA")
                  )
                )
              ))
    )
    #       ),
    #       # 2021
    #       tabItem(tabName = "tab5",
    #               fluidRow(column(4,
    #                               selectInput("origin", "Origin",
    #                                           choices = list("IA", "IL", "WI", "TN", "GA"))),
    #                        column(3,
    #                               checkboxGroupInput("show", "Show",
    #                                                  choices = list("Bar" = 1,
    #                                                                 "Line" = 2),
    #                                                  selected = 1))
    #               ),
    #               fluidRow(
    #                 column(8,
    #                        conditionalPanel(condition = "input.origin == 'IA'",
    #                                         selectInput("destIA", "Destination",
    #                                                     choices = list("IA"="IA","IL"="IL","WI","MN","TX"="TX")),
    #                                         plotOutput("plotIA")),
    #                        conditionalPanel(condition = "input.origin == 'IL'",
    #                                         selectInput("destIL", "Destination",
    #                                                     choices = list("IA"="IA","IL"="IL","WI","TX"="TX","IN")),
    #                                         plotOutput("plotIL")),
    #                        conditionalPanel(condition = "input.origin == 'WI'",
    #                                         selectInput("destWI", "Destination",
    #                                                     choices = list("IA"="IA","WI","IL"="IL","TN","NC")),
    #                                         plotOutput("plotWI")),
    #                        conditionalPanel(condition = "input.origin == 'TN'",
    #                                         selectInput("destTN", "Destination",
    #                                                     choices = list("TN","IL","IA"="IA","NC","GA")),
    #                                         plotOutput("plotTN")),
    #                        conditionalPanel(condition = "input.origin == 'GA'",
    #                                         selectInput("destGA", "Destination",
    #                                                     choices = list("GA","SC","IL"="IL","TX"="TX","TN")),
    #                                         plotOutput("plotGA"))
    #                 )
    #               )
    #       )
    #     )
    #
  )
)

# Define server logic required
server <- function(input, output) {
  # Tab 1: range of services, state & city
  # define color palette for states
  pal <- colorNumeric(palette = "plasma", domain = states1$shipment)
  output$map_state1 <- renderLeaflet({
    leaflet(states1) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolygons(
        fillColor = ~ pal(shipment),
        stroke = TRUE,
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~ shipment,
        opacity = 0.7,
        title = "Shipment",
        position = "bottomright"
      )
  })
  
  # define color palette for cities
  pal_city1 <-
    colorNumeric(palette = "viridis", domain = cities$POP_MAX)
  output$map_city1 <- renderLeaflet({
    cities1 %>%
      leaflet() %>%
      setView(lng = -93.26801,
              lat = 44.29049,
              zoom = 4) %>%
      addTiles() %>%
      addCircles(
        lng = ~ LONGITUDE,
        lat = ~ LATITUDE,
        weight = 1,
        color =  ~ pal_city1(POP_MAX),
        radius = ~ shipment * 30,
        popup = ~ CITY
      )
  })
  
  
  # Tab 2: shipment count in different shipping months
  
  map_filter_month <- reactive({
    # filter data according to user's choice of month
    states2@data %>% filter(`Ship Month` == input$month)
  })
  
  output$map_state2 <- renderLeaflet({
    states2@data <- map_filter_month()
    states2 %>%
      leaflet() %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolygons(
        fillColor = ~ pal(shipment),
        stroke = TRUE,
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~ shipment,
        opacity = 0.7,
        title = "Shipment",
        position = "bottomright"
      )
  })
  
  map_filter_month2 <- reactive({
    cities2 %>% filter(`Ship Month` == input$month_city)
  })
  
  pal_city2 <-
    colorNumeric(palette = "viridis", domain = cities2$POP_MAX)
  
  output$map_city2 <- renderLeaflet({
    cities2 <- map_filter_month2()
    cities2 %>%
      leaflet() %>%
      setView(lng = -93.26801,
              lat = 44.29049,
              zoom = 4) %>%
      addTiles() %>%
      addCircles(
        lng = ~ LONGITUDE,
        lat = ~ LATITUDE,
        weight = 1,
        color =  ~ pal_city1(POP_MAX),
        radius = ~ shipment * 30,
        popup = ~ CITY
      )
  })
  
  
  # Tab 3
  pal3 <- colorNumeric(palette = "RdYlBu", domain = states3$shipment)
  output$chicago <- renderLeaflet({
    leaflet(states3) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      addPolygons(
        fillColor = ~ pal3(shipment),
        stroke = TRUE,
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "grey",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal3,
        values = ~ shipment,
        opacity = 0.7,
        title = "Shipment",
        position = "bottomright"
      ) %>%
      addMarkers(
        data = cities %>% filter(CITY == "CHICAGO"),
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        icon = list(
          iconUrl = 'https://icons.iconarchive.com/icons/goodstuff-no-nonsense/free-space/256/star-icon.png',
          iconSize = c(40, 40)
        ),
        popup = "Chicago"
      )
  })
  
  # Tab 4
  dfIA <- reactive({monthly_count %>% filter(`Pick State` == "IA", `Drop State` == input$destIA)})
  output$plotIA <- renderPlot({
    bar_line_dff <- dfIA()
    if(length(input$show) == 2){
    ggplot(bar_line_dff) +
      geom_bar(aes(x=`Ship Month`, y=total_n), stat = "identity", width = 0.6, fill="#CCCCFF")+
      geom_line(aes(x=`Ship Month`, y=total_n, group=1), color="#5D3FD3", linewidth=1)+
      theme_light()+
      labs(y = "Shipment Count", title = paste("Shipment count from IA to",input$destIA))+
      scale_y_continuous(breaks=seq.int(from=0,to=round(max(bar_line_dff$total_n)),by = round((round(max(bar_line_dff$total_n)) - 0)/(10 - 1)))) +
      geom_text(aes(x=`Ship Month`, y=total_n,label=total_n), position=position_dodge(width=0.9), vjust=-0.25)
    }
    else if(length(input$show) == 1){
      if(input$show==1){
        ggplot(bar_line_dff) +
          geom_bar(aes(x=`Ship Month`, y=total_n), stat = "identity", width = 0.6, fill="#CCCCFF")+
          theme_light()+
          labs(y = "Shipment Count", title = paste("Shipment count from IA to",input$destIA))+
          scale_y_continuous(breaks=seq.int(from=0,to=round(max(bar_line_dff$total_n)),by = round((round(max(bar_line_dff$total_n)) - 0)/(10 - 1)))) +
          geom_text(aes(x=`Ship Month`, y=total_n,label=total_n), position=position_dodge(width=0.9), vjust=-0.25)
      }
      else{
        ggplot(bar_line_dff) +
          geom_line(aes(x=`Ship Month`, y=total_n, group=1), color="#5D3FD3", linewidth=1)+
          theme_light()+
          labs(y = "Shipment Count", title = paste("Shipment count from IA to",input$destIA))+
          scale_y_continuous(breaks=seq.int(from=0,to=round(max(bar_line_dff$total_n)),by = round((round(max(bar_line_dff$total_n)) - 0)/(10 - 1)))) +
          geom_text(aes(x=`Ship Month`, y=total_n,label=total_n), position=position_dodge(width=0.9), vjust=-0.25)
      }
    }
    
  })
  
  dfIL <- reactive({monthly_count %>% filter(`Pick State` == "IL", `Drop State` == input$destIL)})
  output$plotIL <- renderPlot({
    bar_line_dff <- dfIL()
    if(length(input$show) == 2){
      ggplot(bar_line_dff)+
      geom_bar(aes(x=`Ship Month`, y=total_n), stat = "identity",width = 0.6, fill="#CCCCFF")+
      geom_line(aes(x=`Ship Month`, y=total_n, group=1), color="#5D3FD3", linewidth=1)+
      theme_light()+
      labs(y = "Shipment Count", title = paste("Shipment count from IL to",input$destIL))+
      scale_y_continuous(breaks=seq.int(from=0,to=round(max(bar_line_dff$total_n)),by = round((round(max(bar_line_dff$total_n)) - 0)/(10 - 1)))) +
      geom_text(aes(x=`Ship Month`, y=total_n,label=total_n), position=position_dodge(width=0.9), vjust=-0.25)
    }
    else if(length(input$show) == 1){
      if(input$show==1){
        ggplot(bar_line_dff) +
          geom_bar(aes(x=`Ship Month`, y=total_n), stat = "identity", width = 0.6, fill="#CCCCFF")+
          theme_light()+
          labs(y = "Shipment Count", title = paste("Shipment count from IL to",input$destIL))+
          scale_y_continuous(breaks=seq.int(from=0,to=round(max(bar_line_dff$total_n)),by = round((round(max(bar_line_dff$total_n)) - 0)/(10 - 1)))) +
          geom_text(aes(x=`Ship Month`, y=total_n,label=total_n), position=position_dodge(width=0.9), vjust=-0.25)
      }
      else{
        ggplot(bar_line_dff)+
          geom_line(aes(x=`Ship Month`, y=total_n, group=1), color="#5D3FD3", linewidth=1)+
          theme_light()+
          labs(y = "Shipment Count", title = paste("Shipment count from IL to",input$destIL))+
          scale_y_continuous(breaks=seq.int(from=0,to=round(max(bar_line_dff$total_n)),by = round((round(max(bar_line_dff$total_n)) - 0)/(10 - 1)))) +
          geom_text(aes(x=`Ship Month`, y=total_n,label=total_n), position=position_dodge(width=0.9), vjust=-0.25)
      }
    }
  })
  
  dfWI <- reactive({monthly_count %>% filter(`Pick State` == "WI", `Drop State` == input$destWI)})
  output$plotWI <- renderPlot({
    bar_line_dff <- dfWI()
    if(length(input$show) == 2){
      ggplot(bar_line_dff)+
      geom_bar(aes(x=`Ship Month`, y=total_n), stat = "identity", width = 0.6, fill="#CCCCFF")+
      geom_line(aes(x=`Ship Month`, y=total_n, group=1), color="#5D3FD3", linewidth=1)+
      theme_light()+
      labs(y = "Shipment Count", title = paste("Shipment count from WI to",input$destWI))+
      scale_y_continuous(breaks=seq.int(from=0,to=round(max(bar_line_dff$total_n)),by = round((round(max(bar_line_dff$total_n)) - 0)/(10 - 1)))) +
      geom_text(aes(x=`Ship Month`, y=total_n,label=total_n), position=position_dodge(width=0.9), vjust=-0.25)
    }
    else if(length(input$show) == 1){
      if(input$show==1){
        ggplot(bar_line_dff)+
          geom_bar(aes(x=`Ship Month`, y=total_n), stat = "identity", width = 0.6, fill="#CCCCFF")+
          theme_light()+
          labs(y = "Shipment Count", title = paste("Shipment count from WI to",input$destWI))+
          scale_y_continuous(breaks=seq.int(from=0,to=round(max(bar_line_dff$total_n)),by = round((round(max(bar_line_dff$total_n)) - 0)/(10 - 1)))) +
          geom_text(aes(x=`Ship Month`, y=total_n,label=total_n), position=position_dodge(width=0.9), vjust=-0.25)
      }
      else{
        ggplot(bar_line_dff)+
          geom_line(aes(x=`Ship Month`, y=total_n, group=1), color="#5D3FD3", linewidth=1)+
          theme_light()+
          labs(y = "Shipment Count", title = paste("Shipment count from WI to",input$destWI))+
          scale_y_continuous(breaks=seq.int(from=0,to=round(max(bar_line_dff$total_n)),by = round((round(max(bar_line_dff$total_n)) - 0)/(10 - 1)))) +
          geom_text(aes(x=`Ship Month`, y=total_n,label=total_n), position=position_dodge(width=0.9), vjust=-0.25)
      }
    }
  })
  
  dfTN <- reactive({monthly_count %>% filter(`Pick State` == "TN", `Drop State` == input$destTN)})
  output$plotTN <- renderPlot({
    bar_line_dff <- dfTN()
    if(length(input$show) == 2){
      ggplot(bar_line_dff)+
      geom_bar(aes(x=`Ship Month`, y=total_n), stat = "identity", width = 0.6, fill="#CCCCFF")+
      geom_line(aes(x=`Ship Month`, y=total_n, group=1), color="#5D3FD3", linewidth=1)+
      theme_light()+
      labs(y = "Shipment Count", title = paste("Shipment count from TN to",input$destTN))+
      scale_y_continuous(breaks=seq.int(from=0,to=round(max(bar_line_dff$total_n)),by = round((round(max(bar_line_dff$total_n)) - 0)/(10 - 1)))) +
      geom_text(aes(x=`Ship Month`, y=total_n,label=total_n), position=position_dodge(width=0.9), vjust=-0.25)
    }
    else if(length(input$show) == 1){
      if(input$show==1){
        ggplot(bar_line_dff)+
          geom_bar(aes(x=`Ship Month`, y=total_n), stat = "identity", width = 0.6, fill="#CCCCFF")+
          theme_light()+
          labs(y = "Shipment Count", title = paste("Shipment count from TN to",input$destTN))+
          scale_y_continuous(breaks=seq.int(from=0,to=round(max(bar_line_dff$total_n)),by = round((round(max(bar_line_dff$total_n)) - 0)/(10 - 1)))) +
          geom_text(aes(x=`Ship Month`, y=total_n,label=total_n), position=position_dodge(width=0.9), vjust=-0.25)
      }
      else{
        ggplot(bar_line_dff)+
          geom_line(aes(x=`Ship Month`, y=total_n, group=1), color="#5D3FD3", linewidth=1)+
          theme_light()+
          labs(y = "Shipment Count", title = paste("Shipment count from TN to",input$destTN))+
          scale_y_continuous(breaks=seq.int(from=0,to=round(max(bar_line_dff$total_n)),by = round((round(max(bar_line_dff$total_n)) - 0)/(10 - 1)))) +
          geom_text(aes(x=`Ship Month`, y=total_n,label=total_n), position=position_dodge(width=0.9), vjust=-0.25)
      }
    }
  })
  
  dfGA <- reactive({monthly_count %>% filter(`Pick State` == "GA", `Drop State` == input$destGA)})
  output$plotGA <- renderPlot({
    bar_line_dff <- dfGA()
    if(length(input$show) == 2){
      ggplot(bar_line_dff)+
      geom_bar(aes(x=`Ship Month`, y=total_n), stat = "identity", width = 0.6, fill="#CCCCFF")+
      geom_line(aes(x=`Ship Month`, y=total_n, group=1), color="#5D3FD3", linewidth=1)+
      theme_light()+
      labs(y = "Shipment Count", title = paste("Shipment count from GA to",input$destGA))+
      scale_y_continuous(breaks=seq.int(from=0,to=round(max(bar_line_dff$total_n)),by = round((round(max(bar_line_dff$total_n)) - 0)/(10 - 1)))) +
      geom_text(aes(x=`Ship Month`, y=total_n,label=total_n), position=position_dodge(width=0.9), vjust=-0.25)
    }
    else if(length(input$show) == 1){
      if(input$show==1){
        ggplot(bar_line_dff)+
          geom_bar(aes(x=`Ship Month`, y=total_n), stat = "identity", width = 0.6, fill="#CCCCFF")+
          theme_light()+
          labs(y = "Shipment Count", title = paste("Shipment count from GA to",input$destGA))+
          scale_y_continuous(breaks=seq.int(from=0,to=round(max(bar_line_dff$total_n)),by = round((round(max(bar_line_dff$total_n)) - 0)/(10 - 1)))) +
          geom_text(aes(x=`Ship Month`, y=total_n,label=total_n), position=position_dodge(width=0.9), vjust=-0.25)
      }
      else{
        ggplot(bar_line_dff)+
          geom_line(aes(x=`Ship Month`, y=total_n, group=1), color="#5D3FD3", linewidth=1)+
          theme_light()+
          labs(y = "Shipment Count", title = paste("Shipment count from GA to",input$destGA))+
          scale_y_continuous(breaks=seq.int(from=0,to=round(max(bar_line_dff$total_n)),by = round((round(max(bar_line_dff$total_n)) - 0)/(10 - 1)))) +
          geom_text(aes(x=`Ship Month`, y=total_n,label=total_n), position=position_dodge(width=0.9), vjust=-0.25)
      }
    }
  })
  

}

# Run the application
shinyApp(ui = ui, server = server)

