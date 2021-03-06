#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(ggplot2)
library(shinydashboard)
library(dplyr)
library(stringr)
library(leaflet)
library(leaflet.providers)
library(leaflet.extras)
library(DT)
library(shinyjs)
library(tidyverse)
library(reshape2) 

jsCode <- 'shinyjs.markerClick = function(id) {
              map.eachLayer(function (layer) {
                if (layer.options.layerId == id) {
                  layer.fire("click");
                }
              })
           };'

#read all file names in a temp variable
temp = list.files(pattern="parta..tsv")
allData2 <- lapply(temp, read.delim)
allData <- do.call(rbind, allData2)

#converting dates
allData$newDate <- as.Date(allData$date, "%m/%d/%Y")
allData$date <- NULL
lat_long <- read.table(file = "CTA_-_System_Information_-_List_of__L__Stops.tsv", sep = "\t", header = TRUE, quote = "\"")
lat_long$lines <- str_extract(lat_long$STATION_DESCRIPTIVE_NAME, "\\(.*\\)")
lat_long$lines <- str_remove_all(lat_long$lines, "[\\(\\)]")
lat_long <- lat_long %>% distinct(MAP_ID, lines, .keep_all = TRUE)
mergedData <- merge(x = allData, y= lat_long, by.x = c("station_id"), by.y = c("MAP_ID"), all.x = TRUE)


mergedData$Location[mergedData$stationname == 'Homan'] = "(41.884914, -87.711327)"
mergedData$lines[mergedData$stationname == 'Homan'] = "Green Line"

mergedData$Location[mergedData$stationname == 'Madison/Wabash'] = "(41.882023, -87.626098)"
mergedData$lines[mergedData$stationname == 'Madison/Wabash'] = "Brown, Green, Orange, Pink & Purple Lines"

mergedData$Location[mergedData$stationname == 'Randolph/Wabash'] = "(41.884431, -87.626149)"
mergedData$lines[mergedData$stationname == 'Randolph/Wabash'] = "Green, Orange, Pink, Purple & Brown Lines"

mergedData$Location[mergedData$stationname == 'Washington/State'] = "(41.8837, -87.6278)"
mergedData$lines[mergedData$stationname == 'Washington/State'] = "Red Line"

mergedData$lat <- as.numeric(str_extract(mergedData$Location, "\\d+.\\d+"))
mergedData$long <- as.numeric(str_extract(mergedData$Location, "-\\d+.\\d+"))
cta <- c("#c60c30", "#00a1de", "#62361b", "#009b3a", "#522398", "#522398", "#f9e300", "#e27ea6", "#f9461c")


################

trial <-mergedData[, c("stationname", "RED", "BLUE","BRN", "G","P", "Pexp", "Y", "Pnk", "O")]
trial <- melt(trial, value.name = "group", id="stationname")
trial <- subset(trial, group=="true")
trial <- trial %>% distinct()
trial <- trial %>% group_by(variable)
trial$group <- factor(trial$group)

mergedData <- merge(mergedData, trial, by = 'stationname')



###############




orders <- c("Alphabetical", "Ascending", "Descending")
years<-c(2001:2021)
LtoM <-colorRampPalette(c('red', 'red' ))
Mid <- "snow3"
MtoH <-colorRampPalette(c('green', 'darkgreen'))
# newchoices <- subset(mergedData, newDate=="2021-08-23") %>% distinct(stationname) %>% arrange()

ui <- dashboardPage(
  dashboardHeader(title="Sleepy Subway"),
  # Application title
  
  dashboardSidebar(
    
    
    sidebarMenu(
      br(),br(),br(),br(), br(),br(),br(),br(), br(),br(),br(),br(), br(),br(),br(),br(), 
      menuItem("Home", tabName = "dashboard", icon = NULL),
      
      menuItem("About", tabName = "about", icon = NULL),
      
      menuItem("Yearly Plots", tabName = "yearlyPlots", icon = NULL)
      
    )
    
  ),
  
  
  dashboardBody(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = jsCode, functions = c('markerClick')),
    tabItems(
      
      tabItem(tabName = "dashboard",
              
              
              fluidRow(
                
                column(2,
                       
                       fluidRow(style="height:40vh"),
                       p("Input controls"),
                       dateInput("date1", "Date:", value = "2021-08-23"),
                       dateInput("date2", "Date 2:", value = "2020-08-23"),
                       actionButton("prevDay", "Previous Day"),
                       actionButton("nextDay", "Next  Day"),
                       selectInput("orders", "Select the order of bar chart", orders, selected = "Alphabetical"),
                       selectInput(inputId = "station", label = "Select station", choices = NULL),
                       radioButtons(
                         inputId = "radio",
                         label = "Mode",
                         choices = c("Single Date", "Different Dates"),
                         selected = "Single Date",
                         inline = FALSE,
                         width = NULL
                       )
                ),
                
                column(10, 
                       
                       
                       fluidRow(style='height:40vh',
                                box( title = textOutput("text"), solidHeader = TRUE, status = "primary", width = 12,
                                     plotOutput("hist1", height="36vh"), height="40vh"
                                )
                       ),
                       
                       fluidRow(style='height:50vh; margin-top: 50px',
                                column(6,
                                       leafletOutput("mymap", height="40vh")
                                ),
                                
                                column(4, 
                                       box(solidHeader = TRUE, status = "primary", width = 180,
                                           dataTableOutput("barplottable")
                                       )
                                )
                                
                                
                                
                       )   
                       
                       
                       
                       
                       
                )
                
                
                
                
                
                
              )
              
              
              
              
      ),
      
      tabItem(tabName="yearlyPlots",
              
              fluidRow(
                
                column(2,
                       
                       fluidRow(style="height:40vh"),
                       p("Input controls"),
                       selectInput("years", "Select the year", years, selected = "2021"),
                       selectInput(inputId = "yearly_station", label = "Select station", choices = NULL),
                       selectInput("order_for_single", "Select chart Type", c("BarPlot", "Table"), selected = "BarPlot")
                       
                ),
                column(10,
                       
                       column(6, 
                              
                              fluidRow(style="height:40vh", 
                                       box( title = "Daily entries", solidHeader = TRUE, status = "primary", width = 12,
                                            plotOutput("daily"), dataTableOutput("tableDaily")
                                       )
                                       
                                       
                              ),
                              
                              fluidRow(style="height: 40vh", 
                                       
                                       leafletOutput("mymap2")
                              )
                              
                       ),
                       column(4,
                              
                              fluidRow(
                                column(12, 
                                       box( title = "Yearly entries", solidHeader = TRUE, status = "primary", width = 12,
                                            plotOutput("yearly"), dataTableOutput("tableYearly")
                                       )
                                       
                                )
                              ),
                              fluidRow(
                                column(6,
                                       box( title = "Monthly entries", solidHeader = TRUE, status = "primary", width = 12,
                                            plotOutput("monthly"), dataTableOutput("tableMonthly")
                                       )
                                ),
                                column(6, 
                                       box( title = "Day of Week entries", solidHeader = TRUE, status = "primary", width = 12,
                                            plotOutput("weekly"), dataTableOutput("tableWeekly")
                                       )
                                )
                                
                              )
                              
                       )
                       
                )
                
              )
              
      ),
      
      tabItem(tabName= "about",
              h2("About"), 
              p("Application written by Gautam Kushwah for CS424 spring 2022 taught by Dr. Andrew Johnson"),
              p("Data taken from https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
              p("The app helps visualize CTA L data over the last 20 years and helps uncover trends in data"),
              p("Reference for R functions through https://shiny.rstudio.com/reference/shin")
      )
      
    )
    
    
  )
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  date1 <- reactive({input$date1})
  date2 <- reactive({input$date2})
  orders <- reactive({input$orders})   #order for the bar plot
  values <- reactiveValues(selected=NULL)
  yearly_values <- reactiveValues(selected="UIC-Halsted")
  mode <- reactive({input$radio}) ###checking for which mode the user is in
  single_year <- reactive({input$years})
  yearly_station <- reactive({input$yearly_station})
  
  single_df <- reactive({ subset(mergedData, year(newDate)==single_year()) })
  
  #   code for dynamic header
  output$text <-renderText({ paste("Total entries for", date( input$date1), ", ", weekdays(input$date1) ) })
  tmpdata <- reactive({ subset(mergedData, newDate==input$date1)})
  tmpdata2 <-reactive ({subset(mergedData, newDate==input$date2) })
  #   dfnew3 <- reactive({data.frame(tmpdata()$stationname, tmpdata()$rides)})
  #   names(dfnew3) <- c("Station", "Rides")
  
  choices_stations <- reactive({
    choices_stations <- tmpdata() %>% distinct(stationname) %>% arrange()
    
  })
  
  choices_stations_new <- reactive({
    choices_stations_new <- single_df() %>% distinct(stationname) %>% arrange()
    
  })
  
  
  observe({
    updateSelectInput(session = session, inputId = "station", choices = choices_stations())
  })
  
  ####updating station list on the yearly plot page, single df is the data frame
  observe({
    updateSelectInput(session = session, inputId = "yearly_station", choices = choices_stations_new(), selected="UIC-Halsted")
  })
  
  
  observeEvent(input$prevDay, {
    curDate <- date1()
    curDate <- curDate - 1
    updateDateInput(session, "date1", value = curDate)
  })
  
  observeEvent(input$nextDay, {
    curDate <- date1()
    curDate <- curDate + 1
    updateDateInput(session, "date1", value = curDate)
  })
  
  
  
  output$hist1 <- renderPlot({
    
    
    tmpdata <- subset(mergedData, newDate==date1())
    
    if(mode()=="Single Date"){
      if(orders() == "Descending"){
        ggplot(tmpdata, aes(x=reorder(stationname, -rides), y=rides, fill=variable)) +labs(x="station ", y = "Total number of entries") + geom_bar(stat="identity") + scale_x_discrete(guide=guide_axis( angle = 45)) +  scale_fill_manual(values = cta)
      }else if(orders()=="Ascending"){
        ggplot(tmpdata, aes(x=reorder(stationname, rides), y=rides, fill=variable)) +labs(x="station ", y = "Total number of entries") + geom_bar(stat="identity") + scale_x_discrete(guide=guide_axis( angle = 45))  + scale_fill_manual(values = cta)
      } else{
        ggplot(tmpdata, aes(x=stationname, y=rides, fill=variable)) +labs(x="station ", y = "Total number of entries") + geom_bar(stat="identity") + scale_x_discrete(guide=guide_axis( angle = 45))  + scale_fill_manual(values = cta)
      }  
    }else{  ##if two dates are selected
      
      dfl <- tmpdata %>% full_join(tmpdata2(), by="station_id")
      dfl <- dfl[!is.na(dfl$stationname.x),]
      dfl$rides.y <- replace_na(dfl$rides.y, 0)
      dfl$diff = dfl$rides.x - dfl$rides.y    
      
      if(orders() == "Descending"){
        ggplot(dfl, aes(x=reorder(stationname.x, -diff), y=diff, fill=diff)) +labs(x="station ", y = "Total number of entries") + geom_bar(stat="identity", position="dodge") + scale_x_discrete(guide=guide_axis( angle = 45)) + scale_fill_gradient2(low=LtoM(100), mid='snow3', high=MtoH(100), space='Lab')
      }else if(orders()=="Ascending"){
        ggplot(dfl, aes(x=reorder(stationname.x, diff), y=diff, fill=diff)) +labs(x="station ", y = "Total number of entries") + geom_bar(stat="identity", position="dodge") + scale_x_discrete(guide=guide_axis( angle = 45)) + scale_fill_gradient2(low=LtoM(100), mid='snow3', high=MtoH(100), space='Lab')
      } else{
        ggplot(dfl, aes(x=stationname.x, y=diff, fill=diff)) +labs(x="station ", y = "Total number of entries") + geom_bar(stat="identity", position="dodge") + scale_x_discrete(guide=guide_axis( angle = 45)) +  scale_fill_gradient2(low=LtoM(100), mid='snow3', high=MtoH(100), space='Lab')
      }
      
      
    }
    
    
  })
  
  
  output$mymap <- renderLeaflet({
    df <- subset(mergedData, newDate==date1())
    df$line_color <-  str_extract(df$line, "\\w+")
    map <- leaflet(options= leafletOptions()) %>%
      addTiles(group="Base") %>% 
      addCircleMarkers(data = df, lat = ~lat, lng = ~long, 
                       
                       radius = ~log(rides+10)*1.25,
                       layerId = ~stationname,
                       color = ~line_color,
                       fillOpacity = 0.7,
                       popup = paste("<center><strong>" ,df$stationname, "</strong>", "<br>",
                                     df$lines, "<br>",
                                     "Rides: ", df$rides, "<br> </center>")
      )%>%
      setView( lat = 41.8781, lng = -87.6298, zoom = 10) %>%
      addResetMapButton() %>%
      addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%
      addProviderTiles("CartoDB.Positron", group="Positron") %>%
      addLayersControl(
        baseGroups = c("Base", "Satellite", "Positron"),
        options = layersControlOptions(collapsed = FALSE)
      )
    map <- map %>% 
      htmlwidgets::onRender("
          function(el, x) {
            map = this;
          }"
      )     
    return(map)
    
  })
  
  
  output$barplottable <- renderDataTable({
    df <- tmpdata()
    df2 <- tmpdata2()
    
    if(mode()=="Single Date"){
      dfl <- df[, c("stationname", "rides", "lines")]
      fm <- "rides"
    }else{
      dfl <- df %>% full_join(df2, by="station_id")
      dfl <- dfl[!is.na(dfl$stationname.x),]
      dfl$rides.y <- replace_na(dfl$rides.y, 0)
      dfl$diff = dfl$rides.x - dfl$rides.y 
      dfl <- dfl[, c("stationname.x", "diff", "lines.x")]
      colnames(dfl) <- c("Station Name", "Difference", "Lines")
      fm <- "Difference"
    }
    
    
    
    
    if(orders() == "Descending"){
      tab_order <- list(list(2, 'dsc'))
    } else if(orders()=="Ascending"){
      tab_order <- list(list(2, 'asc'))
    } else{
      tab_order <- list(list(1, 'asc'))
    }
    datatable(dfl, 
              options = list(
                searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15),
                order = tab_order
              )) %>% 
      formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
      formatRound(fm, digits = 0)
  })
  
  
  
  
  ### observer for map
  observe({
    proxy <- leafletProxy("mymap")
    proxy %>% clearPopups() 
    event <- input$mymap_marker_click
    values$selected <- event$id
    if (is.null(event))
      return()
    # print(length(input$mymap_click))
    if(length(input$mymap_click) > 0) {
      updateSelectInput(session = session, inputId = "station", selected =  values$selected)
    }  
    
    
  })
  
  ### anotherObserver
  observeEvent(input$station, {
    proxy <- leafletProxy("mymap")
    proxy %>% clearPopups() 
    shinyjs::js$markerClick(input$station)
    # df <- tmpdata()
    # values$selected <- input$station
    # df <- subset(df, stationname == values$selected)
    # print("it comes here")
    
    # content <-  paste("<center><strong>" ,df$stationname, "</strong>", "<br>",
    #                   df$lines, "<br>",
    #                   "Rides: ", df$rides, "<br> </center>")
    # if(length(input$mymap_click) == 0){
    #     leafletProxy("mymap") %>% addPopups(df$long, df$lat, content)
    # }
    
    
  })
  
  ##############for second map
  
  ### observer for map
  observe({
    proxy <- leafletProxy("mymap2")
    proxy %>% clearPopups() 
    event <- input$mymap2_marker_click
    yearly_values$selected <- event$id
    if (is.null(event))
      return()
    # print(length(input$mymap_click))
    if(length(input$mymap2_click) > 0) {
      updateSelectInput(session = session, inputId = "yearly_station", selected =  yearly_values$selected)
    }  
    
    
  })
  
  
  observeEvent(input$yearly_station, {
    proxy <- leafletProxy("mymap2")
    proxy %>% clearPopups() 
    shinyjs::js$markerClick(input$yearly_station)
    
  })
  
  observeEvent(input$radio, {
    if(input$radio == "Single Date"){
      shinyjs::hide(id="date2")
    }else{
      shinyjs::show(id="date2")
    }
  })
  
  
  observeEvent(input$order_for_single, {
    if(input$order_for_single == "BarPlot"){
      shinyjs::hide(id="tableYearly")
      shinyjs::hide(id="tableMonthly")
      shinyjs::hide(id="tableWeekly")
      shinyjs::hide(id="tableDaily")
      shinyjs::show(id="yearly")
      shinyjs::show(id="monthly")
      shinyjs::show(id="weekly")
      shinyjs::show(id="daily")
      
    }else{
      shinyjs::hide(id="yearly")
      shinyjs::hide(id="monthly")
      shinyjs::hide(id="weekly")
      shinyjs::hide(id="daily")
      shinyjs::show(id="tableYearly")
      shinyjs::show(id="tableMonthly")
      shinyjs::show(id="tableWeekly")
      shinyjs::show(id="tableDaily")
    }
  })
  
  
  
  
  
  
  
  
  ########### yearly page
  
  
  output$mymap2 <- renderLeaflet({
    df <- subset(mergedData, newDate==date1())
    df$line_color <-  str_extract(df$line, "\\w+")
    map <- leaflet(options= leafletOptions()) %>%
      addTiles(group="Base") %>% 
      addCircleMarkers(data = df, lat = ~lat, lng = ~long, 
                       
                       radius = ~log(rides+10)*1.25,
                       layerId = ~stationname,
                       color = ~line_color,
                       fillOpacity = 0.6,
                       popup = paste("<center><strong>" ,df$stationname, "</strong>", "<br>",
                                     df$lines, "<br>",
                                     "Rides: ", df$rides, "<br> </center>")
      )%>%
      setView( lat = 41.8781, lng = -87.6298, zoom = 10) %>%
      addResetMapButton() %>%
      addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%
      addProviderTiles("CartoDB.Positron", group="Positron") %>%
      addLayersControl(
        baseGroups = c("Base", "Satellite", "Positron"),
        options = layersControlOptions(collapsed = FALSE)
      )
    map <- map %>% 
      htmlwidgets::onRender("
          function(el, x) {
            map = this;
          }"
      )     
    return(map)
    
  })
  
  
  
  output$daily <- renderPlot({
    
    dft <- single_df()
    dft <- subset(dft, stationname==yearly_station())
    ggplot(dft, aes(x= newDate, y=rides)) +labs(x="Date ", y = "Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4") 
    
  })
  
  output$yearly <- renderPlot({
    
    dft <- mergedData
    dft <- subset(dft, stationname==yearly_station())
    ggplot(dft, aes(x= year(newDate), y=rides)) +labs(x="Year ", y = "Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4") 
    
  })
  
  
  output$monthly <- renderPlot({
    
    dft <- single_df()
    dft <- subset(dft, stationname==yearly_station())
    ggplot(dft, aes(x= month(newDate), y=rides)) +labs(x="Months (1 = Jan, 12=Dec)", y="Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4")  + scale_x_continuous(breaks = seq(1, 12, by = 1)) + ggtitle(paste("Station Name: ", input$yearly_station), subtitle=paste("Year", input$years))
    
  })
  
  
  output$weekly <- renderPlot({
    
    dft <- single_df()
    dft <- subset(dft, stationname==yearly_station())
    f <- factor(weekdays(dft$newDate), levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
    ggplot(dft, aes(x= f, y=rides)) +labs(x="Days of the week", y="Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4")  + ggtitle(paste("Station Name: ", input$yearly_station), subtitle=paste("Year", input$years))
    
  })
  
  output$tableYearly <- renderDataTable({
    
    df<- subset(mergedData, stationname==yearly_station()) %>%
      mutate(year = format(newDate, "%Y")) %>%
      group_by(year) %>%
      summarise(rides = sum(rides))
    
    
    
    datatable(df, 
              options = list(
                searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15),
                order = list(list(1, 'asc'))
              )) %>% 
      formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
      formatRound('rides', digits = 0)
  })
  
  
  output$tableMonthly <- renderDataTable({
    
    df<- subset(mergedData, stationname==yearly_station()) %>%
      mutate(month = month(newDate)) %>%
      group_by(month) %>%
      summarise(rides = sum(rides))
    
    
    
    datatable(df, 
              options = list(
                searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15),
                order = list(list(1, 'asc'))
              )) %>% 
      formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
      formatRound('rides', digits = 0)
  })
  
  
  output$tableWeekly <- renderDataTable({
    
    df<- subset(mergedData, stationname==yearly_station()) %>%
      mutate(weekdays = weekdays(newDate)) %>%
      group_by(weekdays) %>%
      summarise(rides = sum(rides))
    
    
    
    datatable(df, 
              options = list(
                searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15),
                order = list(list(1, 'asc'))
              )) %>% 
      formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
      formatRound('rides', digits = 0)
  })
  
  output$tableDaily <- renderDataTable({
    df<- single_df()
    df<- subset(mergedData, stationname==yearly_station()) 
    
    df <- df[, c("newDate", "rides")]
    
    datatable(df, 
              options = list(
                searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15),
                order = list(list(1, 'asc'))
              )) %>% 
      formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
      formatRound('rides', digits = 0)
  })
  
  
  
  ##############last bracket
  
}

# Run the application 
shinyApp(ui = ui, server = server)
