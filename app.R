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
orders <- c("Alphabetical", "Ascending", "Descending")
# newchoices <- subset(mergedData, newDate=="2021-08-23") %>% distinct(stationname) %>% arrange()

ui <- dashboardPage(
  dashboardHeader(title="Sleepy Subway"),
  # Application title
  
  dashboardSidebar(
    
    
    sidebarMenu(
      br(),br(),br(),br(), br(),br(),br(),br(), br(),br(),br(),br(), br(),br(),br(),br(), 
      menuItem("Home", tabName = "dashboard", icon = NULL),
      
      menuItem("About", tabName = "about", icon = NULL)
      
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
                
                column(5, 
                       
                       
                       fluidRow(style='height:30vh',
                                box( title = textOutput("text"), solidHeader = TRUE, status = "primary", width = 12,
                                     plotOutput("hist1")
                                )
                       ),
                       
                       fluidRow(style='height:50vh',
                                
                                leafletOutput("mymap")
                                
                       )   
                       
                       
                       
                       
                       
                ),
                
                
                
                column(5,
                       
                       
                       box(solidHeader = TRUE, status = "primary", width = 200,
                           dataTableOutput("barplottable")
                       )
                       
                       
                )
                
                
              )
              
              
              
              
      ),
      
      tabItem(tabName= "about"
              
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
  mode <- reactive({input$radio}) ###checking for which mode the user is in
  
  
  #   code for dynamic header
  output$text <-renderText({ paste("Total entries for", date( input$date1), ", ", weekdays(input$date1) ) })
  tmpdata <- reactive({ subset(mergedData, newDate==input$date1)})
  tmpdata2 <-reactive ({subset(mergedData, newDate==input$date2) })
  #   dfnew3 <- reactive({data.frame(tmpdata()$stationname, tmpdata()$rides)})
  #   names(dfnew3) <- c("Station", "Rides")
  
  choices_stations <- reactive({
    choices_stations <- tmpdata() %>% distinct(stationname) %>% arrange()
    
  })
  observe({
    updateSelectInput(session = session, inputId = "station", choices = choices_stations())
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
        ggplot(tmpdata, aes(x=reorder(stationname, -rides), y=rides)) +labs(x="station ", y = "Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4") + scale_x_discrete(guide=guide_axis( angle = 45))
      }else if(orders()=="Ascending"){
        ggplot(tmpdata, aes(x=reorder(stationname, rides), y=rides)) +labs(x="station ", y = "Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4") + scale_x_discrete(guide=guide_axis( angle = 45))
      } else{
        ggplot(tmpdata, aes(x=stationname, y=rides)) +labs(x="station ", y = "Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4") + scale_x_discrete(guide=guide_axis( angle = 45))
      }  
    }else{  ##if two dates are selected
      
      dfl <- tmpdata %>% full_join(tmpdata2(), by="station_id")
      dfl <- dfl[!is.na(dfl$stationname.x),]
      dfl$rides.y <- replace_na(dfl$rides.y, 0)
      dfl$diff = dfl$rides.x - dfl$rides.y    
      
      if(orders() == "Descending"){
        ggplot(dfl, aes(x=reorder(stationname.x, -diff), y=diff)) +labs(x="station ", y = "Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4") + scale_x_discrete(guide=guide_axis( angle = 45))
      }else if(orders()=="Ascending"){
        ggplot(dfl, aes(x=reorder(stationname.x, diff), y=diff)) +labs(x="station ", y = "Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4") + scale_x_discrete(guide=guide_axis( angle = 45))
      } else{
        ggplot(dfl, aes(x=stationname.x, y=diff)) +labs(x="station ", y = "Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4") + scale_x_discrete(guide=guide_axis( angle = 45))
      }
      
      
    }
    
    
  })
  
  
  output$mymap <- renderLeaflet({
    df <- subset(mergedData, newDate==date1())
    
    map <- leaflet(options= leafletOptions()) %>%
      addTiles(group="Base") %>% 
      addCircleMarkers(data = df, lat = ~lat, lng = ~long, 
                       
                       radius = ~log(rides+10)*1.25,
                       layerId = ~stationname,
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
      fm <- "diff"
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
  
  
  observeEvent(input$radio, {
    if(input$radio == "Single Date"){
      shinyjs::hide(id="date2")
    }else{
      shinyjs::show(id="date2")
    }
  })
  
  observeEvent(input$date2, {
    print(input$date2)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
