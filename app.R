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
    tabItems(
      
      tabItem(tabName = "dashboard",
              
              
              fluidRow(
                
                column(2,
                       p("Input controls"),
                       fluidRow(style="height:40vh"),
                       dateInput("date1", "Date:", value = "2021-08-23"),
                       
                       actionButton("prevDay", "Previous Day"),
                       actionButton("nextDay", "Next  Day"),
                       selectInput("orders", "Select the order of bar chart", orders, selected = "Alphabetical")
                       
                ),
                
                column(5, 
                       
                       
                       fluidRow(style='height:30vh',
                                box( title = textOutput("text"), solidHeader = TRUE, status = "primary", width = 12,
                                     plotOutput("hist1")
                                )
                       ),
                       
                       fluidRow(style='height:40vh',
                                
                                leafletOutput("mymap")
                                
                       )   
                       
                       
                       
                       
                       
                ),
                
                
                
                column(5,
                       
                       
                       
                       
                       
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
  orders <- reactive({input$orders})
  #   code for dynamic header
  output$text <-renderText({ paste("Total entries for", date( input$date1), ", ", weekdays(input$date1) ) })
  
  
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
    if(orders() == "Descending"){
      ggplot(tmpdata, aes(x=reorder(stationname, -rides), y=rides)) +labs(x="station ", y = "Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4") + scale_x_discrete(guide=guide_axis( angle = 45))
    }else if(orders()=="Ascending"){
      ggplot(tmpdata, aes(x=reorder(stationname, rides), y=rides)) +labs(x="station ", y = "Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4") + scale_x_discrete(guide=guide_axis( angle = 45))
    } else{
      ggplot(tmpdata, aes(x=stationname, y=rides)) +labs(x="station ", y = "Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4") + scale_x_discrete(guide=guide_axis( angle = 45))
    }
    
  })
  
  
  output$mymap <- renderLeaflet({
    df <- subset(mergedData, newDate==date1())
    
    map <- leaflet(options= leafletOptions()) %>%
      addTiles(group="Base") %>% 
      addCircleMarkers(data = df, lat = ~lat, lng = ~long, 
                       
                       radius = ~log(rides+10)*1.25,
                       
                       popup = paste("<center><strong>" ,df$stationname, "</strong>", "<br>",
                                     df$line, "<br>",
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
    
    return(map)
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
