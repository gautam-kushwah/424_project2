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



# Define UI for application that draws a histogram

ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
