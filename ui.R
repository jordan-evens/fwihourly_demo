library(shiny)
library(shinycssloaders)
library(shinyjs)

library(xml2)
library(jsonlite)
library(stringr)
library(rvest)
library(DT)
library(lubridate)

source('common.R')

makePlot <- function(id) {
    withSpinner(tagList(plotOutput(id)))
}

urlServer <- "https://ws.lioservices.lrc.gov.on.ca/arcgis1061a/rest/services/MNRF/Ontario_Fires_Map/MapServer"
xml_server <- xml2::read_html(urlServer)
ref <- str_extract((html_nodes(xml_server, xpath=".//ul[contains(., 'Spatial Reference: ')]") %>% html_text())[1], "Spatial Reference: .*")
urlIds <- "https://ws.lioservices.lrc.gov.on.ca/arcgis1061a/rest/services/MNRF/Ontario_Fires_Map/MapServer/29/query?where=1%3D1&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=WEATHER_STATION_CODE&returnGeometry=false&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=true&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&f=pjson"

# jsonlite is giving ssl error
json_stns <- jsonlite::fromJSON(rawToChar(curl::curl_fetch_memory(urlIds)$content))

stns <- sort(json_stns$features[[1]][[1]])
stns <- stns[grepl("^[A-Z]*$", stns)]
# stns <- c('ABL', 'BAK')

time_min <- as.POSIXct(as_date(lubridate::now()))
time_max <- as.POSIXct(as_date(lubridate::now()) + hours(hour(lubridate::now())))
time_value <- as.POSIXct(as_date(lubridate::now()) + hours(hour(lubridate::now())))

# DEBUG
stn <- stns[1]
currentTime <- time_value
since <-as.Date(time_value - days(1))
# df <- calculateWeather(stn, currentTime, since)

shinyUI(fluidPage(
    useShinyjs(),
    tags$script(src = "keepalive.js"),
    tags$style(HTML("
        #keep_alive {
            visibility: hidden;
        }
    ")),
    textOutput("keep_alive"),
    # Application title
    titlePanel("Hourly Data"),
    fluidRow(
    column(6,
    selectInput("station", "Station", stns),
    dateInput("since", "Since", value=as.Date(time_value - days(1))),
    sliderInput("currentTime", "Current Time:",
                min=time_min,
                max=time_max,
                value=time_value,
                timeFormat='%Y-%m-%d %H:00',
                step=60 * 60)
    ),
    column(6,
    p('This is a demo of how the hourly FWI could work and update through the day.'),
    p('The "Station" input is the station to graph for. If no hourly data is available for a station at any time, the default FWI "drying day" weather values will be used.'),
    p('The "Since" input is the time to graph from.'),
    p('The "Current Time" input is the time to pretend it is currently.'),
    p('The table shows startup FWI values and date.')
    )),
    DT::dataTableOutput("startup"),
    makePlot("tempPlot"),
    makePlot("rhPlot"),
    makePlot("wsPlot"),
    makePlot("precPlot"),
    makePlot("ffmcPlot"),
    makePlot("dmcPlot"),
    makePlot("dcPlot"),
    makePlot("isiPlot"),
    makePlot("buiPlot"),
    makePlot("fwiPlot")
))
