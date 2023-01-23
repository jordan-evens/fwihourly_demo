#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(shinyjs)

library(xml2)
library(jsonlite)
library(anytime)
library(stringr)
library(rvest)
library(DT)
library(lubridate)

makePlot <- function(id) {
    withSpinner(tagList(plotOutput(id)))
}

urlServer <- "https://ws.lioservices.lrc.gov.on.ca/arcgis1061a/rest/services/MNRF/Ontario_Fires_Map/MapServer"
xml_server <- xml2::read_html(urlServer)
ref <- str_extract((html_nodes(xml_server, xpath=".//ul[contains(., 'Spatial Reference: ')]") %>% html_text())[1], "Spatial Reference: .*")
urlIds <- "https://ws.lioservices.lrc.gov.on.ca/arcgis1061a/rest/services/MNRF/Ontario_Fires_Map/MapServer/29/query?where=1%3D1&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=WEATHER_STATION_CODE&returnGeometry=false&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=true&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&f=pjson"

json_stns <- jsonlite::fromJSON(urlIds)

stns <- sort(json_stns$features[[1]][[1]])
stns <- stns[grepl("^[A-Z]*$", stns)]
# stns <- c('ABL', 'BAK')
# Define UI for application that draws a histogram
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
    selectInput("station", "Station", stns),
    dateInput("since", "Since"),
    sliderInput("currentTime", "Current Time:",
                min=as.POSIXct(as_date(lubridate::now())),
                max=as.POSIXct(as_date(lubridate::now()) + hours(hour(lubridate::now()))),
                value=as.POSIXct(as_date(lubridate::now()) + hours(hour(lubridate::now()))),
                timeFormat='%Y-%m-%d %H:00',
                step=60 * 60),
    DT::dataTableOutput("startup"),
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
