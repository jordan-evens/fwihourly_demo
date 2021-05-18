#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(cffdrs)
library(lubridate)
library(sf)
library(data.table)
library(fasttime)
library(rvest)
library(ggplot2)

#~ library('devtools')
#~ devtools::install_github('datastorm-open/suncalc')
library(suncalc)
library(lutz)

source('fwiHourly/hFWI.r')

renderPlots <- function(input, output)
{
    stn <- input$station
    #print(stn)
    urlStn <- sprintf("https://ws.lioservices.lrc.gov.on.ca/arcgis1061a/rest/services/MNRF/Ontario_Fires_Map/MapServer/14/query?where=WEATHER_STATION_CODE%%3D'%s'&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=false&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=true&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&f=pjson", stn)
    #print(urlStn)
    json_cur <- jsonlite::fromJSON(urlStn, flatten=TRUE)
    json_cur <- json_cur$features[order(json_cur$features$attributes.OBSERVATION_DATE),]
    json_cur$attributes.OBSERVATION_DATE <- lubridate::as_datetime(json_cur$attributes.OBSERVATION_DATE / 1000)
    all_stns <- json_cur
    df <- data.frame(all_stns)
    names(df) <- gsub(names(df), pattern='attributes.', replacement='')
    df$RAINFALL[is.na(df$RAINFALL)] <- 0
    df$ID <- df$WEATHER_STATION_CODE
    df$TIMESTAMP <- df$OBSERVATION_DATE
    # find proper time zone
    tz <- tz_lookup_coords(df$LAT[[1]], df$LONG[[1]], method='accurate')
    # make sure we stick with standard time and not daylight time
    start <- lubridate::with_tz(df$TIMESTAMP[[1]], tz)
    zone <- strftime(start, format='%Z')
    if (nchar(zone) != 3) {
        stop(sprintf('Expected three letter acronym for time zone but got %s', zone))
    }
    # change to standard time if it's daylight time
    zone <- str_replace(zone, 'DT$', 'ST')
    df$TIMESTAMP <- lubridate::with_tz(df$TIMESTAMP, zone)
    df$HR <- hour(df$TIMESTAMP)
    df$MINUTE <- minute(df$TIMESTAMP)
    df$DATE <- as.character(date(df$TIMESTAMP))
    df$PREC <- as.double(df$RAINFALL)
    df$YR <- year(df$TIMESTAMP)
    df$MON <- month(df$TIMESTAMP)
    df$DAY <- day(df$TIMESTAMP)
    df$WS <- df$ADJWINDSPEED
    df$LAT <- df$LATITUDE
    df$LONG <- df$LONGITUDE
    weatherstream <- as.data.table(df)
    
    if (!isSequentialHours(weatherstream))
    {
        # need to fix weather
        start <- min(weatherstream$TIMESTAMP)
        end <- max(weatherstream$TIMESTAMP)
        # want to generate all missing hours and then interpolate from nearby readings
        h <- c(start)
        cur <- start + hours(1)
        while (cur <= end)
        {
            h <- c(h, cur)
            cur <- cur + hours(1)
        }
        h <- as.data.table(h)
        colnames(h) <- c('TIMESTAMP')
        weatherstream <- merge(h, weatherstream, by=c('TIMESTAMP'), all=TRUE)
        # should have all the times we need but need to replace NA values in their data
        # fill missing PREC values with 0 so we don't add rain
        weatherstream$PREC <- nafill(weatherstream$PREC, fill=0)
        # just carry observations forward for now
        for (col in colnames(weatherstream))
        {
            if (typeof(weatherstream[[col]]) != 'double' && typeof(weatherstream[[col]]) != 'integer')
            {
                const <- na.omit(unique(weatherstream[[col]]))[1]
                weatherstream[[col]] <- rep(const)
            }
            else
            {
                weatherstream[[col]] <- nafill(weatherstream[[col]], type='locf')
            }
        }
        weatherstream[, DATE := as.character(as.Date(TIMESTAMP))]
        weatherstream[, YR := year(TIMESTAMP)]
        weatherstream[, MON := month(TIMESTAMP)]
        weatherstream[, DAY := day(TIMESTAMP)]
        weatherstream[, HR := hour(TIMESTAMP)]
        weatherstream[, MINUTE := minute(TIMESTAMP)]
    }
    # fix stations that just don't have values for things
    if (typeof(weatherstream$TEMP) == 'logical') {
        weatherstream <- weatherstream[, -c('TEMP')]
        weatherstream[, TEMP := 21.1]
    }
    if (typeof(weatherstream$RH) == 'logical') {
        weatherstream <- weatherstream[, -c('RH')]
        weatherstream[, RH := 45]
    }
    if (typeof(weatherstream$WS) == 'logical') {
        weatherstream <- weatherstream[, -c('WS')]
        weatherstream[, WS := 13]
    }
    if (typeof(weatherstream$PREC) == 'logical') {
        weatherstream <- weatherstream[, -c('PREC')]
        weatherstream[, PREC := 0]
    }
    weatherstream$TEMP <- nafill(weatherstream$TEMP, fill=21.1)
    weatherstream$RH <- nafill(weatherstream$RH, fill=45)
    weatherstream$WS <- nafill(weatherstream$WS, fill=13)
    weatherstream$PREC <- nafill(weatherstream$PREC, fill=0)
    
    x <- hFWI(weatherstream)
    daily <- fwi(toDaily(weatherstream))
    setnames(daily,
             c('FFMC', 'DMC', 'DC', 'ISI', 'BUI', 'FWI', 'DSR'),
             c('DFFMC', 'DDMC', 'DDC', 'DISI', 'DBUI', 'DFWI', 'DDSR'))
    daily <- daily[, c('YR', 'MON', 'DAY', 'DFFMC', 'DDMC', 'DDC', 'DISI', 'DBUI', 'DFWI', 'DDSR')]
    x <- merge(x,
               daily,
               by=c('YR', 'MON', 'DAY'))
    
    #print(x)

    output$tempPlot <- renderPlot({
        ggplot(x, aes(x=TIMESTAMP)) +
            geom_line(aes(y=TEMP), color='red')
    })
    output$rhPlot <- renderPlot({
        ggplot(x, aes(x=TIMESTAMP)) +
            geom_line(aes(y=RH), color='blue')
    })
    output$wsPlot <- renderPlot({
        ggplot(x, aes(x=TIMESTAMP)) +
            geom_line(aes(y=WS), color='green')
    })
    output$precPlot <- renderPlot({
        ggplot(x, aes(x=TIMESTAMP)) +
            geom_line(aes(y=PREC), color='blue')
    })
    output$ffmcPlot <- renderPlot({
        ggplot(x, aes(x=TIMESTAMP)) +
            geom_line(aes(y=FFMC), color='red') +
            geom_line(aes(y=DFFMC), color='black')
    })
    output$dmcPlot <- renderPlot({
        ggplot(x, aes(x=TIMESTAMP)) +
            geom_line(aes(y=DMC), color='red') +
            geom_line(aes(y=DDMC), color='black')
    })
    output$dcPlot <- renderPlot({
        ggplot(x, aes(x=TIMESTAMP)) +
            geom_line(aes(y=DC), color='red') +
            geom_line(aes(y=DDC), color='black')
    })
    output$isiPlot <- renderPlot({
        ggplot(x, aes(x=TIMESTAMP)) +
            geom_line(aes(y=ISI), color='red')+
            geom_line(aes(y=DISI), color='black')
    })
    output$buiPlot <- renderPlot({
        ggplot(x, aes(x=TIMESTAMP)) +
            geom_line(aes(y=BUI), color='red')+
            geom_line(aes(y=DBUI), color='black')
    })
    output$fwiPlot <- renderPlot({
        ggplot(x, aes(x=TIMESTAMP)) +
            geom_line(aes(y=FWI), color='red')+
            geom_line(aes(y=DFWI), color='black')
    })
}
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    observeEvent(input$station, renderPlots(input, output))
})
