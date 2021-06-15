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

library(suncalc)
library(lutz)

source('fwiHourly/hFWI.r')
source('diurnal/diurnal.R')

HOURLY_DATA <- list()
CALCULATED <- list()
ORIG_FORECAST <- list()
FORECAST <- list()
OLD_STN <- ''

cleanWeather <- function(wx)
{
    if (!isSequentialHours(wx))
    {
        # need to fix weather
        start <- min(wx$TIMESTAMP)
        end <- max(wx$TIMESTAMP)
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
        wx <- merge(h, wx, by=c('TIMESTAMP'), all=TRUE)
        # should have all the times we need but need to replace NA values in their data
        # fill missing PREC values with 0 so we don't add rain
        wx$PREC <- nafill(wx$PREC, fill=0)
        # just carry observations forward for now
        for (col in colnames(wx))
        {
            if (typeof(wx[[col]]) != 'double' && typeof(wx[[col]]) != 'integer')
            {
                const <- na.omit(unique(wx[[col]]))[1]
                wx[[col]] <- rep(const)
            }
            else
            {
                wx[[col]] <- nafill(wx[[col]], type='locf')
            }
        }
        wx[, DATE := as.character(as.Date(TIMESTAMP))]
        wx[, YR := year(TIMESTAMP)]
        wx[, MON := month(TIMESTAMP)]
        wx[, DAY := day(TIMESTAMP)]
        wx[, HR := hour(TIMESTAMP)]
        wx[, MINUTE := minute(TIMESTAMP)]
    }
    # fix stations that just don't have values for things
    if (typeof(wx$TEMP) == 'logical') {
        wx <- wx[, -c('TEMP')]
        wx[, TEMP := 21.1]
    }
    if (typeof(wx$RH) == 'logical') {
        wx <- wx[, -c('RH')]
        wx[, RH := 45]
    }
    if (typeof(wx$WS) == 'logical') {
        wx <- wx[, -c('WS')]
        wx[, WS := 13]
    }
    if (typeof(wx$PREC) == 'logical') {
        wx <- wx[, -c('PREC')]
        wx[, PREC := 0]
    }
    wx$TEMP <- nafill(wx$TEMP, fill=21.1)
    wx$RH <- nafill(wx$RH, fill=45)
    wx$WS <- nafill(wx$WS, fill=13)
    wx$PREC <- nafill(wx$PREC, fill=0)
    return(wx)
}

fixTimezone <- function(zone)
{
    if (nchar(zone) != 3) {
        stop(sprintf('Expected three letter acronym for time zone but got %s', zone))
    }
    # change to standard time if it's daylight time
    zone <- str_replace(zone, 'DT$', 'ST')
    # HACK: fix missing CST zone
    zone <- str_replace(zone, '^CST$', 'Etc/GMT+6')
    return(zone)
}

getHourly <- function(stn)
{
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
    lat <- df$LAT[[1]]
    long <- df$LONG[[1]]
    tz <- tz_lookup_coords(lat, long, method='accurate')
    print(sprintf('getHourly(): %f, %f => %s', lat, long, tz))
    # make sure we stick with standard time and not daylight time
    start <- lubridate::with_tz(df$TIMESTAMP[[1]], tz)
    print(start)
    zone <- fixTimezone(strftime(start, tz=tz, format='%Z'))
    print(sprintf('%s => %s', tz, zone))
    df$TIMEZONE <- zone
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
    return(as.data.table(df))
}

getAFFESForecasts <- function()
{
    base_url <- 'http://www.affes.mnr.gov.on.ca/extranet/bulletin_boards/WXProducts/CFS/'
    files <- c('DFOSS_Day1_NWR.txt', 'DFOSS_Day1_NER.txt',
               'DFOSS_Day2_NWR.txt', 'DFOSS_Day2_NER.txt', 
               'DFOSS_Day3.18z.txt', 'DFOSS_Day4.18z.txt', 'DFOSS_Day5.18z.txt')
    data <- NULL
    last_created <- NULL
    for (f in files)
    {
        url <- sprintf('%s%s', base_url, f)
        print(url)
        this_file <- read.csv(url, header=FALSE)
        colnames(this_file) <- c('ID', 'PREC_INTERVAL', 'DATE', 'HR', 'UNK1', 'CREATED', 'UNK2', 'TEMP', 'RH', 'WD', 'WS', 'PREC')
        if (is.null(last_created) || unique(this_file$CREATED)[[1]] >= last_created)
        {
            data <- rbind(data, this_file)
            last_created <- unique(this_file$CREATED)[[1]]
        }
    }
    data <- data.table(data)
    data <- data[!is.na(CREATED)]
    # FIX: warning about wrapping with as.POSIXct
    data[, DATE := strptime(DATE, format='%Y%m%d')]
    data[, YR := year(DATE)]
    data[, MON := month(DATE)]
    data[, DAY := day(DATE)]
    data$HR <- 12
    data$MINUTE <- 0
    data[, TIMESTAMP := as_datetime(sprintf('%04d-%02d-%02d %02d:%02d:00', YR, MON, DAY, HR, MINUTE))]
}

getAFFESForecast <- function(stn)
{
    if (!exists("affes"))
    {
        print('Getting AFFES forecast')
        affes <<- getAFFESForecasts()
    }
    return(affes[ID == stn])
}

findQ <- function(TEMP, RH)
{
    # find absolute humidity
    svp <- 6.108 * exp(17.27 * TEMP / (TEMP + 237.3))
    vp <- svp * RH / 100.0
    return(217 * vp / (273.17 + TEMP))
}

findRH <- function(q, TEMP)
{
    cur_vp <- (273.17 + TEMP) * q / 217
    return(100 * cur_vp / (6.108 * exp(17.27 * TEMP / (TEMP + 237.3))))
}

findRHFixed <- function(q, TEMP)
{
    return(pmin(100, pmax(0, findRH(q, TEMP))))
}

toMinMax <- function(forecast)
{
    minMax <- copy(forecast)
    minMax[, Q := findQ(TEMP, RH)]
    minMax[, `:=`(TEMP_MAX = TEMP + 2.1,
              TEMP_MIN = TEMP - 14.8,
              WS_MAX = WS * 1.25,
              WS_MIN = WS * 0.15)]
    minMax[, `:=`(RH_OPP = 1.0 - RH / 100.0,
              RH_OPP_MAX = 1.0 - findRHFixed(Q, TEMP_MAX) / 100.0,
              RH_OPP_MIN = 1.0 - findRHFixed(Q, TEMP_MIN) / 100.0)]
    return(minMax)
}

doForecast <- function(minMax)
{
    df <- getWx(minMax)
    row_temp <- list(c_alpha=0.03, c_beta=2.14, c_gamma=-2.97)
    row_WS <- list(c_alpha=1.21, c_beta=1.50, c_gamma=-2.28)
    row_RH <- list(c_alpha=0.39, c_beta=2.07, c_gamma=-3.50)
    intervals <- 1
    df[, HOUR := HR]
    df[, APCP := PREC]
    df[, RH_MAX := 100 * (1.0 - RH_OPP_MIN)]
    df[, RH_MIN := 100 * (1.0 - RH_OPP_MAX)]
    df[, RAIN0000 := 0]
    df[, RAIN0600 := PREC]
    df[, RAIN1200 := 0]
    df[, RAIN1800 := 0]
    pred <- doPrediction(df, row_temp, row_WS, intervals=intervals, row_RH=row_RH)
    return(pred)
}

renderPlots <- function(input, output, session)
{
    COLS <- c( 'ID', 'LAT', 'LONG', 'TIMESTAMP', 'TEMP', 'RH', 'WS', 'PREC')
    stn <- input$station
    #print(stn)
    if (is.null(CALCULATED[[stn]]))
    {
        print(sprintf('Calculating for %s', stn))
        if (is.null(HOURLY_DATA[[stn]]))
        {
            print(sprintf('Getting hourly data for %s', stn))
            hourly <- getHourly(stn)
            HOURLY_DATA[[stn]] <<- cleanWeather(hourly)
        }
        hourly <- HOURLY_DATA[[stn]]
        wx <- hourly[, ..COLS]
        wx[, TYPE := 'OBS']
        lat <- hourly$LAT[[1]]
        long <- hourly$LONG[[1]]
        tz <- hourly$TIMEZONE[[1]]
        print(sprintf("%f, %f => %s", lat, long, tz))
        forecast <- getAFFESForecast(stn)
        if (nrow(forecast) > 0)
        {
            minMax <- toMinMax(forecast)
            minMax[, LAT := lat]
            minMax[, LONG := long]
            minMax[, TIMEZONE := tz]
            minMax$HOUR <- minMax$HR
            minMax$DATE <- as.character(minMax$DATE)
            fcst <- doForecast(minMax)
            fcst[, TIMESTAMP := as.character(TIMESTAMP)]
            fcst[, TIMESTAMP := as.POSIXct(TIMESTAMP, tz=hourly$TIMEZONE[[1]])]
            #fcst <- fcst[TIMESTAMP > max(as.Date(wx$TIMESTAMP, tz=tz)),]
            fcst[, `:=`(LAT = wx$LAT[[1]],
                     LONG = wx$LONG[[1]])]
            setnames(fcst,
                     c('P_TEMP', 'P_RH', 'P_WS', 'P_PREC'),
                     c('TEMP', 'RH', 'WS', 'PREC'))
            fcst <- fcst[, ..COLS]
            fcst[, TYPE := 'FCST']
            FORECAST[[stn]] <<- fcst
        }
        f <- FORECAST[[stn]]
        w <- rbind(wx[TIMESTAMP < min(f$TIMESTAMP)], f)
        w[, `:=`(YR = year(TIMESTAMP),
                 MON = month(TIMESTAMP),
                 DAY = day(TIMESTAMP),
                 HR = hour(TIMESTAMP),
                 MINUTE = minute(TIMESTAMP))]
        x <- hFWI(w)
        daily <- fwi(toDaily(w))
        setnames(daily,
                 c('FFMC', 'DMC', 'DC', 'ISI', 'BUI', 'FWI', 'DSR'),
                 c('DFFMC', 'DDMC', 'DDC', 'DISI', 'DBUI', 'DFWI', 'DDSR'))
        daily <- daily[, c('YR', 'MON', 'DAY', 'DFFMC', 'DDMC', 'DDC', 'DISI', 'DBUI', 'DFWI', 'DDSR')]
        daily[, HR := 17]
        x <- merge(x,
                   daily,
                   by=c('YR', 'MON', 'DAY', 'HR'),
                   all.x=TRUE)
        ORIG_FORECAST[[stn]] <<- x
        w <- rbind(wx, f[TIMESTAMP > max(wx$TIMESTAMP)])
        w[, `:=`(YR = year(TIMESTAMP),
                 MON = month(TIMESTAMP),
                 DAY = day(TIMESTAMP),
                 HR = hour(TIMESTAMP),
                 MINUTE = minute(TIMESTAMP))]
        x <- hFWI(w)
        daily <- fwi(toDaily(w))
        setnames(daily,
                 c('FFMC', 'DMC', 'DC', 'ISI', 'BUI', 'FWI', 'DSR'),
                 c('DFFMC', 'DDMC', 'DDC', 'DISI', 'DBUI', 'DFWI', 'DDSR'))
        daily <- daily[, c('YR', 'MON', 'DAY', 'DFFMC', 'DDMC', 'DDC', 'DISI', 'DBUI', 'DFWI', 'DDSR')]
        daily[, HR := 17]
        x <- merge(x,
                   daily,
                   by=c('YR', 'MON', 'DAY', 'HR'),
                   all.x=TRUE)
        CALCULATED[[stn]] <<- x
    }
    hourly <- HOURLY_DATA[[stn]]
    forecasted <- ORIG_FORECAST[[stn]]
    actual <- CALCULATED[[stn]]
    max_reading <- max(HOURLY_DATA[[stn]]$TIMESTAMP)
    min_reading <- min(HOURLY_DATA[[stn]]$TIMESTAMP)
    tz <- hourly$TIMEZONE[[1]]
    # HACK: convert to character to get tz to work
    since <- as.POSIXct(as.character(as.Date(max_reading) - days(1)), tz=tz)
    if (OLD_STN != stn || is.null(input$since) || since < as.POSIXct(input$since))
    {
        updateDateInput(session, "since", value=(since), max=(as.Date(max_reading) - days(1)), min=(min_reading))
    }
    OLD_STN <<- stn
    last_day <- c(as.POSIXct(as.character(input$since), tz=tz), as.POSIXct(as.Date(max(actual$TIMESTAMP))))
    #print(x)
    
    plotIndex <- function(index, colour)
    {
        return(renderPlot({
            ggplot(NULL, aes(x=TIMESTAMP)) +
                geom_point(data=actual[TYPE == 'OBS'], shape=16, aes(y=get(index)), colour=colour) +
                geom_line(data=actual[TYPE == 'FCST'], aes(y=get(index)), linetype=5, colour=colour) +
                geom_line(data=forecasted[TYPE == 'FCST'], aes(y=get(index)), linetype=3, colour=colour) +
                coord_cartesian(xlim=last_day) +
                ylab(index)
        }))
    }
    
    plotDaily <- function(index)
    {
        renderPlot({
            ggplot(NULL, aes(x=TIMESTAMP)) +
                geom_point(data=actual[TYPE == 'OBS'], shape=16, aes(y=get(sprintf('D%s', index))), colour='black', na.rm=TRUE) +
                geom_point(data=actual[TYPE == 'OBS'], shape=16, aes(y=get(index)), colour='red') +
                geom_point(data=actual[TYPE == 'FCST'], shape=1, aes(y=get(sprintf('D%s', index))), colour='black', na.rm=TRUE) +
                geom_line(data=actual[TYPE == 'FCST'], aes(y=get(index)), linetype=5, colour='red') +
                geom_point(data=forecasted[TYPE == 'FCST'], shape=8, aes(y=get(sprintf('D%s', index))), colour='black', na.rm=TRUE) +
                geom_line(data=forecasted[TYPE == 'FCST'], aes(y=get(index)), linetype=3, colour='red') +
                coord_cartesian(xlim=last_day) +
                ylab(index)
        })
    }

    output$tempPlot <- plotIndex('TEMP', 'red')
    output$rhPlot <- plotIndex('RH', 'blue')
    output$wsPlot <- plotIndex('WS', 'green')
    output$precPlot <- plotIndex('PREC', 'blue')
    output$ffmcPlot <- plotDaily('FFMC')
    output$dmcPlot <- plotDaily('DMC')
    output$dcPlot <- plotDaily('DC')
    output$isiPlot <- plotDaily('ISI')
    output$buiPlot <- plotDaily('BUI')
    output$fwiPlot <- plotDaily('FWI')
}
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    observeEvent(input$station, renderPlots(input, output, session))
    observeEvent(input$since, renderPlots(input, output, session))
})
