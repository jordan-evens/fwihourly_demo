#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(sf)
library(data.table)
library(fasttime)
library(rvest)
library(ggplot2)
library(DT)

library(suncalc)
library(lutz)
library(stringr)

dir_root <- getwd()
setwd(paste0(dir_root, '/cffdrs-ng'))
source('make_minmax.r')
source('make_hourly.r')
source('NG_FWI.r')
source('old_cffdrs.r')
setwd(dir_root)

FLAG_RUN_DEMO <- TRUE
FAKE_TIME <- NULL
HOURLY_DATA <- list()
CALCULATED <- list()
ORIG_FORECAST <- list()
FORECAST <- list()
OLD_STN <- ''
OLD_TIME <- NULL

cleanWeather <- function(wx) {
    if (!isSequentialHours(wx)) {
        # need to fix weather
        start <- min(wx$TIMESTAMP)
        end <- max(wx$TIMESTAMP)
        num_hours <- as.numeric((end - start), units='hours')
        # want to generate all missing hours and then interpolate from nearby readings
        h <- as.data.table(list(TIMESTAMP=start + hours(0:num_hours)))
        wx <- merge(h, wx, by=c('TIMESTAMP'), all=TRUE)
        # should have all the times we need but need to replace NA values in their data
        # fill missing PREC values with 0 so we don't add rain
        wx$PREC <- nafill(wx$PREC, fill=0)
        # just carry observations forward for now
        for (col in colnames(wx)) {
            if (typeof(wx[[col]]) != 'double' && typeof(wx[[col]]) != 'integer') {
                const <- na.omit(unique(wx[[col]]))[1]
                wx[[col]] <- rep(const)
            } else {
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
    urlStn <- sprintf("https://ws.lioservices.lrc.gov.on.ca/arcgis1061a/rest/services/MNRF/Ontario_Fires_Map/MapServer/29/query?where=WEATHER_STATION_CODE%%3D'%s'&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=false&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=true&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&f=pjson", stn)
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

getAFFESForecasts <- function() {
    base_url <- 'http://www.affes.mnr.gov.on.ca/extranet/bulletin_boards/WXProducts/CFS/'
    files <- c('DFOSS_Day1_NWR.txt', 'DFOSS_Day1_NER.txt',
               'DFOSS_Day2_NWR.txt', 'DFOSS_Day2_NER.txt', 
               'DFOSS_Day3.18z.txt', 'DFOSS_Day4.18z.txt', 'DFOSS_Day5.18z.txt')
    data <- NULL
    last_created <- NULL
    orig_date <- NULL
    for (f in files) {
        url <- sprintf('%s%s', base_url, f)
        print(url)
        this_file <- read.csv(url, header=FALSE)
        colnames(this_file) <- c('ID', 'PREC_INTERVAL', 'DATE', 'HR', 'UNK1', 'CREATED', 'UNK2', 'TEMP', 'RH', 'WD', 'WS', 'PREC')
        now <- unique(this_file$CREATED)[[1]]
        # FIX: HACK: just force sequential dates for now to get demo running
        if (FLAG_RUN_DEMO) {
            if (is.null(orig_date)) {
                orig_date <- as_date(as_datetime(as.character(this_file$DATE[1]), format='%Y%m%d')) - days(1)
            }
            cur_date <- orig_date + days(as.character(substr(f, 10, 10)))
            print(cur_date)
            this_file$DATE <- as.integer(format(cur_date, '%Y%m%d'))
            data <- rbind(data, this_file)
        } else if (is.null(last_created) || now >= last_created) {
            data <- rbind(data, this_file)
            last_created <- now
        }
    }
    data <- data.table(data)
    if (!FLAG_RUN_DEMO) {
        data <- data[!is.na(CREATED)]
    }
    data[, DATE := as_date(as_datetime(as.character(DATE), format='%Y%m%d'))]
    data[, YR := year(DATE)]
    data[, MON := month(DATE)]
    data[, DAY := day(DATE)]
    data$HR <- 12
    data$MINUTE <- 0
    data[, TIMESTAMP := as_datetime(sprintf('%04d-%02d-%02d %02d:%02d:00', YR, MON, DAY, HR, MINUTE))]
    return(data)
}

getAFFESForecast <- function(stn, tz) {
    if (!exists("affes")) {
        print('Getting AFFES forecast')
        affes <<- getAFFESForecasts()
    }
    fcst <- affes[ID == stn]
    fcst$TIMESTAMP <- lubridate::force_tz(fcst$TIMESTAMP, tzone=tz)
    return(fcst)
}

getDFOSS <- function(stn) {
    urlStn <- paste0('https://ws.lioservices.lrc.gov.on.ca/arcgis1061a/rest/services/MNRF/Ontario_Fires_Map/MapServer/30/query?where=WEATHER_STATION_CODE+%3D+%27',
                  stn,
                  '%27+AND+DFOSS_WEATHER_TYPE%3D%27PM%27&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=false&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&f=pjson')
    json_cur <- jsonlite::fromJSON(urlStn, flatten=TRUE)
    df <- data.frame(json_cur$features)
    names(df) <- gsub(names(df), pattern='attributes.', replacement='')
    df$RAINFALL[is.na(df$RAINFALL)] <- 0
    df$ID <- df$WEATHER_STATION_CODE
    if (is.null(df$ID)) {
        return(NULL)
    }
    # find proper time zone
    lat <- df$LAT[[1]]
    long <- df$LONG[[1]]
    tz <- tz_lookup_coords(lat, long, method='accurate')
    print(sprintf('getDFOSS(): %f, %f => %s', lat, long, tz))
    # make sure we stick with standard time and not daylight time
    df$TIMESTAMP <- lubridate::force_tz(lubridate::as_datetime(df$DFOSS_WEATHER_DATE / 1000), tzone=tz)
    start <- as.POSIXct(df$TIMESTAMP[[1]], tz=tz)
    print(start)
    zone <- fixTimezone(strftime(start, tz=tz, format='%Z'))
    print(sprintf('%s => %s', tz, zone))
    df$TIMEZONE <- zone
    df$TIMESTAMP <- lubridate::force_tz(df$TIMESTAMP, zone) + hours(12)
    df$PREC <- as.double(df$RAINFALL)
    df$WS <- df$ADJWINDSPEED
    df$LAT <- df$LATITUDE
    df$LONG <- df$LONGITUDE
    df <- df[, c('ID', 'LAT', 'LONG', 'TIMESTAMP', 'TYPE', 'TEMP', 'RH',  'WS', 'PREC', 'FFMC', 'DMC', 'DC', 'ISI', 'BUI', 'FWI')]
    df$DATE <- as.character(date(df$TIMESTAMP))
    df$YR <- year(df$TIMESTAMP)
    df$MON <- month(df$TIMESTAMP)
    df$DAY <- day(df$TIMESTAMP)
    df$HR <- hour(df$TIMESTAMP)
    df$MINUTE <- minute(df$TIMESTAMP)
    return(as.data.table(df))
}

findQ <- function(TEMP, RH) {
    # find absolute humidity
    svp <- 6.108 * exp(17.27 * TEMP / (TEMP + 237.3))
    vp <- svp * RH / 100.0
    return(217 * vp / (273.17 + TEMP))
}

findRH <- function(q, TEMP) {
    cur_vp <- (273.17 + TEMP) * q / 217
    return(100 * cur_vp / (6.108 * exp(17.27 * TEMP / (TEMP + 237.3))))
}

findRHFixed <- function(q, TEMP) {
    return(pmin(100, pmax(0, findRH(q, TEMP))))
}

toMinMax <- function(forecast) {
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

doForecast <- function(minMax) {
    df <- getWx(minMax)
    print('Got weather')
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
    print('Doing prediction')
    pred <- doPrediction(df, row_temp, row_WS, intervals=intervals, row_RH=row_RH)
    return(pred)
}

renderPlots <- function(input, output, session) {
    COLS <- c( 'ID', 'LAT', 'LONG', 'TIMESTAMP', 'TEMP', 'RH', 'WS', 'PREC')
    stn <- input$station
    #print(stn)
    dfoss <- getDFOSS(stn)
    if (is.null(CALCULATED[[stn]]) || OLD_TIME != input$currentTime)
    {
        OLD_TIME <<- input$currentTime
        print(sprintf('Calculating for %s', stn))
        if (is.null(HOURLY_DATA[[stn]])) {
            print(sprintf('Getting hourly data for %s', stn))
            hourly <- getHourly(stn)
            HOURLY_DATA[[stn]] <<- cleanWeather(hourly)
            # HOURLY_DATA[[stn]] <- cleanWeather(hourly)
        }
        hourly <- HOURLY_DATA[[stn]]
        lat <- hourly$LAT[[1]]
        long <- hourly$LONG[[1]]
        tz <- hourly$TIMEZONE[[1]]
        print(tz)
        timezone <- tz_offset(hourly$TIMESTAMP[1], tz=tz)$utc_offset_h
        print(timezone)
        print(sprintf("%f, %f => %s", lat, long, tz))
        forecast <- getAFFESForecast(stn, tz)
        print('Got AFFES forecast')
        if (FLAG_RUN_DEMO) {
            if (is.null(FAKE_TIME)) {
                print("Setting current time")
                print(forecast$TIMESTAMP[1])
                updateSliderInput(inputId='currentTime',
                                  value=as.POSIXct(min(as_date(forecast$TIMESTAMP)) + hours(15), tz=tz),
                                  min=as.POSIXct(min(as_date(forecast$TIMESTAMP)), tz=tz),
                                  max=as.POSIXct(max(as_date(forecast$TIMESTAMP)) + hours(24), tz=tz),
                                  timezone=tz)
                # pretend we're partway through day 1
                FAKE_TIME <<- input$currentTime
            }
        }
        FAKE_TIME <<- input$currentTime
        hourly <- hourly[TIMESTAMP <= FAKE_TIME, ]
        wx <- hourly[, ..COLS]
        wx[, TYPE := 'OBS']
        if (nrow(forecast) > 0) {
            print('Getting min/max')
            fcst_affes <- copy(forecast)
            colnames(fcst_affes) <- tolower(colnames(fcst_affes))
            setnames(fcst_affes, c('ws', 'yr', 'hr', 'prec'), c('wind', 'year', 'hour', 'rain'))
            fcst_affes$lat <- lat
            fcst_affes$long <- long
            minMax <- daily_to_minmax(fcst_affes)
            # colnames(minMax) <- toupper(colnames(minMax))
            #minMax <- toMinMax(forecast)
            #minMax[, LAT := lat]
            #minMax[, LONG := long]
            # minMax[, TIMEZONE := tz]
            # minMax$HOUR <- minMax$HR
            # minMax$DATE <- as.character(minMax$DATE)
            fcst <- minmax_to_hourly(minMax, timezone)
            print('Got min/max')
            #fcst <- doForecast(minMax)
            setnames(fcst, c('year', 'hour', 'wind', 'rain'), c('yr', 'hr', 'ws', 'prec'))
            #fcst <- fwi(fcst_hourly)
            colnames(fcst) <- toupper(colnames(fcst))
            # fcst[, TIMESTAMP := as.character(TIMESTAMP)]
            # fcst[, TIMESTAMP := as.POSIXct(TIMESTAMP, tz=hourly$TIMEZONE[[1]])]
            # #fcst <- fcst[TIMESTAMP > max(as.Date(wx$TIMESTAMP, tz=tz)),]
            # fcst[, `:=`(LAT = wx$LAT[[1]],
            #          LONG = wx$LONG[[1]])]
            # setnames(fcst,
            #          c('P_TEMP', 'P_RH', 'P_WS', 'P_PREC'),
            #          c('TEMP', 'RH', 'WS', 'PREC'))
            fcst[, TIMESTAMP := make_datetime(YR, MON, DAY, HR, tz=tz)]
            fcst <- fcst[, ..COLS]
            fcst[, TYPE := 'FCST']
            # FORECAST[[stn]] <- fcst
            FORECAST[[stn]] <<- fcst
            print('Got hourly forecast')
        }
        startup <- dfoss[1]
        if (is.null(startup)) {
            # HACK: if no startup data found
            startup <- data.frame(list(FFMC=FFMC_DEFAULT,
                                       DMC=DMC_DEFAULT,
                                       DC=DC_DEFAULT,
                                       TIMESTAMP=as.Date(min(fcst$TIMESTAMP) - days(1), tzone=tz)))
        }
        init <- data.frame(ffmc=as.double(startup$FFMC),
                           dmc=as.double(startup$DMC),
                           dc=as.double(startup$DC),
                           lat=lat[[1]])
        output$startup <- DT::renderDT(startup,
                                       options=list(dom='t'),
                                       rownames=FALSE)
        f <- copy(FORECAST[[stn]])
        f <- f[TIMESTAMP >= (lubridate::force_tz(as.Date(min(TIMESTAMP)), tzone=tz) + hours(min(8, hour(wx[, max(TIMESTAMP)])))),]
        # w <- rbind(wx[TIMESTAMP < min(f$TIMESTAMP)], f)
        w <- rbind(wx[TIMESTAMP < min(f$TIMESTAMP)], f)
        w[, `:=`(DATE = as.character(as.Date(TIMESTAMP, tz=tz)),
                 YR = year(TIMESTAMP),
                 MON = month(TIMESTAMP),
                 DAY = day(TIMESTAMP),
                 HR = hour(TIMESTAMP),
                 MINUTE = minute(TIMESTAMP))]
        active <- w[DATE > as.Date(startup$TIMESTAMP, tz=tz),]
        inactive <- w[DATE <= as.Date(startup$TIMESTAMP, tz=tz),]
        print(active)
        x <- hFWI(active, timezone=timezone, ffmc_old=init$ffmc, dmc_old=init$dmc, dc_old=init$dc)
        x <- rbind(inactive, x, fill=TRUE)
        x$STREAM <- 'Original'
        x$FREQUENCY <- 'Hourly'
        df <- x
        daily_past <- toDaily(active)
        daily_forecast_orig <- forecast[, c('DATE', 'ID', 'TIMESTAMP', 'TEMP', 'RH', 'WS', 'PREC', 'YR', 'MON', 'DAY', 'HR', 'MINUTE')]
        daily_forecast_orig$TIMESTAMP <- lubridate::force_tz(daily_forecast_orig$TIMESTAMP, tz)
        daily_forecast_orig$DATE <- as.character(daily_forecast_orig$DATE)
        daily_forecast_orig$LAT <- lat[[1]]
        daily_forecast_orig$LONG <- long[[1]]
        daily_forecast_orig$TYPE <- 'FCST'
        daily_orig <- rbind(daily_past[DATE < min(daily_forecast_orig$DATE)], daily_forecast_orig)
        daily_orig <- fwi(daily_orig, init=init)
        daily <- rbind(toDaily(inactive), daily_orig, fill=TRUE)
        daily$STREAM <- 'Original'
        daily$FREQUENCY <- 'Daily'
        #setnames(daily,
        #         c('FFMC', 'DMC', 'DC', 'ISI', 'BUI', 'FWI', 'DSR'),
        #         c('DFFMC', 'DDMC', 'DDC', 'DISI', 'DBUI', 'DFWI', 'DDSR'))
        #daily <- daily[, c('YR', 'MON', 'DAY', 'DFFMC', 'DDMC', 'DDC', 'DISI', 'DBUI', 'DFWI', 'DDSR')]
        # FIX: put indices at 1700 and weather at 1200
        #daily[, HR := 17]
        #x <- merge(x,
        #           daily,
        #           by=c('YR', 'MON', 'DAY', 'HR'),
        #           all.x=TRUE)
        print("Combining with daily")
        # HACK: FIX: for now just throw out extra columns
        keep_cols <- colnames(daily)
        df <- df[, ..keep_cols]
        df <- rbind(df, daily)
        # ORIG_FORECAST[[stn]] <- df
        ORIG_FORECAST[[stn]] <<- df
        f <- copy(FORECAST[[stn]])
        # HACK: FIX: for now just throw out extra columns
        keep_cols <- colnames(f)
        wx <- wx[, ..keep_cols]
        w <- rbind(wx, f[TIMESTAMP > max(wx$TIMESTAMP)])
        w[, `:=`(DATE = as.character(as.Date(TIMESTAMP, tz=tz)),
                 YR = year(TIMESTAMP),
                 MON = month(TIMESTAMP),
                 DAY = day(TIMESTAMP),
                 HR = hour(TIMESTAMP),
                 MINUTE = minute(TIMESTAMP))]
        # HACK: keep to forecast period
        if (FLAG_RUN_DEMO) {
            w <- w[TIMESTAMP <= max(fcst$TIMESTAMP)]
        }
        active <- w[DATE > as.Date(startup$TIMESTAMP, tz=tz),]
        inactive <- w[DATE <= as.Date(startup$TIMESTAMP, tz=tz),]
        x <- hFWI(active, timezone=timezone, ffmc_old=init$ffmc, dmc_old=init$dmc, dc_old=init$dc)
        x <- rbind(inactive, x, fill=TRUE)
        x$STREAM <- 'Revised'
        x$FREQUENCY <- 'Hourly'
        df <- x
        daily_past <- toDaily(active)
        daily_forecast_orig <- forecast[, c('DATE', 'ID', 'TIMESTAMP', 'TEMP', 'RH', 'WS', 'PREC', 'YR', 'MON', 'DAY', 'HR', 'MINUTE')]
        daily_forecast_orig$TIMESTAMP <- lubridate::force_tz(daily_forecast_orig$TIMESTAMP, tz)
        daily_forecast_orig$DATE <- as.character(daily_forecast_orig$DATE)
        daily_forecast_orig$LAT <- lat[[1]]
        daily_forecast_orig$LONG <- long[[1]]
        daily_forecast_orig$TYPE <- 'FCST'
        daily_orig <- rbind(daily_past, daily_forecast_orig[DATE > max(daily_past$DATE)])
        daily_orig <- fwi(daily_orig, init=init)
        daily <- rbind(toDaily(inactive), daily_orig, fill=TRUE)
        daily$STREAM <- 'Revised'
        daily$FREQUENCY <- 'Daily'
        print("Combining with revised")
        # HACK: FIX: for now just throw out extra columns
        keep_cols <- colnames(daily)
        df <- df[, ..keep_cols]
        df <- rbind(df, daily)
        # CALCULATED[[stn]] <- df
        CALCULATED[[stn]] <<- df
    }
    hourly <- HOURLY_DATA[[stn]]
    forecasted <- ORIG_FORECAST[[stn]]
    actual <- CALCULATED[[stn]]
    fcst <- FORECAST[[stn]]
    if (FLAG_RUN_DEMO) {
        max_reading <- min(max(fcst$TIMESTAMP), max(HOURLY_DATA[[stn]]$TIMESTAMP))
    } else {
        max_reading <- max(HOURLY_DATA[[stn]]$TIMESTAMP)
    }
    min_reading <- min(HOURLY_DATA[[stn]]$TIMESTAMP)
    tz <- hourly$TIMEZONE[[1]]
    # HACK: convert to character to get tz to work
    since <- as.POSIXct(as.character(as.Date(max_reading) - days(1)), tz=tz)
    if (as.POSIXct(input$since) > max_reading || OLD_STN != stn || is.null(input$since) || since < as.POSIXct(input$since)) {
    # if (OLD_STN != stn || is.null(input$since) || since < as.POSIXct(input$since)) {
        updateDateInput(session, "since", value=(since), max=(as.Date(max_reading) - days(1)), min=(min_reading))
    }
    OLD_STN <<- stn
    # if (FLAG_RUN_DEMO) {
    #     # last_day <- c(as_date(min(max(actual$TIMESTAMP), as.POSIXct(as.character(input$since), tz=tz))), max(actual$TIMESTAMP))
    #     last_day <- c(as.POSIXct(as.Date(min(forecast$TIMESTAMP))), max(actual$TIMESTAMP))
    # } else {
    last_day <- c(min(min(fcst$TIMESTAMP), as.POSIXct(as.character(input$since), tz=tz)), max(actual$TIMESTAMP))
        #last_day <- c(as.POSIXct(as.character(input$since), tz=tz), as.POSIXct(as.Date(max(actual$TIMESTAMP))))
    # }
    #print(x)
    print(last_day)

    SIZE <- list(line=0.75, point=2, daily=3)
    print("Combining forecasted and actual")
    df <- rbind(forecasted, actual)
    if (!is.null(dfoss)) {
        dfoss$TYPE <- 'OBS'
        dfoss[, DSR := .dsrCalc(FWI)]
        dfoss$STREAM <- 'DFOSS'
        dfoss$FREQUENCY <- 'Daily'
        df <- rbind(df, dfoss)
    }
    
    # convert to factors for plotting
    df$TYPE = as.factor(df$TYPE)
    df$STREAM = as.factor(df$STREAM)
    df$FREQUENCY = ordered(df$FREQUENCY)
    df$TIMESTAMP <- lubridate::force_tz(df$TIMESTAMP, tz)
    shape_names <- list('OBS_DFOSS'='DFOSS',
                        'OBS_Revised'='Observed',
                        'OBS_Original'='Observed',
                        'FCST_DFOSS'='DFOSS',
                        'FCST_Revised'='Revised Forecast',
                        'FCST_Original'='Original Forecast')
    df$SOURCE <- as.factor(as.vector(unlist(shape_names[df[, paste0(TYPE, '_', STREAM)]])))
    shape_options <- list('DFOSS'=15, 'Observed'=16, 'Revised Forecast'=1, 'Original Forecast'=8)

    print('Defining plot functions')
    plotIndex <- function(index, colour='red') {
        return(renderPlot({
            g <- ggplot(data=df[!is.na(get(index))], aes(x=TIMESTAMP)) +
                geom_point(data=df[!is.na(get(index)) & (FREQUENCY!='Hourly' | TYPE=='OBS')],
                           aes(y=get(index), shape=SOURCE, size=FREQUENCY, colour=FREQUENCY), na.rm=TRUE) +
                scale_shape_manual(values=shape_options) +
                scale_size_manual(values=c('Daily'=SIZE$daily, 'Hourly'=SIZE$point)) +
                scale_colour_manual(values=c('Daily'='black', 'Hourly'=colour)) +
                geom_line(data=df[!is.na(get(index)) & FREQUENCY!='Daily' & TYPE!='OBS'],
                          aes(y=get(index), linetype = STREAM), colour=colour, size=SIZE$line, na.rm=TRUE) +
                scale_linetype_manual(values=c('Revised'=5, 'Original'=3)) +
                xlim(last_day)
            return(g)
        }))
    }

    output$tempPlot <- plotIndex('TEMP', 'red')
    output$rhPlot <- plotIndex('RH', 'blue')
    output$wsPlot <- plotIndex('WS', 'green')
    output$precPlot <- plotIndex('PREC', 'blue')
    output$ffmcPlot <- plotIndex('FFMC')
    output$dmcPlot <- plotIndex('DMC')
    output$dcPlot <- plotIndex('DC')
    output$isiPlot <- plotIndex('ISI')
    output$buiPlot <- plotIndex('BUI')
    output$fwiPlot <- plotIndex('FWI')
    output$keep_alive <- renderText({
        req(input$alive_count)
        input$alive_count
    })
    print('Done')
}
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    observeEvent(input$station, renderPlots(input, output, session))
    observeEvent(input$since, renderPlots(input, output, session))
    observeEvent(input$currentTime, renderPlots(input, output, session))
})
