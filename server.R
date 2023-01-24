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

source('common.R')

renderPlots <- function(input, output, session) {
    COLS <- c( 'ID', 'LAT', 'LONG', 'TIMESTAMP', 'TEMP', 'RH', 'WS', 'PREC')
    stn <- input$station
    cached_data <- cacheData(stn)
    dfoss <- cached_data$dfoss
    hourly <- cached_data$hourly
    forecast <- cached_data$forecast
    lat <- cached_data$lat
    long <- cached_data$long
    tz <- cached_data$tz
    timezone <- cached_data$timezone
    # if (is.null(OLD_TIME)) {
    #     OLD_TIME <<- input$currentTime
    # }
    if (is.null(CALCULATED[[stn]])
        || OLD_TIME != input$currentTime
        || OLD_STN != input$station
        || OLD_SINCE != input$since)
    {
        print(strrep('*', 50))
        print(dfoss)
        print(hourly)
        print(forecast)
        print(strrep('*', 50))
        # print('Got AFFES forecast')
        # if (FLAG_RUN_DEMO) {
        #     if (is.null(OLD_TIME)) {
        #         # pretend we're partway through day 1
        #         print("Setting current time")
        #         print(forecast$TIMESTAMP[1])
        #         updateSliderInput(inputId='currentTime',
        #                           value=as.POSIXct(min(as_date(forecast$TIMESTAMP)) + hours(15), tz=tz),
        #                           min=as.POSIXct(min(as_date(forecast$TIMESTAMP)), tz=tz),
        #                           max=as.POSIXct(max(as_date(forecast$TIMESTAMP)) + hours(24), tz=tz),
        #                           timezone=tz)
        #     }
        # }
        OLD_TIME <<- input$currentTime
        OLD_STN <<- input$station
        OLD_SINCE <<- input$since
        hourly <- hourly[TIMESTAMP <= OLD_TIME, ]
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
            fcst <- minmax_to_hourly(minMax, timezone)
            print('Got min/max')
            setnames(fcst, c('year', 'hour', 'wind', 'rain'), c('yr', 'hr', 'ws', 'prec'))
            colnames(fcst) <- toupper(colnames(fcst))
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

        # hourly <- HOURLY_DATA[[stn]]
        forecasted <- ORIG_FORECAST[[stn]]
        actual <- CALCULATED[[stn]]
        fcst <- FORECAST[[stn]]
        max_reading <- max(hourly$TIMESTAMP)
        min_reading <- min(hourly$TIMESTAMP)
        # tz <- hourly$TIMEZONE[[1]]
        # # HACK: convert to character to get tz to work
        # since <- as.POSIXct(as.character(as.Date(max_reading) - days(1)), tz=tz)
        since <- min(as.POSIXct(input$since, tz=tz), as.POSIXct(as.character(as.Date(max_reading) - days(1)), tz=tz))
        updateDateInput(session, "since", value=(since), max=(as.Date(max_reading) - days(1)), min=(min_reading))
        OLD_SINCE <<- since
        OLD_STN <<- stn
        last_day <- c(as.POSIXct(as.character(input$since), tz=tz), max(actual$TIMESTAMP))
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
                    ylab(index) +
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
    }
    print('Done')
}
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    observeEvent(input$station, renderPlots(input, output, session))
    observeEvent(input$since, renderPlots(input, output, session))
    observeEvent(input$currentTime, renderPlots(input, output, session))
})
