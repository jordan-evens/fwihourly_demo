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
library(data.table)
library(parallel)
library(snow)
library(fasttime)
library(rvest)
library(ggplot2)

#~ library('devtools')
#~ devtools::install_github('datastorm-open/suncalc')
library(suncalc)

# FIX: figure out what this should be
DEFAULT_LATITUDE <- 55.0
DEFAULT_LONGITUDE <- -110.0

.dmcCalcPieces <- function(dmc_yda, temp, rh, prec, lat, mon, lat.adjust=TRUE) {
    #############################################################################
    # Description: Duff Moisture Code Calculation. All code
    #              is based on a C code library that was written by Canadian
    #              Forest Service Employees, which was originally based on
    #              the Fortran code listed in the reference below. All equations
    #              in this code refer to that document.
    #
    #              Equations and FORTRAN program for the Canadian Forest Fire 
    #              Weather Index System. 1985. Van Wagner, C.E.; Pickett, T.L. 
    #              Canadian Forestry Service, Petawawa National Forestry 
    #              Institute, Chalk River, Ontario. Forestry Technical Report 33. 
    #              18 p.
    #
    #              Additional reference on FWI system
    #
    #              Development and structure of the Canadian Forest Fire Weather 
    #              Index System. 1987. Van Wagner, C.E. Canadian Forestry Service,
    #              Headquarters, Ottawa. Forestry Technical Report 35. 35 p.
    #  
    #
    # Args:  dmc_yda:   The Duff Moisture Code from previous iteration
    #           temp:   Temperature (centigrade)
    #             rh:   Relative Humidity (%)
    #           prec:   Precipitation(mm)
    #            lat:   Latitude (decimal degrees)
    #            mon:   Month (1-12)
    #     lat.adjust:   Latitude adjustment (TRUE, FALSE, default=TRUE)
    #       
    #
    # Returns: list(DMC starting point after decrease,
    #               DMC increase during the day,
    #               DMC decrease from yesterday that resulted in starting point)
    #
    #############################################################################
    
    #Reference latitude for DMC day length adjustment
    #46N: Canadian standard, latitude >= 30N   (Van Wagner 1987)
    ell01 <- c(6.5, 7.5, 9, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 8, 7, 6)
    #20N: For 30 > latitude >= 10
    ell02 <- c(7.9, 8.4, 8.9, 9.5, 9.9, 10.2, 10.1, 9.7, 9.1,8.6, 8.1, 7.8)
    #20S: For -10 > latitude >= -30  
    ell03 <- c(10.1, 9.6, 9.1, 8.5, 8.1, 7.8, 7.9, 8.3, 8.9, 9.4, 9.9, 10.2)
    #40S: For -30 > latitude
    ell04 <- c(11.5, 10.5, 9.2, 7.9, 6.8, 6.2, 6.5, 7.4, 8.7, 10, 11.2, 11.8)
    #For latitude near the equator, we simple use a factor of 9 for all months
    
    #constrain low end of temperature
    temp <- ifelse(temp < (-1.1), -1.1, temp)
    #Eq. 16 - The log drying rate
    rk <- 1.894 * (temp + 1.1) * (100 - rh) * ell01[mon] * 1e-04
    #Adjust the day length  and thus the drying r, based on latitude and month
    if (lat.adjust) {
        rk <- ifelse(lat <= 30 & lat > 10, 1.894 * (temp + 1.1) * 
                         (100 - rh) * ell02[mon] * 1e-04, rk)
        rk <- ifelse(lat <= -10 & lat > -30, 1.894 * (temp + 1.1) * 
                         (100 - rh) * ell03[mon] * 1e-04, rk)
        rk <- ifelse(lat <= -30 & lat >= -90, 1.894 * (temp + 1.1) * 
                         (100 - rh) * ell04[mon] * 1e-04, rk)
        rk <- ifelse(lat <= 10 & lat > -10, 1.894 * (temp + 1.1) * 
                         (100 - rh) * 9 * 1e-04, rk)
    }
    ra <- prec
    #Eq. 11 - Net rain amount
    rw <- 0.92 * ra - 1.27
    #Alteration to Eq. 12 to calculate more accurately
    wmi <- 20 + 280/exp(0.023 * dmc_yda)
    #Eqs. 13a, 13b, 13c
    b <- ifelse(dmc_yda <= 33, 
                100/(0.5 + 0.3 * dmc_yda), 
                ifelse(dmc_yda <= 65, 
                       14 - 1.3 * log(dmc_yda), 
                       6.2 * log(dmc_yda) - 17.2))
    #Eq. 14 - Moisture content after rain
    wmr <- wmi + 1000 * rw/(48.77 + b * rw)
    op <- options(warn = (-1))
    #Alteration to Eq. 15 to calculate more accurately
    pr0 <- 43.43 * (5.6348 - log(wmr - 20))
    options(op)
    #Constrain P
    pr <- ifelse(prec <= 1.5, dmc_yda, pr0)
    pr <- ifelse(pr < 0, 0, pr)
    #Calculate final P (DMC)
    return(list(pr, rk, dmc_yda - pr))
}

.dcCalcPieces <- function(dc_yda, temp, rh, prec, lat, mon, lat.adjust=TRUE) {
    #############################################################################
    # Description: Drought Code Calculation. All code
    #              is based on a C code library that was written by Canadian
    #              Forest Service Employees, which was originally based on
    #              the Fortran code listed in the reference below. All equations
    #              in this code refer to that document.
    #
    #              Equations and FORTRAN program for the Canadian Forest Fire 
    #              Weather Index System. 1985. Van Wagner, C.E.; Pickett, T.L. 
    #              Canadian Forestry Service, Petawawa National Forestry 
    #              Institute, Chalk River, Ontario. Forestry Technical Report 33. 
    #              18 p.
    #
    #              Additional reference on FWI system
    #
    #              Development and structure of the Canadian Forest Fire Weather 
    #              Index System. 1987. Van Wagner, C.E. Canadian Forestry Service,
    #              Headquarters, Ottawa. Forestry Technical Report 35. 35 p.
    #  
    #
    # Args:   dc_yda:   The Drought Code from previous iteration
    #           temp:   Temperature (centigrade)
    #             rh:   Relative Humidity (%)
    #           prec:   Precipitation(mm)
    #            lat:   Latitude (decimal degrees)
    #            mon:   Month (1-12)
    #     lat.adjust:   Latitude adjustment (TRUE, FALSE, default=TRUE)
    #
    # Returns: list(DC starting point after decrease,
    #               DC increase during the day,
    #               DC decrease from yesterday that resulted in starting point)
    #
    #############################################################################
    #Day length factor for DC Calculations
    #20N: North of 20 degrees N
    fl01 <- c(-1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5, 2.4, 0.4, -1.6, -1.6)
    #20S: South of 20 degrees S
    fl02 <- c(6.4, 5, 2.4, 0.4, -1.6, -1.6, -1.6, -1.6, -1.6, 0.9, 3.8, 5.8)
    #Near the equator, we just use 1.4 for all months.
    #Constrain temperature
    temp <- ifelse(temp < (-2.8), -2.8, temp)
    
    #Eq. 22 - Potential Evapotranspiration
    pe <- (0.36 * (temp + 2.8) + fl01[mon]) / 2
    #Daylength factor adjustment by latitude for Potential Evapotranspiration
    if (lat.adjust) {
        pe <- ifelse(lat <= -20, (0.36 * (temp + 2.8) + fl02[mon]) / 2, pe)
        pe <- ifelse(lat > -20 & lat <= 20, (0.36 * (temp + 2.8) + 1.4)/2, pe)
    }
    # Cap potential evapotranspiration at 0 for negative winter DC values
    pe <- ifelse(pe < 0, 0, pe)
    ra <- prec
    #Eq. 18 - Effective Rainfall
    rw <- 0.83 * ra - 1.27
    #Eq. 19
    smi <- 800 * exp(-1 * dc_yda/400)
    #Alteration to Eq. 21
    dr0 <- dc_yda - 400 * log(1 + 3.937 * rw/smi)
    dr0 <- ifelse(dr0 < 0, 0, dr0)
    #if precip is less than 2.8 then use yesterday's DC
    dr <- ifelse(prec <= 2.8, dc_yda, dr0)
    return(list(dr, pe, dc_yda - dr))
}

toDecimal <- function(t){
    return(hour(t) + (minute(t) + (second(t) / 60.0)) / 60.0)
}

.stnHFWI <- function(w, intervals, ffmc_old, dmc_old, dc_ol)
{
    r <- NULL
    if (!(any(is.na(w$RH)) || any(is.na(w$WINDSPEED))))
    {
        times <- suncalc::getSunlightTimes(date(unique(w$DATE)), w$LAT[[1]], w$LONG[[1]])
        morning <- w[w$HR <= 12,]
        after <- w[w$HR > 12,]
        am <- morning[, sum(PREC, na.rm=TRUE), by=c('DATE')]
        setnames(am, 'V1', 'am')
        am$DATE <- as.character(am$DATE)
        pm <- after[, sum(PREC, na.rm=TRUE), by=c('DATE')]
        pm[, for_day := as.character(as.Date(DATE) + 1)]
        pm <- pm[, -c('DATE')]
        setnames(pm, 'V1', 'prev')
        setnames(pm, 'for_day', 'DATE')
        precip <- merge(am, pm)
        precip[, PREC := am + prev]
        daily <- NULL
        for (year in unique(w$YR))
        {
            merged <- merge(w[YR == year & HR == 12 & MINUTE == 0, -c('PREC')],
                            precip[,c('DATE', 'PREC')], by=c('DATE'))
            if (0 < nrow(merged))
            {
                merged$DATE <- as.character(date(merged$DATE))
                d <- fwi(merged)
                d[, DMC_YDA := data.table::shift(DMC, 1, dmc_old)]
                d[, c('DDMC_START', 'DDMC_INC', 'DDMC_DEC') := .dmcCalcPieces(DMC_YDA, TEMP, RH, PREC, LAT, MON)]
                d[, DC_YDA := data.table::shift(DC, 1, dc_old)]
                d[, c('DDC_START', 'DDC_INC', 'DDC_DEC') := .dcCalcPieces(DC_YDA, TEMP, RH, PREC, LAT, MON)]
                daily <- rbind(daily, d)
            }
        }
        if (!is.null(daily))
        {
            # want to take 0.5 mm off of the total but proportional to amounts per hour
            # CHECK: does this make more sense than just removing the first 0.5mm?
            today <- w[HR <= 17, c('ID', 'TIMESTAMP', 'DATE', 'PREC')]
            yest <- w[HR > 17, c('ID', 'TIMESTAMP', 'DATE', 'PREC')]
            sum_today <- today[, sum(PREC), by=c('ID', 'DATE')]
            setnames(sum_today, 'V1', 'TODAY')
            sum_yest <- yest[, sum(PREC), by=c('ID', 'DATE')]
            setnames(sum_yest, 'V1', 'YEST')
            yest[, DATE := as.character(as.Date(DATE) + 1)]
            sum_prec <- merge(sum_today, sum_yest)
            sum_prec[, TOTAL := TODAY + YEST]
            # figure out what fraction of the total rain to be counted 1mm is
            sum_prec[, FRACTION := ifelse(0.5 >= TOTAL, 0, (TOTAL - 0.5) / TOTAL)]
            r <- merge(w, sum_prec[, c('ID', 'DATE', 'FRACTION')], by=c('ID', 'DATE'))
            setnames(r, 'PREC', 'PREC_ORIG')
            r[, PREC := PREC_ORIG * FRACTION]
            ffmc <- daily[, c('ID', 'DATE', 'FFMC')]
            setnames(ffmc, 'FFMC', 'DFFMC')
            r <- merge(r, daily[, c('ID', 'DATE', 'DMC', 'DC', 'BUI')], by=c('ID', 'DATE'))
            r <- hffmc(r, time.step=1.0 / intervals, hourlyFWI=TRUE)[, -c('prec', 'fraction')]
            names(r) <- toupper(names(r))
            # revert to actual precip values
            setnames(r, 'PREC_ORIG', 'PREC')
            r <- merge(r, ffmc)
            
            # calculate vapour pressure deficit
            # https://physics.stackexchange.com/questions/4343/how-can-i-calculate-vapor-pressure-deficit-from-temperature-and-relative-humidit
            r[, VPS := 0.6108 * exp(17.27 * TEMP / (TEMP + 237.3))]
            r[, VPA := RH / 100 * VPS]
            r[, VPD := VPA - VPS]
            
            # divide DDMC_DEC proportionally among hours with rain from 1200 -> 1200
            # split along 1200 -> 1200 line
            # BUT rain at 12 is from 1100 to 1200, so split at 13
            # CHECK: does that mean 0000 should be in the other group of hours then?
            r[, FOR_DATE := ifelse(hour(TIMESTAMP) < 13, DATE, as.character(as.Date(DATE) + 1))]
            
            d <- daily[, c('ID', 'DATE', 'DMC', 'DC')]
            setnames(d, 'DMC', 'DDMC')
            setnames(d, 'DC', 'DDC')
            r <- merge(r, d, by=c('ID', 'DATE'), all=TRUE)
            r$DDMC <- nafill(r$DDMC, fill=dmc_old)
            
            
            r$DDC <- nafill(r$DDC, fill=dc_old)
            
            # rain is already allocated to 1st hour past start of 6hr period
            # tabulate rain per day
            # FIX: probably duplicating something but just do it for now
            prec_by_for_date <- r[, sum(PREC), by=c('FOR_DATE')]
            setnames(prec_by_for_date, 'V1', 'DAILY_PREC')
            
            
            # make it easier to compare hours within the same day, since 1200 is actually hour 0 of the day
            #~ r[, PERIOD_HOUR := ifelse(hour(TIMESTAMP) >= 12, hour(TIMESTAMP) - 12, hour(TIMESTAMP) + 12)]
            # BUT, ... 12 is actually 1100 - 1200
            r[, PERIOD_HOUR := ifelse(hour(TIMESTAMP) >= 13, hour(TIMESTAMP) - 13, hour(TIMESTAMP) + 11)]
            
            # NOTE: include the 1.5mm that gets deducted because the daily dmc calculation subtracts it
            #       so if we just divide proportional to all rain that will apply the reduction proportionally too
            
            # divide rain for each hour by the precip for that day, but need to join first so we can do that
            r <- merge(r, prec_by_for_date, by=c('FOR_DATE'))
            
            # so first day probably goes from 0000 -> 1200 because it was a hard cut at midnight
            # to deal with that, we're going to calculate the decrease to date for every hour and then
            # add that plus any drying to DMC_YDA
            
            # this should be equivalent to determining the fraction of total rain for the day up to this hour, if that's easier
            
            # do precip calculation based on FWI FOR_DATE and VPD calculation based on calendar day
            
            # decrease is total decrease * fraction of rain for the day
            r[, PREC_FRACTION := ifelse(DAILY_PREC == 0, 0, PREC / DAILY_PREC)]
            
            names(times) <- toupper(names(times))
            times$LONG <- times$LON
            times$DATE <- as.character(times$DATE)
            times$SUNRISE <- toDecimal(times$SUNRISE)
            times$SUNSET <- toDecimal(times$SUNSET)
            r <- merge(r, times, by=c('DATE', 'LAT', 'LONG'))
            
            # need to figure out the drying hours and then what the fractional VPD for each of them is
            drying_vpd <- r[HR >= as.integer(SUNRISE) & HR <= ceiling(SUNSET), c('DATE', 'TIMESTAMP', 'FOR_DATE', 'SUNRISE', 'SUNSET', 'VPD')]
            daily_vpd <- drying_vpd[, sum(VPD), by=c('DATE')]
            setnames(daily_vpd, 'V1', 'DAILY_VPD')
            vpd <- merge(drying_vpd, daily_vpd, by=c('DATE'))
            vpd[, VPD_FRACTION := VPD / DAILY_VPD]
            vpd <- merge(vpd, daily[, c('DATE', 'DDMC_INC', 'DDC_INC')])
            
            vpd[, DMC_INC := DDMC_INC * VPD_FRACTION]
            vpd[, DC_INC := DDC_INC * VPD_FRACTION]
            
            r <- merge(r, vpd[, c('TIMESTAMP', 'DMC_INC', 'DC_INC')], by=c('TIMESTAMP'), all=TRUE)
            
            # increase is 0 for any hour that wasn't in vpd
            r$DMC_INC <- nafill(r$DMC_INC, fill=0)
            r$DC_INC <- nafill(r$DC_INC, fill=0)
            
            d <- daily[, c('DATE', 'DDMC_DEC', 'DDC_DEC')]
            setnames(d, 'DATE', 'FOR_DATE')
            # wetting is a result of the previous day's rain, so need to push it back a day
            d[, FOR_DATE := as.character(as.Date(FOR_DATE) - 1)]
            
            r <- merge(r, d, c('FOR_DATE'))
            r[, DMC_DEC := PREC_FRACTION * DDMC_DEC]
            r[, DC_DEC := PREC_FRACTION * DDC_DEC]
            
            # just do sum from start of period to now for hourly values
            r[, DMC := dmc_old + cumsum(DMC_INC) - cumsum(DMC_DEC)]
            r[, DC := dc_old + cumsum(DC_INC) - cumsum(DC_DEC)]
            
            # get rid of intermediate calculations
            r <- r[,
                   -c('PREC_FRACTION', 'DAILY_PREC', 'DMC_INC', 'DC_INC', 'DDMC_DEC', 'DDC_DEC', 'DMC_DEC', 'DC_DEC',
                      'PERIOD_HOUR', 'VPS', 'VPA', 'VPD')]
            
            # get rid of daily values
            # r <- r[, -c('DFFMC', 'DDMC', 'DDC')]
            
            # calculate daily values
            # NOTE: this will still vary by hour because of WS
            r[, DISI := cffdrs:::.ISIcalc(DFFMC, WS, fbpMod = FALSE)]
            
            r[, DBUI := cffdrs:::.buiCalc(DDMC, DDC)]
            r[, DFWI := cffdrs:::.fwiCalc(DISI, DBUI)]
            # taken from package code
            r[, DDSR := 0.0272 * (DFWI ^ 1.77)]
            
            
            # recalculate hourly values for dependent indices
            # FIX: what is fbpMod doing? this is the default value for it
            # this might be covered by hffmc()
            r[, ISI := cffdrs:::.ISIcalc(FFMC, WS, fbpMod = FALSE)]
            
            r[, BUI := cffdrs:::.buiCalc(DMC, DC)]
            r[, FWI := cffdrs:::.fwiCalc(ISI, BUI)]
            # taken from package code
            r[, DSR := 0.0272 * (FWI ^ 1.77)]
            
            # reorder columns
            r <- r[, c('TIMESTAMP', 'ID', 'DATE', 'TEMP', 'WS', 'RH',
                       'PREC', 'LAT', 'LONG', 'YR', 'MON', 'DAY', 'HR',
                       'MINUTE', 'SUNRISE', 'SUNSET',
                       'FFMC', 'DMC', 'DC', 'ISI', 'BUI', 'FWI', 'DSR',
                       'DFFMC', 'DDMC', 'DDC', 'DISI', 'DBUI', 'DFWI', 'DDSR')]
        }
    }
    return(r)
}

hFWI <- function(weatherstream, intervals, ffmc_old=85, dmc_old=6, dc_old=15)
{
    wx <- copy(weatherstream)
    colnames(wx) <- toupper(colnames(wx))
    hadStn <- 'ID' %in% colnames(wx)
    hadMinute <- 'MINUTE' %in% colnames(wx)
    hadDate <- 'DATE' %in% colnames(wx)
    hadLatitude <- 'LAT' %in% colnames(wx)
    hadLongitude <- 'LONG' %in% colnames(wx)
    hadTimestamp <- 'TIMESTAMP' %in% colnames(wx)
    if (!hadStn)
    {
        wx[, ID := 'STN']
    }
    if (!hadMinute)
    {
        wx[, MINUTE := 0]
    }
    if (!hadDate)
    {
        wx[, DATE := as.character(as.Date(sprintf('%04d-%02d-%02d', YR, MON, DAY)))]
    }
    if (!hadLatitude)
    {
        wx[, LAT := DEFAULT_LATITUDE]
    }
    if (!hadLongitude)
    {
        wx[, LONG := DEFAULT_LONGITUDE]
    }
    if (!hadTimestamp)
    {
        wx[, TIMESTAMP := as_datetime(sprintf('%04d-%02d-%02d %02d:%02d:00', YR, MON, DAY, HR, MINUTE))]
    }
    results <- NULL
    for (stn in unique(weatherstream$ID))
    {
        w <- wx[ID == stn]
        r <- .stnHFWI(w, intervals, ffmc_old, dmc_old, dc_old)
        results <- rbind(results, r)
    }
    if (!hadStn)
    {
        #weatherstream <- weatherstream[, -c('ID')]
        if (!is.null(results)) { results <- results[, -c('ID')] }
    }
    if (!hadMinute)
    {
        #weatherstream <- weatherstream[, -c('MINUTE')]
        if (!is.null(results)) { results <- results[, -c('MINUTE')] }
    }
    if (!hadDate)
    {
        #weatherstream <- weatherstream[, -c('DATE')]
        if (!is.null(results)) { results <- results[, -c('DATE')] }
    }
    if (!hadLatitude)
    {
        #weatherstream <- weatherstream[, -c('LAT')]
        if (!is.null(results)) { results <- results[, -c('LAT')] }
    }
    if (!hadLongitude)
    {
        #weatherstream <- weatherstream[, -c('LONG')]
        if (!is.null(results)) { results <- results[, -c('LONG')] }
    }
    if (!hadTimestamp)
    {
        #weatherstream <- weatherstream[, -c('TIMESTAMP')]
        if (!is.null(results)) { results <- results[, -c('TIMESTAMP')] }
    }
    return(results)
}

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
    df$HR <- hour(df$OBSERVATION_DATE)
    df$MINUTE <- minute(df$OBSERVATION_DATE)
    df$DATE <- as.character(date(df$OBSERVATION_DATE))
    df$PREC <- as.double(df$RAINFALL)
    df$YR <- year(df$OBSERVATION_DATE)
    df$MON <- month(df$OBSERVATION_DATE)
    df$DAY <- day(df$OBSERVATION_DATE)
    df$WS <- df$ADJWINDSPEED
    df$LAT <- df$LATITUDE
    df$LONG <- df$LONGITUDE
    weatherstream <- as.data.table(df)
    
    x <- hFWI(weatherstream, 1)
    #print(x)

    output$tempPlot <- renderPlot({
        ggplot(x, aes(TIMESTAMP, TEMP)) +
            geom_point()
        #plot(TEMP ~ TIMESTAMP, x)
    })
    output$rhPlot <- renderPlot({
        plot(RH ~ TIMESTAMP, x)
    })
    output$wsPlot <- renderPlot({
        plot(WS ~ TIMESTAMP, x)
    })
    output$precPlot <- renderPlot({
        plot(PREC ~ TIMESTAMP, x)
    })
    output$ffmcPlot <- renderPlot({
        plot(FFMC ~ TIMESTAMP, x)
        lines(DFFMC ~ TIMESTAMP, x)
    })
    output$dmcPlot <- renderPlot({
        plot(DMC ~ TIMESTAMP, x)
        lines(DDMC ~ TIMESTAMP, x)
    })
    output$dcPlot <- renderPlot({
        plot(DC ~ TIMESTAMP, x)
        lines(DDC ~ TIMESTAMP, x)
    })
    output$isiPlot <- renderPlot({
        plot(ISI ~ TIMESTAMP, x)
        lines(DISI ~ TIMESTAMP, x)
    })
    output$buiPlot <- renderPlot({
        plot(BUI ~ TIMESTAMP, x)
        lines(DBUI ~ TIMESTAMP, x)
    })
    output$fwiPlot <- renderPlot({
        plot(FWI ~ TIMESTAMP, x)
        lines(DFWI ~ TIMESTAMP, x)
    })
}
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    observeEvent(input$station, renderPlots(input, output))
})
