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
HOURLY_DATA <- list()
CALCULATED <- list()
ORIG_FORECAST <- list()
FORECAST <- list()
OLD_STN <- ''
OLD_TIME <- NULL
OLD_SINCE <- NULL

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

cacheData <- function(stn) {
  dfoss <- getDFOSS(stn)
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
  return(list('dfoss'=dfoss,
              'hourly'=hourly,
              'forecast'=forecast,
              'lat'=lat,
              'long'=long,
              'tz'=tz,
              'timezone'=timezone))
}
