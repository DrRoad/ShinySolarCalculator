# *------------------------------------------------------------------
# | PROGRAM NAME: Solar Energy Calculator
# | DATE: Feb 1 2016
# | CREATED BY: La Minh Hoang
# | PROJECT FILE: suncalculator.R
# | CONTACT: hoangrobin@gmail.com
# *----------------------------------------------------------------
# | PURPOSE: 
# | -Provide functions for calculating total solar energy for 
# |  time length of 1 day, 1 month, or 1 year.
# | -Provide functions for calculating PV module optimal angles 
# |  (tilt angle and azimuth angle)
# *------------------------------------------------------------------
# | References: http://www.pveducation.org/pvcdrom/properties-of-sunlight/arbitrary-orientation-and-tilt
# |
# |
# *------------------------------------------------------------------
# | UPDATES:              
# | -Feb 1 2016: Added comments
# |
# *------------------------------------------------------------------
library(foreach)
library(doParallel)

#' Direct beam solar radiation intensity
#' 
#' Calculate Direct beam intensity.
#'
#' @param airmass Air Mass
#' @param seaLevel Location height above sea level in kilometers
#'
#' @return Direct beam intensity in kW/m^2
#' @export
#'
#' @examples
#' S_d(1.5,0.5)
S_d <- function(airmass,seaLevel = 0) {
  result <-
    1.353 * ((1 - 0.14 * seaLevel) * 0.7 ^ (airmass ^ 0.678) + 0.14 * seaLevel)
  return(result)
}

#' Solar Radiation on a Tilted Surface
#' 
#' Solar energy on a surface with arbitrary tilt and orientation.
#'
#' @param S_d Direct beam solar radiation intensity in kW/m^2
#' @param EA Sun elevation angle in degree
#' @param AA Sun azimuth angle in degree
#' @param tilt Module tilt angle in degree
#' @param M_AA Module azimuth angle in degree
#'
#' @return Solar energy in kW/m^2
#' @export
#'
#' @examples
#' S_m(0.8178,53.0242,94.7078,34,0)
S_m <- function(S_d,EA,AA,tilt,M_AA) {
  result <-
    S_d * (
      cos(EA * pi / 180) * sin(tilt * pi / 180) * cos((M_AA - AA) * pi /
                                                        180)
      + sin(EA *
              pi / 180) * cos(tilt * pi / 180)
    )
  return(result)
}

#' Air Mass
#' 
#' The proportion of atmosphere that the light must pass through before striking the Earth
#'
#' @param ZA Sun zenith angle in degree
#'
#' @return Air mass
#' @export
#'
#' @examples
#' AM(30)
AM <- function(ZA) {
  result <- 1 / cos(ZA * pi / 180)
  return(result)
}

#' Local Standard Time Meridian
#' 
#' The difference of the Local Time (LT) from Greenwich Mean Time (GMT) in degree.
#'
#' @param delta The difference of the Local Time (LT) from Greenwich Mean Time (GMT) in hours
#'
#' @return Local Standard Time Meridian (LSTM) in degree
#' @export
#'
#' @examples
#' LSTM(10)
LSTM <- function(delta) {
  result <- 15 * delta
  return(result)
}

#' Equation of Time
#' 
#' The equation of time (EoT) (in minutes) is an empirical equation that corrects for 
#' the eccentricity of the Earth's orbit and the Earth's axial tilt.
#'
#' @param day Number of days since the start of the year
#'
#' @return Equation of Time in minutes
#' @export
#'
#' @examples
#' EOT(256)
EOT <- function(day) {
  B <- 360 / 365 * (day - 81)
  result <-
    9.87 * sin(2 * B * pi / 180) - 7.53 * cos(B * pi / 180) - 1.5 * sin(B *
                                                                          pi / 180)
  return(result)
}

#' Time Correction Factor
#' 
#' The net Time Correction Factor (in minutes) accounts for 
#' the variation of the Local Solar Time (LST) within a given time zone
#'
#' @param longtitude Location's longtitude in degree
#' @param LSTM Local Standard Time Meridian in degree
#' @param EOT Equation of Time in minutes
#'
#' @return Time Correction in minutes
#' @export
#'
#' @examples
#' TC(15.5,150,4.753871)
TC <- function(longtitude,LSTM,EOT) {
  result <- 4 * (longtitude - LSTM) + EOT
  return(result)
}

#' Local Solar Time
#' 
#' Twelve noon local solar time (LST) is defined as when the sun is highest in the sky. 
#' Local time (LT) usually varies from LST because of the eccentricity of the Earth's orbit, 
#' and because of human adjustments such as time zones and daylight saving.
#'
#' @param LC Local time in hours
#' @param TC Time Correction Factor in minutes
#'
#' @return Local solar time in hours
#' @export
#'
#' @examples
#' LST(10,-533.2461)
LST <- function(LC,TC) {
  result <- LC + TC / 60
  return(result)
}

#Hour Angle, HRA, degree
#' Hour Angle
#' 
#' The Hour Angle converts the local solar time (LST) into 
#' the number of degrees which the sun moves across the sky.
#' In the morning the hour angle is negative, in the afternoon the hour angle is positive.
#'
#' @param LST Local solar time in hours
#'
#' @return Hour Angle (HRA) in degree
#' @export
#'
#' @examples
#' HRA(1.112565)
HRA <- function(LST) {
  result <- 15 * (LST - 12)
  return(result)
}

#' Declination Angle
#' 
#' The declination of the sun is the angle between the equator and
#' a line drawn from the centre of the Earth to the centre of the sun.
#'
#' @param day Number of days since the start of the year
#'
#' @return Declination Angle in degree
#' @export
#'
#' @examples
#' DA(256)
DA <- function(day) {
  B <- 360 / 365 * (day - 81)
  result <-
    asin((sin(23.45 * pi / 180) * sin(B * pi / 180))) * 180 / pi
  return(result)
}

#' Elevation Angle
#' 
#' The elevation angle (used interchangeably with altitude angle) is 
#' the angular height of the sun in the sky measured from the horizontal. 
#'
#' @param latitude Location's latitude in degree
#' @param DA Declination Angle in degree
#' @param HRA Hour Angle (HRA) in degree
#'
#' @return Elevation Angle in degree
#' @export
#'
#' @examples
#' EA(51.93,2.936844,-163.3115)
EA <- function(latitude,DA,HRA) {
  result <- asin(
    sin(DA * pi / 180) * sin(latitude * pi / 180) +
      cos(DA * pi / 180) * cos(latitude * pi / 180) * cos(HRA *
                                                            pi / 180)
  ) * 180 / pi
  return(result)
}

#' Zenith Angle
#' 
#' The zenith angle is the angle between the sun and the vertical.
#'
#' @param EA Elevation Angle in degree
#'
#' @return Zenith Angle in degree
#' @export
#'
#' @examples
#' ZA(30)
ZA <- function(EA) {
  result <- 90 - EA
  return(result)
}

#' Sunrise and Sunset
#' 
#' Calculate the sunrise and sunset time.
#'
#' @param latitude Location's latitude in degree
#' @param DA Declination Angle in degree
#' @param TC Time Correction in minutes
#'
#' @return Sunrise and sunset time in hours
#' @export
#'
#' @examples
#' SS(51.53,2.936844,1)
SS <- function(latitude, DA, TC) {
  sunrise <-
    12 - 1 / 15 * (acos(-tan(latitude * pi / 180) * tan(DA * pi / 180)) * 180 /
                     pi) - TC / 60
  sunset <-
    12 + 1 / 15 * (acos(-tan(latitude * pi / 180) * tan(DA * pi / 180)) * 180 /
                     pi) - TC / 60
  result <- list(sunrise = sunrise,sunset = sunset)
  return(result)
}

#' Azimuth Angle
#' 
#' The azimuth angle is the compass direction from which the sunlight is coming.
#'
#' @param latitude Location's latitude in degree
#' @param EA Elevation Angle in degree
#' @param DA Declination Angle in degree
#' @param LST Local solar time in hours 
#' @param HRA Hour Angle in degree
#'
#' @return Azimuth Angle in degree
#' @export
#'
#' @examples
#' AA(51.53,30,30,1,1)
AA <- function(latitude,EA,DA,LST,HRA) {
  Azimuth <-
    acos((
      sin(DA * pi / 180) * cos(latitude * pi / 180) - cos(DA * pi / 180) * sin(latitude *
                                                                                 pi / 180) * cos(HRA * pi / 180)
    ) / cos(EA * pi / 180)) * 180 / pi
  
  if (LST < 12 | HRA < 0)
  {
    result <- Azimuth
  } else {
    result <- 360 - Azimuth
  }
  return(result)
}

#' Solar Statistic Calculation
#' 
#' Calculate solar energy statistics on tilted surface for a specific date and time.
#'
#' @param year Year number
#' @param month Month number
#' @param day Day number
#' @param hour Hour number
#' @param minute Minute number
#' @param latitude Location's latitude in degree
#' @param longtitude Location's longtitude in degree
#' @param GMT Location's GMT offset
#' @param titlAngle Tilt Angle of the surface in degree
#' @param moduleAzi Direction the surface is facing
#' @param seaLevel Location height above sea level in kilometers
#'
#' @return Direct beam solar radiation, Solar Radiation on Tilted Surface, Air Mass, Sun Azimuth Angle, Sun Zenith Angle,
#' Sunrise Time, Sunset Time
#' @export
#'
#' @examples
#' SunOnTiltedSurface(2015,6,22,10,0,51.56,15.32,1,30,180,0.145)
SunOnTiltedSurface <-
  function(year,month,day,hour,minute,latitude,longtitude,GMT,titlAngle,moduleAzi,seaLevel) {
    
    #Generate number of days
    date  <-
      as.Date(paste (c(year,month,day), collapse = "/"),'%Y/%m/%d')
    beginDate  <-
      as.Date(paste (c(year,1,1), collapse = "/"),'%Y/%m/%d')
    days <- as.integer((date - beginDate))
    
    #Calculate Declination Angle
    DA <- DA(days)
    
    #Calculate Local Time
    LC <-  hour + minute / 60
    
    #Calculate Local Standard Time Meridian
    LSTM <- (GMT)
    
    #Calculate Equation of Time
    EOT <- EOT(days)
    
    #Calculate Time Correction Factor
    TC <- TC(longtitude,LSTM,EOT)
    
    #Calculate Local Solar Time
    LST <- LST(LC,TC)
    
    #Calculate HRA
    HRA <- HRA(LST)
    
    #Calculate Elevation Angle
    EA <- EA(latitude, DA, HRA)
    
    #Calculate Zenith Angle
    ZA <- ZA(EA)
    
    #Calculate Air Mass
    AM <- AM(ZA)
    
    #Calculate Sun Azimuth Angle
    AA <- AA(latitude,EA,DA,LST,HRA)
    
    #Calculate Solar Radiation Incident
    S_d <- S_d(AM,seaLevel)
    
    #Calculate Solar Radiation Module
    S_m <- S_m(S_d,EA,AA,titlAngle,moduleAzi)
    
    #Calculate Sunrise and Sunset
    SS <- SS(latitude,DA,TC)
    
    if (!is.finite(S_d))
    {
      S_d = 0
    }
    if (!is.finite(S_m))
    {
      S_m = 0
    }
    
    result <-
      list(
        S_d = S_d, S_m = S_m, airmass = AM, sunazi = AA, sunzenith = ZA ,sunrise = SS$sunrise, sunset =
          SS$sunset
      )
    
    return(result)
  }

#' Check Leap Year
#' 
#' Check if year is leap year or not.
#'
#' @param year Year number
#'
#' @return TRUE if year is leap year, FALSE if not
#' @export
#'
#' @examples
#' is.leapyear(2016)
is.leapyear = function(year) {
  #http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) &
            (year %% 100 != 0)) | (year %% 400 == 0))
}

#' Solar Energy For One Day
#' 
#' Calculate Solar Energy for One day.
#'
#' @param year Year number
#' @param month Month number
#' @param day Day number
#' @param latitude Location's latitude in degree
#' @param longtitude Location's longtitude in degree
#' @param GMT Location's GMT offset
#' @param titlAngle Tilt Angle of the surface in degree
#' @param moduleAzi Direction the surface is facing
#' @param seaLevel Location height above sea level in kilometers
#' @param useBin Print solar energy for every hour in day if TRUE
#'
#' @return Combined vector of Solar Energy for one day (kWh/m^2/day) and every hour (kWh/m^2) in that day
#' @export
#'
#' @examples
#' solarOneDay(2015,6,22,51.56,15.32,0,30,180,0.145)
solarOneDay <-
  function(year,month,day,latitude,longtitude,GMT,titlAngle,moduleAzi,seaLevel,useBin = TRUE) {
    sum = 0
    y = NULL;
    
    for (i in 1:24) {
      test = SunOnTiltedSurface(year,month,day,i,0,latitude,longtitude,GMT,titlAngle,moduleAzi,seaLevel)
      if (test$S_m < 0) {
        test$S_m = 0
      }
      sum = test$S_m + sum
      if (useBin) {
        y = c(y,test$S_m)
      }
    }
    return(c(sum,y))
  }

#' Solar Energy For One Month
#' 
#' Calculate Solar Energy for One Month.
#'
#' @param year Year number
#' @param month Month number
#' @param latitude Location's latitude in degree
#' @param longtitude Location's longtitude in degree
#' @param GMT Location's GMT offset
#' @param titlAngle Tilt Angle of the surface in degree
#' @param moduleAzi Direction the surface is facing
#' @param seaLevel Location height above sea level in kilometers
#' @param useBin Print solar energy for every day in month if TRUE
#'
#' @return Combined vector of Solar Energy for one month (kWh/m^2/month) and every day (kWh/m^2/day) in that month
#' @export
#'
#' @examples
#' solarOneMonth(2015,6,51.56,15.32,0,30,180,0.145)
solarOneMonth <- 
  function(year,month,latitude,longtitude,GMT,titlAngle,moduleAzi,seaLevel,useBin = TRUE) {
    sum = 0;
    y = NULL;
    bin = 0;
    maxDays = 0;
    
    if (month %in% c(1,3,5,7,8,10,12)) {
      maxDays = 31;
    } else if (month %in% c(4,6,9,11)) {
      maxDays = 30;
    } else if (is.leapyear(year)) {
      maxDays = 29;
    } else {
      maxDays = 28;
    }
    
    for (i in 1:maxDays) {
      bin = 0;
      for (j in 1:24) {
        test = SunOnTiltedSurface(year,month,i,j,0,latitude,longtitude,GMT,titlAngle,moduleAzi,seaLevel)
        if (test$S_m < 0) {
          test$S_m = 0
        }
        sum = test$S_m + sum
        bin = test$S_m + bin
      }
      if (useBin) {
        y = c(y,bin)
      }
    }
    
    return(c(sum,y))
  }

#' Solar Energy For One Year
#' 
#' Calculate Solar Energy for One Year
#'
#' @param year Year number
#' @param latitude Location's latitude in degree
#' @param longtitude Location's longtitude in degree
#' @param GMT Location's GMT offset
#' @param titlAngle Tilt Angle of the surface in degree
#' @param moduleAzi Direction the surface is facing
#' @param seaLevel Location height above sea level in kilometers
#' @param useBin Print solar energy for every day in year if TRUE
#'
#' @return Combined vector of Solar Energy for one year (kWh/m^2/year) and every day (kWh/m^2/day) in that year
#' @export
#'
#' @examples
#' solarOneYear(2015,51.56,15.32,0,30,180,0.145)
solarOneYear <-
  function(year,latitude,longtitude,GMT,titlAngle,moduleAzi,seaLevel,useBin = TRUE) {
    sum = 0;
    y = NULL;
    bin = 0;
    day = 0;
    maxDay = 0;
    
    for (m in 1:12) {
      if (m %in% c(1,3,5,7,8,10,12)) {
        maxDay = 31;
      } else if (m %in% c(4,6,9,11)) {
        maxDay = 30;
      } else if (is.leapyear(year)) {
        maxDay = 29;
      } else {
        maxDay = 28;
      }
      
      for (i in 1:maxDay) {
        bin = 0;
        for (j in 1:24) {
          test = SunOnTiltedSurface(year,m,i,j,0,latitude,longtitude,GMT,titlAngle,moduleAzi,seaLevel)
          if (test$S_m < 0) {
            test$S_m = 0
          }
          sum = test$S_m + sum
          bin = test$S_m + bin
        }
        if (useBin) {
          y = c(y,bin)
        }
      }
    }
    return(c(sum,y))
  }

#calculate optimal angle for one year
#' Optimal Angles Calculation
#' 
#' Calculate optimal angles (Module Tilt Angle and Module Azimuth Angle)
#' to get maximum Solar Energy output for one year.
#'
#' @param year Year number
#' @param latitude Location's latitude in degree
#' @param longtitude Location's longtitude in degree
#' @param GMT Location's GMT offset
#' @param seaLevel Location height above sea level in kilometers
#'
#' @return Dataframe contains Solar Energy output from 112 combination of Module Tilt Angle and Module Azimuth Angle
#' @export
#'
#' @examples
#' df <- findOptimalTiltAngleYear(2015,0,0,0,0)
#' optimalAzi  <- df$azi[df$solar == max(df$solar)][1]
#' optimalTilt <- df$tilt[df$solar == max(df$solar)][1]
#' 
findOptimalTiltAngleYear <-
  function(year,latitude,longtitude,GMT,seaLevel) {
    matrix = NULL;
    for (i in 1:7) {
      for (j in 1:16) {
        matrix = rbind(matrix,c((i - 1) * 15,(j - 1) * 22.5))
      }
    }
    #strt <- Sys.time();
    worker <-
      function(year, latitude, longtitude, GMT,tilt,azi,seaLevel) {
        solarOneYear(year, latitude, longtitude, GMT,tilt,azi,seaLevel,FALSE)
      }
    
    cl <- makeCluster(detectCores())
    ex <-
      Filter(function(x)
        is.function(get(x, .GlobalEnv)), ls(.GlobalEnv))
    clusterExport(cl, ex)
    result <-
      clusterMap(
        cl,worker, year = year,latitude = latitude, longtitude = longtitude, GMT = GMT, seaLevel = seaLevel,tilt = matrix[,1], azi = matrix[,2]
      )
    result <- sapply(result, "[", i = 1)
    stopCluster(cl)
    #print(Sys.time() - strt)
    matrix = cbind(matrix,result)
    df <-
      data.frame(
        tilt = matrix[,1], azi = matrix[,2], solar = matrix[,3], fromNorth = NA
      )
    df$fromNorth[df$azi == 0] <- "North"
    df$fromNorth[df$azi == 22.5] <- "North-Northeast"
    df$fromNorth[df$azi == 45] <- "Northeast"
    df$fromNorth[df$azi == 67.5] <- "East-Northeast"
    df$fromNorth[df$azi == 90] <- "East"
    df$fromNorth[df$azi == 112.5] <- "East-Southeast"
    df$fromNorth[df$azi == 135] <- "Southeast"
    df$fromNorth[df$azi == 157.5] <- "South-Southeast"
    df$fromNorth[df$azi == 180] <- "South"
    df$fromNorth[df$azi == 202.5] <- "South-Southwest"
    df$fromNorth[df$azi == 225] <- "Southwest"
    df$fromNorth[df$azi == 247.5] <- "West-Southwest"
    df$fromNorth[df$azi == 270] <- "West"
    df$fromNorth[df$azi == 292.5] <- "West-Northwest"
    df$fromNorth[df$azi == 315] <- "Northwest"
    df$fromNorth[df$azi == 337.5] <- "North-Northwest"
    return(df)
  }