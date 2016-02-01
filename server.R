# *------------------------------------------------------------------
# | PROGRAM NAME: Solar Energy Calculator
# | DATE: Feb 1 2016
# | CREATED BY: La Minh Hoang
# | PROJECT FILE: server.R
# | CONTACT: hoangrobin@gmail.com
# *----------------------------------------------------------------
# | PURPOSE: 
# | -Provide  server script for shiny package
# *------------------------------------------------------------------
# | References: http://shiny.rstudio.com/tutorial/
# |
# |
# *------------------------------------------------------------------
# | UPDATES:              
# | -Feb 1 2016: Added comments
# |
# *------------------------------------------------------------------
library(shiny)
library(scatterplot3d)
source("R/suncalculator.R",chdir=T)

shinyServer(function(input, output) {
  data <- reactiveValues(
    date = Sys.Date(),
    #                          hour = 8,
    #                          minute = 0,
    #     year = as.numeric(format(Sys.Date(), "%Y")),
    #     month = as.numeric(format(Sys.Date(), "%m")),
    lat = 0,
    long = 0,
    gmt = 0,
    tilt = 45,
    azi = 180,
    sealevel = 0
  )
  
  totalSolarEnergyDay <- reactiveValues(data = solarOneDay(
    as.numeric(format(Sys.Date(), "%Y")),
    as.numeric(format(Sys.Date(), "%m")),
    as.numeric(format(Sys.Date(), "%d")),
    0,
    0,
    0,
    45,
    180,
    0
  ))
  
  dataMonth <- reactiveValues(
    year = as.numeric(format(Sys.Date(), "%Y")),
    month = as.numeric(format(Sys.Date(), "%m")),
    lat = 0,
    long = 0,
    gmt = 0,
    tilt = 45,
    azi = 180,
    sealevel = 0
  )
  
  totalSolarEnergyMonth <-
    reactiveValues(data = solarOneMonth(as.numeric(format(Sys.Date(
      
    ), "%Y")),
    as.numeric(format(Sys.Date(
      
    ), "%m")),
    0,
    0,
    0,
    45,
    180,
    0))
  
  dataYear <- reactiveValues(
    year = as.numeric(format(Sys.Date(), "%Y")),
    lat = 0,
    long = 0,
    gmt = 0,
    tilt = 45,
    azi = 180,
    sealevel = 0
  )
  
  totalSolarEnergyYear <-
    reactiveValues(data = solarOneYear(as.numeric(format(Sys.Date(
      
    ), "%Y")),
    0,
    0,
    0,
    45,
    180,
    0))
  
  ###Data for optimal
  matrix = NULL;
  for (i in 1:7) {
    for (j in 1:16) {
      matrix = rbind(matrix,c((i - 1) * 15,(j - 1) * 22.5, 0))
    }
  }
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
  
  dataOptimalYear <- reactiveValues(
    data = df,
    year = as.numeric(format(Sys.Date(), "%Y")),
    lat = 0,
    long = 0,
    gmt = 0,
    sealevel = 0
  )
  
  ###Day Calculation###
  observeEvent(input$actCalculateDay, {
    data$date <- input$dateDay
  })
  #   observeEvent(input$actCalculateDay, { data$hour <- input$hourslider })
  #   observeEvent(input$actCalculateDay, { data$minute <- input$minuteslider })
  observeEvent(input$actCalculateDay, {
    data$lat <- input$latDay
  })
  observeEvent(input$actCalculateDay, {
    data$long <- input$longDay
  })
  observeEvent(input$actCalculateDay, {
    data$gmt <- input$gmtsliderDay
  })
  observeEvent(input$actCalculateDay, {
    data$tilt <- input$tiltsliderDay
  })
  observeEvent(input$actCalculateDay, {
    data$azi <- input$aziDay
  })
  observeEvent(input$actCalculateDay, {
    data$sealevel <- input$sealevelDay
  })
  observeEvent(input$actCalculateDay, {
    totalSolarEnergyDay$data <- solarOneDay(
      as.numeric(format(data$date, "%Y")),
      as.numeric(format(data$date, "%m")),
      as.numeric(format(data$date, "%d")),
      data$lat,
      data$long,
      data$gmt,
      data$tilt,
      data$azi,
      data$sealevel
    )
  })
  
  output$totalEnergyDay <- renderUI({
    withMathJax(
      "$$\\text{Total solar energy received on ", format(data$date, "%A %B %d %Y") ,": }", round(totalSolarEnergyDay$data[1], digits =
                                                                                                   2) , "\\text{ }kW/m^2/day$$"
    )
  })
  
  #Generate bar plot
  output$plotDay <- renderPlot({
    labelsY = parse(text = paste("kW/m", "^2"))
    par(mar = c(5,6,4,2) + 0.1)
    barplot(
      totalSolarEnergyDay$data[2:length(totalSolarEnergyDay$data)],main = "Solar Energy in one day",xlab =
        "hour of day", ylab = labelsY,names.arg = 1:24
    )
  })
  
  #Generate a summary of the data
  output$summaryDay <- renderPrint({
    summary(totalSolarEnergyDay$data[2:length(totalSolarEnergyDay$data)])
  })
  
  # Generate an HTML table view of the data
  output$tableDay <- renderTable({
    tableSolar <-
      cbind(totalSolarEnergyDay$data[2:length(totalSolarEnergyDay$data)]);
    colnames(tableSolar, do.NULL = FALSE)
    colnames(tableSolar) <- c("Solar Energy")
    rownames(tableSolar) <-
      rownames(tableSolar, do.NULL = FALSE, prefix = "Hour.")
    tableSolar
  })
  ###Day Calculation###
  
  ###Month Calculation###
  #   observeEvent(input$actCalculateMonth, {
  #     data$date <- input$dateMonth
  #   })
  #   observeEvent(input$actCalculateDay, { data$hour <- input$hourslider })
  #   observeEvent(input$actCalculateDay, { data$minute <- input$minuteslider })
  observeEvent(input$actCalculateMonth, {
    dataMonth$year <- input$yearMonth
  })
  observeEvent(input$actCalculateMonth, {
    dataMonth$month <- input$monthMonth
  })
  observeEvent(input$actCalculateMonth, {
    dataMonth$lat <- input$latMonth
  })
  observeEvent(input$actCalculateMonth, {
    dataMonth$long <- input$longMonth
  })
  observeEvent(input$actCalculateMonth, {
    dataMonth$gmt <- input$gmtsliderMonth
  })
  observeEvent(input$actCalculateMonth, {
    dataMonth$tilt <- input$tiltsliderMonth
  })
  observeEvent(input$actCalculateMonth, {
    dataMonth$azi <- input$aziMonth
  })
  observeEvent(input$actCalculateMonth, {
    dataMonth$sealevel <- input$sealevelMonth
  })
  observeEvent(input$actCalculateMonth, {
    totalSolarEnergyMonth$data <- solarOneMonth(
      dataMonth$year,
      dataMonth$month,
      dataMonth$lat,
      dataMonth$long,
      dataMonth$gmt,
      dataMonth$tilt,
      dataMonth$azi,
      dataMonth$sealevel
    )
  })
  
  output$totalEnergyMonth <- renderUI({
    date = as.Date(paste(dataMonth$year,dataMonth$month,1,sep = "-"));
    
    withMathJax(
      "$$\\text{Total solar energy received in ", format(date, "%B %Y") ,": }", round(totalSolarEnergyMonth$data[1], digits =
                                                                                        2) , "\\text{ }kW/m^2/month$$"
    )
  })
  
  #Generate bar plot
  output$plotMonth <- renderPlot({
    labelsY = parse(text = paste("kW/m", "^2","/day"))
    par(mar = c(5,6,4,2) + 0.1)
    barplot(
      totalSolarEnergyMonth$data[2:length(totalSolarEnergyMonth$data)],main = "Solar Energy in one month",xlab =
        "day of month", ylab = labelsY,names.arg = 1:(length(totalSolarEnergyMonth$data) -
                                                        1)
    )
  })
  
  #Generate a summary of the data
  output$summaryMonth <- renderPrint({
    summary(totalSolarEnergyMonth$data[2:length(totalSolarEnergyMonth$data)])
  })
  
  # Generate an HTML table view of the data
  output$tableMonth <- renderTable({
    tableSolar <-
      cbind(totalSolarEnergyMonth$data[2:length(totalSolarEnergyMonth$data)]);
    colnames(tableSolar, do.NULL = FALSE)
    colnames(tableSolar) <- c("Solar Energy")
    rownames(tableSolar) <-
      rownames(tableSolar, do.NULL = FALSE, prefix = "Day.")
    tableSolar
  })
  ###Month Calculation###
  
  ###Year Calculation###
  observeEvent(input$actCalculateYear, {
    dataYear$year <- input$yearYear
  })
  observeEvent(input$actCalculateYear, {
    dataYear$lat <- input$latYear
  })
  observeEvent(input$actCalculateYear, {
    dataYear$long <- input$longYear
  })
  observeEvent(input$actCalculateYear, {
    dataYear$gmt <- input$gmtsliderYear
  })
  observeEvent(input$actCalculateYear, {
    dataYear$tilt <- input$tiltsliderYear
  })
  observeEvent(input$actCalculateYear, {
    dataYear$azi <- input$aziYear
  })
  observeEvent(input$actCalculateYear, {
    dataYear$sealevel <- input$sealevelYear
  })
  observeEvent(input$actCalculateYear, {
    totalSolarEnergyYear$data <- solarOneYear(
      dataYear$year,
      dataYear$lat,
      dataYear$long,
      dataYear$gmt,
      dataYear$tilt,
      dataYear$azi,
      dataYear$sealevel
    )
  })
  
  #data for Optimal
  ###Year Calculation###
  observeEvent(input$actCalculateOptimal, {
    dataOptimalYear$year <- input$yearOptimal
  })
  observeEvent(input$actCalculateOptimal, {
    dataOptimalYear$lat <- input$latOptimal
  })
  observeEvent(input$actCalculateOptimal, {
    dataOptimalYear$long <- input$longOptimal
  })
  observeEvent(input$actCalculateOptimal, {
    dataOptimalYear$gmt <- input$gmtsliderOptimal
  })
  observeEvent(input$actCalculateOptimal, {
    dataOptimalYear$sealevel <- input$sealevelOptimal
  })
  
  observeEvent(input$actCalculateOptimal, {
    #print(paste(dataOptimalYear$year," ",dataOptimalYear$lat," ",dataOptimalYear$long," ",dataOptimalYear$gmt," ",dataOptimalYear$sealevel))
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Computing data",
                 detail = 'This may take a while...', value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    dataOptimalYear$data <-
      findOptimalTiltAngleYear(
        dataOptimalYear$year,
        dataOptimalYear$lat,
        dataOptimalYear$long,
        dataOptimalYear$gmt,
        dataOptimalYear$sealevel
      )
  })
  
  
  
  output$totalEnergyYear <- renderUI({
    date = as.Date(paste(dataYear$year,1,1,sep = "-"));
    
    withMathJax(
      "$$\\text{Total solar energy received in ", format(date, "%Y") ,": }", round(totalSolarEnergyYear$data[1], digits =
                                                                                     2) , "\\text{ }kW/m^2/year$$"
    )
  })
  
  #Generate bar plot
  output$plotYear <- renderPlot({
    labelsY = parse(text = paste("kW/m", "^2","/day"))
    par(mar = c(5,6,4,2) + 0.1)
    barplot(
      totalSolarEnergyYear$data[2:length(totalSolarEnergyYear$data)],main = paste("Solar Energy in year", dataYear$year),xlab =
        "day of year", ylab = labelsY,names.arg = 1:(length(totalSolarEnergyYear$data) -
                                                       1)
    )
  })
  
  #Generate a summary of the data
  output$summaryYear <- renderPrint({
    summary(totalSolarEnergyYear$data[2:length(totalSolarEnergyYear$data)])
  })
  
  # Generate an HTML table view of the data
  output$tableYear <- renderTable({
    tableSolar <-
      cbind(totalSolarEnergyYear$data[2:length(totalSolarEnergyYear$data)]);
    colnames(tableSolar, do.NULL = FALSE)
    colnames(tableSolar) <- c("Solar Energy")
    rownames(tableSolar) <-
      rownames(tableSolar, do.NULL = FALSE, prefix = "Day.")
    tableSolar
  })
  
  #Optimal Angle Year
  output$plotOptimal <- renderPlot({
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Drawing plot", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    df <- dataOptimalYear$data
    labelsZ = parse(text = paste("kW/m", "^2","/year"))
    with(df, {
      s3d <- scatterplot3d(
        tilt, azi, solar,        # x y and z axis
        pch = 19,        # circle color indicates no. of cylinders
        type = "h", lty.hplot = 2,       # lines to the horizontal plane
        scale.y = .75,                 # scale y axis (reduce by 25%)
        main = "Solar Energy by Module Tilt and Azimuth angle",
        xlab = "Tilt Angle (degree)",
        ylab = "Module Azimuth Angle (degree)",
        zlab = labelsZ,
        highlight.3d = TRUE
      )
      s3d.coords <- s3d$xyz.convert(tilt, azi, solar)
      #   text(s3d.coords$x, s3d.coords$y,     # x and y coordinates
      #        labels=row.names(df),       # text to plot
      #        pos=4, cex=.5)                  # shrink text 50% and place to right of points)
      # add the legend
      #       legend("topleft", inset=.05,      # location and inset
      #              bty="n", cex=.5,              # suppress legend box, shrink text 50%
      #              title="Solar Energy",
      #              c(">1500", "1500-1000", "1000-500","500-0"), fill=c("red", "orange","green","blue"))
      fit <- lm(solar ~ tilt + azi)
      s3d$plane3d(fit)
    })
    
    #Generate a summary of the optimal data
    output$summaryOptimal <- renderPrint({
      summary(dataOptimalYear$data)
    })
    
    # Generate an HTML table view of the optimal data
    output$tableOptimal <- renderTable({
      dataOptimalYear$data
    })
    
    output$textOptimal <- renderUI({
      df <- dataOptimalYear$data
      optimalAzi  <- df$azi[df$solar == max(df$solar)][1]
      optimalTilt <- df$tilt[df$solar == max(df$solar)][1]
      facing <- df$fromNorth[df$azi == optimalAzi][1]
      maxSolar <- max(df$solar)[1]
      #       withMathJax(
      #         "$$\\text{Total solar energy received in ", format(date, "%Y") ,": }", round(totalSolarEnergyYear$data[1], digits =
      #                                                                                        2) , "\\text{ }kW/m^2/year$$"
      #       )
      withMathJax(
        helpText("Optimal Tilt Angle is :$$", optimalTilt,"^o$$"),
        helpText("Optimal Module Azimuth Angle is :$$",optimalAzi,"^o$$"),
        helpText("The direction the module will face is:$$", facing,"$$"),
        helpText(
          "Maximum Solar Energy output from module according to optimal angles is:$$", round(maxSolar,digits = 2),"\\text{ }kW/m^2/year$$"
        )
      )
    })
    
  })
  ###Year Calculation###
  
})