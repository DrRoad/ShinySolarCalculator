# *------------------------------------------------------------------
# | PROGRAM NAME: Solar Energy Calculator
# | DATE: Feb 1 2016
# | CREATED BY: La Minh Hoang
# | PROJECT FILE: ui.R
# | CONTACT: hoangrobin@gmail.com
# *----------------------------------------------------------------
# | PURPOSE: 
# | -Provide user-interface script for shiny package
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

shinyUI(
  navbarPage(
    # Application title
    title = "Solar Energy Calculator",
    
    tabPanel(title = "Day",
             #Sidebar
             sidebarLayout(
               sidebarPanel(
                 actionButton("actCalculateDay", label = "Calculate"),
                 br(),
                 br(),
                 dateInput("dateDay", label = "Date input:", value = Sys.Date()),
                 # Copy the line below to make a slider bar
                 #         sliderInput(
                 #           "hourslider", "Hour:", min = 0,
                 #           max = 24, value = 8
                 #         )
                 #         ,
                 #         sliderInput(
                 #           "minuteslider", "Minute:", min = 0,
                 #           max = 60, value = 0
                 #         )
                 #         ,
                 numericInput("latDay", label = "Latitude", value = 0),
                 numericInput("longDay", label = "Longitude", value = 0),
                 sliderInput(
                   "gmtsliderDay", "GMT offsets:", min = -12,
                   max = 12, value = 0
                 ),
                 sliderInput(
                   "tiltsliderDay", "Module Tilt Angle:", min = 0,
                   max = 90, value = 45
                 ),
                 sliderInput(
                   "aziDay", "Module Azimuth Angle:", min = 0,
                   max = 360, value = 180
                 ),
                 numericInput("sealevelDay", label = "Sea Level (in kilometres):", value = 0)
               ),
               
               # Show a tabset that includes a plot, summary, and table view
               # of the generated distribution
               mainPanel(tabsetPanel(
                 type = "tabs",
                 tabPanel("Plot", plotOutput("plotDay"), uiOutput("totalEnergyDay")),
                 tabPanel("Summary", verbatimTextOutput("summaryDay")),
                 tabPanel("Table", tableOutput("tableDay"))
               ))
             )),
    tabPanel(title = "Month",
             #Sidebar
             sidebarLayout(
               sidebarPanel(
                 actionButton("actCalculateMonth", label = "Calculate"),
                 br(),
                 br(),
                 numericInput("yearMonth", label = "Year: ", value = as.numeric(format(Sys.Date(
                   
                 ), "%Y"))),
                 selectInput(
                   "monthMonth", label = "Month: ",
                   choices = list(
                     "January" = 1, "February" = 2, "March" = 3,
                     "April" = 4, "May" = 5, "June" = 6,
                     "July" = 7, "August" = 8, "September" = 9,
                     "October" = 10, "November" = 11, "December" = 12
                   ),
                   selected = as.numeric(format(Sys.Date(), "%m"))
                 ),
                 numericInput("latMonth", label = "Latitude", value = 0),
                 numericInput("longMonth", label = "Longitude", value = 0),
                 sliderInput(
                   "gmtsliderMonth", "GMT offsets:", min = -12,
                   max = 12, value = 0
                 ),
                 sliderInput(
                   "tiltsliderMonth", "Module Tilt Angle:", min = 0,
                   max = 90, value = 45
                 ),
                 sliderInput(
                   "aziMonth", "Module Azimuth Angle:", min = 0,
                   max = 360, value = 180
                 ),
                 numericInput("sealevelMonth", label = "Sea Level (in kilometres):", value = 0)
               ),
               
               # Show a tabset that includes a plot, summary, and table view
               # of the generated distribution
               mainPanel(tabsetPanel(
                 type = "tabs",
                 tabPanel(
                   "Plot", plotOutput("plotMonth"), uiOutput("totalEnergyMonth")
                 ),
                 tabPanel("Summary", verbatimTextOutput("summaryMonth")),
                 tabPanel("Table", tableOutput("tableMonth"))
               ))
             )),
    tabPanel(title = "Year",
             #Sidebar
             sidebarLayout(
               sidebarPanel(
                 actionButton("actCalculateYear", label = "Calculate"),
                 br(),
                 br(),
                 numericInput("yearYear", label = "Year: ", value = as.numeric(format(Sys.Date(
                   
                 ), "%Y"))),
                 numericInput("latYear", label = "Latitude", value = 0),
                 numericInput("longYear", label = "Longitude", value = 0),
                 sliderInput(
                   "gmtsliderYear", "GMT offsets:", min = -12,
                   max = 12, value = 0
                 ),
                 sliderInput(
                   "tiltsliderYear", "Module Tilt Angle:", min = 0,
                   max = 90, value = 45
                 ),
                 sliderInput(
                   "aziYear", "Module Azimuth Angle:", min = 0,
                   max = 360, value = 180
                 ),
                 numericInput("sealevelYear", label = "Sea Level (in kilometres):", value = 0)
               ),
               
               # Show a tabset that includes a plot, summary, and table view
               # of the generated distribution
               mainPanel(tabsetPanel(
                 type = "tabs",
                 tabPanel("Plot", plotOutput("plotYear"), uiOutput("totalEnergyYear")),
                 tabPanel("Summary", verbatimTextOutput("summaryYear")),
                 tabPanel("Table", tableOutput("tableYear"))
               ))
             )),
    tabPanel(title = "Optimal Angle",
             #Sidebar
             sidebarLayout(
               sidebarPanel(
                 actionButton("actCalculateOptimal", label = "Calculate"),
                 br(),
                 br(),
                 numericInput("yearOptimal", label = "Year: ", value = as.numeric(format(Sys.Date(
                   
                 ), "%Y"))),
                 numericInput("latOptimal", label = "Latitude", value = 0),
                 numericInput("longOptimal", label = "Longitude", value = 0),
                 sliderInput(
                   "gmtsliderOptimal", "GMT offsets:", min = -12,
                   max = 12, value = 0
                 ),
                 numericInput("sealevelOptimal", label = "Sea Level (in kilometres):", value = 0)
               ),
               
               # Show a tabset that includes a plot, summary, and table view
               # of the generated distribution
               mainPanel(tabsetPanel(
                 type = "tabs",
                 tabPanel(
                   "Optimal Angle",
                   br(),
                   plotOutput("plotOptimal"),
                   uiOutput("textOptimal")
                 ),
                 tabPanel("Summary", verbatimTextOutput("summaryOptimal")),
                 tabPanel("Table", tableOutput("tableOptimal"))
                 
               ))
             ))
    
  )
)