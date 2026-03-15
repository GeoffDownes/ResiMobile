#Geoff Downes 11 March 2026
#Forest Quality Pty. Ltd. 
#webr::unmount("/usr/lib/R/library/shinyMobile")
#webr::install("shinyMobile", repos = c("https://rinterface.github.io/rinterface-wasm-cran/"))


library(shiny)
library(shinyMobile)
library(FQResi)
library(tidyverse)    #ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats, lubridate
library(data.table)   
library(lmodel2)      #Standardised major axis regression
library(randomForest)
library(minpack.lm)    #nls.LM curve fitting
library(moments)       #Calculates skewness and kurtosis


#includeCSS("www/shinyMobile.css")   

app_opts <-   list(
  #theme = "ios",
  dark = FALSE, #"auto",
  skeletonsOnLoad = FALSE,
  preloader = FALSE,
  filled = FALSE,
  color = "blue",
  touch = list(
    touchClicksDistanceThreshold = 5,
    tapHold = TRUE,
    tapHoldDelay = 750,
    tapHoldPreventClicks = TRUE,
    iosTouchRipple = FALSE,
    mdTouchRipple = TRUE
  ),
  iosTranslucentBars = FALSE,
  navbar = list(
    iosCenterTitle = TRUE,
    hideOnPageScroll = TRUE
  ),
  toolbar = list(
    hideOnPageScroll = FALSE
  ),
  pullToRefresh = FALSE
)

shinyApp(
    ui = f7Page(
        options = app_opts,
        title = "FQ Resi Mobile",
        f7TabLayout(
            panels = tagList(
                f7Panel(id = "leftPanel", side = "left", effect = "push", title = "Process controls",
                        f7Block("A panel with push effect")
                ),
                
                # Fixed: Wrapped f7PanelMenu in f7Panel
                f7Panel(id = "menuPanel", side = "left", effect = "push", title = "Main Menu",
                        f7PanelMenu(id = "mainPanel",
                                    # Fixed: Changed tabName to match the f7Tab below
                                    f7PanelItem(tabName = "SummaryData", title = "Tab 1", icon = f7Icon("folder"), active = TRUE),
                                    f7PanelItem(tabName = "Tab3", title = "Tab 3", icon = f7Icon("layers_alt"))
                        )
                )
            ),
            
            navbar = f7Navbar(title = "ResiMobile", hairline = TRUE, leftPanel = TRUE, rightPanel = FALSE, shadow = TRUE),
            
            f7Tabs(
                id = "tabs",
                animated = TRUE,
                f7Tab(title = "Load and Summarise", tabName = "SummaryData", icon = f7Icon("folder"),
                      active = TRUE,
                      f7Card(
                          f7File(inputId="fileread", label = "Load PDC Files", multiple = TRUE, accept = ".pdc", width = "100%"), 
                          width = 10
                      ),
                      f7Card(
                          title = "Summary Table",
                          f7SmartSelect(inputId = "dataCol", label = "", choices = c("DOB","UBDIA" = "DUB","OWDensity", "CoreDensity","DiscDensity"), openIn = "sheet", multiple = TRUE),
                          tableOutput("summaryData")
                      )
                ),
                f7Tab(title = "Car specs", tabName = "Tab3", icon = f7Icon("layers_alt"),
                      f7Card(
                          title = "Car Specifications",
                          f7SmartSelect(inputId = "variable", label = "Select variables:", choices = c("Cylinders" = "cyl", "Transmission" = "am", "Gears" = "gear"), openIn = "sheet", multiple = TRUE),
                          tableOutput("data")
                      )
                )
            ) # Close f7Tabs
        ) # Close f7TabLayout
    ), # Close f7Page
  
  
  server = function(input, output, session) {
      # update tabs depending on side panel
      observeEvent(input$menu, {
          updateF7Tabs(id = "tabs",
                       selected = input$menu,
                       session = session)
      })
      Version         <- "Version 5.3.0"
      
      #Load PrMOE and PrAWV models
      MOE.rf  <<- readRDS("models/MOEApr24_RF.rds")
      AWV.rf  <<- readRDS("models/AWVApr24_RF.rds")
      MOE.seg.reg <<- readRDS("models/SegmentUSMOE_SPRC_reg.rds")
      
      #Establish reactive values
      values <- reactiveValues(
          traceChoices    = c("None read in yet")
          ,Standardise_sw = T
          ,traceNum       = NULL
          ,BaselineMethod = "NonLinear"
          ,rVar_sw        = FALSE
          ,RingMethod     = "Interval.percent"
          ,reRunMethod    = FALSE
          ,callerID       = NULL
          ,trimTrace      = "No"
          ,pithAllocate   = "No"
          ,manualAllocate = "NoA"
          ,MOEmodel      = "Young"
          ,RadialMOEMethod= "Std"   #Default should be "Std" testing here at the moment
          ,areaWeight     = FALSE
          ,rSlope         = 7.81
          ,rIntcpt        = 256.2      #Slope and Intcpt for the Torque => density regression
          ,RPMCoef        = -0.786
          ,denThr         = NULL
          ,ringCols       = c(1:9) #c(1:2,5:10,14:17) #defines which columns in the AnnualRingData frame are displayed. rest are used to determine regVar
          ,pNum           = 9         #
          ,df             = NULL
          ,ResourceList   = c("RadiataPine","SouthernPine", "Eucalypt", "Other")
          ,speciesSelect  = "RadiataPine"
          ,whichEquation  = "Linear"  #Control to determine whether the coefficients displayed are linear of natural log
          ,fixCoef        = FALSE     #Control to determine whether the Coef's displayed in the window are used Yes = TRUE
          ,useResiCoef    = TRUE
          ,useRPMCoef     = TRUE
          ,zoom.x         = 1
          ,traceMaster    = 1
          ,saveCorrTrace  = FALSE
          ,ResiSilviScan  = "Resi"
      )
      
      #May need to load PrAWV models here also
      globalList <- reactiveValues(
          fileName        = "FileName"
          ,sortOrder      = NULL
          ,licenseNotice  = F
          ,WebSite        = "FQ"
          ,version        = "Version 5.3.0"
          ,RemoteSha      = "RemoteSha"
          #,ResiInfo       = read.csv(system.file("extdata", "ResiIDcodes.csv", package = "FQResi"))
          #,ResiLicense    = read.csv("inst/extdata/ResiLicense.csv")#fread("inst/extdata/ResiLicense.csv")#
       )  
      
      observeEvent(input$fileread,{
          #print("Start FileRead")
          callerID <- "fileRead"        
          inFile <- input$fileread
          if (is.null(inFile)) {return()}

          #Need to delete any existing data and clear table.  This prevents double entries with the same file
          values$df             <- NULL
          values$FRdf           <- NULL
          values$regVar         <- NULL
 
          fName <- inFile$name[1]  #For error meassage line 617
          n <- length(inFile$datapath)
          
          #trying an lapply application
          tData <- lapply(inFile$datapath, TraceReader)
          
          #compile the sData table
          sData <- rbindlist((lapply(tData,"[[", "sData")))
          
          #extract DR traces into list
          resTraces <- lapply(tData,`[`, c('trace'))
          #extract FR traces into list
          feedTraces <- lapply(tData,`[`, c('feedTrace'))
          
          rm(tData)
        
          #Fill sData with necessary info
          sData$Resource       <- values$speciesSelect
          sData$MOEmodel       <- "2024.rf"
          sData$BaselineMethod <- "NonLinear"
          sData$Standardised   <- values$Standardise_sw
          sData$WebSite        <- globalList$WebSite
          sData$Version        <- globalList$version
          sData$RemoteSha      <- globalList$RemoteSha
          sData$RingMethod     <- "10%" #values$RingMethod
          sData$EqnType        <- values$whichEquation
          sData$rSlope         <- values$rSlope
          sData$rIntcpt        <- values$rIntcpt
          sData$RadialMOEMethod<- values$RadialMOEMethod
          
          #print(paste(sData$ResiCode[1],": " ,sData$FileName[1]), "Mean DR = ", mean(unlist(resTraces[1])))
          #Initialise  variables
          ringData <- data.table()
          rPos     <- data.table()
          regVar   <- data.table()
    
          withProgress(message = 'Processing Traces', value = 1, min = 1, max = n, {
              for (i in 1:n) {
                  #---  Process the Resi trace  #************** In "ResiFunctionsProcessing.R"
                  
                  traceData <- list(sData = sData[i], trace = resTraces[i], feedTrace = feedTraces[i],
                                    RPMCoef=list(a = -0.786, b = 1))
#browser()
                  ### Input Trace process loop ####
                  traceData     <- WoodPropertyCalculation(traceData, FALSE, callerID)
                  
                  #Update sData
                  sData[i] <- traceData$sData
                  
                  #Update ringData
                  ringData <- rbind(ringData,traceData$ringData)
                  
                  #update ring positions
                  rPos <- c(rPos, list(traceData$rPos))
                  
                  #update the regression variables if collected
                  regVar  <- rbind(regVar,traceData$regVar)
                  #
                  setProgress(i, detail = paste(i, " of ", n))
              }
              #browser()
              updateTextInput(session, "slope", value = values$rSlope)
              updateTextInput(session, "intcpt", value = values$rIntcpt)
              updateSelectInput(session, inputId = "specList", selected = values$speciesSelect)
              
              #  *** Check if any traces are collected at a low RPM and alert user
              if (any(as.integer(sData$RPM) < 2500)) {
                  message = "In some traces the RPM is lower than 2500. Density predictions may be less accurate"
                  shinyalert(type = "info",  title = "Notification",
                             text = message,
                             closeOnEsc = TRUE,closeOnClickOutside = TRUE,html = FALSE,
                             showConfirmButton = TRUE,showCancelButton = FALSE,confirmButtonText = "OK",confirmButtonCol = "green",
                             timer = 0,imageUrl = "",animation = TRUE
                  )}
              
              
              
          }) #end For loop
          
          #Identify any "TraceofZeros in trace type and delete from descriptors, rPos ,resTraces and feedtraces
          # i.e. make sure the data sets all link by trace number.
          #   confirm by checking list lengths
#browser()      
          delTrace <- (which(sData$TraceType == "Zeros"))
          if (length(delTrace == 0)) {
              sData      <- sData[-delTrace]
              resTraces  <- resTraces[-delTrace]
              feedTraces <- feedTraces[-delTrace]
              rPos       <- rPos[-delTrace]
          }
          #browser()
          names(resTraces)   <- sData$FileName
          names(feedTraces)  <- sData$FileName
          #
          values$df          <- sData
          values$ringData    <- ringData
          values$resTraces   <- resTraces
          values$feedTraces  <- feedTraces
          values$rPos        <- rPos
          values$regVar      <- regVar
          values$traceNum    <- 1   #Set the first plot as the first trace
          values$traceList   <- c(1:length(values$df$FileName))
          values$xmx         <- length(values$resTraces[[1]])/10
          values$xmn         <- 0
          
          #Check the percentage of traces where the drill offset is too high
          drillOffset <- min(0,round(length(which(values$df$OffsetDrill > 600)) / length(values$df$OffsetDrill)*100,0))
          
          if (drillOffset > 5) {
              message = paste("OffsetDrill on",  drillOffset, "% of the traces is greater than 600. If this occurs frequently,
                                    check Resi operation, clean telescope and discuss with IML if a service is required")
              shinyalert(title = "Warning",
                         text = message,
                         confirmButtonText = "Acknowledge and proceed",
                         confirmButtonCol = "orange")
          }
          
          rm(resTraces,ringData,traceData,sData,feedTraces,
             inFile,regVar,rPos,callerID,delTrace,fName,i,n,)

      })
      
      # sData table
      output$summaryData <- renderTable({
          if (is.null(values$df$FileName) == T) return()
          #req(input$dataCol)
          df <- (values$df)
          browser()
          #result <- df[,c("FileName", "TreeID", "DOB", "DUB", "CoreDensity", input$dataCol), drop=FALSE] |> as.data.frame()
          df[,c(1,2,3,4,6,9,10,11,13:15)]
      }, ,server = FALSE
      ,selection = 'single'
      ,rownames=TRUE
      #,colnames = TRUE
      ,extensions = c('FixedColumns',"FixedHeader")
      ,options = list(scrollX = TRUE
                      ,scrollY = '600px'
                      ,paging=TRUE, pageLength = 100#, dom = 't'
                      #,fixedHeader=TRUE                  #https://datatables.net/forums/discussion/47395/fixedheader-and-scrollx
                      ,fixedRow     = list(topRow = 1)
                      ,fixedColumns = list(leftColumns = 1)
                      ,sort = TRUE
                      #,fillContainer = TRUE
      ))

    # datatable
    output$data <- renderTable({
        #browser()
        # 1. Ensure input exists and is not empty
        #req(input$variable)
        
        # 2. Subset mtcars based on selected variables
        # input$variable returns a character vector of the selected keys (e.g., "cyl", "am")
        # We select those columns from mtcars
        result <- mtcars[,c("mpg", input$variable), drop = FALSE]
        result
    }, rownames = TRUE)
    
    
  } #end server/R
)
