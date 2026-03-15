
#' Reads in Individual Files from IML Resi PD series Power drills
#'
#' @description This function co-ordinates the reading of files
#'     produced by the IML Resi PD-series power drills as TXT, RGP, or
#'     PDC files. It is developed for use with a call from shiny
#'     server for use with a web-based processing platform
#'
#' @param datapath List of paths from inFile$datapath
#' @param fName List of filenames from inFile$name
#' @param Standardise_sw Logical indicating whether traces will be standardised (Default = TRUE)
#'
#' @return list(resTraces, feedTraces, descriptors)
#' @export
TraceReader <- function(datapath, fName = NA) {
#browser()
    if (is.na(fName)) fName <- basename(datapath) # shortcut for local files

  # Extract the filetype here (txt or rgp) and use this to define filetype on the fly
  nm <- unlist(strsplit(fName, ".", fixed = TRUE))
  fType <- nm[length(nm)]

  switch(fType,
    "dpa" = {
      dp <- dpload(datapath)
      rinnTrace <- dpdetrend(dp, type = "linear")$data$amplitude

      # Bin the trace down to 0.1mm sampling intervals from 0.01mm
      len <- length(rinnTrace) # Determine the number of array elements
      pos <- c(1:len)
      scl <- 10
      bin <- as.integer((array(1:len - 1) + scl) / scl) # Prepare a category where indices represent 0.1 mm bins
      pos1 <- (tapply(pos, bin, mean) + 0.5125) # 0.125 makes the pos in the middle of the bin for 1 mm scale
      returnData <- NULL
      returnData$trace <- as.numeric(tapply(rinnTrace, bin, mean))

      # collect the sData
      mData <- dp$footer
      dateTime         <- paste(mData$Date, ":", mData$Time) # Need to get this better
      sData <- Define.sData()
      sData$FileName = fName
      sData$TreeID = "dpa"
      sData$ResiCode  = "RinnTech"
      sData$FeedSpeed = NA
      sData$RPM       = NA
      sData$SampleDate = dateTime
      sData$Comment    = "Ignore Feedspeed and RPM metrics for RinnTech"
      returnData$sData <- sData
      feedTrace <- integer(0)
    },
    "pdc" = {
      # PDC JSON files are proaduced from the IML iPad app and based
      # on the RGP JSon format with some differences
      con <- file(datapath)
      on.exit(close(con), add = TRUE)
      textData <- rjson::fromJSON(file = con, method = "C")
      returnData <- rgpRead(fName, textData, callerID = "PDC")
    },
    "rgp" = {
      rgpType <- rgpCheck(datapath)
      if (rgpType == FALSE) {
        returnData <- rgpReadBinary(datapath, fName, "rgpReadBinary")
      } else {
#browser()
        con <- file(datapath)
        on.exit(close(con), add = TRUE)
        json_str <- paste(readLines(con, warn = FALSE), collapse = "")
        # Check for quote marks in id field between - "idNumber\": \" and \",\"remark\
        s <- stringr::str_locate(json_str, 'idNumber\": \"')[2] + 1
        f <- stringr::str_locate(json_str, "\",\"remark")[1] - 1
        id <- substr(json_str, s, f)

        x <- stringr::str_replace_all(substr(json_str, s, f), '[\"]', "-")
        #remove asterisks from string
#browser()
        x <- stringr::str_replace_all(x, "[*]", "-")

                  #ifelse(length(x) > 3,
        #str_replace cannot handle empty x variable
        if (x == "") {x <- "EmptyIdField"; id <- "EmptyIDField"}

        if (length(id) < 2 | id == "") {x1 <- gsub(id, x, json_str, fixed = TRUE)}
               #, x1 <- json_str)
        textData <- rjson::fromJSON(x1, method = "C") # https://stackoverflow.com/questions/2617600/importing-data-from-a-json-file-into-r
        returnData <- rgpRead(fName, textData, callerID = "RGP")
      }
    },
    "txt" = {
      con <- file(datapath)
      on.exit(close(con), add = TRUE)
      # May need to upgrade this for newer txt file formats
      # textData   <- read.csv(datapath, blank.lines.skip = FALSE)   #csv skips blank lines
      # textData    <- data.table(readLines(paste(con, sep = "")))
      textData <- data.table(readLines(con))
      returnData <- txtRead(fName, textData)
    },
    {warning(sprintf("%s: unknown fType (dpa, pdc, rgp, txt)", fType))}
  )


  # Check if trace is an air trace and if so return an error message
  trace <- returnData$trace
  mnTrace <- as.numeric(max(1, max(trace[which(trace != 0)])))
  if (mean(mnTrace) <= 5) {
    message <- paste(returnData$sData$FileName, " Probably an empty trace")
    Alert("info", "Problem with trace", message, "red")
    # ideally we want to delete that trace from the list and notify the user that has been done
    # either add a trace of zeros or (preferably) return o the server function and delete that trace
    #   from the inFile structure and continue with the upload
    returnData$trace <- as.numeric(100)
    stop(message, .call = T, domain = NA)
  }

  return(returnData)
}


#' Processes the header from JSON formatted RGP files
#' @description Called by FQResiTraceReader and passed the header of
#'   the RGP file, this function extracts the necessary descriptive
#'   data identifying the instrument used, time, date and location (if
#'   available) when the file was made and the various sampling
#'   conditions used.
#'
#' @param fName Filename identifying the individual trace filename (*.rgp).
#' @param header Text variable extracted from the JSON format file with the "header" tag.
#' @param callerID Text variable identifying which original format was used to call this function
#'
#' @return data table of extracted information
#' @export
#'

rgpRead <- function(fName, textData, callerID) {
  sData <- Define.sData()
  sData$FileName <- fName

  header <- (textData$header)
  # browser()
  sData$ResiCode <- as.character(header$snrMachine)
  sData$TreeID <- as.character(header$idNumber)
  sData$FeedSpeed <- as.integer(as.character(header$speedFeed)) * 10
  sData$RPM <- as.integer(as.character(header$speedDrill))
  switch(callerID,
    "RGP" = {
      collectTime <- paste0(header$timeHour, ":", header$timeMinute, ":", header$timeSecond)
      collectDate <- as.Date(paste0(header$dateYear, "-", header$dateMonth, "-", header$dateDay))
    },
    "PDC" = {
      d <- unlist(strsplit(header$dateTime, "-"))
      collectTime <- data.table::as.ITime(d[2])
      collectDate <- as.Date(d[1], format = "%Y%m%d")
      # location    <- header$location
    }
  )
  sData$SampleDate <- strftime(paste(collectDate, " ", collectTime), format = "%Y-%m-%d %H:%M:%S", tz = "")
  sData$Comment <- strtrim(header$remark, 48)

  sData$OffsetDrill <- header$offsetDrill
  sData$OffsetFeed <- header$offsetFeed

  location <- header$location
  if (is.null(location)) {
    sData$Lat <- "Not Available"
    sData$Lon <- "Not Available"
  } else {
    x <- unlist(strsplit(location, "°"))
    sData$Lat <- stringr::str_trim(x[1])
    x <- unlist(strsplit(x[2], "S,"))
    sData$Lon <- stringr::str_trim(x[2])
  }

  resTrace <- unlist(textData$profile$drill) #* 100
  feedTrace <- unlist(textData$profile$feed) #* 100

  returnData <- list(sData = sData, trace = resTrace, feedTrace = feedTrace)
  return(returnData)
}



#' Processes Resi Trace delivered as a text file
#' @description Called by FQResiTraceReader and passed the header of
#'   the TXT file. In older versions of the IML firmware (pre firmware
#'   version 1.32) RGP files were in binary format and needed to be
#'   converted to text using the PDToolsPro software supplied by
#'   IML. It is rarely used now. This function takes the text header
#'   data from that TXT file and extracts the necessary descriptive
#'   data identifying the instrument used, time, date and location (if
#'   available) when the file was made and the various sampling
#'   conditions used.
#'
#' @param fName Filename identifying the individual trace filename (*.txt).
#' @param textData Text variable extracted from the TXT input file (first 130 rows of data).
#'
#' @return data table of extracted information
#' @export

txtRead <- function(fName, textData) {
  sData <- Define.sData()
  sData$FileName <- fName
#browser()
  # textData <- as.character(textData[[1]])

  ### There are three possibilities that this function needs to accomodate
  # 1. A version 1.32 or earlier produced by the Old PDTools pro where the text file is a long single column string
  # 2. A version 1.32 or earlier produced by the new PDTools Pro where the text file is similar to the
  # 3. A version 1.75 or later produced by the new PDTools Pro.

  # Old PDToolsPro the firmware version is in element 1
  # New PDToolsPro the firmware version is in element 3

  # firmware <- textData[3]  #if firmware not in 3 returns "NA_real_". Text file produced by older version of PDTools
  firmware.version <- NULL
  len <- lengths(strsplit(as.character(textData[4]), " ")) # lengths(strsplit(textData[[3]]," "))

  # ver <- as.numeric(textData[3])

  if (len > 1) {
    format <- "old"
  } else {
    # ifelse(firmware < 1.75, firmware.version <- "old", firmware.version <- "new")
    format <- "new"
  }

  switch(format,
    "old" = { # ver = 1.32 or earlier
      sData$ResiCode <- as.character(textData[3])
      sData$TreeID <- as.character(textData[6])
      sData$FeedSpeed <- as.integer(as.character(textData[15]))
      sData$RPM <- as.integer(textData[16])
      collectDate <- as.character(textData[7]) # , "%d.%m.%Y")
      collectTime <- as.character(textData[8])
      sData$SampleDate <- paste(collectTime, as.character(collectDate))
      # dateTime         <- as.character(as.POSIXlt(paste(collectDate,collectTime)))
      sData$Comment <- strtrim(textData[124], 24)
      tLen <- as.integer(textData[14]) + 130 # Need to work out which it is
      sData$OffsetDrill <- as.integer(textData[18])
      sData$OffsetFeed <- as.integer(textData[17])

      combData <- unlist(textData[130:tLen])
      combData <- unlist(strsplit(combData, ";"))
      # Identify if trace has forward speed as well as resistance traces.
      type <- as.integer(ceiling(length(combData) / tLen))
      # browser()
      if (type == 1) {
        resTrace <- na.omit(as.integer(unlist(combData)))
        feedTrace <- NULL
      } else {
        resTrace <- combData[c(T, F)] # Resistance profile
        feedTrace <- combData[c(F, T)] # Forward Resistance profile
      }
      resTrace <- as.numeric(resTrace) / 100
      feedTrace <- as.numeric(feedTrace) / 100
    },
    "new" = {
      sData$ResiCode <- as.character(textData[3])
      sData$TreeID <- as.character(textData[9])
      sData$FeedSpeed <- as.integer(textData[23]) * 10 # *10 converts to mm/min to match the previous firmware
      sData$RPM <- as.integer(textData[24])
      collectTime <- paste0(textData[14], ":", textData[15], ":", textData[16])
      collectDate <- as.Date(paste0(textData[11], "-", textData[12], "-", textData[13]))
      sData$SampleDate <- strftime(paste(collectDate, " ", collectTime), format = "%Y-%m-%d %H:%M:%S", tz = "")
      # print(paste("Line 202 | Collect Date:",collectDate));print(collectTime)
      sData$Comment <- strtrim(textData[10], 24)
      tLen <- as.numeric(as.character(textData[22])) * 10
      sData$OffsetDrill <- as.integer(textData[26])
      sData$OffsetFeed <- as.integer(textData[25])

      resTrace <- as.numeric(unlist(strsplit((textData$V1[253]), ",")))
      feedTrace <- as.numeric(unlist(strsplit((textData$V1[254]), ",")))
      # plot(feedTrace, type = "l")
      # Generally newer files with feed resistance traces will be read as RGP
    }
  )

  returnData <- list(sData = sData, trace = resTrace, feedTrace = feedTrace)
}


#*****************************************
#' Defines Structure of the Resi Trace Summary Information
#' @description To ensure a consistent structure the sData table is defined here.  New columns can be added or unsed ones deleted
#'
#' @return sData
#' @export
#'
Define.sData <- function() {
 #browser()
  sData <- data.table(
    FileName = "",
    TreeID = "",
    ResiCode = "",
    SampleDate = "",
    MOEmodel = "", # GD, would AWVModel be better here?
    TraceType = "",
    CheckStatus = "TBD",
    AdjustRecord = "Auto",
    DOB = numeric(1),
    UBDIA = numeric(1),
    BarkThickness = numeric(1),
    ExitBarkThickness = numeric(1),
    OWDensity = numeric(1),
    CoreDensity = numeric(1),
    DiscDensity = numeric(1),
    PrAWV = numeric(1),
    PrMOE = numeric(1),
    EntryRadius = numeric(1),
    EntryDensity = numeric(1),
    ExitRadius = numeric(1),
    ExitDensity = numeric(1),
    ## ,DensityProportion = numeric(1)
    Decay = numeric(1),
    DiscArea = numeric(1),
    RingCount = numeric(1),
    BLCorrDen = numeric(1),
    FeedSpeed = numeric(1),
    RPM = numeric(1),
    Comment = "",
    Resource = "",
    BaselineMethod = "",
    Standardised = logical(1), # this needs to be of logical not "", e.g. TraceStandardised()
    RingMethod = "",
    OffsetDrill = 0,
    OffsetFeed = 0,
    Lat = numeric(1),
    Lon = numeric(1),
    WebSite = "",
    Version = "",             #Allows code processing differently according to version
    RemoteSha = "Github Repo version",
    rSlope = 1,  # These need to be included in desc for
    rIntcpt = 0, #  when manual processing uses different values
    EqnType = "Linear",
    RadialMOEMethod = "Std" #Added 26 May 2025 Version
  )
  return(sData)
}



#******************************************************************************
####  RESI ID LIST
####
####  Lists serial numbers of Resis able to use specific sites and there calibration details as
####  available
####
#******************************************************************************

#' Title Checks the serial number of the Resi instrument
#'     Used to determine whether Resi is licensed for this application
#' @param ResiID Resi Serial number from trace
#' @param webSite WebSIte being used
#'
#' @return list(rCheck,Slope, Intcpt, species, status, expDate)
#' @export
ResiID <- function(ResiID, R.Info, webSite) {
  status <- "OK"
  if (webSite == "Eucalypt" | webSite == "TasNet") ResiID <- "PD400-0000" # makes sites available to all Resi instruments
  # browser()
  # work out which column in ResiInfo = the website name
  SiteNo <- which(colnames(R.Info) == webSite) # | WebSiteList$WebSite == "FQ")

  subList <- R.Info[which(R.Info[[SiteNo]] == "Yes"), ]
  rCheck <- as.integer(which(subList$ResiCode == ResiID))

  if (length(rCheck) == 0) {
    message <- paste("Invalid Instrument Serial Number: ", ResiID)
    title <- "License Notification"
    type <- "info"
    status <- "invalid"
    stop(message, call. = FALSE, domain = NA)
    # return()
  } else {
    # Check the license expiry date
    resiData <- as.data.table(subList[rCheck, ])
    if (length(resiData$Slope) > 1) print("Two Resis with same serial number")
    Slope <- resiData$Slope
    Intcpt <- resiData$Intercept

    expDate <- as.numeric(as.Date(resiData$LicenceExpires[1], tryFormats = c("%Y-%m-%d", "%d/%m/%Y")))
    currentDate <- as.numeric(Sys.Date())
    if (expDate < currentDate) { # & licenseNotice == F) {
      if (expDate < currentDate - 61) {
        type <- "error"
        message <- paste("License for this instrument (", ResiID, ") expired on", as.Date(expDate, origin = "1970-01-01"))
        stop(message, call. = FALSE, domain = NA)
      }
      status <- "expired"
    }
  }

  if (is.na(Slope) == T) {
    Slope <- 8.3
  }
  if (is.na(Intcpt) == T) {
    Intcpt <- 200
  }
  species <- resiData$Resource[1]

  return(list(rCheck = rCheck, Slope = Slope, Intcpt = Intcpt, species = species, status = status, expDate = expDate))
}
