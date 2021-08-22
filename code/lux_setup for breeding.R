#code to 'Monitoring waterfowl breeding activities with geolocators: a case study with Wood Ducks (Aix sponsa) in northern Nevada' by Cook et al., in prep

# General_variables -------------------------------------------------------



# mainDir <- "WODU Breeding"

# species <- "WODU" # Choose between CANV, MALL and WODU
# spDir <- file.path(mainDir,species)
mainDir <- file.path("data")

setwd(mainDir)

# read in csv with the summary information - narrow to one species
geo_df <- read.csv("CMW_geo.csv",stringsAsFactors = FALSE)
geo_df <- geo_df[geo_df$Species == species,]

fileList <- list.files(spDir)

index.bird <- which(fileList %in% paste0(geo_df$Species,"_",geo_df$Metal))
birdList <- fileList[index.bird]

geo_df <- geo_df[order(geo_df$Metal),]
if (!all(birdList == paste0(geo_df$Species,"_",geo_df$Metal))) {
  message("Geolocator data frame (geo_df) order does not match birds with birdList")
}

  
  
  # Create Start and End Dates List (seDatesList)----
  -library(readxl)
  ssDir <- file.path(startDir,"WODU data","NC spreadsheet")
  
  # geoSummary <- data.frame(read_excel(file.path(ssDir,"NC_geo_results_numbers.xlsx")))
  # geoSummary$ID <- paste0(geoSummary$Species,"_",geoSummary$ID)
  
  geoSummary <- data.frame(read.csv(file.path(ssDir,"NC_geo_results_numbers.csv"),stringsAsFactors = FALSE))
  geoSummary$ID <- paste0(geoSummary$Species,"_",geoSummary$ID)
  
  geoSummary$Date.of.Initial.fitting <- as.POSIXct(as.character(geoSummary$Date.of.Initial.fitting),format="%m/%d/%Y",tz="US/Pacific")
  geoSummary$shot.recovered.date <- as.POSIXct(as.character(geoSummary$shot.recovered.date),format="%m/%d/%Y",tz="US/Pacific")
  
  geoSummary$year.of.initial.fitting <- year(geoSummary$Date.of.Initial.fitting)
  geoSummary$year.of.shot.recovered <- year(geoSummary$shot.recovered.date)
  
  
  
  seDates <- geoSummary[geoSummary$Species == species & geoSummary$lux.file == "Y",c("ID","Geo.Code","Date.of.Initial.fitting","shot.recovered.date")]
  # seDates$Date.of.Initial.fitting <- as.numeric(seDates$Date.of.Initial.fitting)
  # seDates$shot.recovered.date <- as.numeric(seDates$shot.recovered.date)
  
  # Data frame must be sorted by individual and start date so they are all in chronological order by individual
  seCombDatesList <- lapply(split(seDates,seDates$ID), function(x) {
    # combRows <- x$Date.of.Initial.fitting[-1] %in% x$shot.recovered.date[-nrow(x)]
    if (nrow(x) > 1) {
      combRows <- abs(x$Date.of.Initial.fitting[-1] - 
                        x$shot.recovered.date[-nrow(x)]) < 32 &
      duplicated(x$Geo.Code)[-1]
    
    # Combine the rows with the same end date as the next start date
    # if (length(combRows) > 0) {
      for (i in 1:length(combRows)) {
        if (rev(combRows)[i] & !is.na(rev(combRows)[i])) {
          x$shot.recovered.date[rev(row(x)[,1])[2]] <- x$shot.recovered.date[rev(row(x)[,1])[1]]
          x <- x[-nrow(x),]
        }
      }
    
      for (j in 1:nrow(x)) {
        if (any(substr(x$ID[j],1,14) == substr(x$ID[-j],1,14))) {
          # rename so they are all unique ID
          efNum <- sum(substr(x$ID[j],1,14) == substr(x$ID[1:j],1,14))
          x$ID[j] <- paste0(substr(x$ID[j],1,14),"_",efNum)
        }
      }
    }
    x
  })
  
  seDatesRefined <- do.call(rbind,seCombDatesList)
  
  seDatesList <- lapply(split(seDatesRefined,row(seDatesRefined)[,1]),function(x) {
    list(sdate=x$Date.of.Initial.fitting[1],
         edate=x$shot.recovered.date[1])
    # list(sdate=as.Date(x$Date.of.Initial.fitting,origin="1899-12-31",tz="US/Pacific"),
    #      edate=as.Date(x$shot.recovered.date,origin="1900-01-01"))
  })
  names(seDatesList) <- seDatesRefined$ID
  
  
  
# Light for breeding setup process -----------------------------------------------------
  setupBreedLightData <- function(birdFold,startEndList=seDatesList,speciesWD=spDir,birdList=fileList[index.bird],skipPre=FALSE) {
    require(TwGeos)
    require(lubridate)
    try({
      if (substr(birdFold,15,15) == "_") {
        appendVal <- substr(birdFold,15,17)
        birdFold <- substr(birdFold,1,14)
      }
      
      birdNum <- which(birdList %in% birdFold)
      
      pb <- winProgressBar(paste(species,"Progress"),label = "__ of __")
      setWinProgressBar(pb,birdNum/length(birdList),label = paste0(birdFold," (",birdNum," of ",length(birdList),")"))
      
      l.folder <- paste0("light_","breed")
      
      if (skipPre) {
        if (file.exists(file.path(birdFold,l.folder,"Initial_values.RData"))) {
          close(pb)
          next
        }
      }
      
      
      if (!dir.exists(file.path(speciesWD,birdFold,l.folder))) {dir.create(file.path(speciesWD,birdFold,l.folder))}
      LUXfiles <- grep("LUXcombined",list.files(file.path(speciesWD,birdFold)))
      LUXdata <- read.csv(list.files(file.path(speciesWD,birdFold),full.names = TRUE)[LUXfiles],col.names = c("Date","Light"))
      # format the date so it can be used later in the analysis
      LUXdata$Date <- as.POSIXct(strptime(LUXdata$Date,format="%Y-%m-%d %H:%M:%S",tz="GMT"))
      attributes(LUXdata$Date)$tzone <- "US/Pacific"
      
      # Set the lowest light value to 1.
      LUX.min <- min(LUXdata$Light)
      LUXdata$Light[LUXdata$Light == LUX.min] <- 1
      
      # Take the log of light data # 
      LUXdata$Light <- log(LUXdata$Light)
      
      if (!exists("appendVal")) {
        pdfPlotName <- "LUX_startend_dates_plot.pdf"
        luxSaveName <- "LUXdata_values.RData"
        seListVal <- startEndList[[birdFold]]
      } else {
        pdfPlotName <- paste0("LUX_startend_dates_plot",appendVal,".pdf")
        luxSaveName <- paste0("LUXdata_values",appendVal,".RData")
        seListVal <- startEndList[[paste0(birdFold,appendVal)]]
      }
      
      
      pdf(file = file.path(speciesWD,birdFold,l.folder,pdfPlotName))
      offset=19
      lightImage(LUXdata, offset = offset, zlim = c(0,12))
      
      seListVal$sdate
      seListVal$edate
      hour(LUXdata$Date[1]) < offset
      if (hour(LUXdata$Date[1]) < offset) {
        abline(v = seListVal$sdate,lwd=2, col="yellow")
      } else {
        abline(v = seListVal$sdate + 86400,lwd=2, col="yellow")
      }
      # abline(v = as.POSIXct(strptime(geobreed_df$shot.recovered.date[i],"%Y-%m-%d",tz="GMT")),lwd=1, lty=3, col="yellow")
      # if (hour(LUXdata$Date[length(LUXdata$Date)]) > offset) {
      abline(v = seListVal$edate,lwd=2, lty=3, col="red")
      # } else {
      #   abline(v = seListVal$edate - 86400,lwd=2, lty=3, col="red")
      # }
      # abline(v = as.POSIXct(strptime(geobreed_df$x.nSdate[i],"%Y-%m-%d",tz="GMT")),lwd=1, lty=3, col="yellow")
      title(paste(birdFold," (",geo_df$Sex[birdNum],") Start(yellow) and End(red) dates",sep = ""))
      dev.off()
      
      
      
      #Edit luxdata to only within the start and end dates
      length(LUXdata$Date)
      
      
      
      
      
      LUXList <- list()
      for (i in 1:length(seListVal$sdate)) {
        length(which(LUXdata$Date > seListVal$sdate[i] + 3600)) &
        length(which(LUXdata$Date < seListVal$edate[i] - 3600))
        
        LUXList[[i]] <- LUXdata[LUXdata$Date > seListVal$sdate[i] + 3600 & 
                                  LUXdata$Date < seListVal$edate[i] - 3600,]
      }
      LUXdata <- do.call(rbind,LUXList)
      
      save(LUXdata,file = file.path(speciesWD,birdFold,l.folder,luxSaveName))
      
      close(pb)
    })
  }
  
  
  sapply(names(seDatesList),setupBreedLightData,birdList=birdList)
  
  #This opens all of the PDFs of the nesting start and end periods - Don't need to run this code but
  #the user should check these plots to check the start and end dates compared to the light data to make
  #any desired changes such as not using the data or making adjustments to dates in case of error
  
  # sapply(names(seDatesList)[1:50],FUN=function(birdFold) {
  #  if (nchar(birdFold) >= 16) {
  #     fileName <- file.path(spDir,substr(birdFold,1,14),"light_breed",paste0("LUX_startend_dates_plot_",
  #                           substr(birdFold,gregexpr("_",birdFold)[[1]][2]+1,nchar(birdFold)),".pdf"))
  #   } else {
  #     fileName <- file.path(spDir,birdFold,"light_breed","LUX_startend_dates_plot.pdf")
  #   }
  #   
  #   shell.exec(fileName)
  # })
  
  