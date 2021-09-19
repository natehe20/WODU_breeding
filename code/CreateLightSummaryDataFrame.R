  # General_variables -------------------------------------------------------
  # l.correct <- "breed"
  
  
  mainDir <- file.path("data")

    
  setwd(mainDir)
  # load the nesting periods determined from previous analysis
  load(file.path("luxsum","nestingDatesList.RData"))
  # read in csv with the summary information - narrow to one species
  geo_df <- read.csv("CMW_geo.csv",stringsAsFactors = FALSE)
  
  fileList <- list.files(mainDir)
  
  # Get a subset geo_df in order to run for just the individuals you want 
  # (listing species, "_" and metal band number ie "WODU_110552589")
  # subsetted with variable runInd - written out or calculated by looking for certain files - code toward bottom
  # runInd <- c("WODU_110563232","WODU_110594323","WODU_110594791","WODU_116538680","WODU_116538682","WODU_116538717","WODU_116591708")
  # geo_df <- geo_df[match(substr(runInd,6,14),geo_df$Metal),]
  
  # Run a quick check that the light data recorded past the day geo was attached
  # lcheck <- which(geo_df$Turned.on.date <= geo_df$Date.of.Initial.fitting & geo_df$Date.of.Initial.fitting <= geo_df$last.record.date)
  lcheck <- function (df=geo_df) {
    # Additional Check vector (arunInd) gives the individual birds that have lux data past geo attachment
    arunInd <- which(as.POSIXct(strptime(df$Date.of.Initial.fitting,"%m/%d/%Y",tz = "GMT")) < 
                       as.POSIXct(strptime(df$last.record.date,"%m/%d/%Y",tz = "GMT")))
    df <- df[arunInd,]
    return(df)
  }
  
  # geo_df <- lcheck()
  
  
  index.bird <- which(fileList %in% paste0(geo_df$Species,"_",geo_df$Metal))
  birdList <- fileList[index.bird]
  
  geo_df <- geo_df[order(geo_df$Metal),]
  if (!all(birdList == paste0(geo_df$Species,"_",geo_df$Metal))) {
    message("Geolocator data frame (geo_df) order does not match birds with birdList")
  }
  
  # setwd(spDir)
  
  
  
  # The next step --------
  
  # birdFold=birdList[4]
  # speciesWD=spDir
  # nestingTimes=nestTimes
  # timeStepLength=30
  # summaryWindowLength=5
  # startBuff=21
  # endBuff=14
  
  createLightSummary <- function(birdFold,timeStepLength,summaryWindowLength,mDir=mainDir,
                                 nestingTimes=nestTimes,startBuff=21,endBuff=14) {
    require(suncalc)
    
    print(birdFold)
    
    lfoldbreed <- "light_breed"
    load(file.path(mDir,birdFold,lfoldbreed,"LUXdataCombined_values.RData"))
    
    # LUXdata$DateTimePacific <- LUXdata$Date
    # attributes(LUXdata$DateTimePacific)$tzone <-  "US/Pacific"
    # LUXdata$DatePacific <- LUXdata$Date
    
    LUXdata$DateTime <- LUXdata$Date
    # attributes(LUXdata$Date)$tzone <-  "US/Pacific"
    LUXdata$Date <- as.Date(LUXdata$Date,tz="US/Pacific")
    
    
    # nestingPeriod=nestingTimes[[birdFold]]$nestPeriodList[[2]]
    # IndFullNestPeriod=nestingTimes[[birdFold]]$nestPeriod
    # lux=LUXdata
    # slotLength=timeStepLength
    # summWindow=summaryWindowLength
    # sBuff=startBuff
    # eBuff=endBuff
    
    
    nestingPeriodLightSummary <- function(nestingPeriod,IndFullNestPeriod,lux=LUXdata,slotLength=timeStepLength,
                                          summWindow=summaryWindowLength,sBuff=startBuff,eBuff=endBuff) {
      nestBufferPeriod <- seq(nestingPeriod[1] - sBuff,nestingPeriod[2] + eBuff,1)
      
      npSd <- IndFullNestPeriod[seq(1,length(IndFullNestPeriod),2)]
      npEd <- IndFullNestPeriod[seq(2,length(IndFullNestPeriod),2)]
      
      closeNP <- npSd %in% seq(nestingPeriod[2],nestingPeriod[2] + (sBuff + eBuff),1)
      if (any(closeNP)) {
        nestBufferPeriod <- seq(nestingPeriod[1] - sBuff,npSd[closeNP] - (sBuff + 1),1)
      }
      
      lux <- lux[lux$Date %in%  nestBufferPeriod,]
      twlTimesDF <- getSunlightTimes(date=nestBufferPeriod,lat=39.47,lon=-118.82,
                                     keep=c("sunrise","sunset"),tz="US/Pacific")#tz="GMT"
      # twlTimesDF <- getSunlightTimes(date=unique(lux$Date),lat=39.47,lon=-118.82,
      #                                keep=c("sunrise","sunset"),tz="US/Pacific")#tz="GMT"
      
      # twlDF=twlTimesDF[1,]
      # ldata=LUXdata
      # timestep=5
      # summLength=summWindow
      
      
      slotMeansByDay <- function(twlDF,ldata,timestep=NULL,summLength=summWindow) { #,sunrise=TRUE) {
        # timestep <- 30 #time in minutes for size of light summary bins
        # if (sunrise) {
        #   dawnSlots <- seq(twlDF$sunrise[1],twlDF$sunrise[1] + 5*3600,timestep * 60)
        #   dawnslotNum <- cut(as.numeric(ldata$DateTime),as.numeric(dawnSlots),include.lowest=TRUE)
        #   dawnMean <- tapply(ldata$Light,dawnslotNum,mean)
        # } else {
        #   duskSlots <- seq(twlDF$sunset[1] - 5*3600, twlDF$sunset[1],timestep * 60)
        #   duskslotNum <- cut(as.numeric(ldata$DateTime),as.numeric(duskSlots),include.lowest=TRUE)
        #   duskMean <- tapply(ldata$Light,duskslotNum,mean)
        # }
        dawnSlots <- seq(twlDF$sunrise,twlDF$sunrise + summLength*3600,timestep * 60)
        duskSlots <- seq(twlDF$sunset - summLength*3600, twlDF$sunset,timestep * 60)
        
        dawnslotNum <- cut(as.numeric(ldata$DateTime),as.numeric(dawnSlots),include.lowest=TRUE,
                           labels = paste("Rise Period",1:((summLength*3600) / (timestep*60))))
        duskslotNum <- cut(as.numeric(ldata$DateTime),as.numeric(duskSlots),include.lowest=TRUE,
                           labels = paste("Set Period",1:((summLength*3600) / (timestep*60))))
        
        dawnMean <- tapply(ldata$Light,dawnslotNum,mean)
        duskMean <- tapply(ldata$Light,duskslotNum,mean)
        
        dawnMax <- tapply(ldata$Light,dawnslotNum,max)
        duskMax <- tapply(ldata$Light,duskslotNum,max)
        
        # x=split(ldata$Light,duskslotNum)[[5]]
        # y=duskMax[5]
        # dawnLowCount <- sapply(lapply(split(ldata$Light,dawnslotNum),FUN=function(x) x < 0.5),sum)
        # duskLowCount <- sapply(lapply(split(ldata$Light,duskslotNum),FUN=function(x) x < 0.5),sum)
        # 
        # 
        # c(dawnLowCount,duskLowCount)
        
        
        # dawnLowCount <- sapply(mapply(split(ldata$Light,dawnslotNum),dawnMax,FUN=function(x,y) x < (y/3),SIMPLIFY=FALSE),sum),
        # duskLowCount <- sapply(mapply(split(ldata$Light,duskslotNum),duskMax,FUN=function(x,y) x < (y/3),SIMPLIFY=FALSE),sum)
        
        return(list(meanLight=c(dawnMean,duskMean),slotTimes=c(dawnSlots[-1],duskSlots[-1])))
        # return(list(meanLight=c(dawnMean,duskMean),slotTimes=c(dawnSlots[-1],duskSlots[-1])))
        # return(c(date=twlDF$date,dawnMean,duskMean))
      }
      # split(twlTimesDF,row(twlTimesDF)[,1])
      # meanLightDF <-  data.frame(t(sapply(split(twlTimesDF,row(twlTimesDF)[,1]),slotMeansByDay,lux,timestep=slotLength)))
      
      lightTimesList <- lapply(split(twlTimesDF,row(twlTimesDF)[,1]),slotMeansByDay,LUXdata,timestep=slotLength)
      meanLightDF <- data.frame(t(sapply(lightTimesList,function(x) x$meanLight)))
      slotTimesDF <- data.frame(t(sapply(lightTimesList,function(x) x$slotTimes)))
      threshCountDF <- data.frame(t(sapply(lightTimesList,function(x) x$threshCount)))
      
      
      colnames(slotTimesDF) <- colnames(meanLightDF)
      
      # This will help to better match the dates of the nesting periods when they are close together
      if(any(closeNP)) {
        nestDayNum <- c(-sBuff:-1,rep(0,length(seq(nestingPeriod[1],nestingPeriod[2],1))),1:as.numeric((npSd[closeNP] - (sBuff + 1))-nestingPeriod[2]))
      } else {
        nestDayNum <- c(-sBuff:-1,rep(0,nrow(twlTimesDF)- (sBuff + eBuff)),1:eBuff)
      }
      
      meanLightDF <- data.frame(date=as.Date(twlTimesDF$date,tz="US/Pacific"),
                       day=nestDayNum,
                       meanLightDF)
      
      slotTimesDF <- data.frame(date=as.Date(twlTimesDF$date,tz="US/Pacific"),
                                # day=c(-sBuff:-1,rep(0,nrow(twlTimesDF)- (sBuff + eBuff)),1:eBuff),
                                slotTimesDF)
      
      # meanLightList <-  lapply(split(twlTimesDF,row(twlTimesDF)[,1]),slotMeansByDay,lux,timestep=slotLength)
      # meanLightDF <- do.call(rbind,meanLightList)
      return(list(meanLightDF=meanLightDF,slotTimes=slotTimesDF))
      # return(list(meanLightDF=meanLightDF,twlTimes=twlTimesDF))
    }
    
    lightTimesOutList <- lapply(nestingTimes[[birdFold]]$nestPeriodList,nestingPeriodLightSummary,
                             nestingTimes[[birdFold]]$nestPeriod)
    
    summaryOutList <- lapply(lightTimesOutList,function(x) x$meanLightDF)
    summaryOutDF <- do.call(rbind,summaryOutList)
    summaryOutDF <- data.frame(bird=rep(birdFold,nrow(summaryOutDF)),summaryOutDF)
    # slotNum <- cut(as.numeric(LUXdata$DateTime),as.numeric(c(dawnSlots,duskSlots)),include.lowest=TRUE,right=FALSE)
    timesOutList <- lapply(lightTimesOutList,function(x) x$slotTimes)
    timesOutDF <- do.call(rbind,timesOutList)
    # timesOutDF <- data.frame(bird=rep(birdFold,nrow(timesOutDF)),timesOutDF)
    

    
    # require(maptools)
    # sunriset()
    
    # LUXsummary <- data.frame()
    # 
    # 
    # dawnSlots
    
    
    
    
    return(list(lightSumm=summaryOutDF,slotTimes=timesOutDF))
  }
  
  # lightSummaryList <- lapply(birdList,createLightSummary,timeStepLength=30,summaryWindowLength=5,
  #        speciesWD=spDir,nestingTimes=nestTimes)
  timeStepLength <- 5 #in minutes, the length of bins to summarize
  summaryWindowLength <- 5 # in hours, the length of time after sunrise and before sunset summarize
  lightSlotSummaryList <- lapply(names(nestTimes),createLightSummary,timeStepLength=timeStepLength,
                             summaryWindowLength=summaryWindowLength,mDir=mainDir,
                             nestingTimes=nestTimes)
  lightSummaryList <- lapply(lightSlotSummaryList,function(x) x$lightSumm)
  names(lightSummaryList) <- paste0(names(nestTimes),"_",timeStepLength,"_",summaryWindowLength)
  
  lightSummaryDF <- do.call(rbind,lightSummaryList)
  dim(lightSummaryDF)
  head(lightSummaryDF)
  
  # Check in LightSummaryDF for rows of duplicate dates for an individual bird
  birdDate <- paste0(lightSummaryDF$bird,"_",lightSummaryDF$date)
  if (length(which(duplicated(birdDate))) > 0) {
    warning(paste("LightSummaryDF has row with duplicate dates for the following individuals and dates:",
                  paste(birdDate[which(duplicated(birdDate))],collapse = ",")))
  } else {
    print("No duplicate rows")
  }
  # save the lightSummary (Mean) values by bin to csv
  write.csv(lightSummaryDF,file.path("luxsum",
                                     paste0("NestingLightSummary_",timeStepLength,"_",
                                            summaryWindowLength,".csv")),row.names = FALSE)
  
  # Get and save to RData the slot time boundaries
  slotTimesList <- lapply(lightSlotSummaryList,function(x) x$slotTimes)
  names(slotTimesList) <- names(nestTimes)
  
  save(slotTimesList,file=file.path("luxsum","SlotTimesList.RData"))
  
  # # Get and save to csv the count of lux values below a threshold by bin
  # lightCountList <- lapply(lightSlotSummaryList,function(x) x$)
  # names(lightCountList) <- names(nestTimes)
  # 
  # lightCountDF <- do.call(rbind,lightCountList)
  # dim(lightCountDF)
  # head(lightCountDF)
  # 
  # write.csv(DF,file.path(spDir,paste(species,"luxsum"),
  #                                    paste0("NestingLightCount_",timeStepLength,"_",
  #                                           summaryWindowLength,".csv")),row.names = FALSE)
  