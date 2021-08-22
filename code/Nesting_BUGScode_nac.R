###
# TEMPLATE CODE FOR DETERMINING NESTING BOUTS FROM DAY-SUMMARIZED LUX DATA
# K. Shoemaker and N. Cook
###

## Get the Lux daily sum for duration of lux files
## 2/6/18
## N.Cook

  # General_variables -------------------------------------------------------
# Enter the values here to name and draw from the correct folders
  #This whole section doesn't need to be run if keeping data from previous scripts
# load packages
# library(lubridate)

  # startDir <- "WODU breeding"
  mainDir <- file.path("data")
  # species <- "WODU" # Choose between CANV, MALL and WODU
  # spDir <- file.path(mainDir,species)
  
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
  
  
  
  # Get just the daily sum of light values --------------------------------------------
  l.correct <- "breed"
  
  
  getLuxSum <- function(birdFold,speciesWD=spDir,sp=species,birdList=fileList[index.bird],lcorr=l.correct) {
    runerror <- c()
    
    birdNum <- which(birdList %in% birdFold)
    pb <- winProgressBar(paste(sp,"Progress"),label = "__ of __")
    # update progress bar
    setWinProgressBar(pb,birdNum/length(birdList),label = paste("Processing Lux sums for ",birdFold," (",birdNum," of ",length(birdList),")",sep = ""))
    
    l.folder <- paste("light_",lcorr,sep = "")
    
    breedFileList <- list.files(file.path(speciesWD,birdFold,l.folder))
    luxdataNum <- grep("LUXdata_values",breedFileList)
    
    sumDailyLux <- function(luxdataFile,birdF=birdFold,lfold=l.folder,spWD=speciesWD) {
      tryCatch({
        load(file.path(spWD,birdF,lfold,luxdataFile))
        
        if (substr(luxdataFile,15,15) == "_" ) {
          appendVal <- substr(luxdataFile,15,16)
        } else {
          appendVal <- ""
        }
        
        matchDays <- which(LUXdata$Light != 0)
        LuxsumAll <- tapply(LUXdata$Light, as.Date(LUXdata$Date,tz="US/Pacific"), sum)
        trim <- tapply(LUXdata$Light, as.Date(LUXdata$Date,tz="US/Pacific"),length) >= 287
        LuxsumAll <- LuxsumAll[trim]
        
        Luxsum <- tapply(LUXdata$Light[matchDays],as.Date(LUXdata$Date[matchDays]),sum)
        
        Dates <- names(LuxsumAll)
        
        luxdf <- data.frame(dates=Dates,lux=LuxsumAll)
        luxsumWD <- file.path(spDir,paste(sp,"luxsum"))
        
        if(!dir.exists(luxsumWD)) {dir.create(luxsumWD)}
        write.csv(luxdf, file = file.path(luxsumWD,paste0(birdF,appendVal,".csv")))
        
      }, warning = function(warn) {
        runerror <- c(runerror,paste("warning on ",birdF," (birdNum=",birdNum,")",warn,sep = ""))
        print(paste("MY_WARNING: ",birdF,warn))
        close(pb)
      }, error = function(err) {
        runerror <- c(runerror,paste("error on ",birdF," (birdNum=",birdNum,")",err,sep = ""))
        print(paste("MY_ERROR: ",birdF,err))
        close(pb)
      })
      return(luxdf)
    }
    
    luxdf <- lapply(breedFileList[luxdataNum],sumDailyLux)
    
    
    close(pb)
    return(list(lux=luxdf,error=runerror))
  }
  
  system.time(
    luxsumOut <- lapply(birdList,getLuxSum)
  )
  
  runError <- lapply(luxsumOut,FUN = function(x) x$error)
  
  luxsumData <- lapply(luxsumOut,FUN = function(x) x$lux)
  names(luxsumData) <- birdList
  
  save(luxsumData,file = file.path(spDir,paste(species,"luxsum"),"LuxsumData.RData"))
  
  rm(list=ls())
  
  # Load packages -----------------------------------------------------------
  library(lubridate)
  library(jagsUI)


  # load Data ---------------------------------------------------------------
  # startDir <- "WODU breeding"
  mainDir <- file.path("data")
  # species <- "WODU" # Choose between CANV, MALL and WODU
  # spDir <- file.path(mainDir,species)
  
  setwd(file.path(spDir,paste(species,"luxsum")))
  
  allfiles <- list.files()[grep(paste0(species,"_"),list.files())]
  allfiles <- allfiles[grep(".csv",allfiles)]
  
  
  # Run breeding model for each bird ----------------------------------------
  pdf("Nesting_HistAndPlots.pdf")

  ## locate all "sum lux" files
  for (g in 1:length(allfiles)) {
    tryCatch({
      
      
      # allfiles <- list.files()[grep("WODU_",list.files())]
      
      # indNum <- g
      indName <- paste(sub(".csv","",allfiles[g]))
      
      df <- read.csv(allfiles[g])
      if(nrow(df) < 2) {
        message(paste("Only one day of data exists for ",indName,"- No analysis run"))
        rm(list=setdiff(ls(),c("allfiles","mainDir","species","spDir")))
        next
      }
      
      df$DATE <- ymd(df$dates)
      
      df$JDAY <- yday(df$DATE)
      
      
      
      ###
      ### Visualize data
      ###
      
      # plot(df$DATE,df$lux,type="l")
      
      
      ### Play with sine wave (R code)
      
      meanlight <- 1100
      amplight <- 320
      stretch365 <- (2*pi)/365    # convert to radians
      shift365 <- yday(mdy("3/20/2017")) - yday(mdy("12/20/2017"))+365
      
      luxfunction <- function(jdate,meanl=meanlight,ampl=amplight,sh365=shift365,st365=stretch365){
        ampl*sin((jdate-sh365)*st365)+meanl
      }
      
      
      
      ###
      ### BUGS CODE
      ###
      
      ### DATA FOR BUGS
      
      # ts: time series of daily total light readings
      # jday: julian day associated with each mean light reading in ts
      # ndays: total time series length
      
      # init.jday : first day of time series (julian day)
      # init.nesting : first day of possible nesting (julian day)
      # final.nesting : final day of possible nesting (julian day)
      
      
      ### KEY VARIABLES FOR BUGS
      
      # light.prec (and light.sd): day to day variability in light levels
      
      
      
      filename <- "nestingBUGS.txt"
      
      cat("

  model{

    ## FIXED PARAMETERS

    stretch365 <- 0.01721421
    shift365 <- 90
    init.nesting <- 60           # first possible nesting is around March 1 
    final.nesting <- 212         # last possible nesting is around July 31

    ## LIKELIHOOD

    
    for(day in 1:ndays){
      nonesting.light[day] <- amplight*sin((jday[day]-shift365)*stretch365)+meanlight                  # expected light based on sine wave
      exp.nesting[day] <- nesting.prob*can.nest[day]                                                    # expectation for daily nesting probability
      is.nesting[day] ~ dbern(exp.nesting[day])                                                         # determine if this day the bird is nesting
      expected.light[day] <- is.nesting[day]*nesting.light + (1-is.nesting[day])*nonesting.light[day]
      ts[day] ~ dnorm(expected.light[day],light.prec)                                                   # EVALUATE DATA LIKELIHOOD
    }


    ## PRIORS

    amplight ~ dunif(50,500)                           # annual fluctuation in lux (amplitude of sin wave)
    meanlight ~ dunif(200,2000)                        # mean sum of lux value
    light.prec ~ dgamma(0.001,0.001)                   # variability of light from day to day
    nesting.light.frac ~ dunif(0.01,0.75)              # extent to which light is reduced during nesting
    nesting.prob ~ dunif(0,1)                          # probability of nesting during nesting season


    ## CORRECT FOR NON-NESTING SEASON

    for(day in 1:ndays){
      crit1[day] <- step(jday[day]-init.nesting)       # 0 if before initial nesting date, 1 if at or after
      crit2[day] <- step(final.nesting-jday[day])      # 0 if after final nesting date, 1 if at or before
      can.nest[day] <- crit1[day] * crit2[day]
    }



    ## DERIVED TERMS 

    light.var <- 1/light.prec
    light.sd <- pow(light.var,0.5)

    nesting.light <- meanlight*nesting.light.frac


  }

",file=filename
      )
      
      
      ###
      # PREPARE DATA FOR BUGS
      ###
      
      data.for.bugs <- list(
        ts = df$lux,
        jday = df$JDAY,
        ndays = nrow(df)
      )
      
      inits.for.bugs <- function(){ list(
        amplight = runif(1,150,350),
        meanlight = runif(1,median(df$lux)-50,median(df$lux)+50),
        light.prec = runif(1,0.00001,0.0001),
        nesting.light.frac = runif(1,0.1,0.2),
        nesting.prob = runif(1,0.2,0.3)
      )}
      
      inits.for.bugs()
      
      params.to.save <- c(
        "amplight",
        "meanlight",
        "light.sd",
        "nesting.light.frac",
        "nesting.prob",
        "is.nesting"
      )
      
      Mod <- jagsUI::jags(data=data.for.bugs,inits=inits.for.bugs,parameters.to.save = params.to.save, 
                          model.file = filename,
                          n.chains = 2, n.adapt = 1000, n.iter = 5000, n.burnin = 1000,
                          n.thin = 2, parallel = TRUE, n.cores = 3, DIC = FALSE) 
      

      
      
      ###
      # VISUALIZE OUTPUT
      ###
      
      par(mfrow=c(3,2))
      hist(Mod$sims.list$amplight, main = paste(indName,"Light Amplitude"))
      # mean light during equinox during non-nesting period
      hist(Mod$sims.list$meanlight, main = paste(indName,"Mean Equinox Light"))
      
      hist(Mod$sims.list$light.sd, main = paste(indName,"Light sd"))
      # mean light during nesting as fraction of meanlight during equinox
      hist(Mod$sims.list$nesting.light.frac, main = paste(indName,"Nesting Light Fraction"))
      
      hist(Mod$sims.list$nesting.prob, main = paste(indName,"Nesting Probability"))
      
      temp <- apply(Mod$sims.list$is.nesting,2,sum)
      is.nesting <- ifelse(temp>1000,TRUE,FALSE)
      
      
      plot(df$DATE,df$lux,type="l",ylim=c(0,2000), main = paste(indName,"Nesting Periods"))
      
      meanlight <- mean(Mod$sims.list$meanlight)
      amplight <- mean(Mod$sims.list$amplight)
      points(df$DATE,luxfunction(df$JDAY),type="l",col="green",lwd=3,lty=2)
      
      points(df$DATE,ifelse(is.nesting,100,-1000),pch="X",cex=1,col="red")
      
      save(Mod,df,luxfunction,shift365,stretch365,file = paste(indName,"_BmodelNest.RData",sep = ""))
      
      rm(list=setdiff(ls(),c("allfiles","mainDir","species","spDir")))
    }, error=function(err) {
      cat(paste("Error:",err))
    })
  }
  
  dev.off()



  # Going through individual models for greater detail ----------------------
  # startDir <- "WODU breeding"
  mainDir <- file.path("data")
  # species <- "WODU" # Choose between CANV, MALL and WODU
  # spDir <- file.path(mainDir,species)
  
  -setwd(file.path(spDir,paste(species,"luxsum")))
  
  files <- list.files()[grep("_BmodelNest.RData",list.files())]
  nestTimes <- list()
  # This sets wether luxplot is plotted below luxsum plot for comparison
  luxplot <- TRUE
  # This sets wether the small nesting periods are left as is or 
  # manipulated by combination or removal based on criteria later in code 
  combineOrRemoveSmall <- TRUE
  
  allnesting <- c()
  
  fileList <- list.files(spDir)
  geo_df <- read.csv(file.path(mainDir,"CMW_geo.csv"),stringsAsFactors = FALSE)
  geo_df <- geo_df[geo_df$Species == species,]
  geo_df <- geo_df[order(geo_df$Metal),]

  index.bird <- which(fileList %in% paste0(geo_df$Species,"_",geo_df$Metal))
  birdList <- fileList[index.bird]
  if (!all(birdList == paste0(geo_df$Species,"_",geo_df$Metal))) {
    message("Geolocator data frame (geo_df) order does not match birds with birdList")
  }

  indPlot <- FALSE
  if(!indPlot) {
    pdf(file = "Nesting_plots.pdf")
  }
  # for (k in 1:length(files)) {
  for (k in c(1:7,9:25,27:length(birdList))) {
    print(k)
    # birdFile <- files[k]
    # birdFold <- substring(birdFile,1,14)
    birdFold <- birdList[k]
    
    fileNum <- grep(birdFold,files)
    if (length(fileNum) == 0) next
    
    combineSeperateBAruns <- function (baNum,baFiles=files,baFileNum=fileNum) {
      load(files[fileNum[baNum]])
      # Mod,df,luxfunction,shift365,stretch365
      return(list(Mod=Mod,df=df,luxfunction=luxfunction,shift365=shift365,stretch365=stretch365))
    }
    
    baModAll <- lapply(1:length(fileNum),combineSeperateBAruns)
    
    
    df <- lapply(baModAll,function(x) x$df)
    df <- do.call(rbind,df)
    
    temp <- unlist(lapply(baModAll,function(x) apply(x$Mod$sims.list$is.nesting,2,sum)))
    is.nesting <- ifelse(temp>1000,TRUE,FALSE)
    
    # png(file=paste0(birdFold,"nestingPlot.png"))
    # png(file=paste0(birdFold,"nestingPlot.png"),width = 2000, height = 1125)
    # pdf(file=paste0(birdFold,"nestingPlot.pdf"))
    if(indPlot) {
      if(!dir.exists("Nesting_plots")) dir.create("Nesting_plots")
      png(file = file.path("Nesting_plots",paste0(birdFold,"_Nesting_plots.png")),
          width = 1000, height = 800)
    }
    if (luxplot) {
      par(mfrow=c(2,1))
    } else {
      par(mfrow=c(1,1))
    }
    min(df$DATE)
    max(df$DATE)
    
    l.folder <- "light_breed"
    
    luxfiles <- list.files(file.path(spDir,birdFold,l.folder),full.names = TRUE)
    luxfiles <- luxfiles[grep("LUXdata_values",luxfiles)]
    
    LUXdataList <- lapply(luxfiles,function(x) { 
      load(x)
      LUXdata
    })
    LUXdata <- do.call(rbind,LUXdataList)
    
    save(LUXdata,file=file.path(spDir,birdFold,l.folder,"LUXdataCombined_values.RData"))
    
    
    xlim <- range(as.Date(LUXdata$Date))
    
    plot(df$DATE,df$lux,type="n",ylim=c(0,2000),xlim=xlim, main = paste(birdFold,"Nesting Periods"), cex.main=3,
         xlab="Date",ylab="Daily sum (light values)",cex.lab=2)
    
    plotMultipleLux <- function (baModel) {
      Mod <- baModel$Mod
      
      meanlight <- mean(Mod$sims.list$meanlight)
      amplight <- mean(Mod$sims.list$amplight)
      luxfunction <- baModel$luxfunction
      shift365 <- baModel$shift365
      stretch365 <- baModel$stretch365
      
      lines(baModel$df$DATE,baModel$df$lux)
      points(baModel$df$DATE,luxfunction(baModel$df$JDAY,meanl=meanlight,ampl = amplight,sh365 = shift365,st365 = stretch365),type="l",col="blue",lwd=3,lty=2)
    }
    
    sapply(baModAll,plotMultipleLux)
    points(df$DATE,ifelse(is.nesting,100,-1000),pch=20,cex=1,col="red")
    
    par1 <- par()
    
    # lux plot---
    if (luxplot) {
      require(SGAT)
      require(TwGeos)
      
      birdNum <- which(geo_df$Metal %in% substr(birdFold,6,14))
      # set offset for plotting - this is just a plotting parameter
      offset=19
      
      lightImage(LUXdata, offset = offset, zlim = c(0,12))
      # adds line at equinoxes. Dates run 2011 to 2020
      # Change the dates if necessary (can vary by year). This is for illustration only. Has no effect on analysis
      eqnx<-as.POSIXct(c("2011-3-20","2011-9-23","2012-3-20","2012-9-22","2013-3-20","2013-9-22","2014-3-20","2014-9-23",
                         "2015-3-20","2015-9-23","2016-3-20","2016-9-22","2017-3-20","2017-9-22","2018-3-20","2018-9-23",
                         "2019-3-20","2019-9-23","2020-3-20","2020-9-22"), tz = "GMT")
      # abline(v = eqnx, lwd=3, lty=3, col="purple")
      
      # useDates <- as.POSIXct(c(twl$Twilight[1],twl$Twilight[nrow(twl)]),tz = "GMT")
      abline(v = as.POSIXct(strptime(geo_df$Date.of.Initial.fitting[birdNum],"%m/%d/%Y",tz="GMT")),lwd=1, lty=3, col="yellow")
      abline(v = as.POSIXct(strptime(geo_df$shot.recovered.date[birdNum],"%m/%d/%Y",tz="GMT")),lwd=1, lty=3, col="yellow")
      title(paste(birdFold," (",geo_df$Sex[birdNum],") LUX plot",sep = ""))
    }
    
    
    if (length(which(is.nesting)) > 0) {
      nesting.dates <- df$DATE[is.nesting]
      allnesting <- c(allnesting,birdFold)
      # nesting.dates <- df$JDAY[is.nesting]
      
      dateContinuity <- diff(nesting.dates)
      continuousDate <- which(diff(nesting.dates) > 1)
      
      if (length(continuousDate) == 0) {
        print("The nesting period is all together")
        # nesting.period <- c(nesting.dates[1],nesting.dates[length(nesting.dates)])
      }
      nesting.period <- nesting.dates[sort(c(1,continuousDate,continuousDate+1,length(nesting.dates)))]
      
      # If the total length of nesting is less than 3 days skip to next bird
      if (length(nesting.dates) < 3) {
        # first show the nesting dates were deleted
        par(list(mfg=c(1,1),usr=par1$usr))
        points(x=nesting.dates,y=rep(100,length(nesting.dates)),pch=3,xlim=xlim)
        next
      }
      
      
      
      # Code to combine multiple nesting bouts ---
      
      if (combineOrRemoveSmall & length(continuousDate) > 0) {
        # # remove the nesting periods that are less than 3 days
        # npLength <- diff(nesting.period)
        # npLengthThresh <- !((npLength[seq(1,length(npLength),2)] + 1) < 3)
        # # nestpLength <- !diff(c(0,continuousDate)) < 3
        # nesting.period <- nesting.period[rep(npLengthThresh,each=2)]
        
        # Combine the nesting periods within 8 days of each other
        sepNP <- dateContinuity[continuousDate] > 7
        
        sFact <- 1
        for (sepRow in 1:length(sepNP)) {
          if (sepNP[sepRow]) {
            sFact <- c(sFact,sFact[sepRow]+1)
          } else {
            sFact <- c(sFact,sFact[sepRow])
          }
          
        }
        
        #split by this vector as factor to get list of each seperate nesting period
        tempList <- split(nesting.period,rep(sFact,each=2))
        
        npList <- lapply(tempList,function(x) {
          nestDatesNew <- seq(x[1],x[length(x)],1)
          nestPeriodNew <- c(x[1],x[length(x)])
          
          if (diff(x)[1] > 0 | length(x) > 2) {
            
            # } else {
            nestDatesNew <- seq(x[1],x[length(x)],1)
            nestPeriodNew <- c(x[1],x[length(x)])
            return(list(nestDatesNew,nestPeriodNew))
          }
          # nestDatesNew <- seq(x[1],x[length(x)],1)
          # nestPeriodNew <- c(x[1],x[length(x)])
          # return(list(nestDatesNew,nestPeriodNew))
        })
        
        npList <- npList[!sapply(npList,is.null)]
        
        nesting.datesNew <- do.call(c,lapply(npList,function(x) x[[1]]))
        nesting.periodNew <- do.call(c,lapply(npList,function(x) x[[2]]))
        names(nesting.datesNew) <- c()
        names(nesting.periodNew) <- c()
        
        # recalculate continunity for nesting dates with small periods either removed or combined
        dateContinuity <- diff(nesting.datesNew)
        continuousDate <- which(diff(nesting.datesNew) > 1)
        
        
        par(list(mfg=c(1,1),usr=par1$usr))
        
        nX <- as.Date(setdiff(nesting.dates,nesting.datesNew),origin = "1970-01-01")
        points(x=nX,y=rep(100,length(nX)),pch=3,xlim=xlim)
        
        sapply(lapply(npList,function(x) x[[2]]), function(x) {
          lines(x=x,y=rep(100,length(x)),lwd=2)
        })
        # set new nesting Dates and periods to old object names
        nesting.dates <- nesting.datesNew
        nesting.period <- nesting.periodNew
        
      } else {
        npListFull <- split(nesting.period,
                            sort(rep(seq(1,length(nesting.period)/2),2)))
        par(list(mfg=c(1,1),usr=par1$usr))
        sapply(npListFull, function(x) {
          lines(x=x,y=rep(100,length(x)))
        })
      }
      
      nestTimes[[birdFold]] <- list(nestDates=nesting.dates,nestPeriod=nesting.period,
                                    nestPeriodList=split(nesting.period,
                                                         sort(rep(seq(1,length(nesting.period)/2),2))),
                                    continuousDate=continuousDate, dateCont=dateContinuity)
    }
    if (indPlot){
      dev.off()
    }
    # } else {
    #   nesting.dates <- "None found"
    #   dateContinuity <- "No nesting Dates"
    #   continuousDate <- "None found"
    #   nesting.period <- "No nesting Dates"
    #   nestTimes[[birdFold]] <- list(nestDates=nesting.dates,nestPeriod=nesting.period,
    #                                continuousDate=continuousDate, dateCont=dateContinuity)
    # }
    rm(list = setdiff(ls(),c("files","nestTimes","nestTimes1000","spDir","mainDir","species","luxplot",
                             "birdList","allnesting","geo_df","combineOrRemoveSmall","indPlot")))
    
  }
  if(!indPlot) {
    dev.off()
  }
  

  save(nestTimes,file = "nestingDatesListPreVisual.RData")


  # Post check for which to combine and which to delete ----
  
  # combRmvNestPeriods <- 
  fixNestTimes <- function(nestPerList) {#,combNP,rmvNP) {
    # if (length(rmvNP) > 0) {
    #   nestPeriodListNew <- nestPerList$nestPeriodList[-rmvNP]
    # } else {
    #   nestPeriodListNew <- nestPerList$nestPeriodList
    # }
    # # tom <- which(!names(nestPeriodListNew) %in% combNP)
    # as.Date(unlist(nestPerList$nestPeriodList[combNP]),origin="1970-01-01")
    # 
    # nestPerList$nestPeriodList[[combNP[1]]][1]
    # nestPerList$nestPeriodList[[combNP[length(combNP)]]][2]
    
    # nestPeriodNew <- as.Date(unlist(nestPeriodListNew),origin = "1970-01-01")
    nestPeriodNew <- as.Date(unlist(nestPerList),origin = "1970-01-01")
    names(nestPeriodNew) <- c()
    # nestDatesNew <- as.Date(unlist(lapply(nestPeriodListNew,function(x) x[1]:x[2])),origin="1970-01-01")
    nestDatesNew <- as.Date(unlist(lapply(nestPerList,function(x) x[1]:x[2])),origin="1970-01-01")
    names(nestDatesNew) <- c()
    dateContNew <- diff(nestDatesNew)
    continuousDateNew <- which(diff(nestDatesNew) > 1)
    
    list(nestDates=nestDatesNew,nestPeriod=nestPeriodNew,
              nestPeriodList=nestPerList,
              continuousDate=continuousDateNew, dateCont=dateContNew)
    
    # list(nestDates=nestDatesNew,nestPeriod=nestPeriodNew,
    #      nestPeriodList=split(nestPeriodListNew,
    #                           sort(rep(seq(1,length(nesting.period)/2),2))),
    #      continuousDate=continuousDateNew, dateCont=dateContNew)
  }

  # nestTimes[[birdfold]] <- fixNestTimes(birdFold,c(),c())
  
  npFixList <- list(
    WODU_110563602=list(
        as.Date(c("2015-04-15","2015-04-23")),
        as.Date(c("2015-05-30","2015-06-19")),
        as.Date(c("2016-04-01","2016-05-03")),
        as.Date(c("2016-06-05","2016-06-28")),
        as.Date(c("2017-03-13","2017-03-24")),
        as.Date(c("2017-04-29","2017-05-07"))),
    WODU_110563805=list(
        as.Date(c("2015-05-12","2015-06-10"))),
    WODU_110594285=list(
        as.Date(c("2015-04-22","2015-05-18")),
        as.Date(c("2016-03-22","2016-04-19")),
        as.Date(c("2016-05-23","2016-06-22")),
        as.Date(c("2017-03-14","2017-04-13")))#,
    # WODU_110594907=list(
    # as.Date(c("2014-07-14","2014-07-31")))
  )
  
  npFixedList <- lapply(npFixList,fixNestTimes)
  
  nestTimes[c("WODU_110563602",
              "WODU_110563805",
              "WODU_110594285"#,
              # "WODU_110594907"
              )
            ] <- npFixedList
  (rmI <- which(names(nestTimes) == "WODU_110594907"))
  nestTimes <- nestTimes[-rmI] # This removes WODU_110594907 from nestTimes completely
  (rmI <- which(names(nestTimes) == "WODU_110563232"))
  nestTimes <- nestTimes[-rmI]# This removes WODU_110563232 from nestTimes completely
  
  nestTimes["WODU_110563602"]$WODU_110563602$nestPeriodList
  nestTimes["WODU_110563805"]$WODU_110563805$nestPeriodList
  nestTimes["WODU_110594285"]$WODU_110594285$nestPeriodList
  
  nestTimes["WODU_110594907"]$WODU_110594907$nestPeriodList
  nestTimes["WODU_110563232"]$WODU_110594907$nestPeriodList
  
  save(nestTimes,file = "nestingDatesList.RData")
  