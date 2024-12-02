# 20241125 hysplit-control.R
# Intended purpose is to build N number of CONTROL FILES
# based on user input. For information on running
# batch processes with HYSPLIT refer to the online tutorial
# at http://www.meteozone.com/home/tutorial/html/index.html



library(xts)
# We'll need:
# Desired Times
# Locations
# Run lengths
# Met files

met.file.loc <- "" # HYSPLIT met file directory
wd.hysplit <- "/home/halfacre/hysplit/trunk/working" # HYSPLIT working directory

# get.met.code will take a time and return
# a code that is used to identify the met files
# needed for the analysis.
get.met.code <- function(TIME){
  month <- format(TIME, "%m")
  day <- as.numeric(format(TIME,"%d"))
  
  if(day%in%1:7){
    week <- "w1"
  }else if(day%in%8:14){
    week <- "w2"
  }else if(day%in%15:21){
    week <- "w3"
  }else if(day%in%22:28){
    week <- "w4"
  }else if(day%in%29:31){
    week <- "w5"
  }
  return(paste(month,week,sep="."))
}

# met.files.ctrl() will take two outputs of 
# get.met.code(), a start code and an end code,
# and return the corresponding met file names.
# NOTE: THIS WILL NOT WORK FOR DATA AFTER JULY!!!
# (technically won't work for any data without the
# corresponding met files)
met.files.ctrl <- function(START.CODE, END.CODE, MET.FILE.LIST){
  month.st <- as.numeric(strsplit(START.CODE,"[.]")[[1]][1])
  month.et <- as.numeric(strsplit(END.CODE,"[.]")[[1]][1])
#   if(month.st>=7||month.st<=2||month.et>=7||month.et<=2){
#     return(NULL)
#   }else{
    start.ix <- grep(START.CODE, MET.FILE.LIST)
    end.ix <- grep(END.CODE, MET.FILE.LIST)
    return(MET.FILE.LIST[(end.ix-1):(start.ix+1)])
  #}
}

met.file.list.syntax <- function(MET.FILES,YEAR){
  MET.FILES.VEC <- list()
  for(ix in 1:length(MET.FILES)){
    MET.FILES.VEC[[ix]] <- c(paste(met.file.loc,YEAR,"/",sep=""),MET.FILES[ix])
  }
  return(unlist(MET.FILES.VEC))
}

# time.loc() will build a data.frame of times and GPS locations
# to be injected into separate CONTROL files.  It will determine
# the number of runs for each event.
time.loc <- function(START,END, GPS){
  START <- as.POSIXct(START, tz='UTC')
  END <- as.POSIXct(END,tz='UTC')
  
  # Step 1: Generate a list of trajectory start times.  
  # This will also tell us how many trajectories we will run.
  # I think it makes sense to run one each hour during the ODE
  # since we have batch capabilities.
  time.seq <- seq.POSIXt(from=START,to=END,by="hours")
  
  # Step 2: Convert each time into a start time usable by 
  # HYSPLIT.  This is "YY MM DD HH MM SS" (but we won't use MM SS).
  st.hysplit <- sapply(time.seq, function(x){format(x,"%y %m %d %H")})
  if(length(GPS)==2){
    gps.hysplit <- data.frame(lat=GPS[2],lon=GPS[1])
  }else{
    gps.hysplit <- GPS[sapply(time.seq,function(x){which.min(abs(GPS$ts-x))}),2:3]
  }
  
  # Step 3: Now combine into a data frame so they can come out at once. 
  time.gps.df <- data.frame(st.hysplit,gps.hysplit,row.names=NULL)
  names(time.gps.df) <- c("start.time","lat","lon")
  time.gps.df$start.time <- as.character(time.gps.df$start.time)
  # Step 4: Round GPS value to the nearest 0.25, consistent with the resolution of the HYSPLIT
  time.gps.df$lat <- round(time.gps.df$lat * 4, 0)/4
  time.gps.df$lon <- round(time.gps.df$lon * 4, 0)/4
  
  return(time.gps.df)
}


# build.ctrl() will take the output of time.loc(), as well as some other
# user-defined variables.  HEIGHTS represents height of particle release, RT is run time,
# VERT represents the vertical transport method.  Met files typially have this 
# within them, so I just leave it at 0.  
build.ctrl <- function(TIME.LOC, HEIGHTS=10, RT=-24, VERT=0, BL.height = 4000, WD){

  no.heights = length(HEIGHTS)
  # Generate the list of locations/heights
  loc <- sapply(HEIGHTS,function(x){paste(TIME.LOC[2], TIME.LOC[3], x, sep=" ")})
  
  traj.st <- as.POSIXct(TIME.LOC[1], format="%y %m %d %H")
  name <- format(traj.st,"%Y%m%d%H")
    
  # Need to determine which met files we need.
  traj.et <- traj.st - as.difftime(-RT,units="hours")
  met.file.list <- list.files(paste(met.file.loc,format(traj.st,"%Y"),sep=""))
  met.files <- met.files.ctrl(START.CODE=get.met.code(traj.st),END.CODE=get.met.code(traj.et),
                              MET.FILE.LIST=met.file.list)
  if(is.null(met.files)){
    return(NULL)
  }
  # Afterward, rearrange it into the proper CONTROL file syntax
  met.syntax <- met.file.list.syntax(met.files,YEAR=format(traj.st,"%Y"))
  
  fileCon <- file(paste("CONTROL",name,sep="."))
  #SETUP INFO
  # includes lines for start time, location, heights, met files../.
  writeLines(c(TIME.LOC[1], # Traj start time
               no.heights, # number of starting heights
               loc, # lat,lon,starting height
               RT, # run time
               VERT, # vertical motion method
               BL.height, # top of the model
               length(met.files), # number of met files
               met.syntax, #filepaths for relevant met files,
               "1", # pollutant no.
               "O3", #name of said pollutant
               "1.0", # emission rate (units/hr)
               "1.0", # hours of emission
               "00 00 00 00 00", # release start time, all 0's default to model start
               "1",  # grid no.
               "0.0 0.0", # center of lat /lon
               "0.25 0.25", # grid spacing
               "30.0 180", # grid span from starting loc
               paste(WD,"/",sep=""), # output dir
               paste("cback-",name,".bin",sep=""), # conc output filename
               "1", # no of vertical levels
               "50", #averaging height, i.e. 0-Xm where x is the top of the avg height
               "00 00 00 00 00", #sampling start time
               "00 00 00 00 00", # sampling stop time
               paste("00 ",abs(RT) ," 00",sep=""), # Avg vs inst, hours across whicht o avg 
               "1", #dep
               "0.0 0.0 0.0", #dep
               "0.0 0.0 0.0 0.0 0.0", #dep
               "0.0 0.0 0.0", #dep
               "0.0", #dep
               "0.0"),#dep
                fileCon)
  
  close(fileCon)
}


buoy14.alltimes <- seq(from=as.POSIXct("2016-04-20 00:00:00",tz="UTC"), 
                      to = as.POSIXct("2016-05-22 00:00:00",tz="UTC"), by="hour")

make.ctrl.files <- function(LOC, YEAR, OB.TIMES = buoy14.alltimes){
  ode.time.file <- list.files(wd.ode,full.names=TRUE)[grep(paste(LOC,YEAR,sep="-"),list.files(wd.ode))]
  # the above line returns the file that matches the pattern "LOC-YEAR" in the
  # ODE times working directory.
  
  # Now we should load the file and create vectors of the start and stop times:
  # ode.times <- read.csv(ode.time.file,stringsAsFactors=FALSE,header=TRUE)
  st <- OB.TIMES[1]
  et <- OB.TIMES[length(OB.TIMES)]
  # good job.  Now GPS which calls on the load.data function in load.files.R:
  gps <- load.data(loc=LOC, year=YEAR,what="gps")
  if(is.xts(gps)==TRUE){
    gps <- data.frame(ts=index(gps),coredata(gps[1:2]))
  }
  # time.loc() will build a data frame for each event with GPS and times throughout
  # the trajectory.  I guess I need to mapply it?
  ctrl.files <- mapply(time.loc,OB.TIMES[1],OB.TIMES[length(OB.TIMES)],
                       MoreArgs=list(GPS=gps),SIMPLIFY=FALSE)
  # Make appropriate directory tree:
  # buoyN
  # bin control particle plot
  # and then place control files within the "control" directory
  
  for(ix in 1:length(ctrl.files)){
    ed <- file.path(wd.hysplit,LOC,YEAR,ix) # ed for "event directory"
    cd <- file.path(ed,"control")
    bd <- file.path(ed, "bin")
    pd <- file.path(ed, "particle")
    plot.d <- file.path(ed, "plot")
    sapply(c(ed,cd,bd,pd, plot.d),dir.create,showWarnings=FALSE)
    setwd(cd)
    ctrl.list <- apply(ctrl.files[[ix]],1,build.ctrl,WD=bd)
    setwd(wd.hysplit)
  }

  return(ctrl.files)
}




buoy1.alltimes <- seq(from=as.POSIXct("2010-03-01 17:00:00",tz="UTC"), 
                      to = as.POSIXct("2010-06-01 00:00:00",tz="UTC"), by="hour")


# TESTING -----------------------------------------------------------------

# st <- as.POSIXct('2012-04-12 12:00:00',tz='UTC')
# et <- as.POSIXct('2012-04-14 15:00:00',tz='UTC')
# 
# gps <- load.data(loc="buoy4",year="2012",what="gps")
# ctrl.files <- time.loc(st,et,GPS=gps)
# 
# apply(ctrl.files,1,function(x){build.ctrl(TIME.LOC=x)})

# 
# fileCon <- file(paste(paste(wd.hysplit,LOC,"/CONTROL", sep=""),name,sep="."))
# writeLines(c(TIME.LOC[1],no.heights,loc,RT,VERT,BL.height,length(met.files),met.syntax,"./",
#              paste("tback-",name,sep="")),
#            fileCon)
# close(fileCon)



#
