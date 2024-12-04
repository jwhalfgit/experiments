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

met.file.loc <- "C:/HYSPLIT/metfiles/" # HYSPLIT met file directory
wd.hysplit <- "C:/HYSPLIT/working" # HYSPLIT working directory

sites <- as.data.frame(matrix(data = c(53.4542503,-2.2487979,"Manchester Air Quality Site",
                                       52.476145, -1.874978, "Birmingham Roadside Site"),ncol=3,byrow=TRUE,dimnames = list(c(),c("lat","lon","loc"))))

sites$lat <- as.numeric(sites$lat)
sites$lon <- as.numeric(sites$lon)

# GDAS Data Codes ---------------------------------------------------------


# get.met.code will take a time and return
# a code that is used to identify the met files
# needed for the analysis.  NOTE THIS IS ONLY 
# NEEDED FOR GDAS DATA!! NOT USEFUL FOR REANALYSIS DATA
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
# met.files.ctrl <- function(START.CODE, END.CODE, MET.FILE.LIST){
#   month.st <- as.numeric(strsplit(START.CODE,"[.]")[[1]][1])
#   month.et <- as.numeric(strsplit(END.CODE,"[.]")[[1]][1])
# #   if(month.st>=7||month.st<=2||month.et>=7||month.et<=2){
# #     return(NULL)
# #   }else{
#     start.ix <- grep(START.CODE, MET.FILE.LIST)
#     end.ix <- grep(END.CODE, MET.FILE.LIST)
#     return(MET.FILE.LIST[(end.ix-1):(start.ix+1)])
#   #}
# }


# This is for the renalysis(RA) met data
met.files.ctrl.RA <- function(START, END, MET.FILE.LIST){
  month.st <- format(START, "%Y%m")
  month.et <- format(END, "%Y%m")

  start.ix <- grep(pattern = month.st, x=  MET.FILE.LIST)
  end.ix <- grep(pattern = month.et, x= MET.FILE.LIST)
  return(MET.FILE.LIST[(end.ix-1):(start.ix+1)])
  #}
}

#GDAS versuib
# met.file.list.syntax <- function(MET.FILES,YEAR){
#   MET.FILES.VEC <- list()
#   for(ix in 1:length(MET.FILES)){
#     MET.FILES.VEC[[ix]] <- c(paste(met.file.loc,YEAR,"/",sep=""),MET.FILES[ix])
#   }
#   return(unlist(MET.FILES.VEC))
# }


met.file.list.syntax <- function(MET.FILES){
  MET.FILES.VEC <- list()
  for(ix in 1:length(MET.FILES)){
    MET.FILES.VEC[[ix]] <- c(met.file.loc,MET.FILES[ix])
  }
  return(unlist(MET.FILES.VEC))
}

##################################################################



# time.loc() will build a data.frame of times and GPS locations
# to be injected into separate CONTROL files.  It will determine
# the number of runs for each event.
time.syntax <- function(TIMES){

  st.hysplit <- sapply(TIMES, function(x){format(x,"%y %m %d %H")})
  # if(length(GPS)==2){
  #   gps.hysplit <- data.frame(lat=GPS[1],lon=GPS[2])
  # }else{
  #   gps.hysplit <- GPS[sapply(time.seq,function(x){which.min(abs(GPS$ts-x))}),2:3]
  # }
  
  # Step 3: Now combine into a data frame so they can come out at once. 
  time.df <- data.frame(st.hysplit,row.names=NULL)
  names(time.df) <- c("start.time")
  time.df$start.time <- as.character(time.df$start.time)

  return(time.df)
}


# build.ctrl() will take the output of time.loc(), as well as some other
# user-defined variables.  HEIGHTS represents height of particle release, RT is run time,
# VERT represents the vertical transport method.  Met files typially have this 
# within them, so I just leave it at 0.  
build.ctrl <- function(TIME.LOC, GPS, HEIGHTS=10, RT=-24, VERT=0, BL.height = 4000, 
                       WD = wd.hysplit){
  gps <- GPS[1:2]
  gps$lat <- round(as.numeric(gps$lat), 0)
  gps$lon <- round(as.numeric(gps$lon), 0)
  
  
  no.heights = length(HEIGHTS)
  # Generate the list of locations/heights
  
  locList <- list()
  for(ix in 1:nrow(gps)){
    for(iy in 1:no.heights){
      locList[ix] <- paste(gps[ix,1], gps[ix,2], HEIGHTS[iy], sep=" ")
    }
  }
  loc <- unlist(locList)
  
  #loc <- sapply(HEIGHTS,function(x){paste(GPS[1], GPS[2], x, sep=" ")})
  no.loc.height <- length(loc)
  
  traj.st <- as.POSIXct(TIME.LOC[1], format="%y %m %d %H",tz = "UTC")
  NAME <- format(traj.st,"%Y%m%d%H")
    
  # Need to determine which met files we need.
  traj.et <- traj.st - as.difftime(-RT,units="hours")
  # met.file.list <- list.files(paste(met.file.loc,format(traj.st,"%Y"),sep=""))
  met.file.list <- list.files(met.file.loc)
  met.files <- met.files.ctrl.RA(START=traj.st,END=traj.et,
                              MET.FILE.LIST=met.file.list)
  if(is.null(met.files)){
    return(NULL)
  }
  # Afterward, rearrange it into the proper CONTROL file syntax
  met.syntax <- met.file.list.syntax(met.files)
  
  fileCon <- file(paste("CONTROL",NAME,sep="."))
  #SETUP INFO
  # includes lines for start time, location, heights, met files../.
  writeLines(as.character(c(TIME.LOC[1], # Traj start time
               no.loc.height, # number of starting locations and heights
               loc, # lat,lon,starting height
               RT, # run time
               VERT, # vertical motion method
               BL.height, # top of the model
               length(met.files), # number of met files
               met.syntax, #filepaths for relevant met files,
               # "1", # pollutant no.
               # "O3", #name of said pollutant
               # "1.0", # emission rate (units/hr)
               # "1.0", # hours of emission
               # "00 00 00 00 00", # release start time, all 0's default to model start
               # "1",  # grid no.
               # "0.0 0.0", # center of lat /lon
               # "0.25 0.25", # grid spacing
               # "30.0 180", # grid span from starting loc
               paste(WD,"/",sep=""), # output dir
               paste("tback-",NAME,sep=""))), # conc output filename
               # "1", # no of vertical levels
               # "50", #averaging height, i.e. 0-Xm where x is the top of the avg height
               # "00 00 00 00 00", #sampling start time
               # "00 00 00 00 00", # sampling stop time
               # paste("00 ",abs(RT) ," 00",sep=""), # Avg vs inst, hours across whicht o avg 
               # "1", #dep
               # "0.0 0.0 0.0", #dep
               # "0.0 0.0 0.0 0.0 0.0", #dep
               # "0.0 0.0 0.0", #dep
               # "0.0", #dep
               # "0.0"),#dep
                fileCon)
  
  close(fileCon)
}


projTimes <- seq(from=as.POSIXct("2023-04-20 00:00:00",tz="UTC"), 
                      to = as.POSIXct("2023-04-22 00:00:00",tz="UTC"), by="6 hours")

make.ctrl.files <- function(LOC = sites, TIMES = projTimes){

  # Now we should load the file and create vectors of the start and stop times:
  st <- TIMES[1]
  et <- TIMES[length(TIMES)]

  gps <- LOC[,1:2]

  # time.loc() will build a data frame for each event with GPS and times throughout
  # the trajectory.  I guess I need to mapply it?
  ctrl.files <- time.syntax(TIMES)
  # Make appropriate directory tree:
  # buoyN
  # bin control particle plot
  # and then place control files within the "control" directory
  
  #for(ix in 1:length(ctrl.files)){

    # ed <- file.path(wd.hysplit,LOC,YEAR,ix) # ed for "event directory"
    ed <- file.path(wd.hysplit) # ed for "event directory"
    cd <- file.path(ed,"control")
    bd <- file.path(ed, "bin")
    pd <- file.path(ed, "particle")
    plot.d <- file.path(ed, "plot")
    sapply(c(ed,cd,bd,pd, plot.d),dir.create,showWarnings=FALSE)
    setwd(cd)
    ctrl.list <- apply(ctrl.files,1,build.ctrl,GPS = gps, WD=bd)
    setwd(wd.hysplit)
  #}

  return(ctrl.files)
}



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





