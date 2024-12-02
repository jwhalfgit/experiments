library(lubridate)
library(openair)


# Winter Processing -------------------------------------------------------
#
#
# Winter OSCA (bad data before 3 Feb).  The campaign started on Jan 22, but HCl
# standards were leaking into the inertial inlet for whatever reason.  Also,
# I think we were struggling to get good background measurements.
# Data from 3 Feb - 8 Feb should be okay at least.  Sometime over the weekend, 
# the chiller for Babushka box died (or at least began cooling to some negative 
# temperature).  This should have the largest effects on the permeation source,
# and our uncertainty will be correspondingly larger in this period. 
# Chiller back online 17 February
# Data collection ends at 10AM on 21 February.

###############################################################################
# 10Hz Processing ---------------------------------------------------------
###############################################################################

winterDates <- seq(as.Date("2022-02-05"), as.Date("2022-02-20"),by=1)
winterDates <- winterDates[-which(winterDates == as.Date("2022-02-16"))]

# The below will return 
# write.daily.data <- function(DATE){
#   load.tildas.data(START = DATE, STOP = DATE,STC=TRUE,tenHz = TRUE)
#   offset.correct(tenHz=TRUE)
#   write.zoo(str.xts,
#     paste("G:/My Drive/Data/TILDAS/OSCA/Winter/",DATE,".csv",sep=""),
#     index.name = "ts",sep=",",quote = FALSE)
#   print(DATE)
# }
# 
# lapply(winterDates,write.daily.data)
#
#
# THIS LOADS 10Hz DATA!!! WATCH OUT! Additionally, 
#   this is "background corrected"
# winterOSCA <- read.csv("G:/My Drive/Data/TILDAS/OSCA/Winter/OSCA-HCl.csv") %>% 
#   tibble() %>% 
#   select(!ends_with("_1")) %>% 
#   mutate(ts = ymd_hms(ts))
# beep()
#   
#
#
###############################################################################
# Convert to 1Hz Data -----------------------------------------------------
###############################################################################

# load.tildas.data(START = "2022-02-05", STOP = "2022-02-20", tenHz=TRUE, 
#                  STC = TRUE, output.type = "data.frame")
# # rm(str.xts)
# 
# df.str <- df.str %>% 
#   tibble() %>%
#   select(!ends_with("_1")) %>%
#   rename(date = ts) %>% 
#   mutate(date = ymd_hms(date)) %>% 
#   timeAverage(avg.time = "sec")
# 
# df.str1Hz <- timeAverage(df.str, avg.time = "sec")
# rm(df.str)
# write.csv(df.str1Hz, "G:/My Drive/Experiments/OSCA/winter-osca-1hz.csv",
#           quote = FALSE, row.names = FALSE)



# write.zoo(str.xts.BckCorr, 
#           paste("G:/My Drive/Data/TILDAS/OSCA/Winter/",DATE,".csv",sep=""),
#           index.name = "ts",sep=",",quote = FALSE)
# 
# 
# write.zoo(winterOSCAAvg, 
#           "G:/My Drive/Data/TILDAS/OSCA/Winter/winterhcl-1savg.csv",
#           index.name = "ts",sep=",",quote = FALSE)
#


# Load 1Hz  ----------------------------------------------------------

# Load the HCl data.  Note this is 10Hz data averaged to 1Hz with no other 
# processing
winterAvg <- read.csv("G:/My Drive/Experiments/OSCA/winter-osca-1hz.csv") %>% 
  tibble() %>% 
  select(!ends_with("_1")) %>% 
  rename(ts = date) %>% 
  mutate(ts = ymd_hms(ts)) %>% 
  mutate(ts = floor_date(ts, unit = "second")) %>% 
  filter(ts < ymd_hm("2022-02-11 11:00") | ts > ymd_hm("2022-02-11 12:30")) # This period is the start after a shutdown I think.  High to low HCl decay.

beep()

# Load STC data
load.tildas.data(START = "2022-02-05", STOP = "2022-02-20",STC=TRUE,STR = FALSE, 
                 output = "data.frame",STCALL = TRUE)
#rm(stc.xts)

df.stc <- df.stc %>% 
  tibble() %>% 
  mutate(ts = ymd_hms(ts)) %>% 
  mutate(ts = floor_date(ts), unit = "second") #%>% 
  # select(ts, Praw, Traw, Range_F.1_L.1, AD8, ValveW, T.Laser.1,
  #        ChillerT)

dfAnal <- winterAvg %>% 
  left_join(df.stc)

# Plot the full time series
TIME1 = ymd_hms("2022-02-05 00:00:00")
TIME2 = ymd_hms("2022-02-10 00:00:00")

P1 <- dfAnal %>% 
  filter(between(ts, TIME1, TIME2),ValveW ==0) %>% 
  ggplot(aes(x=ts,y=hcl)) +
  geom_line()+
  scale_y_continuous(name = "HCl")
  #scale_y_continuous(limits = c(-0.2,1), name = "HCl")


# QC -----------------------------------------------------------
df.str <- as.data.frame(winterAvg)


str.xts <- xts(df.str[2:ncol(df.str)], order.by = df.str$ts, tzone = "UTC")

# the stc.xts as loaded from load.tildas.data does not match time stamps from 
# winterAvg due to decimal time.  I will just convert the df.stc as I did the
# df.str above.  
stc.xts <- xts(df.stc[2:ncol(df.stc)], order.by = df.stc$ts, tzone = "UTC")


offset.correct()
remove.cal()

str.xts.CalCorr$hcl <- str.xts.CalCorr$hcl *1.015 #(sd of %error is 1.589%)
# the 1.015 correction factor is the summer (1.0515) multipled by dilution factor (3.7/12.7)
# I am estimating this offset right now, as we don't have experimental data to confirm yet.  (2024-03-18)


dfAnalNoFilter <- data.frame("ts" = index(str.xts.CalCorr),
                         coredata(str.xts.CalCorr)) %>% 
  tibble() %>% 
  mutate(ts = ymd_hms(ts))


dfAnalNoFilter60s <- dfAnalNoFilter %>% 
  mutate(ts = floor_date(ts, "60 seconds")) %>% 
  group_by(ts) %>% 
  summarise_all(mean)

P1 <- dfAnalNoFilter %>% 
  filter(between(ts, TIME1, TIME2)) %>% 
  ggplot(aes(x=ts,y=hcl)) +
  geom_line()+
  scale_y_continuous(name = "HCl")
#scale_y_continuous(limits = c(-0.2,1), name = "HCl")

P2 <- dfAnal %>% 
  filter(between(ts, TIME1, TIME2)) %>% 
  ggplot(aes(x=ts,y=T.Laser.1)) +
  geom_line()

grid.arrange(P1,P2,nrow = 2,ncol=1)


dfAnalCorrRange <- filter.noise.Range(STR = dfAnalNoFilter60s,stdev = 1)
dfAnalCorrChi <- filter.noise.chi(STR = dfAnalNoFilter, stdev = 1)



dfAnalMerge <- merge(dfAnalNoFilter, blankAnalDF,all=TRUE) 


dfAnalUnc <- dfAnalMerge %>% 
  # uncertainty here comes from... uncertainty in blank, 2% from spectral fit,
  # plus 2.3% variability in perm source.  this is larger than the anticipated
  # offset from line loss.
  mutate(hclBlankSd2 = sqrt(na.locf(hclBlankSd,na.rm = FALSE)^2 + (.02*hcl)^2 + (0.023*hcl)^2)) %>% 
  select(ts,hcl, hclBlankSd2) %>% 
  rename(hclUnc = hclBlankSd2) %>% 
  filter(complete.cases(.)) %>% 
  mutate(hcl = round(hcl, digits = 4), hclUnc = round(hclUnc,digits = 4)) %>% 
  write.csv("G:/My Drive/Experiments/OSCA/data/2022/hclCEDA.csv",quote=FALSE,row.names=FALSE)






length(dfAnalUnc$hclBlankSd)


#dfAnalCorr <- filter.noise(STR = dfAnalCorrRange, stdev = 3)

#outXts <- filter.noise(stdev = 3)

# dfAnalCorr <- data.frame("ts" = index(outXts),
#                          coredata(outXts)) %>% 
#   tibble() %>% 
#   mutate(ts = ymd_hms(ts))

# This was great for correcting the backgrounds and removing calibration points.
# However, there are still errant spikes in the data of unknown origin.


# For Plotting
TIME1 <- "2022-02-19 00:00:00"
TIME2 <- "2022-02-20 00:00:00"

P1 <- dfAnalCorr %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hcl)) +
  geom_line()+
  scale_y_continuous(limits = c(-0.2,0.5),name = "HCl")


P2 <- dfAnalCorrRange %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  ggplot(aes(x=ts,y=hcl)) +
  geom_line()+
  scale_y_continuous(limits = c(-0.2,0.5),name = "HCl")

# valveLastWeekPlot <- dfAnal %>% 
#                       filter(between(ts, ymd_hms(TIME1), 
#                                      ymd_hms(TIME2))) %>% 
#                       ggplot(aes(x=ts,y= ValveW)) +
#                       geom_line()
#                       #scale_y_continuous(name = "ValveW")

# grid.arrange(hclLastWeekPlot,valveLastWeekPlot,nrow = 2,ncol=1)
grid.arrange(P1,P2,nrow = 2,ncol=1)

write.csv(dfAnalCorrChi, "G:/My Drive/Experiments/OSCA/winter-osca-60s-rangefilter-1sdchifilter-TOPUBLISH.csv",
         quote=FALSE,row.names=FALSE)


# Winter Analysis ---------------------------------------------------------
ambBlanks <- dfAnal %>% 
  filter(ValveW == 2, )

ambBlanks <- dfAnal %>% 
  filter(between(minute(ts)+ (second(ts)/60), 11,12) | 
         between(minute(ts)+ (second(ts)/60), 21,22) | 
         between(minute(ts)+ (second(ts)/60), 31,32) | 
         between(minute(ts)+ (second(ts)/60), 41,42) |
         between(minute(ts)+ (second(ts)/60), 51,52) |
         between(hour(ts)*60 + minute(ts) + (second(ts)/60), 61, 62) | 
         between(hour(ts)*60 + minute(ts) + (second(ts)/60), 121, 122) | 
         between(hour(ts)*60 + minute(ts) + (second(ts)/60), 241, 242) | 
         between(hour(ts)*60 + minute(ts) + (second(ts)/60), 301, 302) |
         between(hour(ts)*60 + minute(ts) + (second(ts)/60), 421, 422)|
         between(hour(ts)*60 + minute(ts) + (second(ts)/60), 481, 482)|
         between(hour(ts)*60 + minute(ts) + (second(ts)/60), 601, 602)|
         between(hour(ts)*60 + minute(ts) + (second(ts)/60), 661, 662) | 
         between(hour(ts)*60 + minute(ts) + (second(ts)/60), 781, 782) | 
         between(hour(ts)*60 + minute(ts) + (second(ts)/60), 841, 842) | 
         between(hour(ts)*60 + minute(ts) + (second(ts)/60), 961, 962) |
         between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1021, 1022)|
         between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1141, 1142)|
         between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1201, 1202)|
         between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1321, 1322)|
         between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1381, 1382), 
    ValveW == 2)

ambBlanksList <- split.groups(ambBlanks)
ambBlanksMean <- lapply(ambBlanksList,
                        function(x){data.frame(ts=floor_date(median(x$ts),
                                                             unit = "10 min"),
                                                             hclBlank = mean(x$hcl,na.rm = TRUE))})

ambBlanksSd <- lapply(ambBlanksList,
                        function(x){data.frame(ts=floor_date(median(x$ts),
                                                             unit = "10 min"),
                                               hclBlankSd = sd(x$hcl,na.rm=TRUE))})
                                                                
ambBlanksDF <- do.call(rbind, ambBlanksMean)
ambBlanksSdDF <- do.call(rbind, ambBlanksSd)
blankAnalDF <- base::merge(ambBlanksDF,ambBlanksSdDF, all=TRUE)
###############################################################
blankAnalDF <- blankAnalDF[complete.cases(blankAnalDF),]
# USE blankAnalDF FOR noise analysis!
###############################################################

x <- blankAnalDF[blankAnalDF$hclBlankSd >= quantile(blankAnalDF$hclBlankSd,(0+5/100)) & blankAnalDF$hclBlankSd <= quantile(blankAnalDF$hclBlankSd,(1-5/100)),]

# ambPostBlank <- dfAnal %>% 
#   filter(between(minute(ts)+ (second(ts)/60), 12,13) | 
#            between(minute(ts)+ (second(ts)/60), 22,23) | 
#            between(minute(ts)+ (second(ts)/60), 32,33) | 
#            between(minute(ts)+ (second(ts)/60), 42,43) |
#            between(minute(ts)+ (second(ts)/60), 52,53) |
#            between(hour(ts)*60 + minute(ts) + (second(ts)/60), 62, 63) | 
#            between(hour(ts)*60 + minute(ts) + (second(ts)/60), 122, 123) | 
#            between(hour(ts)*60 + minute(ts) + (second(ts)/60), 242, 243) | 
#            between(hour(ts)*60 + minute(ts) + (second(ts)/60), 302, 303) |
#            between(hour(ts)*60 + minute(ts) + (second(ts)/60), 422, 423) |
#            between(hour(ts)*60 + minute(ts) + (second(ts)/60), 482, 483) |
#            between(hour(ts)*60 + minute(ts) + (second(ts)/60), 602, 603) |
#            between(hour(ts)*60 + minute(ts) + (second(ts)/60), 662, 663) | 
#            between(hour(ts)*60 + minute(ts) + (second(ts)/60), 782, 783) | 
#            between(hour(ts)*60 + minute(ts) + (second(ts)/60), 842, 843) | 
#            between(hour(ts)*60 + minute(ts) + (second(ts)/60), 962, 963) |
#            between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1022, 1023)|
#            between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1142, 1143)|
#            between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1202, 1203)|
#            between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1322, 1323)|
#            between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1382, 1383),
#          ValveW == 0)

# This looks at the two minutes after blanks.  
post2minBlanks <- dfAnal %>% 
  filter(between(minute(ts)+ (second(ts)/60), 15,16) | 
           between(minute(ts)+ (second(ts)/60), 25,26) | 
           between(minute(ts)+ (second(ts)/60), 35,36) | 
           between(minute(ts)+ (second(ts)/60), 45,46) |
           between(minute(ts)+ (second(ts)/60), 55,56) |
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 65, 66) | 
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 125, 126) | 
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 245, 246) | 
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 305, 306) |
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 425, 426) |
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 485, 486) |
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 605, 606) |
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 665, 666) | 
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 785, 786) | 
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 845, 846) | 
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 965, 966) |
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1025, 1026)|
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1145, 1146)|
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1205, 1206)|
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1325, 1326)|
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1385, 1386),
         ValveW == 0)
#  }else{
    filter(between(minute(ts)+ (second(ts)/60), 12,13) | 
             between(minute(ts)+ (second(ts)/60), 22,23) | 
             between(minute(ts)+ (second(ts)/60), 32,33) | 
             between(minute(ts)+ (second(ts)/60), 42,43) |
             between(minute(ts)+ (second(ts)/60), 52,53) |
             between(hour(ts)*60 + minute(ts) + (second(ts)/60), 62, 63) | 
             between(hour(ts)*60 + minute(ts) + (second(ts)/60), 122, 123) | 
             between(hour(ts)*60 + minute(ts) + (second(ts)/60), 242, 243) | 
             between(hour(ts)*60 + minute(ts) + (second(ts)/60), 302, 303) |
             between(hour(ts)*60 + minute(ts) + (second(ts)/60), 422, 423) |
             between(hour(ts)*60 + minute(ts) + (second(ts)/60), 482, 483) |
             between(hour(ts)*60 + minute(ts) + (second(ts)/60), 602, 603) |
             between(hour(ts)*60 + minute(ts) + (second(ts)/60), 662, 663) | 
             between(hour(ts)*60 + minute(ts) + (second(ts)/60), 782, 783) | 
             between(hour(ts)*60 + minute(ts) + (second(ts)/60), 842, 843) | 
             between(hour(ts)*60 + minute(ts) + (second(ts)/60), 962, 963) |
             between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1022, 1023)|
             between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1142, 1143)|
             between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1202, 1203)|
             between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1322, 1323)|
             between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1382, 1383),
           ValveW == 0)
  }


ambPostBlankList <- split.groups(ambPostBlank)
ambPostBlankMean <- lapply(ambPostBlankList,function(x){data.frame(ts=floor_date(median(x$ts),unit = "10 min"),
                                                                hclAmb=mean(x$hcl))})

ambPostBlankDF <- do.call(rbind, ambPostBlankMean)

hclCompare <- ambBlanksDF %>% 
  left_join(ambPostBlankDF,by = "ts")


ambBlanks %>%  
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2)),
         ValveW == 2) %>% 
  
  ggplot(aes(x=ts,y=hcl)) +
  geom_line()+
  scale_y_continuous(limits = c(-0.2,2),name = "HCl")

# calBlanks represents the blanks just after perm source additions
calBlanks <- dfAnal %>% 
  filter(between(hour(ts)*60 + minute(ts) + (second(ts)/60), 191, 192) | 
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 371, 372) | 
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 551, 552) | 
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 731, 732) |
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 911, 912)|
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1091, 1092)|
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1271, 1272), 
         ValveW == 2)




# Get perm source points
winterDates <- seq(as.Date("2022-02-05"), as.Date("2022-02-20"),by=1)
winterDates <- winterDates[-which(winterDates == as.Date("2022-02-16"))]

# Let's isolate permeation source data
# outList <- list()
# for(ix in 1:length(winterDates)){
#   # load.tildas.data(START = winterDates[ix],STOP = winterDates[ix],STC = TRUE,
#   #                  tenHz = TRUE)
#   # offset.correct(tenHz=TRUE)
#   # 
#   outList <- append(outList, remove.cal(STR = str.xts.BckCorr, 
#                                         tenHz = TRUE, RETURNCAL = TRUE))
# }


calList <- remove.cal(RETURNCAL = TRUE)

# calList <- unlist(outList, recursive = FALSE)

# calList contains the calibration data in a List.  
# Be aware - our chiller that warmed the inlet box failed sometime before 11 Feb.
# Looking at the perm source data, this appears to be sometime on 9 Feb, and was
# restored on 17 Feb by me (as reflected by the data).  
calList.narm <- lapply(calList, function(x){x[complete.cases(x)]})
calListLastThree <- lapply(calList.narm, function(x){xts::last(x,"3 min")})
calListMeans <- lapply(calListLastThree,function(x){data.frame(ts=median(index(x)),
                                                               hcl =mean(x$hcl),
                                                               sem = sd(x$hcl)/length(x$hcl))})



calListFlat <- do.call(rbind, calListMeans)
calListXts <- xts(x = calListFlat[2:ncol(calListFlat)], order.by=calListFlat$ts)
calListFilter <- calListXts[which(calListXts$hcl > 1.8),]
calListAverage <- period.apply(calListFilter$hcl, 
                               endpoints(calListFilter, on = "days"),mean)
calListSd <- period.apply(calListFilter$hcl, 
                               endpoints(calListFilter, on = "days"),sd)

calListUncPerc <- calListSd / calListAverage * 100


calOut <- data.frame(ts = index(calListAverage),hcl = calListAverage,
                     stdev = calListSd, stdevPct = calListUncPerc,
                         row.names=NULL)
names(calOut) <- c("ts","hcl","hclSd","hclSdPct")
# 
# calOut
# ts      hcl       hclSd  hclSdPct
# 1  2022-02-05 21:08:29 2.105939 0.037922068 1.8007201
# 2  2022-02-06 21:08:29 2.044119 0.040829791 1.9974277
# 3  2022-02-07 21:09:00 1.992718 0.019144011 0.9606984
# 4  2022-02-08 21:09:00 2.021016 0.012097709 0.5985953
# 5  2022-02-09 15:08:29 1.992737 0.012174417 0.6109396
# 6  2022-02-16 15:08:29 1.819033          NA        NA
# 7  2022-02-17 21:09:00 1.889630 0.009011386 0.4768864
# 8  2022-02-18 21:09:00 1.889587 0.035499839 1.8787094
# 9  2022-02-19 21:09:00 1.839837 0.017755969 0.9650837
# 10 2022-02-20 21:08:29 1.874669 0.028425193 1.5162777

# 
# > mean(calOut$hclSdPct,na.rm=TRUE)
# [1] 1.200593

# I think these cal points will just be on pump air overblows, no dry air
# for comparison :( 


calListFlatTs <- as.numeric(calListFlatFilter$ts) - as.numeric(calListFlatFilter$ts[1])
# lmCal <- lm(calListFlatFilter$hcl~calListFlatTs)
# 
# detrendCalData <- data.frame(ts = calListFlatTs, 
#                              deltahcl = calListFlatFilter$hcl - (-1.618e-7*calListFlatTs + 2.074))
# 
#


#
#
#
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
#
# Summer OSCA
# Good data start around 2021-06-10 17:00.  
# load.tildas.data(START = "2021-06-17", STOP = "2021-06-24",STC=TRUE)
# load.tildas.data(START = "2021-06-21", STOP = "2021-07-02",STC=TRUE, 
#                  output.type="tibble", MASK = FALSE, STCALL = TRUE)
load.tildas.data(START = "2021-06-10", STOP = "2021-07-21",STC=TRUE,
                 output.type = "tibble", MASK = FALSE,STCALL = FALSE)


# df.str <- df.str %>% 
#   tibble() %>% 
#   select(!ends_with("_1")) %>% 
#   mutate(ts = ymd_hms(ts)) %>% 
#   mutate(ts = floor_date(ts, unit = "second"))
#   
# df.stc <- df.stc %>% 
#   tibble() %>% 
#   mutate(ts = ymd_hms(ts)) %>% 
#   mutate(ts = floor_date(ts, unit = "second")) %>% 
#   select(ts, Praw, Traw, Range_F.1_L.1, AD8, ValveW, T.Laser.1,
#          ChillerT,X1)


dfAnal <- df.str %>% 
  left_join(df.stc) %>% 
  filter(ts > ymd_hm("2021-06-10 17:00")) %>% 
  filter(ts < ymd_hm("2021-06-18 10:30") | ts > ymd_hm("2021-06-22 11:45"))%>% # denuder
  filter(ts < ymd_hm("2021-06-22 12:05") | ts > ymd_hm("2021-06-22 12:07"))%>% # Hcl addition
  filter(ts < ymd_hm("2021-06-22 14:30") | ts > ymd_hm("2021-06-22 16:30"))%>% # Hcl addition through overblow
  filter(ts < ymd_hm("2021-06-23 10:30") | ts > ymd_hm("2021-06-24 15:15"))%>% # denuder leak testing
  filter(ts < ymd_hm("2021-06-25 12:30") | ts > ymd_hm("2021-06-25 17:15"))%>% # test Hcl additions
  filter(ts < ymd_hm("2021-06-29 10:25") | ts > ymd_hm("2021-06-29 11:40"))%>% # test hcl cylinder additions
  filter(ts < ymd_hm("2021-06-29 13:08") | ts > ymd_hm("2021-06-30 15:15"))%>% # site power outage til power restoration
# not sure how much I like 30 June into 1 July. Leavning it for now
  filter(ts < ymd_hm("2021-07-01 00:00") | ts > ymd_hm("2021-07-01 05:15"))%>% # Looks bad... not sure if I can remove?  
  filter(ts < ymd_hm("2021-07-01 10:30") | ts > ymd_hm("2021-07-01 11:05"))%>% # denuder
  filter(ts < ymd_hm("2021-07-01 12:55") | ts > ymd_hm("2021-07-01 13:10"))%>%  # cylinder
  filter(ts < ymd_hm("2021-07-01 13:55") | ts > ymd_hm("2021-07-01 14:35"))%>% # denuder
  filter(ts < ymd_hm("2021-07-01 15:25") | ts > ymd_hm("2021-07-01 16:30"))%>% # cylinder
# cylinder additions during 2300 hour on 2 July
# eod 2 July has a few negative data points before the cylinder addns.  Likely will wash out in avg.
  filter(ts < ymd_hm("2021-07-03 08:00") | ts > ymd_hm("2021-07-03 10:00"))%>% # cylinder
# cylinder additions during 0000 hour on 4 July
  filter(ts < ymd_hm("2021-07-05 10:15") | ts > ymd_hm("2021-07-05 14:30"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-05 21:00") | ts > ymd_hm("2021-07-05 21:15"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-06 00:00") | ts > ymd_hm("2021-07-06 00:15"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-06 03:00") | ts > ymd_hm("2021-07-06 03:10"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-06 06:00") | ts > ymd_hm("2021-07-06 06:10"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-06 09:00") | ts > ymd_hm("2021-07-06 09:10"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-06 12:00") | ts > ymd_hm("2021-07-06 12:10"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-06 12:52") | ts > ymd_hm("2021-07-06 13:05"))%>% # hno3
  filter(ts < ymd_hm("2021-07-06 15:00") | ts > ymd_hm("2021-07-06 15:10"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-06 18:00") | ts > ymd_hm("2021-07-06 18:10"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-06 21:00") | ts > ymd_hm("2021-07-06 21:10"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-07 00:00") | ts > ymd_hm("2021-07-07 00:10"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-07 03:00") | ts > ymd_hm("2021-07-07 03:10"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-07 06:00") | ts > ymd_hm("2021-07-07 06:10"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-07 09:05") | ts > ymd_hm("2021-07-07 12:00"))%>% # hno3 (for sure from 9:05, assuming til noon based on MeOH signal)
  filter(ts < ymd_hm("2021-07-07 12:00") | ts > ymd_hm("2021-07-07 12:10"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-07 15:00") | ts > ymd_hm("2021-07-07 15:10"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-07 18:00") | ts > ymd_hm("2021-07-07 18:10"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-07 21:00") | ts > ymd_hm("2021-07-07 21:10"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-08 00:00") | ts > ymd_hm("2021-07-08 00:10"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-08 03:00") | ts > ymd_hm("2021-07-08 03:10"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-08 06:00") | ts > ymd_hm("2021-07-08 06:10"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-08 09:00") | ts > ymd_hm("2021-07-08 09:10"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-08 13:10") | ts > ymd_hm("2021-07-08 14:00"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-08 23:50") | ts > ymd_hm("2021-07-09 00:00"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-09 05:50") | ts > ymd_hm("2021-07-09 06:10"))%>% # cylinder

  filter(ts < ymd_hm("2021-07-09 11:00") | ts > ymd_hm("2021-07-09 12:30"))%>% # cylinder
  # odd data on 9 July between 11 and 12 :o
  filter(ts < ymd_hm("2021-07-09 17:50") | ts > ymd_hm("2021-07-09 18:10"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-12 08:50") | ts > ymd_hm("2021-07-12 12:00"))%>% # cylinder
  # for reasons I don't know, the bg's are really high from 9-12 on 12 July
  filter(ts < ymd_hm("2021-07-12 15:50") | ts > ymd_hm("2021-07-12 16:30"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-13 09:00") | ts > ymd_hm("2021-07-13 17:00"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-14 09:00") | ts > ymd_hm("2021-07-14 10:30"))%>% # denuder and cylinder
  filter(ts < ymd_hm("2021-07-14 13:25") | ts > ymd_hm("2021-07-14 15:30"))%>% # denuder and cylinder
  filter(ts < ymd_hm("2021-07-14 15:58") | ts > ymd_hm("2021-07-14 16:15"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-15 08:15") | ts > ymd_hm("2021-07-15 15:00"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-16 10:50") | ts > ymd_hm("2021-07-16 12:00"))%>% # cylinder
  filter(ts < ymd_hm("2021-07-16 15:00") | ts > ymd_hm("2021-07-16 17:30"))%>% #  passivant
  # 16 July noonish to 3ish, passivant replacement
  #filter(ts < ymd_hms("2021-07-17 07:17:20") | ts > ymd_hm("2021-07-17 07:18"))%>% #  bad
  #filter(ts < ymd_hms("2021-07-17 07:42:30") | ts > ymd_hms("2021-07-17 07:42:50"))%>% #  bad
  #filter(ts < ymd_hms("2021-07-18 02:38:30") | ts > ymd_hms("2021-07-18 02:39:00"))%>% #  bad
  #filter(ts < ymd_hms("2021-07-18 02:46:00") | ts > ymd_hms("2021-07-18 02:46:20"))%>% #  bad
  #filter(ts < ymd_hm("2021-07-19 04:24") | ts > ymd_hm("2021-07-19 04:25")) %>%  #  bad
  #filter(ts < ymd_hms("2021-07-19 05:07:05") | ts > ymd_hms("2021-07-19 05:07:40"))%>% #  bad
  #filter(ts < ymd_hms("2021-07-19 06:26:30") | ts > ymd_hms("2021-07-19 06:27:10"))%>% #  bad
  #filter(ts < ymd_hms("2021-07-19 08:27:45") | ts > ymd_hms("2021-07-19 08:28:15"))%>% #  bad
  
  filter(ts < ymd_hm("2021-07-19 13:28") | ts > ymd_hm("2021-07-19 15:30")) %>%  #  cylinder
  #filter(ts < ymd_hm("2021-07-19 22:36") | ts > ymd_hm("2021-07-19 22:40")) %>%  #  bad
  filter(ts < ymd_hm("2021-07-19 23:55") | ts > ymd_hm("2021-07-20 00:00")) %>%  #  cylinder
  
  filter(ts < ymd_hm("2021-07-20 00:10") | ts > ymd_hm("2021-07-20 00:15")) %>% #  additions
  
  filter(ts < ymd_hm("2021-07-20 01:00") | ts > ymd_hm("2021-07-20 03:30")) %>% #  additions
  #filter(ts < ymd_hm("2021-07-20 05:10") | ts > ymd_hm("2021-07-20 05:15")) %>% #  bad
  
  
  filter(ts < ymd_hm("2021-07-20 11:58") | ts > ymd_hm("2021-07-20 13:05")) %>% #  cy;omder
  filter(ts < ymd_hm("2021-07-20 13:16") | ts > ymd_hm("2021-07-20 13:17")) %>% #  cy;omder
  
  filter(ts < ymd_hm("2021-07-20 14:00") | ts > ymd_hm("2021-07-20 14:30")) %>% #  cy;omder
  filter(ts < ymd_hm("2021-07-20 15:30") | ts > ymd_hm("2021-07-20 16:30")) %>% # clyinder
  #filter(ts < ymd_hm("2021-07-20 22:14") | ts > ymd_hm("2021-07-20 22:15")) %>% #  bad
  
  # weird bg between 1:30 and 3am
  filter(ts < ymd_hm("2021-07-21 00:10") | ts > ymd_hm("2021-07-21 00:15")) %>% #  additions
  
  filter(ts < ymd_hm("2021-07-21 01:25") | ts > ymd_hm("2021-07-21 03:30")) %>% #  cy;omder
  #filter(ts < ymd_hms("2021-07-21 04:58:28") | ts > ymd_hm("2021-07-21 04:59")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 05:13:35") | ts > ymd_hms("2021-07-21 05:13:50")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 05:21:30") | ts > ymd_hms("2021-07-21 05:22:00")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 05:23:07") | ts > ymd_hms("2021-07-21 05:23:40")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 05:26:30") | ts > ymd_hms("2021-07-21 05:27:00")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 06:22:45") | ts > ymd_hms("2021-07-21 06:22:55")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 06:32:35") | ts > ymd_hms("2021-07-21 06:32:50")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 06:34:22") | ts > ymd_hms("2021-07-21 06:34:30")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 06:44:00") | ts > ymd_hms("2021-07-21 06:44:15")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 06:51:15") | ts > ymd_hms("2021-07-21 06:51:40")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 06:52:45") | ts > ymd_hms("2021-07-21 06:53:00")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 07:18:20") | ts > ymd_hms("2021-07-21 07:18:45")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 07:20:10") | ts > ymd_hms("2021-07-21 07:20:40")) %>% #  bad
  filter(ts < ymd_hm("2021-07-21 08:45") | ts > ymd_hm("2021-07-21 10:00")) %>% #  cy;omder
  filter(ts < ymd_hm("2021-07-21 12:25") | ts > ymd_hm("2021-07-21 13:08")) %>% #  cy;omder
  #filter(ts < ymd_hms("2021-07-21 13:37:20") | ts > ymd_hms("2021-07-21 13:37:25")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 13:49:30") | ts > ymd_hms("2021-07-21 13:49:50")) %>% #  bad
  filter(ts < ymd_hm("2021-07-21 14:23") | ts > ymd_hm("2021-07-21 15:20")) %>%  #  cy;omder
  filter(ts < ymd_hm("2021-07-21 16:00") | ts > ymd_hm("2021-07-21 16:23")) %>%  #  cy;omder
  #filter(ts < ymd_hms("2021-07-21 17:38:15") | ts > ymd_hms("2021-07-21 17:38:30")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 17:47:37") | ts > ymd_hms("2021-07-21 17:47:52")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 22:10:45") | ts > ymd_hms("2021-07-21 22:10:00")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 22:28:37") | ts > ymd_hms("2021-07-21 22:28:52")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 22:39:30") | ts > ymd_hms("2021-07-21 22:40:00")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 22:49:00") | ts > ymd_hms("2021-07-21 22:49:20")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 23:05:20") | ts > ymd_hms("2021-07-21 23:06:00")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 23:13:30") | ts > ymd_hms("2021-07-21 23:14:30")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 23:43:15") | ts > ymd_hms("2021-07-21 23:43:35")) %>% #  bad
  
  #filter(ts < ymd_hms("2021-07-21 23:46:40") | ts > ymd_hms("2021-07-21 23:47:30")) %>% #  bad
  #filter(ts < ymd_hms("2021-07-21 23:58:30") | ts > ymd_hms("2021-07-21 23:59:59")) %>% #  bad
  
  
  filter(ts < ymd_hm("2021-07-22 08:30")) # cylinder
# Now I need to get rid of the 20 minutes around blanks beyond cal points
  # July 8


ambBlanks <- dfAnal %>% 
  filter(ValveW == 2, )

ambBlanks <- dfAnal %>% 
  filter(between(minute(ts)+ (second(ts)/60), 11,12) | 
           between(minute(ts)+ (second(ts)/60), 21,22) | 
           between(minute(ts)+ (second(ts)/60), 31,32) | 
           between(minute(ts)+ (second(ts)/60), 41,42) |
           between(minute(ts)+ (second(ts)/60), 51,52) |
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 61, 62) | 
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 121, 122) | 
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 241, 242) | 
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 301, 302) |
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 421, 422)|
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 481, 482)|
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 601, 602)|
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 661, 662) | 
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 781, 782) | 
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 841, 842) | 
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 961, 962) |
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1021, 1022)|
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1141, 1142)|
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1201, 1202)|
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1321, 1322)|
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1381, 1382), 
         ValveW == 2)

ambBlanksList <- split.groups(ambBlanks)
ambBlanksMean <- lapply(ambBlanksList,
                        function(x){data.frame(ts=floor_date(median(x$ts),
                                                             unit = "10 min"),
                                               hclBlank = mean(x$hcl,na.rm = TRUE))})

ambBlanksSd <- lapply(ambBlanksList,
                      function(x){data.frame(ts=floor_date(median(x$ts),
                                                           unit = "10 min"),
                                             hclBlankSd = sd(x$hcl,na.rm=TRUE))})

ambBlanksDF <- do.call(rbind, ambBlanksMean)
ambBlanksSdDF <- do.call(rbind, ambBlanksSd)
blankAnalDF <- base::merge(ambBlanksDF,ambBlanksSdDF, all=TRUE)
###############################################################
blankAnalDF <- blankAnalDF[complete.cases(blankAnalDF),]
# USE blankAnalDF FOR noise analysis!
###############################################################








earlyData <- dfAnal %>% 
  filter(ts < ymd("2021-07-08"))

lateData <- dfAnal %>% 
  filter(ts > ymd("2021-07-08")) %>% 
    filter(!(between(hour(ts)*60 + minute(ts) + (second(ts)/60), 171, 202) | 
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 351, 382) | 
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 531, 562) | 
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 721, 742) |
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 891, 922)|
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1071, 1102)|
           between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1251, 1282)))
  
dfOut <- rbind(earlyData, lateData)



# For Plotting
TIME1 <- "2021-06-25 00:00:00"
TIME2 <- "2021-06-27 00:00:00"

P1 <- dfAnal %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hcl)) +
  geom_line()+
  scale_y_continuous(limits = c(-0.2,1),name = "HCl")



P2 <- dfAnal %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  ggplot(aes(x=ts,y=ValveW)) +
  geom_line(colour = "blue")+
  scale_y_continuous(name = "Valve", limits = c(0,5))

P2 <- dfAnal %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  ggplot(aes(x=ts,y=Range_F.1_L.1)) +
  geom_line(colour = "blue")+
  scale_y_continuous(name = "Temp")


grid.arrange(P1,P2,nrow = 2,ncol=1)

  

# QC for Jordan -----------------------------------------------------------
df.str <- as.data.frame(dfOut[1:2])
#df.str$hcl <- df.str$hcl*1.0515 # applying loss factor to data.
str.xts <- xts(df.str[2:ncol(df.str)], order.by = df.str$ts, tzone = "UTC")

# the stc.xts as loaded from load.tildas.data does not match time stamps from 
# winterAvg due to decimal time.  I will just convert the df.stc as I did the
# df.str above.  
stc.xts <- xts(df.stc[2:ncol(df.stc)], order.by = df.stc$ts, tzone = "UTC")


offset.correct()

# The following block of code is in response to a reviewer comment concerning
# the 1s LOD during the measurement period.  

# zero <- offset.correct(just.zero = TRUE)
# plot(zero["2021-07-21 18:00/2021-07-22 00:00"],ylim=c(-0.1,0.5))
# zeroList <- split.groups(zero,gap = 30)
# zeroMean <- sapply(zeroList,mean,na.rm=TRUE)
# zeroSd <- sapply(zeroList,sd,na.rm=TRUE)
# zerots <- format(as.POSIXct(sapply(zeroList,
#                                    function(x){median(index(x))}),
#                             origin = "1970-01-01"),
#                             "%Y-%m-%d %H:%M:%S")
# zeroLOD <- data.frame(ts = zerots, lod = 3*zeroSd)
# write.csv(zeroLOD, "G:/My Drive/Manuscripts/HCl/zeroLOD.csv",quote=FALSE,row.names=FALSE)


remove.cal()

dfAnalNoFilter <- data.frame("ts" = index(str.xts.CalCorr),
                             coredata(str.xts.CalCorr)) %>% 
  tibble() %>% 
  mutate(ts = ymd_hms(ts))


# I think I also need to edit which data are averaged during poorly passivated
# periods
dfAnalPassEdit <- dfAnalNoFilter %>% 
  filter(between(ts,ymd_hm("2021-06-23 00:00"),ymd_hm("2021-06-25 12:00"))) %>% 
  mutate(tsGrp = floor_date(ts,unit = "10 minutes")) %>% 
  group_by(tsGrp) %>% 
  do(tail(., n = 300)) %>% 
  ungroup() %>% 
  select(-tsGrp)

dfAnalNoFilter<- dfAnalNoFilter %>% 
  filter(!between(ts,ymd_hm("2021-06-23 00:00"),ymd_hm("2021-06-25 12:00"))) %>% 
  rows_insert(dfAnalPassEdit, by ="ts") %>% 
  arrange(ts)
  
write.csv(dfAnalNoFilter, 
          "G:/My Drive/Experiments/OSCA/Summer-osca-nonoisefilter.csv", 
          quote = FALSE, row.names = FALSE)


# dfAnalCorr <- filter.noise(STR = dfAnalNoFilter, stdev = 3)
# Filter noise based on the laser power noise
dfAnalCorrRange <- filter.noise.Range(STR = dfAnalNoFilter,stdev = 1)
# Filter noise based on the measurement noise.
#dfAnalCorr <- filter.noise(STR = dfAnalCorrRange, stdev = 2)
dfAnalCorrChi <- filter.noise.chi(STR = dfAnalCorrRange, stdev = 1)

# dfAnalCorr <- filter.noise(STR = dfAnalCorrRange, stdev = 3)
#outXtsAlt <- filter.noise.alt(stdev = 2)
# dfAnalCorr <- data.frame("ts" = index(outXts),
#                          coredata(outXts)) %>% 
#   tibble() %>% 
#   mutate(ts = ymd_hms(ts))
# 
# dfAnalCorrAlt <- data.frame("ts" = index(outXtsAlt),
#                             coredata(outXtsAlt)) %>% 
#   tibble() %>% 
#   mutate(ts = ymd_hms(ts))



dfAnalMerge <- merge(dfAnalNoFilter, blankAnalDF,all=TRUE) 
  
                   
dfAnalUnc <- dfAnalMerge %>% 
  # uncertainty here comes from... uncertainty in blank, 2% from spectral fit,
  # plus 2.3% variability in perm source.  this is larger than the anticipated
  # offset from line loss.
  mutate(hclBlankSd2 = sqrt(na.locf(hclBlankSd,na.rm = FALSE)^2 + (.02*hcl)^2 + (0.0515*hcl)^2)) %>% 
  select(ts,hcl, hclBlankSd2) %>% 
  rename(hclUnc = hclBlankSd2) %>% 
  filter(complete.cases(.)) %>% 
  mutate(hcl = round(hcl, digits = 4), hclUnc = round(hclUnc,digits = 4)) %>% 
  write.csv("G:/My Drive/Experiments/OSCA/data/2021/hclCEDA.csv",quote=FALSE,row.names=FALSE)




# For Plotting
TIME1 <- "2021-06-10 00:00:00"
TIME2 <- "2021-06-17 00:00:00"

P1 <- dfAnalCorr %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hcl)) +
  geom_line()+
  scale_y_continuous(limits = c(-0.2,0.5),name = "HCl")

P2 <- dfAnalCorrChi %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hcl)) +
  geom_line()+
  scale_y_continuous(limits = c(-0.2,0.5),name = "HCl")

# P2 <- dfAnalNoFilter %>% 
#   filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
#   ggplot(aes(x=ts,y=hcl)) +
#   geom_line()+
#   scale_y_continuous(limits = c(-0.2,0.5),name = "HCl")

grid.arrange(P1,P2,nrow = 2,ncol=1)


write.csv(dfAnalCorrChi, "G:/My Drive/Experiments/OSCA/Summer-osca-rangefilter-1sdchifilter-TOPUBLISH.csv",
          quote=FALSE,row.names=FALSE)

# analXts <- xts(as.data.frame(dfAnalCorr)[2:ncol(dfAnalCorr)],order.by = dfAnalCorr$ts)
# analXts <- merge(analXts,stc.xts$ValveW,all = TRUE)
# write.zoo(analXts, "G:/My Drive/Experiments/OSCA/Summer-osca-3sdfilter.csv",
#           quote=FALSE,row.names=FALSE,sep=",",index.name = "ts")
# 

outDF <- dfAnalCorr %>% 
  left_join(df.stc) %>% 
  mutate(ts = floor_date(ts, "60 minute")) %>% 
  group_by(ts) %>% 
  summarize_all(mean) %>% 
  ungroup()

write.csv(outDF, "G:/My Drive/Experiments/OSCA/Summer-osca-rangefilter-1sdchifilter-60min.csv",
          quote=FALSE, row.names= FALSE)


analXts30s <- period.apply(analXts,INDEX = endpoints(x = analXts,on = "s",k=30),mean, na.rm=TRUE)
index(analXts30s) <- align.time(index(analXts30s),n=30)-30
analXts60s <- period.apply(analXts,INDEX = endpoints(x = analXts,on = "s",k=60),mean, na.rm=TRUE)
index(analXts60s) <- align.time(index(analXts60s),n=60)-60

write.zoo(analXts30s, "G:/My Drive/Experiments/OSCA/Summer-osca-3sdfilter-30s.csv",
          quote=FALSE,row.names=FALSE,sep=",",index.name = "ts")


write.zoo(analXts60s, "G:/My Drive/Experiments/OSCA/Summer-osca-3sdfilter-60s.csv",
          quote=FALSE,row.names=FALSE,sep=",",index.name = "ts")


# valveLastWeekPlot <- dfAnal %>% 
#                       filter(between(ts, ymd_hms(TIME1), 
#                                      ymd_hms(TIME2))) %>% 
#                       ggplot(aes(x=ts,y= ValveW)) +
#                       geom_line()
#                       #scale_y_continuous(name = "ValveW")

# grid.arrange(hclLastWeekPlot,valveLastWeekPlot,nrow = 2,ncol=1)
grid.arrange(P1,P2,nrow = 2,ncol=1)

# offset.correct()
# remove.cal()
# filter.noise.alt(fit = "hcl", RM = TRUE)
# x <- qplot(x=Index,y = hcl, data=str.xts$hcl["2021-06-11 00:00/2021-06-13 00:00"],geom="line",ylab = "HCl (ppb)",xlab="UTC",ylim = c(-0.1,1),main="Raw")+theme_bw()
# y <- qplot(x=Index,y = hcl, data=str.xts.BckCorr$hcl["/2021-06-10"],geom="line",ylab = "HCl (ppb)",xlab="UTC",ylim = c(-0.2,1))+theme_bw()
#x <- qplot(x=Index,y = hcl, data=str.xts.CalCorr$hcl["2021-06-11 00:00/"],geom="line",ylab = "HCl (ppb)",xlab="UTC",ylim = c(-0.2,1),main = "Normal Filters")+theme_bw()
#y <- qplot(x=Index,y = hcl, data=str.xts.Filtered$hcl["2021-06-11 00:00/"],geom="line",ylab = "HCl (ppb)",xlab="UTC",ylim = c(-0.2,1),main = "HCl Chi Square Filter")+theme_bw()
#y <- qplot(x=Index,y = hcl, data=str.xts.Filtered$hcl["2021-06-11 00:00/"],geom="line",ylab = "HCl (ppb)",xlab="UTC",ylim = c(-0.2,1),main = "Removed Data")+theme_bw()
#xx <- qplot(x=Index,y = X1, data=stc.xts["2021-06-11 00:00/2021-06-13 00:00"],geom="line",ylab = "HCl Chi Sq",xlab="UTC",ylim = c(-0.1,0.4),main = "Actual HCl Chi Square",colour=I("blue"))+theme_bw()

# Summer Processing -------------------------------------------------------

x <- qplot(x=Index,y = hcl, data=a$hcl["2021-06-11 00:00/"],geom="line",ylab = "HCl (ppb)",xlab="UTC",ylim = c(-0.1,1),main="Filtered")+theme_bw()
y <- qplot(x=Index,y = hcl, data=b$hcl["2021-06-11 00:00/"],geom="line",ylab = "HCl (ppb)",xlab="UTC",ylim = c(-0.1,1),main="Removed Data")+theme_bw()


grid.arrange(x,y,nrow=2)

MAQS <- read.csv("G:/My Drive/Data/Supersite/MAQS_OSCA2021.csv",stringsAsFactors=FALSE)

MAQS$ts <- as.POSIXct(MAQS$ts, "%m/%d/%Y %H:%M:%S",tz = "UTC")
MAQS$Precipitation.Classification <- NULL

MAQS.xts <- xts(MAQS[,2:ncol(MAQS)],order.by = MAQS$ts, tzone = "UTC")
# MAQS.xts$NO2..ppb.[MAQS.xts$NO2..ppb.< -10] <- NA
# MAQS.xts$NO..ppb.[MAQS.xts$NO..ppb.< -10] <- NA
# MAQS.xts$NOy..ppb.[MAQS.xts$NOy..ppb.< -10] <- NA
# MAQS.xts$noz <- MAQS.xts$NOy..ppb.-MAQS.xts$NO2..ppb. - MAQS.xts$NO..ppb.

# write.zoo(MAQS.xts,file="G:/My Drive/Data/Supersite/MAQS_OSCA2021_filtered.csv",quote=FALSE,sep=",",index.name = "ts")


################################################################################
# Perm Source vs RH, ambient humidity
# Let's filter out data and look at relationship between RH and HCl additions
# Cylinder additions began on 26 June, 2.5 sccm til 28 June, when they are upped to 5 sccm
str.filtered <- apply.mask(STR = str.xts.BckCorr, 
                           MASK = c(denuderFilter,
                                    cylinderFilter,
                                    erroneousFilter))

xtsMerge<- merge(str.filtered,stc.xts,all=TRUE)
#permData <- xtsMerge[xtsMerge$ValveW == 3 | xtsMerge$ValveW==51,]
#permData <- xtsMerge[xtsMerge$ValveW == 51,] # 51 ix valves 1,2,5,6
#permData <- xtsMerge[xtsMerge$ValveW == 3,]

permDataAvg <- period.apply(permData$hcl,INDEX=endpoints(permData,on="hours"),
                            FUN = function(x){mean(tail(x, 90))})
permDataAvgDF <- align.time(permDataAvg[which(permDataAvg$hcl > 1),],n=60)
permRhMerge <- merge(permDataAvgDF,MAQS.xts$Relative.Humidity....,all=FALSE)

tildasPermWater <- period.apply(permData$h2o,INDEX=endpoints(permData,on="hours"),FUN = median)
tildasPermWater <- align.time(tildasPermWater,n=60)
waterDF <- merge(tildasPermWater,MAQS.xts$Temperature..deg.C.,all=FALSE)
waterDF$rh <- waterDF$h2o/sat.water(TEMP = waterDF$Temperature..deg.C.+273.15)

test <- merge(permRhMerge$hcl, waterDF$rh,all=FALSE)


plot(coredata(permRhMerge$Relative.Humidity....["2021-06-26 00:00/2021-06-28 00:00"]),coredata(permRhMerge$hcl["2021-06-26 00:00/2021-06-28 00:00"]),pch=19, xlab = "Ambient RH (%)", ylab = "HCl (ppb)")
plot(coredata(test$rh["2021-06-26 00:00/2021-06-28 00:00"]*100),coredata(test$hcl["2021-06-26 00:00/2021-06-28 00:00"]),pch=19, ylab = "HCl (ppb)",xlab = "TILDAS RH (%)" )
earlyLM <- lm(coredata(test$hcl["2021-06-26 00:00/2021-06-28 00:00"])~coredata(test$rh["2021-06-26 00:00/2021-06-28 00:00"]))

 plot(coredata(permRhMerge$Relative.Humidity....["2021-07-16 00:00/"]),coredata(permRhMerge$hcl["2021-07-16 00:00/"]),pch=19)
plot(coredata(test$rh["2021-07-18 00:00/"]),coredata(test$hcl["2021-07-18 00:00/"]),pch=19,ylab = "HCl (ppb)",xlab = "TILDAS RH (%)")
lateLM <- lm(coredata(test$hcl["2021-07-18 00:00/"])~coredata(test$rh["2021-07-18 00:00/"]))
summary(lateLM)

#############################################################################
#
# Perm Source vs RH, dry humidity
# Cylinder additions began on 26 June, 2.5 sccm til 28 June, when they are upped
# to 5 sccm after 12pm

# For July, dry additions only occured prior to 17 July (ie., 16 July is last 
# day for dry additions).  
str.filtered <- apply.mask(STR = str.xts.BckCorr, 
                           MASK = c(denuderFilter,
                                    cylinderFilter,
                                    erroneousFilter))

xtsMerge<- merge(str.filtered,stc.xts,all=TRUE)
permData <- xtsMerge[xtsMerge$ValveW == 51,] # 51 ix valves 1,2,5,6

permDataAvg <- period.apply(permData$hcl,INDEX=endpoints(permData,on="minutes", k =15),
                            FUN = function(x){mean(tail(x, 90))})
permDataAvgDF <- align.time(permDataAvg[which(permDataAvg$hcl > 1),],n=60)
permRhMerge <- merge(permDataAvgDF,MAQS.xts$Relative.Humidity....,all=FALSE)

tildasPermWater <- period.apply(permData$h2o,INDEX=endpoints(permData,on="hours"),FUN = median)
tildasPermWater <- align.time(tildasPermWater,n=60)
waterDF <- merge(tildasPermWater,MAQS.xts$Temperature..deg.C.,all=FALSE)
waterDF$rh <- waterDF$h2o/sat.water(TEMP = waterDF$Temperature..deg.C.+273.15)

test <- merge(permRhMerge$hcl, waterDF$rh,all=FALSE)

plot(coredata(permRhMerge$Relative.Humidity....["2021-06-26 00:00/2021-06-28 00:00"]),coredata(permRhMerge$hcl["2021-06-26 00:00/2021-06-28 00:00"]),pch=19, xlab = "Ambient RH (%)", ylab = "HCl (ppb)")
plot(coredata(test$rh["2021-06-26 00:00/2021-06-28 00:00"]*100),coredata(test$hcl["2021-06-26 00:00/2021-06-28 00:00"]),pch=19, ylab = "HCl (ppb)",xlab = "TILDAS RH (%)" )
earlyLM <- lm(coredata(test$hcl["2021-06-26 00:00/2021-06-28 00:00"])~coredata(test$rh["2021-06-26 00:00/2021-06-28 00:00"]))

plot(coredata(permRhMerge$Relative.Humidity....["2021-07-10 00:00/"]),coredata(permRhMerge$hcl["2021-07-10 00:00/"]),pch=19)
plot(coredata(test$rh["2021-07-10 00:00/"]),coredata(test$hcl["2021-07-10 00:00/"]),pch=19,ylab = "HCl (ppb)",xlab = "TILDAS RH (%)")
lateLM <- lm(coredata(test$hcl["2021-07-18 00:00/"])~coredata(test$rh["2021-07-18 00:00/"]))


#############################################################################

perm.vs.humid <- function(STR=str.xts.BckCorr, STC=stc.xts, CONDITION="humid"){
  if(exists("MAQS.xts")==FALSE){
    MAQS <- read.csv("G:/My Drive/Data/Supersite/MAQS_OSCA2021.csv",stringsAsFactors=FALSE)
    
    MAQS$ts <- as.POSIXct(MAQS$ts, "%m/%d/%Y %H:%M:%S",tz = "UTC")
    MAQS$Precipitation.Classification <- NULL
    
    
    MAQS.xts <- xts(MAQS[,2:ncol(MAQS)],order.by = MAQS$ts, tzone = "UTC")
  }
  

  str.filtered <- apply.mask(STR =STR, 
                             MASK = c(denuderFilter,
                                      cylinderFilter,
                                      erroneousFilter))
  
  xtsMerge<- merge(str.filtered,stc.xts,all=TRUE)
  
  if(CONDITION == "dry"){
    permData <- xtsMerge[xtsMerge$ValveW == 51,] # 51 ix valves 1,2,5,6
  }else if(CONDITION == "humid"){
    permData <- xtsMerge[xtsMerge$ValveW == 3,] # 3 ix valves 1,2
  }else{
    return(print("Please select from the options of *dry* or *humid*"))
  }
  

  permDataAvg <- period.apply(permData$hcl,INDEX=endpoints(permData,on="minutes", k=15),
                              FUN = function(y){mean(tail(y, 90),na.rm=TRUE)})
  
  permDataSem <- period.apply(permData$hcl,INDEX=endpoints(permData,on="minutes", k=15),
                              FUN = function(y){sd(tail(y, 90),na.rm=TRUE)}) / sqrt(90)
  
  permDataAvgDF <- align.time(permDataAvg[which(permDataAvg$hcl > 1),],n=60)
  permDataAvgDF$hclSd <-  align.time(permDataSem[which(permDataAvg$hcl > 1),],n=60)
  
  permRhMerge <- merge(permDataAvgDF,MAQS.xts$Relative.Humidity....,all=FALSE)
  
  tildasPermWater <- period.apply(permData$h2o,INDEX=endpoints(permData,on="minutes", k =15),
                                  FUN = median,na.rm=TRUE)
  tildasPermWater$waterSD <- period.apply(permData$h2o,INDEX=endpoints(permData,on="minutes", k =15),
                                  FUN = sd,na.rm=TRUE)
  #tildasPermWater <- align.time(tildasPermWater,n=60)[c(1:3,5:13)]
  #tildasPermWater$h2o <- testH2o
  tildasPermWater <- align.time(tildasPermWater,n=60)
  
  
  waterDF <- merge(tildasPermWater,MAQS.xts$Temperature..deg.C.,all=TRUE)
  waterDF$rh <- waterDF$h2o/sat.water(TEMP = waterDF$Temperature..deg.C.+273.15)*100
  waterDF$rhsd <- waterDF$waterSD/sat.water(TEMP = waterDF$Temperature..deg.C.+273.15)*100
  
  outDF <- merge(permRhMerge, waterDF,all=FALSE)
  
  if(CONDITION == "dry"){
    outDF <- outDF[outDF$rh < 50,]
    outDF <- period.apply(outDF, INDEX = endpoints(outDF, on = "hours", k=1), mean)
    
  }  
  
  
  outDF <- data.frame(ts = index(outDF), coredata(outDF))

  return(outDF)
  
}



# Let's try doing the thing!
# Early cylinder data

# load.tildas.data(START = "2021-06-26", STOP = "2021-06-29",STC=TRUE)
# offset.correct()
ambEarly <- perm.vs.humid(STR = str.xts.BckCorr["/2021-06-28 01:00",], STC = stc.xts["/2021-06-28 01:00",],
                          CONDITION = "humid")
dryEarly <- perm.vs.humid(STR = str.xts.BckCorr, STC = stc.xts,
                          CONDITION = "dry")
names(dryEarly) <- c("ts","hclDry","hclDrySd","Relative.Humidity....Dry", "rhDry")
earlyDF <- merge(ambEarly,dryEarly,all=TRUE)
earlyDF$hclDry <- na.approx(earlyDF$hclDry, x=earlyDF$ts)


earlyDF$hclNorm <- earlyDF$hcl / earlyDF$hclDry
#ambEarly<- ambEarly[ambEarly$hclNorm < 1,]
earlyLM <- lm(hclNorm~rh,data = earlyDF)
plot(earlyDF$rh,earlyDF$hclNorm,pch = 19)
summary(earlyLM)








# load.tildas.data(START = "2021-07-08", STOP = "2021-07-16",STC=TRUE)
# offset.correct()
amb <- perm.vs.humid(STR = str.xts.BckCorr["/2021-07-16 13:00"], STC = stc.xts,
                          CONDITION = "humid")
amb<- amb[amb$rh < 100,]
dry <- perm.vs.humid(STR = str.xts.BckCorr["/2021-07-16 13:00"], STC = stc.xts,
                          CONDITION = "dry")
dryMean <- mean(dry$hcl)
drySd <- sd(dry$hcl)
dry$day <- 1:9
# dry$day <- 1
# amb$day <- 1



amb$day <- c(rep(1,8),rep(2,8),rep(3,8),rep(4,8), rep(5,7), rep(6,5),rep(7,5),
             rep(8,5),rep(9,4))
amb$dayAlt <- c(rep(1,4),rep(2,8),rep(3,8),rep(4,8), rep(5,8), rep(6,6),
                rep(7,5), rep(8,5),rep(9,6))

amb$hclNorm <- amb$hcl / dryMean
# amb$hclNorm <- mapply(FUN = function(x,y){x/dry$hcl[which(dry$day == y)]},
#                  x = amb$hcl, y = amb$dayAlt)
amb$hclNormsd <- amb$hclNorm * sqrt((amb$hclSd/amb$hcl)^2 + (drySd/dryMean)^2)


dry$hclNorm <- dry$hcl/dryMean
dry$hclNormsd <- dry$hclNorm * sqrt((dry$hclSd/dry$hcl)^2 + (drySd/dryMean)^2)



plot(c(amb$rh,dry$rh),c(amb$hcl,dry$hcl),pch = 19, ylab = "HCl (ppb)",
     xlab = "TILDAS RH (%)", main = "2021-07-08/2021-07-16",
     col=viridis(9)[c(amb$day,dry$day)],cex=1.5)

qplot(c(amb$rh,dry$rh),c(amb$hcl,dry$hcl), ylab = "HCl (ppb)",
      xlab = "TILDAS RH (%)", main = "2021-07-15/2021-07-16",
      colour=c(amb$day,dry$day), ylim = c(4.4,5.65),
      xlim = c(5,100))+
  scale_color_gradientn(limits = c(1,9),colours = rainbow(9),
                        values = c(0:8/8))+
  geom_point(size=4)+
  geom_point(shape=1,size=4,colour="black")+
  theme_bw(base_size = 12)+  
  guides(col = guide_colourbar(title = "Day Number"))

qplot(c(amb$rh),c(amb$hclNorm), ylab = "Normalized HCl (unitless)",
      xlab = "TILDAS RH (%)", main = "2021-07-08/2021-07-16",
      colour=c(amb$day),
      xlim = c(5,100))+
  scale_color_gradientn(limits = c(1,9),colours = rainbow(9),
                        values = c(0:8/8))+
  geom_point(size=4)+
  geom_point(shape=1,size=4,colour="black")+
  theme_bw(base_size = 12)+  
  guides(col = guide_colourbar(title = "Day Number"))




ambLM <- lm(hcl~rh,data = amb)
ambLM <- lm(rh~hcl,data = amb)
summary(ambLM)






# For the later perm source data (after July 16), the dry air overblow doesn't 
# match up with the perm source additions.
# load.tildas.data(START = "2021-07-10", STOP = "2021-07-22",STC=TRUE)
# offset.correct()
# ambLate <- perm.vs.humid(STR = str.xts.BckCorr["2021-07-18/",], STC = stc.xts,
#                          CONDITION = "humid")
# 
# ambLate <- perm.vs.humid(STR = str.xts.BckCorr["2021-07-13 12:00/2021-07-16 23:59",], 
#                          STC = stc.xts,CONDITION = "humid")
# dryLate <- perm.vs.humid(STR = str.xts.BckCorr["2021-07-13 12:00/2021-07-16 23:59",], 
#                          STC = stc.xts,CONDITION = "dry")


ambLate <- perm.vs.humid(STR = str.xts.BckCorr["2021-07-10 00:00/2021-07-16 23:59",], 
                         STC = stc.xts,CONDITION = "humid")
ambLate <- ambLate[ambLate$rh <=100,]
dryLate <- perm.vs.humid(STR = str.xts.BckCorr["2021-07-10 00:00/2021-07-16 23:59",], 
                         STC = stc.xts,CONDITION = "dry")

ambLate$hclNorm <- ambLate$hcl / mean(dryLate$hcl[dryLate$rh<50])
#ambLate<- ambLate[ambLate$hclNorm < 1.5,]
lateLM <- lm(hcl~rh,data = ambLate)
plot(c(ambLate$rh,dryLate$rh),c(ambLate$hcl,dryLate$hcl), pch=19, ylab = "HCl (ppb)",xlab = "TILDAS RH (%)",
     col=heat.colors(rev=TRUE, n = nrow(ambLate)), xlim=c(0,100))
summary(lateLM)




ambplot(x = c(ambEarly$rh,ambLate$rh),y = c(ambEarly$hclNorm,ambLate$hclNorm),pch=19, ylab = "HCl (ppb)",xlab = "TILDAS RH (%)" )
plot(x = ambEarly$rh,y = ambEarly$hclNorm,pch=19, ylab = "HCl (ppb)",xlab = "TILDAS RH (%)" )
plot(x = ambEarly$rh,y = ambEarly$hclNorm,pch=19, ylab = "HCl (ppb)",
     xlab = "TILDAS RH (%)" ,xlim=c(30,100),ylim=c(0.7,1))
points(x = ambLate$rh/100,y = ambLate$hcl,pch=19, col="blue" )


earlyLM <- lm(ambEarly$hcl~ambEarly$rh)

lateLM <- lm(ambLate$hcl~ambLate$rh)


summary(earlyLM)
summary(lateLM)


###################################################################
# This is some test code to add Inf to time gaps in data purely for
# plotting purposes.
timeDiffIX <- which(diff(index(str.xts.Filtered)) > 5)
newRows <- str.xts.Filtered[timeDiffIX]
index(newRows) <- index(newRows)+3
newRows$hcl <- NaN
forPlotting <- rbind(str.xts.Filtered.tensec$hcl,newRows$hcl)
forPlottingSort <- sort(forPlotting)




qplot(x=Index,y = hcl, data=str.filtered,geom="line",ylab = "HCl (ppb)",xlab="UTC")+theme_bw()
remove.cal()
str.xts.CorrFiltered <- filter.noise(str.xts.CalCorr,stdev = 3)
str.xts.CF10s <- period.apply(str.xts.CorrFiltered,endpoints(str.xts.CorrFiltered,"seconds",k=10),mean)
str.xts.CF60s <- period.apply(str.xts.CorrFiltered,endpoints(str.xts.CorrFiltered,"seconds",k=60),mean)
index(str.xts.CF60s) <- align.time(index(str.xts.CF60s), n = 60)
index(str.xts.CF60s) <- index(str.xts.CF60s) -60

forWR <- merge(str.xts.CF60s$hcl,MAQS.xts,all=FALSE)["/2021-06-18",]


plot1 <- qplot(x=Index, y = hcl, data = str.xts.CalCorr,ylim = c(0,3),geom = "line")+theme_bw()
plot2 <- qplot(x=Index, y = hcl, data = str.xts.CorrFiltered,ylim = c(0,3),geom = "line")+theme_bw()
#plot2 <- qplot(x=Index, y = hcl, data = outDFXts,ylim = c(0,1),geom = "line")+theme_bw()

grid.arrange(plot1,plot2)

write.zoo(str.xts.CorrFiltered$hcl,
          file="C:/Users/John/Desktop/hcltildas_202107_OSCA_20210901.csv",sep=",",
          quote=FALSE,index.name="ts")


write.zoo(str.xts.CF10s$hcl,
          file="C:/Users/John/Desktop/hcltildas_202107_OSCA_20210901-10savg.csv",sep=",",
          quote=FALSE,index.name="ts")

##############################
# AVAR
library(avar)
test <- str.xts$hcl["2021-07-01 06:45/2021-07-01 06:53"]
hclAVAR <- avar(test,freq=1)
plot.avar(hclAVAR)
test2 <- detrend(test)
hclAVAR <- avar(test2,freq=1)

#I think the below is what we'll use 
test <- str.xts$hcl["2021-07-13 13:00/2021-07-13 13:28"]
hclAVAR <- avar(test,freq=1)
plot.avar(hclAVAR)
test2 <- detrend(test)
hclAVAR <- avar(test2,freq=1)


################################
# NOz (HNO3) vs HCl
if(exists("MAQS.xts")==FALSE){
  MAQS <- read.csv("G:/My Drive/Data/Supersite/MAQS_OSCA2021.csv",stringsAsFactors=FALSE)
  
  MAQS$ts <- as.POSIXct(MAQS$ts, "%m/%d/%Y %H:%M:%S",tz = "UTC")
  MAQS$Precipitation.Classification <- NULL
  
  
  MAQS.xts <- xts(MAQS[,2:ncol(MAQS)],order.by = MAQS$ts, tzone = "UTC")
}
MAQS.xts$noz <- MAQS.xts$NOy..ppb.-MAQS.xts$NO2..ppb. - MAQS.xts$NO..ppb.
noz <- MAQS.xts$noz[MAQS.xts$noz > -4& MAQS.xts$noz < 5]
toExclude <- noz["2021-07-19 02:00/2021-07-19 03:00", which.i = TRUE]
noz <- noz[-toExclude]
noz.10min <- period.apply(noz, INDEX = endpoints(noz, on = "minutes", k = 1), mean)
noz.10min <- align.time(noz.10min, n = 60)
noz.10min <- noz.10min["2021-07-18/2021-07-19"]


load.tildas.data(START="2021-07-17",STOP="2021-07-22",STC = TRUE)
offset.correct()
remove.cal()
hcl.10min <- period.apply(str.xts.CalCorr$hcl, INDEX = endpoints(str.xts.CalCorr, on = "minutes", k = 1), mean)
hcl.10min <- align.time(hcl.10min, n = 60)
hcl.10min <- hcl.10min["2021-07-18/2021-07-19"]

corDF <- merge(hcl.10min,noz.10min, all=TRUE)
write.zoo(x=corDF,file = "G:/My Drive/Manuscripts/HCl/nozhcldf.csv",sep=",",
          index.name = "ts",quote=FALSE,row.names = FALSE)
cor(corDF$hcl,corDF$noz)
summary(lm(corDF$hcl~corDF$noz))


#          hcl
# noz 0.7329945

#############################
# Denuder looks
#############################
load.tildas.data(START="2021-06-20",STOP="2021-06-21",STC = TRUE)
mergeXts <- merge(str.xts,stc.xts)

blankData <- mergeXts[mergeXts$ValveW==2,]
preBlank <- mean(blankData$hcl["2021-06-22 00:10/2021-06-22 02:15"])
postBlank <- mean(blankData$hcl["2021-06-22 04:00/2021-06-22 05:30"])

rise.period <- mergeXts$hcl["2021-06-22 00:10/2021-06-22 05:30"]
rise.period$blank <- NA
rise.period$blank["2021-06-22 00:10/2021-06-22 02:15"] <- preBlank
rise.period$blank["2021-06-22 04:00/2021-06-22 05:30"] <- postBlank
rise.period$blank <- na.approx(rise.period$blank)
correctHcl <- rise.period$hcl - rise.period$blank


toExclude <- which(mergeXts$ValveW["2021-06-22 00:10/2021-06-22 05:30"]==2)

list.ix <- cumsum(c(1,diff(toExclude)!=1))
zero.ixList <- split(toExclude,f=list.ix)
zero.removal.ix <- lapply(zero.ixList, function(x){x[length(x)]:(x[length(x)]+5)})
zero.removal.ix.vector <- head(sort(c(as.vector(unlist(zero.removal.ix)),toExclude)),-5)
#correctHcl[zero.removal.ix.vector] = NA
#correctHcl <- correctHcl[-zero.removal.ix.vector]

correctHclMean <- period.apply(correctHcl,
                               INDEX=endpoints(correctHcl,on="minutes", k =1),
                              mean)

write.zoo(correctHclMean, file = "G:/My Drive/Manuscripts/HCl/denuderspikedata.csv",
          sep=",",quote=FALSE, row.names= FALSE, index.name = "ts")
plot(correctHcl["2021-06-22 02:30/2021-06-22 03:00"],ylim=c(0.2,0.25))



# The above is for denuder data when we have a verifiabile spike.
# But I also want to show situation normal

load.tildas.data(START="2021-06-20",STOP="2021-06-21",STC = TRUE)
offset.correct()
toExclude <- str.xts.BckCorr["2021-06-21 11:00/2021-06-21 14:00",which.i = TRUE]
str.xts.BckCorr[toExclude] <- NA
str.xts.Filtered <- filter.noise(STR = str.xts.BckCorr, stdev = 3)
str.xts.Filtered.B <- filter.noise.alt(STR = str.xts.BckCorr, stdev = 3,fit="hcl")


correctHclMean <- period.apply(str.xts.Filtered$hcl,
                               INDEX=endpoints(str.xts.Filtered,on="seconds", k =60),
                               mean)

correctHclMeanB <- period.apply(str.xts.Filtered.B$hcl,
                               INDEX=endpoints(str.xts.Filtered.B,on="seconds", k =60),
                               mean)

write.zoo(correctHclMeanB, file = "G:/My Drive/Manuscripts/HCl/denuderregdataB.csv",
          sep=",",quote=FALSE, row.names= FALSE,index.name = "ts")


plot(correctHcl[-(zero.removal.ix.vector)])


offset.correct()
plot(str.xts.BckCorr$hcl["2021-06-22 00:00/2021-06-22 04:00"])

# In case I need it and don't want to type it all out again
#  filter(ts < ymd_hm("2021-06-18 10:30") | ts > ymd_hm("2021-06-22 11:45")) %>% # denuder
# filter(ts < ymd_hm("2021-06-22 14:30") | ts > ymd_hm("2021-06-22 16:30"))# %>% 
# filter(ts < ymd_hm("2021-06-23 10:30") | ts > ymd_hm("2021-06-24 15:15")) %>% 
#   filter(ts < ymd_hm("2021-06-25 12:30") | ts > ymd_hm("2021-06-25 17:15")) %>% 
#   filter(ts < ymd_hm("2021-06-29 10:25") | ts > ymd_hm("2021-06-29 11:40")) %>% 
#   filter(ts < ymd_hm("2021-06-29 13:08") | ts > ymd_hm("2021-06-30 15:15")) %>% 
#   # not sure how much I like 30 June into 1 July. Leavning it for now
#   filter(ts < ymd_hm("2021-07-01 00:00") | ts > ymd_hm("2021-07-01 05:15")) %>% 
#   filter(ts < ymd_hm("2021-07-01 10:30") | ts > ymd_hm("2021-07-01 11:05")) %>% # denuder
#   filter(ts < ymd_hm("2021-07-01 12:55") | ts > ymd_hm("2021-07-01 13:10")) %>%  # cylinder
#   filter(ts < ymd_hm("2021-07-01 13:55") | ts > ymd_hm("2021-07-01 14:35")) %>% # denuder
#   filter(ts < ymd_hm("2021-07-01 15:25") | ts > ymd_hm("2021-07-01 16:30")) %>% # cylinder
#   # cylinder additions during 2300 hour on 2 July
#   # eod 2 July has a few negative data points before the cylinder addns.  Likely will wash out in avg.
#   filter(ts < ymd_hm("2021-07-03 08:00") | ts > ymd_hm("2021-07-03 10:00")) %>% # cylinder
#   # cylinder additions during 0000 hour on 4 July
#   filter(ts < ymd_hm("2021-07-05 10:15") | ts > ymd_hm("2021-07-05 14:30")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-05 21:00") | ts > ymd_hm("2021-07-05 21:15")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-06 00:00") | ts > ymd_hm("2021-07-06 00:15")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-06 03:00") | ts > ymd_hm("2021-07-06 03:10")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-06 06:00") | ts > ymd_hm("2021-07-06 06:10")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-06 09:00") | ts > ymd_hm("2021-07-06 09:10")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-06 12:00") | ts > ymd_hm("2021-07-06 12:10")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-06 12:52") | ts > ymd_hm("2021-07-06 13:05")) %>% # hno3
#   filter(ts < ymd_hm("2021-07-06 15:00") | ts > ymd_hm("2021-07-06 15:10")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-06 18:00") | ts > ymd_hm("2021-07-06 18:10")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-06 21:00") | ts > ymd_hm("2021-07-06 21:10")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-07 00:00") | ts > ymd_hm("2021-07-07 00:10")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-07 03:00") | ts > ymd_hm("2021-07-07 03:10")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-07 06:00") | ts > ymd_hm("2021-07-07 06:10")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-07 09:05") | ts > ymd_hm("2021-07-07 10:00")) %>% # hno3
#   filter(ts < ymd_hm("2021-07-07 12:00") | ts > ymd_hm("2021-07-07 12:10")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-07 15:00") | ts > ymd_hm("2021-07-07 15:10")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-07 18:00") | ts > ymd_hm("2021-07-07 18:10")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-07 21:00") | ts > ymd_hm("2021-07-07 21:10")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-08 00:00") | ts > ymd_hm("2021-07-08 00:10")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-08 03:00") | ts > ymd_hm("2021-07-08 03:10")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-08 06:00") | ts > ymd_hm("2021-07-08 06:10")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-08 09:00") | ts > ymd_hm("2021-07-08 09:10")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-08 13:10") | ts > ymd_hm("2021-07-08 14:00")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-08 23:50") | ts > ymd_hm("2021-07-09 00:00")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-09 05:50") | ts > ymd_hm("2021-07-09 06:10")) %>% # cylinder
#   
#   filter(ts < ymd_hm("2021-07-09 11:00") | ts > ymd_hm("2021-07-09 12:30")) %>% # cylinder
#   # odd data on 9 July between 11 and 12 :o
#   filter(ts < ymd_hm("2021-07-09 17:50") | ts > ymd_hm("2021-07-09 18:10")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-12 08:50") | ts > ymd_hm("2021-07-12 12:00")) %>% # cylinder
#   # for reasons I don't know, the bg's are really high from 9-12 on 12 July
#   filter(ts < ymd_hm("2021-07-12 15:50") | ts > ymd_hm("2021-07-12 16:30")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-13 09:00") | ts > ymd_hm("2021-07-13 17:00")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-14 09:00") | ts > ymd_hm("2021-07-14 10:30")) %>% # denuder and cylinder
#   filter(ts < ymd_hm("2021-07-14 13:25") | ts > ymd_hm("2021-07-14 15:30")) %>% # denuder and cylinder
#   filter(ts < ymd_hm("2021-07-14 15:58") | ts > ymd_hm("2021-07-14 16:15")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-15 08:15") | ts > ymd_hm("2021-07-15 15:00")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-16 11:15") | ts > ymd_hm("2021-07-16 12:00")) %>% # cylinder
#   filter(ts < ymd_hm("2021-07-16 15:00") | ts > ymd_hm("2021-07-16 17:30")) %>% #  passivant
#   
#   # 16 July noonish to 3ish, passivant replacement
#   filter(ts < ymd_hm("2021-07-16 15:00") | ts > ymd_hm("2021-07-16 17:30")) %>% #  passivant
#   filter(ts < ymd_hm("2021-07-19 13:28") | ts > ymd_hm("2021-07-19 15:30")) %>% #  cylinder
#   filter(ts < ymd_hm("2021-07-20 12:00") | ts > ymd_hm("2021-07-20 13:03")) %>% #  cy;omder
#   filter(ts < ymd_hm("2021-07-20 14:00") | ts > ymd_hm("2021-07-20 14:30")) %>% #  cy;omder
#   filter(ts < ymd_hm("2021-07-20 15:30") | ts > ymd_hm("2021-07-20 16:30")) %>% # clyinder
#   # weird bg between 1:30 and 3am
#   filter(ts < ymd_hm("2021-07-21 08:45") | ts > ymd_hm("2021-07-21 10:00")) %>% #  cy;omder
#   filter(ts < ymd_hm("2021-07-21 12:25") | ts > ymd_hm("2021-07-21 13:05")) %>% #  cy;omder
#   filter(ts < ymd_hm("2021-07-21 14:25") | ts > ymd_hm("2021-07-21 15:20")) %>% #  cy;omder
#   filter(ts < ymd_hm("2021-07-21 16:00") | ts > ymd_hm("2021-07-21 16:23")) %>% #  cy;omder
#   filter(ts < ymd_hm("2021-07-22 08:30")) #  cy;omder




