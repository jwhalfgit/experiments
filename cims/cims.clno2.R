calTimes <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/times_master.csv") %>% 
# calTimes <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/humidcaltimes-Sept.csv") %>% 
  mutate(st = dmy_hm(st),et = dmy_hm(et))
# calTimes <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/humidcaltimes-Sept.csv") %>% 
#   mutate(st = dmy_hm(st),et = dmy_hm(et))
#calTimes <- calTimes[1:12,]
calTimes$st <- calTimes$st - 3600 # convert to UTC
calTimes$et <- calTimes$et - 3600 # convert to UTC

# NO ----------------------------------------------------------------------

ffNO2 <- list.files("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CAPS",
                    full.names=TRUE, pattern = ".csv")

fileListNO2 <- lapply(ffNO2,read_csv)

test <- lapply(fileListNO2, function(x){select(x, TheTime,t500u_022838_NO2)})
dfCAPS <- do.call(rbind,test) %>% 
  rename(date = TheTime) %>% 
  mutate(date = mdy_hms(date) - 3600) %>% 
  select(date, t500u_022838_NO2) %>% 
  rename(no2 = t500u_022838_NO2)

write.csv(dfCAPS,
          "G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CAPS/no2-stitch.csv",
           quote = FALSE, 
          row.names = FALSE)


dfCAPS <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CAPS/no2-stitch.csv")

capsList <- list()


for(ix in 1:(nrow(calTimes))){
  capsList[[ix]] <- dfCAPS %>% 
    filter(between(date,ymd_hms(calTimes[[ix,1]]),ymd_hms(calTimes[[ix,2]]))) %>% 
    mutate(grp = calTimes$event[ix])
}

capsDf <- do.call(rbind, capsList) %>% 
  group_by(grp) %>% 
  summarise_all(.funs = list(avg = ~mean(x = .,na.rm=TRUE),
                             stdev = ~sd(x=.,na.rm=TRUE))) %>% 
  select(grp,date_avg,no2_avg,no2_stdev) %>% 
  ungroup() 

capsDf$flag <- calTimes$type[capsDf$grp]
capsDf$comment <- calTimes$comment[capsDf$grp]


#%>% 
  #mutate(NOcorr = no2 - no2[flag == "bgClno2"])

write.csv(capsDf, "G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CAPS/no2-results.csv", quote = FALSE)

# CIMS-HR -----------------------------------------------------------------

# CIMS HR STITCHING -------------------------------------------------------

#Note that data before August excluded a few data points, so I need to segregate
# 

#HRCIMSDIR <- "G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/iodide/Export"
#HRCIMSDIR <- "G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/bromide/Export"

HRCIMSDIR <- "C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2/averaged-iodide/20240611"

#HRCIMSDIR <- dir(HRCIMSDIR, pattern = "^[0-9]+$", full.names = TRUE, ignore.case = TRUE)




ffTs <- list.files(HRCIMSDIR, full.names = TRUE, pattern = "all_tseries.csv",
                   recursive = TRUE)

# # ffTs
# [1] "G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/Export/Export20240628-20240701-field/all_tseries.csv"   
# [2] "G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/Export/Export20240801-hcllineloss/all_tseries.csv"      
# [3] "G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/Export/Export20240805-highhumidityclno2/all_tseries.csv"

workingffTs <-ffTs
#workingffTs <- ffTs[1]


# TofWare outputs the timestamps in a separate file.  We need
# to load these to merge with the chemical data.
cimsTs <- lapply(workingffTs,read.csv,header=FALSE) %>% 
  do.call(rbind,.) %>% # flatten the list 
  rename(time = V1) %>%  # give the timestamps a name
  mutate(time = dmy_hms(time)) # format time into friendly format



# Now data
ffCimsHr <- list.files(HRCIMSDIR, full.names = TRUE, pattern = "all_Mx_data_HR.csv",
                       recursive = TRUE)

# [1] "G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/Export/Export20240628-20240701-field/all_Mx_data_HR.csv"   
# [2] "G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/Export/Export20240801-hcllineloss/all_Mx_data_HR.csv"      
# [3] "G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/Export/Export20240805-highhumidityclno2/all_Mx_data_HR.csv"

workingffCimsHr <- ffCimsHr
#workingffCimsHr <- ffCimsHr[1]


cimsData <- lapply(workingffCimsHr, read_csv) %>% 
  do.call(rbind,.)
names(cimsData) <- gsub(".*\\|",'', names(cimsData))# removes characters before pipe symbol
names(cimsData) <- gsub("\\-",'', names(cimsData)) # removes hyphen from negatively charged compounds
names(cimsData) <- sub(pattern = "^(\\d)", # adds the characters "unk" before unknown masses
                       replacement = "unk\\1", x = names(cimsData)) 



# # Iodide ------------------------------------------------------------------
dfCims<- as_tibble(cbind(cimsTs, cimsData)) %>%
  arrange(time) %>%
  rename(cl37no2 = "I[37Cl]NO2") %>% 
  mutate(clno2norm = ifelse(I>5e4,IClNO2/(I + IH2O),NA)*10^6) %>%
  mutate(clno2norm = replace(clno2norm, I < 5e4,NA)) %>%
  mutate(clno237norm = cl37no2/(I + IH2O)*10^6) %>% 
  mutate(isoratio = clno2norm/clno237norm) %>% 
  mutate(iratio = ifelse(I>5e4,IH2O / I, NA)) %>%
  mutate(iratio = ifelse(iratio>=1e-2,IH2O / I, NA)) 
  # mutate(calFactor = (2104*iratio+362)) %>% 
  # mutate(clno2Cal = clno2norm / (2104*iratio+362))


# Bromide -----------------------------------------------------------------
# dfCims<- as_tibble(cbind(cimsTs, cimsData)) %>%
#   mutate(clno2norm = ifelse(Br>1e4,BrClNO2/(Br + BrH2O)*10^6,NA)) %>%
#   mutate(clno2norm = replace(clno2norm, which(Br < 5e4),NA)) %>%
#   mutate(brratio = ifelse(Br>1e4,BrH2O / Br, NA)) %>%
#   mutate(brratio = ifelse(brratio>=1e-2,BrH2O / Br, NA)) %>%
#   mutate(brratio = ifelse(brratio<=1.2,brratio, NA)) %>%
#   mutate(clno2norm = ifelse(brratio<=1.05,clno2norm, NA))
#  


write.csv(dfCims,file.path(HRCIMSDIR, "I-caldata.csv"),
          quote = FALSE,row.names = FALSE)
# Nice job.  Let's now load the chemical data for merging.  The names
# in the csv are not useful, so I will need to fix that.  

# The calTimes are written for local time, while the CIMS HR data is written in
# UTC.  


dfCims <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/cimsHRDf-thru20240628.csv") %>% 
  mutate(time = dmy_hm(time)) %>% 
  mutate(time = floor_date(time,unit= "1 min")) %>% 
  group_by(time) %>% 
  summarize_all(mean,na.rm=TRUE)
write.csv(dfCims,"G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/cimsHRDf-thru20240628-60s.csv.csv",
          quote = FALSE, row.names = FALSE)

  
  

cimsList <- list()


for(ix in 1:(nrow(calTimes))){
  cimsList[[ix]] <- dfCims %>% 
    filter(between(time,ymd_hms(calTimes[[ix,1]]),ymd_hms(calTimes[[ix,2]]))) %>% 
    mutate(event = calTimes$event[ix]) 
}

cimsDf <- do.call(bind_rows, cimsList) %>% 
  group_by(event) %>% 
  summarise_all(.funs = list(avg = ~mean(x = .,na.rm=TRUE),
                             stdev = ~sd(x=.,na.rm=TRUE))) %>% 
  ungroup()

cimsDf$type <- calTimes$type[cimsDf$event]
cimsDf$grp <- calTimes$event[cimsDf$event]
cimsDf$comment <- calTimes$comment[cimsDf$event]

write.csv(cimsDf,"G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/cims-results-I.csv",
          quote = FALSE, row.names = FALSE)
  


# TILDAS ---------------------------------------------------------------------
load.tildas.data(START = "2024-06-03", STOP = "2024-06-03", STC = FALSE,
                 HCLONLY = TRUE)



  
hclList <- list()

for(ix in 1:(nrow(calTimes))){
    hclList[[ix]] <- df.str %>% 
      filter(between(ts,ymd_hms(calTimes[[ix,1]]),ymd_hms(calTimes[[ix,2]]))) %>% 
      mutate(event = calTimes$event[ix])
  }

hclDf <- do.call(rbind, hclList) %>% 
  group_by(event) %>% 
  summarise_all(.funs = list(avg = ~mean(x = .,na.rm=TRUE),
                             stdev = ~sd(x=.,na.rm=TRUE))) %>% 
  ungroup()

hclDf$type <- calTimes$type[hclDf$event]
hclDf$grp <- calTimes$event[hclDf$event]
hclDf$comment <- calTimes$comment[hclDf$event]


write.csv(hclDf,"G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/TILDAS/hcl-results.csv",
          quote = FALSE, row.names = FALSE)



# TILDAS Line Loss --------------------------------------------------------
# Dry 
rtBg <- df.str %>% 
  filter(between(ts, ymd_hms("2024-05-31 07:00:00"), 
                 ymd_hms(ymd_hms("2024-05-31 07:15:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))

# # A tibble: 1 × 3
# ts                     hclAvg  hclSd
# <dttm>                  <dbl>  <dbl>
#   1 2024-05-31 07:07:30.60 0.0923 0.0127

rtPreFurn <- df.str %>% 
  filter(between(ts, ymd_hms("2024-05-31 09:30:00"), 
                 ymd_hms(ymd_hms("2024-05-31 10:00:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))

# ts                     hclAvg  hclSd
# <dttm>                  <dbl>  <dbl>
#   1 2024-05-31 09:44:59.70   2.99 0.0216

htbg <-df.str %>% 
  filter(between(ts, ymd_hms("2024-05-31 11:45:00"), 
                 ymd_hms(ymd_hms("2024-05-31 11:50:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))

# htbg
# # A tibble: 1 × 3
# ts                     hclAvg  hclSd
# <dttm>                  <dbl>  <dbl>
#   1 2024-05-31 11:47:30.48  0.166 0.0136


htPreFurn <- df.str %>% 
  mutate(hcl = hcl * 3/(3-0.05)) %>% 
  filter(between(ts, ymd_hms("2024-05-31 11:00:00"), 
                 ymd_hms(ymd_hms("2024-05-31 11:15:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))

# htPreFurn
# # A tibble: 1 × 3
# ts                     hclAvg  hclSd
# <dttm>                  <dbl>  <dbl>
#   1 2024-05-31 11:07:30.48   3.12 0.0144

preFurnOut <- data.frame(hclAvg = (htPreFurn$hclAvg-htbg$hclAvg),
                hclSd = sqrt(htPreFurn$hclSd^2 + htbg$hclSd^2))
# 
# preFurnOut
# hclAvg      hclSd
# 1 2.952637 0.01980956

htInlet <- df.str %>% 
  mutate(hcl = hcl*3 / 7.5) %>% 
  filter(between(ts, ymd_hms("2024-05-31 12:00:00"), 
                 ymd_hms(ymd_hms("2024-05-31 12:15:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl),ct = n())

# htInlet
# # A tibble: 1 × 4
# ts                     hclAvg  hclSd    ct
# <dttm>                  <dbl>  <dbl> <int>
#   1 2024-05-31 12:07:31.01   3.12 0.0144   898


inletOut <- data.frame(hclAvg = (htPreFurn$hclAvg-(htbg$hclAvg)),
                         hclSd = sqrt(htPreFurn$hclSd^2 + rtBg$hclSd^2 + htbg$hclSd^2))

 
# inletOut
# hclAvg      hclSd
# 1 2.952637 0.02353883

# 50% RH ------------------------------------------------------------------

# instrument bg, furnace off
rtBg <- df.str %>% 
  filter(between(ts, ymd_hms("2024-06-03 09:30:00"), 
                 ymd_hms(ymd_hms("2024-06-03 09:35:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))
# rtBg
# # A tibble: 1 × 3
# ts                     hclAvg  hclSd
# <dttm>                  <dbl>  <dbl>
#   1 2024-06-03 09:32:29.98  0.233 0.0125


# HCl just before furnace, room temperature
rtPreFurn <- df.str %>% 
  filter(between(ts, ymd_hms("2024-06-03 11:23:00"), 
                 ymd_hms(ymd_hms("2024-06-03 11:33:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))

# rtPreFurn
# # A tibble: 1 × 3
# ts                     hclAvg  hclSd
# <dttm>                  <dbl>  <dbl>
#   1 2024-06-03 11:27:59.57   2.91 0.0166

# HCl just before furnace, furnace set to 450C

htPreFurn <- df.str %>% 
  mutate(hcl = hcl*3 / (3-.05)) %>% # dilution factor
  filter(between(ts, ymd_hms("2024-06-03 13:00:00"), 
                 ymd_hms(ymd_hms("2024-06-03 13:15:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))


# htPreFurn
# # A tibble: 1 × 3
# ts                     hclAvg  hclSd
# <dttm>                  <dbl>  <dbl>
#   1 2024-06-03 13:07:30.86   3.15 0.0160

htbg <- htPreFurn$hclAvg - rtPreFurn$hclAvg
htbgSd <- sqrt(htPreFurn$hclSd^2 + rtPreFurn$hclSd^2)


preFurnOut <- data.frame(hclAvg = (htPreFurn$hclAvg-(rtBg$hclAvg+htbg)),
                         hclSd = sqrt(htPreFurn$hclSd^2 + rtBg$hclSd^2 + htbgSd^2))

# 
# > preFurnOut
# hclAvg      hclSd
# 1 2.679792 0.03073037


# Adding HCl right at instrumnet inlet

htInlet <- df.str %>% 
  mutate(hcl = hcl*3 / 7.5) %>% # dilution factor
  filter(between(ts, ymd_hms("2024-06-03 13:35:00"), 
                 ymd_hms(ymd_hms("2024-06-03 13:45:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))
 
# htInlet
# # A tibble: 1 × 3
# ts                     hclAvg  hclSd
# <dttm>                  <dbl>  <dbl>
#   1 2024-06-03 13:42:30.36   3.12 0.0131
inletOut <- data.frame(hclAvg = (htInlet$hclAvg-(rtBg$hclAvg+htbg)),
                    hclSd = sqrt(htInlet$hclSd^2 + rtBg$hclSd^2 + htbgSd^2))

# inletOut
# hclAvg      hclSd
# 1 2.655569 0.02820338

# 70% RH ------------------------------------------------------------------

# instrument bg, furnace off
rtBg <- df.str %>% 
  filter(between(ts, ymd_hms("2024-06-28 09:10:00"), 
                 ymd_hms(ymd_hms("2024-06-28 09:15:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))
# ts                     hclAvg  hclSd
# <dttm>                  <dbl>  <dbl>
#   1 2024-06-28 09:12:30.21  0.126 0.0169


# for reasons I don't understand, the bg has a big difference
# when heated for these experiments.  I think the heated bg is 
# the one I want to use for the loss metrics.


htBg1 <- df.str %>% 
  filter(between(ts, ymd_hms("2024-06-28 10:38:30"), 
                 ymd_hms(ymd_hms("2024-06-28 10:39:30")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))

# htBg1
# # A tibble: 1 × 3
# ts                     hclAvg
# <dttm>                  <dbl>
#   1 2024-06-28 10:39:00.17  0.956
# # ℹ 1 more variable: hclSd <dbl>

htBg2 <- df.str %>% 
  filter(between(ts, ymd_hms("2024-06-28 11:00:00"), 
                 ymd_hms(ymd_hms("2024-06-28 11:04:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))

# ts                     hclAvg  hclSd
# <dttm>                  <dbl>  <dbl>
#   1 2024-06-28 11:02:00.63  0.654 0.0161

# HCl just before furnace, 450C
htPreFurn <- df.str %>% 
  mutate(hcl = hcl*3 / (3-.05)) %>% # dilution factor
  
  filter(between(ts, ymd_hms("2024-06-28 11:37:00"), 
                 ymd_hms(ymd_hms("2024-06-28 11:42:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))
# 
# htPreFurn
# # A tibble: 1 × 3
# ts                     hclAvg
# <dttm>                  <dbl>
#   1 2024-06-28 11:39:30.14   2.52
# # ℹ 1 more variable: hclSd <dbl>
# # 

preFurnOut <- data.frame(hclAvg = (htPreFurn$hclAvg-(htBg2$hclAvg)),
                         hclSd = sqrt(htPreFurn$hclSd^2 + htBg2$hclSd^2))
# > preFurnOut
# hclAvg      hclSd
# 1 1.870277 0.02517108


# HCl at inlet

htInlet <- df.str %>% 
  mutate(hcl = hcl*3 / 7.5) %>% # dilution factor
  filter(between(ts, ymd_hms("2024-06-28 10:45:00"), 
                 ymd_hms(ymd_hms("2024-06-28 10:50:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))

# 
# # A tibble: 1 × 3
# ts                     hclAvg
# <dttm>                  <dbl>
#   1 2024-06-28 10:47:30.17   2.93
# # ℹ 1 more variable: hclSd <dbl>


inletOut <- data.frame(hclAvg = (htInlet$hclAvg-(htBg1$hclAvg)),
                       hclSd = sqrt(htInlet$hclSd^2 + htBg1$hclSd^2))

# inletOut
# hclAvg     hclSd
# 1 1.970123 0.0270373


# 70% RH??? ------------------------------------------------------------------

# instrument bg, furnace on
htBg <- df.str %>% 
  filter(between(ts, ymd_hms("2024-06-28 14:15:00"), 
                 ymd_hms(ymd_hms("2024-06-28 14:22:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))
# 
# ts                     hclAvg  hclSd
# <dttm>                  <dbl>  <dbl>
#   1 2024-06-28 14:18:30.06  0.443 0.0140


# HCl just before furnace, 450C
htPreFurn <- df.str %>% 
  filter(between(ts, ymd_hms("2024-06-28 14:48:00"), 
                 ymd_hms(ymd_hms("2024-06-28 14:53:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))

# htPreFurn
# # A tibble: 1 × 3
# ts                     hclAvg  hclSd
# <dttm>                  <dbl>  <dbl>
#   1 2024-06-28 14:50:30.06   1.85 0.0154

# 
# HCl at inlet

htInlet <- df.str %>% 
  mutate(hcl = hcl*2.93 / 7.5) %>% # dilution factor
  filter(between(ts, ymd_hms("2024-06-28 13:45:00"), 
                 ymd_hms(ymd_hms("2024-06-28 13:55:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))

# htInlet
# # A tibble: 1 × 3
# ts                     hclAvg  hclSd
# <dttm>                  <dbl>  <dbl>
#   1 2024-06-28 13:50:00.08   2.73 0.0121

# 90% RH??? ------------------------------------------------------------------

htBg <- df.str %>% 
  filter(between(ts, ymd_hms("2024-06-28 16:25:00"), 
                 ymd_hms(ymd_hms("2024-06-28 16:28:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))
# # A tibble: 1 × 3
# ts                     hclAvg  hclSd
# <dttm>                  <dbl>  <dbl>
#   1 2024-06-28 16:26:30.36  0.599 0.0171


# HCl just before furnace, 450C
htPreFurn <- df.str %>% 
  filter(between(ts, ymd_hms("2024-06-28 15:58:00"), 
                 ymd_hms(ymd_hms("2024-06-28 16:03:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))

# htPreFurn
# # A tibble: 1 × 3
# ts                     hclAvg  hclSd
# <dttm>                  <dbl>  <dbl>
#   1 2024-06-28 16:00:30.46   2.24 0.0171


htInlet <- df.str %>% 
  mutate(hcl = hcl*2.93 / 7.5) %>% # dilution factor
  filter(between(ts, ymd_hms("2024-06-28 16:42:00"), 
                 ymd_hms(ymd_hms("2024-06-28 16:45:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))



# htInlet
# # A tibble: 1 × 3
# ts                     hclAvg   hclSd
# <dttm>                  <dbl>   <dbl>
#   1 2024-06-28 16:43:30.36   2.78 0.00667


# August ------------------------------------------------------------------



# 50% RH PART B, 2 Aug 2024------------------------------------------------------------------

htBg <- df.str %>% 
  filter(between(ts, ymd_hms("2024-08-01 07:00:00"), 
                 ymd_hms(ymd_hms("2024-08-01 07:05:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))

# htBg
# # A tibble: 1 × 3
# ts                     hclAvg   hclSd
# <dttm>                  <dbl>   <dbl>
#   1 2024-08-01 07:02:30.06  0.174 0.00870


# HCl just before furnace, 450C
htPreFurn <- df.str %>% 
  filter(between(ts, ymd_hms("2024-08-01 08:10:00"), 
                 ymd_hms(ymd_hms("2024-08-01 08:15:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))

# htPreFurn
# # A tibble: 1 × 3
# ts                     hclAvg  hclSd
# <dttm>                  <dbl>  <dbl>
#   1 2024-08-01 08:12:29.53   2.26 0.0107


htInlet <- df.str %>% 
  mutate(hcl = hcl*2.93 / 7.5) %>% # dilution factor
  filter(between(ts, ymd_hms("2024-08-01 11:00:00"), 
                 ymd_hms(ymd_hms("2024-08-01 11:15:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))

# htInlet
# # A tibble: 1 × 3
# ts                     hclAvg   hclSd
# <dttm>                  <dbl>   <dbl>
#   1 2024-08-01 11:07:30.96   2.32 0.00771

##################################################################
# 90% RH??? PART B, 2 Aug 2024------------------------------------------------------------------

htBg <- df.str %>% 
  filter(between(ts, ymd_hms("2024-08-01 15:45:00"), 
                 ymd_hms(ymd_hms("2024-08-01 15:50:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))
# # A tibble: 1 × 3
# ts                     hclAvg  hclSd
# <dttm>                  <dbl>  <dbl>
#   1 2024-08-01 15:47:30.36  0.280 0.0103


htPreFurn <- df.str %>% 
  filter(between(ts, ymd_hms("2024-08-01 16:25:00"), 
                 ymd_hms(ymd_hms("2024-08-01 16:30:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))

# HCl just before furnace, 450C
# htPreFurn
# # A tibble: 1 × 3
# ts                     hclAvg  hclSd
# <dttm>                  <dbl>  <dbl>
#   1 2024-08-01 16:27:30.33   2.12 0.0114




# Dry Calibration Regressions ---------------------------------------------

dryResults <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/cal-results-dry-csv.csv") %>% 
  select(hclCorr,hclStdev,no2Corr,no2Stdev,cimsCorr,cimsStdev)

attach(dryResults)
capsTildas <- bfsl(x=hclCorr,y=no2Corr,sd_x = hclStdev,sd_y= no2Stdev)
detach(dryResults)

summary(capsTildas)

attach(dryResults)
cimsTildas <- bfsl(x=hclCorr,y=cimsCorr,sd_x = hclStdev,sd_y= cimsStdev)
detach(dryResults)

summary(cimsTildas)













# I THINK THIS IS OLD BUT I'M NOT SURE ------------------------------------


# Load times --------------------------------------------------------------


cimsTimes <- read_csv("G:/My Drive/Experiments/CIMS/calibrations/cimscsvtimes.csv")

cimsPk <- vector()
for(ix in 1:nrow(cimsTimes)){
  cimsPk[ix] <- dfCims %>% 
    filter(between(time, cimsTimes$st.pk[ix], cimsTimes$et.pk[ix])) %>% 
    summarize(mean(clno2norm, na.rm = TRUE))
  
}
cimsPk <- unlist(cimsPk)

cimsBg <- vector()
for(ix in 1:nrow(cimsTimes)){
  cimsBg[ix] <- dfCims %>% 
    filter(between(time, cimsTimes$st.bg[ix], cimsTimes$et.bg[ix])) %>% 
    summarize(mean(clno2norm, na.rm = TRUE))
  
}
cimsBg <- unlist(cimsBg)
cimsPts <- cimsPk - cimsBg


cimsWater <- vector()
for(ix in 1:nrow(cimsTimes)){
  cimsWater[ix] <- dfCims %>% 
    filter(between(time, cimsTimes$st.bg[ix], cimsTimes$et.bg[ix])) %>% 
    summarize(mean(iratio, na.rm = TRUE))
  
}
cimsWater = unlist(cimsWater)

plot(cimsWater,cimsPts,pch = 19)


capsTimes <- read_csv("G:/My Drive/Experiments/CIMS/calibrations/no2csvtimes.csv")

capsPk <- vector()
for(ix in 1:nrow(capsTimes)){
  capsPk[ix] <- dfCaps %>% 
    filter(between(date, capsTimes$st.pk[ix], capsTimes$et.pk[ix])) %>% 
    summarize(mean(t500u_022838_NO2, na.rm = TRUE))
  
}
capsPk <- unlist(capsPk)

capsBg <- vector()
for(ix in 1:nrow(capsTimes)){
  capsBg[ix] <- dfCaps %>% 
    filter(between(date, capsTimes$st.bg[ix], capsTimes$et.bg[ix])) %>% 
    summarize(mean(t500u_022838_NO2, na.rm = TRUE))
}

capsBg <- unlist(capsBg)

capsPts <- capsPk - capsBg

outDf <- tibble(caps = capsPts,cims = cimsPts)

plot(capsPts,cimsPts,pch = 19,xlim=c(0,5),ylim = c(0,0.001))


# 
# ffCims <- list.files("C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2/CIMS/",
#                    full.names=TRUE, pattern = ".csv")
# 
# fileListCims <- lapply(ffCims,read_csv)
# dfCims <- do.call(bind_rows,fileListCims) %>% 
#   replace(. < 0, 0) %>% 
#   mutate(time = time - 3600) %>% 
#   mutate(clno2norm = ifelse(I>1e4,IClNO2/(I + IH2O),NA)) %>% 
#   mutate(clno2norm = replace(clno2norm, I < 9e4,NA)) %>% 
#   mutate(iratio = ifelse(I>1e4,IH2O / I, NA)) %>% 
#   mutate(iratio = ifelse(iratio>=1e-2,IH2O / I, NA))
# 
# TIME1 <- ymd_hms("2024-06-18 21:30:00")
# TIME2 <- ymd_hms("2024-06-18 22:00:00")
# 
# dfCims %>% 
#   filter(between(time, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
#   
#   ggplot(aes(x=time,y=clno2norm)) +
#   geom_line()+
#   scale_y_continuous(name = "IClNO2")+
#   theme_bw()
# 
# # 
# write.csv(dfCims,"C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2/CIMS/cims-stitch.csv",
#           quote = FALSE, row.names = FALSE)
# # 
# cimsList <- list()
# 
# 
# for(ix in 1:(nrow(calTimes))){
#   cimsList[[ix]] <- dfCims %>% 
#     filter(between(time,ymd_hms(calTimes[[ix,1]]),ymd_hms(calTimes[[ix,2]]))) %>% 
#     mutate(grp = calTimes$event[ix]) 
# }
# 
# cimsDf <- do.call(bind_rows, cimsList) %>% 
#   group_by(grp) %>% 
#   summarise_all(mean,na.rm=TRUE) %>% 
#   ungroup() 
# 
# cimsDf$flag <- calTimes$type[cimsDf$grp]
# cimsDf$comment <- calTimes$comment[cimsDf$grp]
# 
# write.csv(cimsDf,"C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2/CIMS/cims-results.csv",
#           quote = FALSE, row.names = FALSE)



