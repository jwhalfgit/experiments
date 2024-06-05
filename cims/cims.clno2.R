calTimes <- read_csv("C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2/times_50%rh.csv")
#calTimes <- calTimes[1:12,]
calTimes$st <- calTimes$st - 3600 # convert to UTC
calTimes$et <- calTimes$et - 3600 # convert to UTC

# NO ----------------------------------------------------------------------

ffNO2 <- list.files("C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2/CAPS",
                    full.names=TRUE, pattern = ".csv")

fileListNO2 <- lapply(ffNO2,read_csv)
dfCAPS <- do.call(rbind,fileListNO2) %>% 
  rename(date = TheTime) %>% 
  mutate(date = mdy_hms(date) - 3600) %>% 
  select(date, t500u_022838_NO2) %>% 
  rename(no2 = t500u_022838_NO2)

write.csv(dfCAPS,"C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2/CAPS/no2-stitch.csv",
           quote = FALSE, row.names = FALSE)


capsList <- list()


for(ix in 1:(nrow(calTimes))){
  capsList[[ix]] <- dfCAPS %>% 
    filter(between(date,ymd_hms(calTimes[[ix,1]]),ymd_hms(calTimes[[ix,2]]))) %>% 
    mutate(grp = calTimes$event[ix])
}

capsDf <- do.call(rbind, capsList) %>% 
  group_by(grp) %>% 
  summarise_all(mean,na.rm=TRUE) %>% 
  ungroup() 

capsDf$flag <- calTimes$type[capsDf$grp]
capsDf$comment <- calTimes$comment[capsDf$grp]


#%>% 
  #mutate(NOcorr = no2 - no2[flag == "bgClno2"])

write.csv(capsDf, "C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2/CAPS/no2-results.csv", quote = FALSE)
# CIMS --------------------------------------------------------------------


ffCims <- list.files("C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2/CIMS",
                   full.names=TRUE, pattern = ".csv")

fileListCims <- lapply(ffCims,read_csv)
dfCims <- do.call(bind_rows,fileListCims) %>% 
  replace(. < 0, 0) %>% 
  mutate(time = time - 3600) %>% 
  mutate(clno2norm = ifelse(I!=0,IClNO2/(I + IH2O),NA)) %>% 
  mutate(clno2norm = replace(clno2norm, I < 2e5,NA)) %>% 
  mutate(iratio = ifelse(I!=0,IH2O / I, NA))

# TIME1 <- ymd_hms("2024-04-08 00:00:00")
# TIME2 <- ymd_hms("2024-04-11 12:00:00")

dfCims %>% 
  #filter(between(time, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=time,y=IHONO)) +
  geom_line()+
  scale_y_continuous(name = "IHONO")+
  theme_bw()

# 
write.csv(dfCims,"C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2/CIMS/cims-stitch.csv",
          quote = FALSE, row.names = FALSE)
# 
cimsList <- list()


for(ix in 1:(nrow(calTimes))){
  cimsList[[ix]] <- dfCims %>% 
    filter(between(time,ymd_hms(calTimes[[ix,1]]),ymd_hms(calTimes[[ix,2]]))) %>% 
    mutate(grp = calTimes$event[ix]) 
}

cimsDf <- do.call(bind_rows, cimsList) %>% 
  group_by(grp) %>% 
  summarise_all(mean,na.rm=TRUE) %>% 
  ungroup() 

cimsDf$flag <- calTimes$type[cimsDf$grp]
cimsDf$comment <- calTimes$comment[cimsDf$grp]

write.csv(cimsDf,"C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2/CIMS/cims-results.csv",
          quote = FALSE, row.names = FALSE)

# CIMS-HR -----------------------------------------------------------------

# The calTimes are written for local time, while the CIMS HR data is written in
# UTC.  

cimsTimes <- calTimes %>% 
  mutate(st = st - 3600, et = et - 3600)

cimsHR <- read_csv("C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/HONO/CIMS/drycal-hr.csv") %>% 
  replace(. < 0, 0) %>% 
  mutate(hononorm = ifelse(I!=0,IHONO/(I + IH2O),NA)) %>% 
  mutate(iratio = ifelse(I!=0,IH2O / I, NA))

cimsList <- list()

for(ix in 1:nrow(cimsTimes)){
  cimsList[[ix]] <- cimsHR %>% 
    filter(between(ts,ymd_hms(cimsTimes[[ix,1]]),ymd_hms(cimsTimes[[ix,2]]))) %>% 
    mutate(grp = cimsTimes$event[ix])
}

cimsHrDf <- do.call(rbind, cimsList) %>% 
  group_by(grp) %>% 
  summarise_all(mean,na.rm=TRUE) %>% 
  mutate(flag = cimsTimes$type) %>% 
  mutate(hononormCorr = hononorm - hononorm[flag == "bg"])

  
plot(t200Df$NOcorr,cimsHrDf$hononormCorr, pch = 19)

# TILDAS ---------------------------------------------------------------------
load.tildas.data(START = "2024-05-31", STOP = "2024-05-31", STC = FALSE)

  
hclList <- list()

for(ix in 1:(nrow(calTimes))){
    hclList[[ix]] <- df.str %>% 
      filter(between(ts,ymd_hms(calTimes[[ix,1]]),ymd_hms(calTimes[[ix,2]]))) %>% 
      mutate(grp = calTimes$event[ix])
  }

hclDf <- do.call(rbind, hclList) %>% 
  group_by(grp) %>% 
  summarise_all(mean,na.rm=TRUE) %>% 
  ungroup() 

hclDf$flag <- calTimes$type[hclDf$grp]
hclDf$comment <- calTimes$comment[hclDf$grp]


write.csv(hclDf,"C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2/TILDAS/hcl-results.csv",
          quote = FALSE, row.names = FALSE)



# TILDAS Line Loss --------------------------------------------------------
# Dry 
rtBg <- df.str %>% 
  filter(between(ts, ymd_hms("2024-05-31 07:00:00"), 
                 ymd_hms(ymd_hms("2024-05-31 07:15:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))

rtPreFurn <- df.str %>% 
  filter(between(ts, ymd_hms("2024-05-31 09:30:00"), 
                 ymd_hms(ymd_hms("2024-05-31 10:00:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))

htPreFurn <- df.str %>% 
  filter(between(ts, ymd_hms("2024-05-31 11:00:00"), 
                 ymd_hms(ymd_hms("2024-05-31 11:15:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))

htInlet <- df.str %>% 
  mutate(hcl = hcl*2.93 / 7.5) %>% 
  filter(between(ts, ymd_hms("2024-05-31 12:00:00"), 
                 ymd_hms(ymd_hms("2024-05-31 12:15:00")))) %>% 
  summarize(ts=mean(ts),hclAvg = mean(hcl),hclSd = sd(hcl))


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
