
# NO ----------------------------------------------------------------------

ffNO <- list.files("C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/HONO/T200",
                    full.names=TRUE, pattern = ".csv")

fileListNO <- lapply(ffNO,read_csv)
dfT200 <- do.call(rbind,fileListNO) %>% 
  rename(date = TheTime) %>% 
  mutate(date = mdy_hms(date)) %>% 
  select(date, T200_NO, T200_NO2, T200_NOx)

write.csv(dfT200,"C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/HONO/T200/no-stitch.csv",
           quote = FALSE, row.names = FALSE)


t200List <- list()


for(ix in 1:nrow(calTimes)){
  t200List[[ix]] <- dfT200 %>% 
    filter(between(date,ymd_hms(calTimes[[ix,1]]),ymd_hms(calTimes[[ix,2]]))) %>% 
    mutate(grp = calTimes$event[ix])
}

t200Df <- do.call(rbind, t200List) %>% 
  group_by(grp) %>% 
  summarise_all(mean,na.rm=TRUE) %>% 
  ungroup() %>% 
  mutate(flag = calTimes$type) %>% 
  mutate(NOcorr = T200_NO - T200_NO[flag == "bg"])

# LOPAP -------------------------------------------------------------------




# CIMS --------------------------------------------------------------------


ffCims <- list.files("C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/HONO/CIMS",
                   full.names=TRUE, pattern = ".csv")

fileListCims <- lapply(ffCims,read_csv)
dfCims <- do.call(rbind,fileListCims) %>% 
  replace(. < 0, 0) %>% 
  mutate(hononorm = ifelse(I!=0,IHONO/(I + IH2O),NA)) %>% 
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
write.csv(dfCims,"C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/HONO/CIMS/cims-stitch.csv",
          quote = FALSE, row.names = FALSE)
# 



# CIMS-HR -----------------------------------------------------------------
calTimes <- read_csv("C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/HONO/times.csv")

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

# NO2 ---------------------------------------------------------------------

ffNo2 <- list.files("G:/My Drive/Experiments/CIMS/calibrations/no2",
                 full.names=TRUE, pattern = ".csv")

fileListNo2 <- lapply(ffNo2,read_csv)
dfCaps <- do.call(rbind,fileListNo2) %>% 
  rename(date = TheTime) %>% 
  mutate(date = mdy_hms(date))

# write.csv(df,"G:/My Drive/Experiments/CIMS/calibrations/no2-stitch.csv",
#           quote = FALSE, row.names = FALSE)


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
