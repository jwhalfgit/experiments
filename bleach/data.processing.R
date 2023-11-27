
# Winter ------------------------------------------------------------------

# UoY CIMS HNO3 data
cims <- read_csv(file.path(EXPTDIR,"BLEACH","data","cims","winter-hno3.csv")) %>% 
  select(ts,hno3)

cimsAvg <- cims[complete.cases(cims),] %>% 
  mutate(groupings = cumsum(c(1,diff(ts)) > 9)) %>% 
  group_by(groupings) %>% 
  filter(n() > 3) %>% 
  do(tail(.,n = 4L)) %>% 
  #summarize(ts = mean(ts),hno3Avg = mean(hno3), hno3Sd = sd(hno3)) %>% 
  ungroup() %>% 
  select(-groupings) %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","data","cims","hno3-avg.csv"),
          quote = FALSE,row.names = FALSE)


# TILDAS data
tildas <- read_csv(file.path(EXPTDIR,"BLEACH","data","tildas","winter.csv")) %>% 
  dplyr::rename(ts = Index) %>% 
  select(ts,hcl)

tildasAvg <- tildas %>% 
  mutate(ts = floor_date(ts, unit = "10 min")) %>% 
  group_by(ts) %>% 
  summarize(ts = mean(ts), hclAvg = mean(hcl), hclSd = sd(hcl)) %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","data","tildas","winter-10min-avg.csv"),
            quote = FALSE,row.names = FALSE)

# meteorology data
met <- read_csv(file.path(EXPTDIR,"BLEACH","data","met","winter-met.dat")) %>% 
  rename(ts = TIMESTAMP) %>% 
  select(-c("Pumping(1)","Pumping(2)","Pumping(3)", "RECORD",
            "Batt_volt_Min")) %>% 
  mutate(ts = floor_date(ts, unit = "10 min")) %>% 
  group_by(ts) %>% 
  summarize_all(mean)

# UW CIMS data
uwCims <- read_csv(file.path(EXPTDIR,"BLEACH","data","cims","winter-clx.csv")) %>% 
  mutate(ts = floor_date(ts, "10 minutes")) %>% 
  group_by(ts) %>% 
  summarize_all(mean)

# GEOS Chem
gc <- read_csv(file.path(EXPTDIR,"BLEACH","data","geoschem","winter.csv")) %>% 
  rename_with(~ paste0(.x, "_gc")) %>% 
  rename(ts = ts_gc) %>% 
  mutate(ts = dmy_hm(ts))

# NOx data
nox <- read_csv(file.path(EXPTDIR,"BLEACH","data","nox","winter-nox.csv")) %>% 
  select(Time, NO_ppt_filt,NO2_ppt_filt, NOx_ppt_filt) %>% 
  rename(ts = Time) %>% 
  mutate(ts = floor_date(ts, unit = "10 min")) %>% 
  group_by(ts) %>% 
  summarize_all(mean)

bigDF <- tildasAvg %>% 
  full_join(met) %>% 
  full_join(uwCims) %>% 
  full_join(nox) %>% 
  mutate(no = na.approx(NO_ppt_filt, rule = 2)) %>% 
  mutate(no2 = na.approx(NO_ppt_filt, rule = 2)) %>% 
  mutate(nox = na.approx(NOx_ppt_filt, rule = 2)) %>% 
  full_join(gc) %>% 
  arrange(ts) %>% 
  filter(between(ts,ymd_hms("2023-01-26 00:00:00"), ymd_hms("2023-02-23 00:00:00")))

# FILTERING.

# Anna C says they use NOx value filtering of 50 ppt for NO and 200 ppt for 
# NO2 to differentiate clean vs polluted.  I suppose I will use an OR 
# to filter.
#
# nox_dirty <- nox %>% 
#   filter(NO_ppt_filt > 50 | NO2_ppt_filt > 200)
# 
# nox_clean <- nox %>% 
#   filter(NO_ppt_filt <= 50 & NO2_ppt_filt <= 200)
#
# This is trivial, but filters out a lot of data (ie, about 15000 points, total
# TILDAS by comparison has
# 373k points compared to nox's 40k.  IF I merge as is, I'm going to lose
# a lot of HCL data, or incorrectly include HCl data that should not be 
# included.  
#
# One option would be to merge the NOx with the HCl data, interpolate the
# NOx data, THEN filter based on the filters.  Yeah, let's do that and see how
# it compares to the wind filter :o


# hclDirty <- tildasNox %>% 
#   filter(NO_ppt_filt > 50 | NO2_ppt_filt > 200)
# 
# hclClean <- tildasNox %>% 
#   filter(NO_ppt_filt <= 50 & NO2_ppt_filt <= 200)

# 
# TIME1 <- "2023-01-26 00:00:00"
# TIME2 <- "2023-02-05 00:00:00"
# 
# tildasNox %>% 
#   filter(between(ts,ymd_hms(TIME1),ymd_hms(TIME2))) %>% 
#   ggplot()+
#   geom_line(aes(x=ts, y = NOx_ppt_filtb, colour = "NOx_ppt_filtb"))+
#   geom_line(aes(x=ts, y = NOx_ppt_filt, colour = "NOx_ppt_filt"))

# Maybe it makes most sense to pre-merge all the data into one mega
# DF.  


regimeFilter <- function(DF = bigDF, METHOD = "wind", REGIME = "clean",
                         WRITE = FALSE){
  if(METHOD == "wind"){
    if(REGIME == "clean"){
      outDF <- DF %>% 
                left_join(met) %>% 
                filter(between(ResultantMean_WD, 190, 300)) %>% 
                filter(ResultantMean_WS >= 0.2) %>% 
                rename(date = "ts")
    }else if(REGIME == "dirty"){
     outDF <-  DF %>% 
                left_join(met) %>% 
                filter(!between(ResultantMean_WD, 190, 300)) %>% 
                filter(ResultantMean_WS >= 0.2) %>% 
                rename(date = "ts")
    }
  }else if(METHOD == "nox"){
    if(REGIME == "clean"){
      outDF <- DF %>% 
                left_join(nox) %>% 
                filter(no <= 50 & no2 <= 200) %>% 
                rename(date = "ts")
      
    }else if(REGIME == "dirty"){
      outDF <- DF %>% 
                left_join(nox) %>% 
                filter(no > 50 | no2 > 200) %>% 
                rename(date = "ts")
    }
  }
  
  if(WRITE == TRUE){
    write.csv(outDF,file.path(EXPTDIR,"BLEACH",paste(deparse(substitute(DF)),
                        "-",METHOD,"-",REGIME, ".csv")),
              quote = FALSE,row.names = FALSE)
  }
  
  return(outDF)
}

bigDFClean <- regimeFilter(DF = bigDF, METHOD = "wind", REGIME = "clean",
                           WRITE = FALSE)

bigDFClean_n <- bigDFClean %>% 
  mutate(hr = format(date,"%H")) %>% 
  group_by(hr) %>% 
  summarize(across(everything(), ~sum(!is.na(.)))) %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","winter-bigDfClean_n.csv"),
            quote = FALSE,row.names = FALSE)


bigDFDirty <- regimeFilter(DF = bigDF, METHOD = "wind", REGIME = "dirty",
                           WRITE = FALSE)

bigDFDirty_n <- bigDFDirty %>% 
  mutate(hr = format(date,"%H")) %>% 
  group_by(hr) %>% 
  summarize(across(everything(), ~sum(!is.na(.)))) %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","winter-bigDfDirty_n.csv"),
            quote = FALSE,row.names = FALSE)



bigDFCleanNox <- regimeFilter(DF = bigDF, METHOD = "nox", REGIME = "clean",
                              WRITE = FALSE)

bigDFCleanNox_n <- bigDFCleanNox %>% 
  mutate(hr = format(date,"%H")) %>% 
  group_by(hr) %>% 
  summarize(across(everything(), ~sum(!is.na(.)))) %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","winter-bigDfCleanNox_n.csv"),
            quote = FALSE,row.names = FALSE)

bigDFDirtyNox <- regimeFilter(DF = bigDF, METHOD = "nox", REGIME = "dirty",
                              WRITE = FALSE)

bigDFDirty_n <- bigDFDirtyNox %>% 
  mutate(hr = format(date,"%H")) %>% 
  group_by(hr) %>% 
  summarize(across(everything(), ~sum(!is.na(.)))) %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","winter-bigDfDirtyNox_n.csv"),
            quote = FALSE,row.names = FALSE)

#######################################################################
# Meteorology filters
cleanHclOut <- timeVariation(bigDFClean,
                          pollutant = "hclAvg",xlab = "hour")$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","winter-cleanDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


uwCimsCleanCl2 <- timeVariation(bigDFClean,
                                pollutant=c("cl2"),type = "hour")$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","cl2-winter-CleanDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


uwCimsCleanClno2 <- timeVariation(bigDFClean,
                                  pollutant=c("clno2"),type = "hour")$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","clno2-winter-CleanDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

# 
# gchemClean <- gc %>% 
#   left_join(met) %>% 
#   filter(between(ResultantMean_WD, 190, 300)) %>% 
#   filter(ResultantMean_WS >= 0.2) %>% 
#   rename(date = "ts") 

gchemCleanHcl <- timeVariation(bigDFClean,
                                pollutant=c("HCl_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
write.csv(file.path(EXPTDIR,"BLEACH","gchem-hcl-winter-cleanDiurnal.csv"),
          quote = FALSE,row.names = FALSE)

  
gchemCleanClno2 <- timeVariation(bigDFClean,
                               pollutant=c("ClNO2_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
write.csv(file.path(EXPTDIR,"BLEACH","gchem-clno2-winter-cleanDiurnal.csv"),
          quote = FALSE,row.names = FALSE)

gchemCleanCl2 <- timeVariation(bigDFClean,
                               pollutant=c("Cl2_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
write.csv(file.path(EXPTDIR,"BLEACH","gchem-cl2-winter-cleanDiurnal.csv"),
          quote = FALSE,row.names = FALSE)

gchemCleanNox <- timeVariation(bigDFClean,
                               pollutant=c("NOx_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
write.csv(file.path(EXPTDIR,"BLEACH","gchem-nox-winter-cleanDiurnal.csv"),
          quote = FALSE,row.names = FALSE)
######################################################################
# NOx filters
cleanHclOut <- timeVariation(bigDFCleanNox,
                             pollutant = "hclAvg",xlab = "hour",
                             plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","winter-cleanNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


uwCimsCleanCl2 <- timeVariation(bigDFCleanNox,
                                pollutant=c("cl2"),type = "hour",
                                plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","cl2-winter-CleanNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


uwCimsCleanClno2 <- timeVariation(bigDFCleanNox,
                                  pollutant=c("clno2"),type = "hour",
                                  plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","clno2-winter-CleanNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

# 
# gchemClean <- gc %>% 
#   left_join(met) %>% 
#   filter(between(ResultantMean_WD, 190, 300)) %>% 
#   filter(ResultantMean_WS >= 0.2) %>% 
#   rename(date = "ts") 

gchemCleanHcl <- timeVariation(bigDFCleanNox,
                               pollutant=c("HCl_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-hcl-winter-cleanNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


gchemCleanClno2 <- timeVariation(bigDFCleanNox,
                                 pollutant=c("ClNO2_gc"),type = "hour",
                                 plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-clno2-winter-cleanNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemCleanCl2 <- timeVariation(bigDFCleanNox,
                               pollutant=c("Cl2_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-cl2-winter-cleanNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemCleanNox <- timeVariation(bigDFCleanNox,
                               pollutant=c("NOx_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-nox-winter-cleanNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)



######################################################################
# Met - Dirty

gchemDirtyHcl <- timeVariation(bigDFDirty,
                               pollutant=c("HCl_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-hcl-winter-DirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemDirtyClno2 <- timeVariation(bigDFDirty,
                                 pollutant=c("ClNO2_gc"),type = "hour",
                                 plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-clno2-winter-DirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemDirtyCl2 <- timeVariation(bigDFDirty,
                               pollutant=c("Cl2_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-cl2-winter-DirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemDirtyNox <- timeVariation(bigDFDirty,
                               pollutant=c("NOx_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-nox-winter-DirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)



dirtyHclOut <- timeVariation(bigDFDirty,
                             pollutant = "hclAvg",xlab = "hour",
                             plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","winter-dirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


uwCimsDirtyCl2 <- timeVariation(bigDFDirty,
                                pollutant=c("cl2"),type = "hour",
                                plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","cl2-winter-dirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


uwCimsDirtyClno2 <- timeVariation(bigDFDirty,
                                  pollutant=c("clno2"),type = "hour",
                                  plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","clno2-winter-dirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)




######################################################################
# Nox - Dirty

gchemDirtyHcl <- timeVariation(bigDFDirtyNox,
                               pollutant=c("HCl_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-hcl-winter-DirtyNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemDirtyClno2 <- timeVariation(bigDFDirtyNox,
                                 pollutant=c("ClNO2_gc"),type = "hour",
                                 plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-clno2-winter-DirtyNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemDirtyCl2 <- timeVariation(bigDFDirtyNox,
                               pollutant=c("Cl2_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-cl2-winter-DirtyNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemDirtyNox <- timeVariation(bigDFDirtyNox,
                               pollutant=c("NOx_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-nox-winter-DirtyNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)



dirtyHclOut <- timeVariation(bigDFDirtyNox,
                             pollutant = "hclAvg",xlab = "hour",
                             plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","winter-dirtyNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


uwCimsDirtyCl2 <- timeVariation(bigDFDirtyNox,
                                pollutant=c("cl2"),type = "hour",
                                plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","cl2-winter-dirtyNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


uwCimsDirtyClno2 <- timeVariation(bigDFDirtyNox,
                                  pollutant=c("clno2"),type = "hour",
                                  plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","clno2-winter-dirtyNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


# specrad
specrad <- read_csv(file.path(EXPTDIR,
                              "BLEACH","data","specrad","winter-specrad.csv")) %>% 
  mutate(date = ts)

specradDiurnal <- timeVariation(specrad, 
                                pollutant = "j(no2)",type = "hour")$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","data", "specrad", "winter-jno2.csv"))




# Summer ------------------------------------------------------------------
tildas <- read_csv(file.path(EXPTDIR,"BLEACH","data","tildas","summer.csv")) %>% 
  dplyr::rename(ts = Index) %>% 
  select(ts,hcl, HCl_dailyoffset_BgCorr)

tildasAvg <- tildas %>% 
  mutate(ts = floor_date(ts, unit = "10 min")) %>% 
  group_by(ts) %>% 
  summarize(ts = mean(ts), hclAvg = mean(hcl), hclSd = sd(hcl), 
            hclCorr = mean(HCl_dailyoffset_BgCorr), 
            hclCorrSd = sd(HCl_dailyoffset_BgCorr)) %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","data","tildas","summer-10min-avg.csv"),
            quote = FALSE,row.names = FALSE)




# I- Cl compounds
icl <- read_csv(file.path(EXPTDIR,"BLEACH","data","cims","summer_icl2_local.csv")) %>% 
  filter(status=="sample") %>% 
  select(ts,icl2, iclno2) %>% 
  mutate(ts = ymd_hms(ts) + 10800) 
  
iclAvg <- icl %>% 
  mutate(ts = floor_date(ts, unit = "10 min")) %>% 
  group_by(ts) %>% 
  summarize(ts = mean(ts), icl2Avg = mean(icl2), icl2Sd = sd(icl2),
            iclno2Avg = mean(iclno2), iclno2Sd = sd(iclno2)) %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","data","cims","summer-icl-avg-utc.csv"),
            quote = FALSE,row.names = FALSE)

  

brcl <- read_csv(file.path(EXPTDIR,"BLEACH","data","cims","summer_brcl2_local.csv")) %>% 
  filter(status=="sample") %>% 
  select(ts,brcl2, brclno2) %>% 
  mutate(ts = dmy_hm(ts) +10800)

brclAvg <- brcl %>% 
  mutate(ts = floor_date(ts, unit = "10 min")) %>% 
  group_by(ts) %>% 
  summarize(ts = mean(ts), brcl2Avg = mean(brcl2), brcl2Sd = sd(brcl2),
            brclno2Avg = mean(brclno2), brclno2Sd = sd(brclno2)) %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","data","cims","summer-brcl-avg-utc.csv"),
            quote = FALSE,row.names = FALSE)


in2o5 <- read_csv(file.path(EXPTDIR,"BLEACH","data","cims","summer_in2o5_local.csv")) %>% 
  filter(status=="sample") %>% 
  select(ts,hpmtf,n2o5) %>% 
  mutate(ts = ymd_hms(ts)+10800)
  

in2o5Avg<- in2o5 %>% 
  mutate(ts = floor_date(ts, unit = "10 min")) %>% 
  group_by(ts) %>% 
  summarize(ts = mean(ts), in2o5Avg = mean(n2o5), 
            in2o5Sd = sd(n2o5)) %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","data","cims","summer-in2o5-avg-utc.csv"),
            quote = FALSE,row.names = FALSE)

nox <- read_csv(file.path(EXPTDIR,"BLEACH","data","nox","summer-nox.csv")) %>% 
  select(ts, NO_ppt_filt,NO2_ppt_filt, NOx_ppt_filt) %>% 
  mutate(ts = floor_date(ts, unit = "10 min")) %>% 
  group_by(ts) %>% 
  summarize_all(mean)


met <- read_csv(file.path(EXPTDIR,"BLEACH","data","met","summer-met.dat")) %>% 
  rename(ts = TIMESTAMP) %>% 
  select(-c("PumpStatus(1)","PumpStatus(2)","PumpStatus(3)", "RECORD",
            "Batt_volt_Min")) %>% 
  mutate(ts = floor_date(ts, unit = "10 min")) %>% 
  group_by(ts) %>% 
  summarize_all(mean)


# GEOS Chem
gc <- read_csv(file.path(EXPTDIR,
                         "BLEACH","data","geoschem","summer.csv"))


bigDF <- tildasAvg %>% 
  full_join(met) %>% 
  full_join(iclAvg) %>% 
  full_join(brclAvg) %>% 
  full_join(in2o5Avg) %>% 
  full_join(nox) %>% 
  mutate(no = na.approx(NO_ppt_filt, rule = 2)) %>% 
  mutate(no2 = na.approx(NO_ppt_filt, rule = 2)) %>% 
  mutate(nox = na.approx(NOx_ppt_filt, rule = 2)) %>% 
  full_join(gc) %>% 
  arrange(ts) %>% 
  filter(between(ts,ymd_hms("2022-06-08 00:00:00"), 
                 ymd_hms("2022-06-28 00:00:00")))


bigDFClean <- regimeFilter(DF = bigDF, METHOD = "wind", REGIME = "clean",
                           WRITE = FALSE)

bigDFClean_n <- bigDFClean %>% 
  mutate(hr = format(date,"%H")) %>% 
  group_by(hr) %>% 
  summarize(across(everything(), ~sum(!is.na(.)))) %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","summer-bigDfClean_n.csv"),
            quote = FALSE,row.names = FALSE)


bigDFDirty <- regimeFilter(DF = bigDF, METHOD = "wind", REGIME = "dirty",
                           WRITE = FALSE)

bigDFDirty_n <- bigDFDirty %>% 
  mutate(hr = format(date,"%H")) %>% 
  group_by(hr) %>% 
  summarize(across(everything(), ~sum(!is.na(.)))) %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","summer-bigDfDirty_n.csv"),
            quote = FALSE,row.names = FALSE)



bigDFCleanNox <- regimeFilter(DF = bigDF, METHOD = "nox", REGIME = "clean",
                              WRITE = FALSE)

bigDFCleanNox_n <- bigDFCleanNox %>% 
  mutate(hr = format(date,"%H")) %>% 
  group_by(hr) %>% 
  summarize(across(everything(), ~sum(!is.na(.)))) %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","summer-bigDfCleanNox_n.csv"),
            quote = FALSE,row.names = FALSE)

bigDFDirtyNox <- regimeFilter(DF = bigDF, METHOD = "nox", REGIME = "dirty",
                              WRITE = FALSE)

bigDFDirty_n <- bigDFDirtyNox %>% 
  mutate(hr = format(date,"%H")) %>% 
  group_by(hr) %>% 
  summarize(across(everything(), ~sum(!is.na(.)))) %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","summer-bigDfDirtyNox_n.csv"),
            quote = FALSE,row.names = FALSE)




#######################################################################
# Meteorology filters
cleanHclOut <- timeVariation(bigDFClean,
                             pollutant = "hclCorr",xlab = "hour", 
                             plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","hcl-summer-cleanDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


uwCimsCleanCl2 <- timeVariation(bigDFClean,
                                pollutant=c("brcl2Avg"),type = "hour",
                                plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","cl2-summer-CleanDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


uwCimsCleanClno2 <- timeVariation(bigDFClean,
                                  pollutant=c("iclno2Avg"),type = "hour",
                                  plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","clno2-summer-CleanDiurnal.csv"),
            quote = FALSE,row.names = FALSE)



cleanNox <- timeVariation(bigDFClean,
                          pollutant = "nox",xlab = "hour", 
                          plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","nox-summer-cleanDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


cleanNo <- timeVariation(bigDFClean,
                          pollutant = "no",xlab = "hour", 
                          plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","no-summer-cleanDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

cleanNo2 <- timeVariation(bigDFClean,
                          pollutant = "no2",xlab = "hour", 
                          plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","no2-summer-cleanDiurnal.csv"),
            quote = FALSE,row.names = FALSE)



gchemCleanHcl <- timeVariation(bigDFClean,
                               pollutant=c("HCl_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-hcl-summer-cleanDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


gchemCleanClno2 <- timeVariation(bigDFClean,
                                 pollutant=c("ClNO2_gc"),type = "hour",
                                 plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-clno2-summer-cleanDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemCleanCl2 <- timeVariation(bigDFClean,
                               pollutant=c("Cl2_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-cl2-summer-cleanDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemCleanNox <- timeVariation(bigDFClean,
                               pollutant=c("NOx_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-nox-summer-cleanDiurnal.csv"),
            quote = FALSE,row.names = FALSE)
######################################################################
# NOx filters
cleanHclOut <- timeVariation(bigDFCleanNox,
                             pollutant = "hclAvg",xlab = "hour",
                             plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","hcl-summer-cleanNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


uwCimsCleanCl2 <- timeVariation(bigDFCleanNox,
                                pollutant=c("brcl2Avg"),type = "hour",
                                plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","cl2-summer-CleanNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


uwCimsCleanClno2 <- timeVariation(bigDFCleanNox,
                                  pollutant=c("iclno2"),type = "hour",
                                  plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","clno2-summer-CleanNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)



cleanNox <- timeVariation(bigDFCleanNox,
                          pollutant = "nox",xlab = "hour", 
                          plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","nox-summer-cleanNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


cleanNo <- timeVariation(bigDFCleanNox,
                         pollutant = "no",xlab = "hour", 
                         plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","no-summer-cleanNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

cleanNo2 <- timeVariation(bigDFCleanNox,
                          pollutant = "no2",xlab = "hour", 
                          plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","no2-summer-cleanNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

# 
# gchemClean <- gc %>% 
#   left_join(met) %>% 
#   filter(between(ResultantMean_WD, 190, 300)) %>% 
#   filter(ResultantMean_WS >= 0.2) %>% 
#   rename(date = "ts") 

gchemCleanHcl <- timeVariation(bigDFCleanNox,
                               pollutant=c("HCl_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-hcl-summer-cleanNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


gchemCleanClno2 <- timeVariation(bigDFCleanNox,
                                 pollutant=c("ClNO2_gc"),type = "hour",
                                 plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-clno2-summer-cleanNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemCleanCl2 <- timeVariation(bigDFCleanNox,
                               pollutant=c("Cl2_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-cl2-summer-cleanNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemCleanNox <- timeVariation(bigDFCleanNox,
                               pollutant=c("NOx_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-nox-summer-cleanNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)



######################################################################
# Met - Dirty

gchemDirtyHcl <- timeVariation(bigDFDirty,
                               pollutant=c("HCl_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-hcl-summer-DirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemDirtyClno2 <- timeVariation(bigDFDirty,
                                 pollutant=c("ClNO2_gc"),type = "hour",
                                 plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-clno2-summer-DirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemDirtyCl2 <- timeVariation(bigDFDirty,
                               pollutant=c("Cl2_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-cl2-summer-DirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemDirtyNox <- timeVariation(bigDFDirty,
                               pollutant=c("NOx_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-nox-summer-DirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)



dirtyHclOut <- timeVariation(bigDFDirty,
                             pollutant = "hclAvg",xlab = "hour",
                             plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","hcl-summer-dirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


uwCimsDirtyCl2 <- timeVariation(bigDFDirty,
                                pollutant=c("brcl2"),type = "hour",
                                plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","cl2-Summer-dirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


uwCimsDirtyClno2 <- timeVariation(bigDFDirty,
                                  pollutant=c("iclno2"),type = "hour",
                                  plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","clno2-Summer-dirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)



dirtyNox <- timeVariation(bigDFClean,
                          pollutant = "nox",xlab = "hour", 
                          plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","nox-summer-dirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


dirtyNo <- timeVariation(bigDFClean,
                         pollutant = "no",xlab = "hour", 
                         plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","no-summer-dirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

dirtyNo2 <- timeVariation(bigDFClean,
                          pollutant = "no2",xlab = "hour", 
                          plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","no2-summer-dirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)



######################################################################
# Nox - Dirty

gchemDirtyHcl <- timeVariation(bigDFDirtyNox,
                               pollutant=c("HCl_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-hcl-summer-DirtyNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemDirtyClno2 <- timeVariation(bigDFDirtyNox,
                                 pollutant=c("ClNO2_gc"),type = "hour",
                                 plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-clno2-summer-DirtyNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemDirtyCl2 <- timeVariation(bigDFDirtyNox,
                               pollutant=c("Cl2_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-cl2-summer-DirtyNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemDirtyNox <- timeVariation(bigDFDirtyNox,
                               pollutant=c("NOx_gc"),type = "hour",
                               plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-nox-summer-DirtyNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)



dirtyHclOut <- timeVariation(bigDFDirtyNox,
                             pollutant = "hclAvg",xlab = "hour",
                             plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","summer-dirtyNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


uwCimsDirtyCl2 <- timeVariation(bigDFDirtyNox,
                                pollutant=c("cl2"),type = "hour",
                                plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","cl2-summer-dirtyNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


uwCimsDirtyClno2 <- timeVariation(bigDFDirtyNox,
                                  pollutant=c("clno2"),type = "hour",
                                  plot = FALSE)$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","clno2-summer-dirtyNoxDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemCleanHcl <- timeVariation(gchemClean,
                               pollutant=c("HCl / ppb"))$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-hcl-summer-cleanDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


gchemCleanClno2 <- timeVariation(gchemClean,
                                 pollutant=c("ClNO2 / ppb"),type = "hour")$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-clno2-summer-cleanDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemCleanCl2 <- timeVariation(gchemClean,
                               pollutant=c("Cl2 / ppb"),type = "hour")$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-cl2-summer-cleanDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemCleanNox <- timeVariation(gchemClean,
                               pollutant=c("NOx / ppb"),type = "hour")$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-nox-summer-cleanDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


gchemDirty <- gc %>% 
  left_join(met) %>% 
  filter(!between(ResultantMean_WD, 190, 300)) %>% 
  filter(ResultantMean_WS >= 0.2) %>% 
  rename(date = "ts") 


gchemDirtyHcl <- timeVariation(gchemDirty,
                               pollutant=c("HCl / ppb"),type = "hour")$data$hour %>% 
  
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-hcl-summer-DirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemDirtyClno2 <- timeVariation(gchemDirty,
                                 pollutant=c("ClNO2 / ppb"),type = "hour")$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-clno2-summer-DirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemDirtyCl2 <- timeVariation(gchemDirty,
                               pollutant=c("Cl2 / ppb"),type = "hour")$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-cl2-summer-DirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

gchemDirtyNox <- timeVariation(gchemDirty,
                               pollutant=c("NOx / ppb"),type = "hour")$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","gchem-nox-summer-DirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)



iclDirty <- iclAvg %>% 
  left_join(met) %>% 
  filter(!between(ResultantMean_WD, 190, 300)) %>% 
  filter(ResultantMean_WS >= 0.2) %>% 
  rename(date = "ts") 
# Converting to UTC in post

iclDirtyOut <- timeVariation(iclDirty,
                               pollutant=c("iclno2Avg"),type = "hour")$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","icl-summer-dirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


iclClean <- iclAvg %>% 
  left_join(met) %>% 
  filter(between(ResultantMean_WD, 190, 300)) %>% 
  filter(ResultantMean_WS >= 0.2) %>% 
  rename(date = "ts") 
# Converting to UTC in post

iclCleanOut <- timeVariation(iclClean,
                             pollutant=c("iclno2Avg"),type = "hour")$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","icl-summer-cleanDiurnal.csv"),
            quote = FALSE,row.names = FALSE)


# Converting to UTC in post
brclDirty <- brclAvg %>% 
  left_join(met) %>% 
  filter(!between(ResultantMean_WD, 190, 300)) %>% 
  filter(ResultantMean_WS >= 0.2) %>% 
  rename(date = "ts") 
# Converting to UTC in post

brclDirtyOut <- timeVariation(brclDirty,
                             pollutant=c("brcl2Avg"),type = "hour")$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","brcl-summer-dirtyDiurnal.csv"),
            quote = FALSE,row.names = FALSE)

# Converting to UTC in post

brclClean <- brclAvg %>% 
  left_join(met) %>% 
  filter(between(ResultantMean_WD, 190, 300)) %>% 
  filter(ResultantMean_WS >= 0.2) %>% 
  rename(date = "ts") 
# Converting to UTC in post

brclCleanOut <- timeVariation(brclClean,
                             pollutant=c("brcl2Avg"),type = "hour")$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","brcl-summer-cleanDiurnal.csv"),
            quote = FALSE,row.names = FALSE)



# POPS
ff <- list.files(file.path(EXPTDIR,"BLEACH","data","pops"),pattern = ".txt",
                 full.names = TRUE)
ff.pops <- lapply(ff,read.csv,skip=27)
popsDf <- do.call(rbind,ff.pops)
head(popsDf)
write.csv(popsDf, file.path(EXPTDIR,"BLEACH","data", "pops", "pops.csv"))


# specrad
specrad <- read_csv(file.path(EXPTDIR,
                              "BLEACH","data","specrad","summer-specrad.csv")) %>% 
  mutate(date = ts)

specradDiurnal <- timeVariation(specrad, 
                                pollutant = "j(no2)",type = "hour")$data$hour %>% 
  write.csv(file.path(EXPTDIR,"BLEACH","data", "specrad", "summer-jno2.csv"))





