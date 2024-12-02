# September 13-16 Field Data ----------------------------------------------------

load.tildas.data(START = "2024-09-13", STOP = "2024-09-16",
                 STC = TRUE, HCLONLY = TRUE)

dfTILDAS <- df.str %>% 
  left_join(df.stc) %>% 
  mutate(ts = round(ts)) %>% 
  filter(between(ts, ymd_hms("2024-09-13 19:00:00"),ymd_hms("2024-09-16 08:00:00"))) %>% 
  mutate(hcl = hcl / 0.83)

# From experiments on 21 Aug, I compared the measurement of ClNO2 through 
# the denuder with the measurement of ClNO2 bypassing the denuder:

# With denuder - 808 pptv (bg - 0.587 ppb, signal - 1.395 ppb)
# Without denuder - 974 pptv (bg - 0.715, signal - 1.689)

  
# I need to separate data based on TILDAS valve states.  I think this is
# probably easiest done if I round the TILDAS time stamps and merge the data 
# at their current frequencies (TILDAS are 1 Hz, CIMS are 0.1 Hz.)

cimsData <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/iodide/Export/I-caldata.csv") %>% 
  filter(between(time, ymd_hms("2024-09-13 19:00:00"),ymd_hms("2024-09-16 08:00:00"))) %>% 
  rename(ts = time) %>% 
  mutate(clno2Cal = clno2norm / 839)

dfAnal <- dfTILDAS %>% 
  left_join(cimsData)


# BG processing
dfBg <- dfAnal %>% 
  filter(ValveW == 2)

listBg <- split.groups(dfBg, gap = 10)
listBgFilter <- lapply(listBg, function(x){x[(61:(nrow(x)-10)),]}) 
# the above cuts off the first minute and final ten seconds of the background
# period.  There seems to be a slight adjustment period between when the valve
# is activated and response on the instruments.  Same with final 10 seconds.


hclBgAvgList <- lapply(listBgFilter, 
                       function(x){c(ts = median(x$ts),
                                     hclBlnkAvg = mean(x$hcl),
                                     hclBlnkSd = sd(x$hcl))})
hclBgAvgDf <- do.call(rbind, hclBgAvgList)



clno2BgAvgList <- lapply(listBgFilter, 
                         function(x){c(clno2BlnkAvg = mean(x$clno2Cal,na.rm=TRUE),
                                       clno2BlnkSd = sd(x$clno2Cal,na.rm=TRUE),
                                       iratioBlnk = mean(x$iratio, na.rm = TRUE))})
clno2BgAvgDf <- do.call(rbind, clno2BgAvgList)



blankSummary <- data.frame(cbind(hclBgAvgDf,clno2BgAvgDf))
blankSummary$hclLod <- blankSummary$hclBlnkSd*3
blankSummary$clno2Lod <- blankSummary$clno2BlnkSd*3
blankSummary$ts <- as.POSIXct(blankSummary$ts, origin ="1970-01-01")



hclLodAvg <- c(mean(blankSummary$hclLod), 
               sd(blankSummary$hclLod[-c(3,29)]))

cimsLodAvg <- c(mean(blankSummary$clno2Lod[-c(3,29)]),
                sd(blankSummary$clno2Lod[-c(3,29)]))

dfBg2 <- do.call(rbind, listBgFilter)
  

TIME1 <- ymd_hms("2024-09-13 21:00:00")
TIME2 <- ymd_hms("2024-09-14 08:30:00")

dfBg2 %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hcl)) +
  geom_point()+
  scale_y_continuous(name = "hcl")+
  theme_bw()


# We have extracted the blanks, now let's try to correct ambient data
blnkDf <- blankSummary[,c(1,2,4)]


dfAmbient <- dfAnal %>% 
  filter(ValveW == 0) 

listAmb <- split.groups(dfAmbient, gap = 10)
listAmb <- listAmb[which(sapply(listAmb,nrow) > 30)]
listAmbFilter <- lapply(listAmb, function(x){x[(31:(nrow(x)-10)),]}) 

dfAmb2 <- do.call(rbind,listAmbFilter)%>% 
  full_join(blnkDf) %>% 
  mutate(hclBlnkInterp = na.approx(hclBlnkAvg, x =ts,rule = 2)) %>%
  mutate(hclCorr = hcl - hclBlnkInterp) %>% 
  mutate(clno2BlnkInterp = na.approx(clno2BlnkAvg, x =ts,rule = 2)) %>% 
  mutate(clno2Corr = clno2Cal - clno2BlnkInterp) 
  
write.csv(dfAmb2,
          "C:/Users/jh2949/OneDrive - University of York/Desktop/fieldcorrecteddataclno2-0913-0916.csv",
          row.names=FALSE,quote=FALSE)

dfAmb60s <- dfAmb2 %>% 
  select(ts,clno2Corr,hclCorr) %>% 
  mutate(ts = floor_date(ts, "60 seconds")) %>% 
  group_by(ts) %>% 
  summarize_all(mean, na.rm=TRUE)

write.csv(dfAmb60s,
          "C:/Users/jh2949/OneDrive - University of York/Desktop/fieldcorrecteddataclno2-0913-0916-60savg.csv",
          row.names=FALSE,quote=FALSE)

dfCal <- dfAnal %>% 
  filter(ValveW %in% c(3,18))

TIME1 <- ymd_hms("2024-09-13 18:00:00")
TIME2 <- ymd_hms("2024-09-14 08:30:00")

dfCal %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=clno2Cal)) +
  geom_point()+
  scale_y_continuous(name = "clno2Cal")+
  theme_bw()


# September 3-4 Field Data ----------------------------------------------------

load.tildas.data(START = "2024-09-03", STOP = "2024-09-04",
                 STC = TRUE, HCLONLY = TRUE)

dfTILDAS <- df.str %>% 
  left_join(df.stc) %>% 
  mutate(ts = round(ts)) %>% 
  filter(between(ts, ymd_hms("2024-09-03 16:00:00"),ymd_hms("2024-09-04 10:00:00"))) %>% 
  mutate(hcl = hcl / 0.83)

# From experiments on 21 Aug, I compared the measurement of ClNO2 through 
# the denuder with the measurement of ClNO2 bypassing the denuder:

# With denuder - 808 pptv (bg - 0.587 ppb, signal - 1.395 ppb)
# Without denuder - 974 pptv (bg - 0.715, signal - 1.689)


# I need to separate data based on TILDAS valve states.  I think this is
# probably easiest done if I round the TILDAS time stamps and merge the data 
# at their current frequencies (TILDAS are 1 Hz, CIMS are 0.1 Hz.)
cimsData <- read_csv("C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2/averaged-iodide/20240903-field-refit/I-caldata.csv") %>% 
# cimsData <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/iodide/Export/I-caldata.csv") %>% 
  filter(between(time, ymd_hms("2024-09-03 16:00:00"),ymd_hms("2024-09-04 10:00:00"))) %>% 
  rename(ts = time) %>% 
  mutate(ts = round(ts)) %>% 
  mutate(clno2Cal = clno2norm / 673)
  
  #mutate(clno2Cal = clno2norm/839)

dfAnal <- dfTILDAS %>% 
  left_join(cimsData)


# BG processing
dfBg <- dfAnal %>% 
  filter(ValveW == 2)

listBg <- split.groups(dfBg, gap = 10)
listBgFilter <- lapply(listBg, function(x){x[(61:(nrow(x)-10)),]}) 
# the above cuts off the first minute and final ten seconds of the background
# period.  There seems to be a slight adjustment period between when the valve
# is activated and response on the instruments.  Same with final 10 seconds.


hclBgAvgList <- lapply(listBgFilter, 
                       function(x){c(ts = median(x$ts),
                                     hclBlnkAvg = mean(x$hcl),
                                     hclBlnkSd = sd(x$hcl))})
hclBgAvgDf <- do.call(rbind, hclBgAvgList)



clno2BgAvgList <- lapply(listBgFilter, 
                         function(x){c(clno2BlnkAvg = mean(x$clno2Cal,na.rm=TRUE),
                                       clno2BlnkSd = sd(x$clno2Cal,na.rm=TRUE),
                                       iratioBlnk = mean(x$iratio, na.rm = TRUE))})
clno2BgAvgDf <- do.call(rbind, clno2BgAvgList)



blankSummary <- data.frame(cbind(hclBgAvgDf,clno2BgAvgDf))
blankSummary$hclLod <- blankSummary$hclBlnkSd*3
blankSummary$clno2Lod <- blankSummary$clno2BlnkSd*3
blankSummary$ts <- as.POSIXct(blankSummary$ts, origin ="1970-01-01")

# blankSummary
# hclAvg      hclSd  clno2Avg     clno2Sd    iratio     hclLod   clno2Lod
# 1  0.2777948 0.01173319 0.1952405 0.009060047 0.4745839 0.03519958 0.02718014
# 2  0.2647628 0.01223062 0.1939919 0.007662732 0.4759264 0.03669187 0.02298819
# 3  0.2578390 0.01285358 0.3004854 0.158292379 0.4855302 0.03856075 0.47487714
# 4  0.2508534 0.01138270 0.1926725 0.009371842 0.4862850 0.03414810 0.02811553
# 5  0.2450484 0.01213552 0.1952730 0.007588935 0.4951912 0.03640657 0.02276681
# 6  0.2401995 0.01150149 0.1910588 0.011148353 0.4939073 0.03450447 0.03344506
# 7  0.2346274 0.01202257 0.1920743 0.009890828 0.5012531 0.03606770 0.02967248
# 8  0.2366332 0.01241909 0.1911416 0.006532000 0.5014782 0.03725727 0.01959600
# 9  0.2379285 0.01289124 0.1845847 0.007042198 0.5026555 0.03867372 0.02112659
# 10 0.2436242 0.01318919 0.1895496 0.009169603 0.5069429 0.03956758 0.02750881
# 11 0.2439536 0.01415923 0.1903436 0.006919418 0.5072005 0.04247768 0.02075825
# 12 0.2418374 0.01266601 0.1907105 0.007070571 0.5126877 0.03799804 0.02121171
# 13 0.2353733 0.01365056 0.1908511 0.010084559 0.5100343 0.04095169 0.03025368
# 14 0.2327594 0.01380815 0.1920732 0.008329143 0.5107810 0.04142446 0.02498743
# 15 0.2322913 0.01348014 0.1890629 0.007913664 0.5100775 0.04044041 0.02374099
# 16 0.2368290 0.01237597 0.1876167 0.008216014 0.5106418 0.03712791 0.02464804
# 17 0.2403921 0.01194413 0.2051387 0.008749596 0.5147347 0.03583240 0.02624879
# 18 0.2318748 0.01250000 0.1895582 0.006530159 0.5107282 0.03750001 0.01959048
# 19 0.2306978 0.01231939 0.1958107 0.007759847 0.5093121 0.03695817 0.02327954
# 20 0.2214520 0.01167454 0.1878559 0.007039540 0.5076483 0.03502363 0.02111862
# 21 0.2229409 0.01174341 0.1868492 0.007399172 0.5105404 0.03523023 0.02219752
# 22 0.2314362 0.01160357 0.1991433 0.006142358 0.5097067 0.03481070 0.01842708
# 23 0.2281033 0.01333582 0.1851684 0.006853991 0.5081070 0.04000745 0.02056197
# 24 0.2083413 0.01376795 0.1829890 0.007704820 0.5057047 0.04130384 0.02311446
# 25 0.2026791 0.01167270 0.1792050 0.007463075 0.5089053 0.03501809 0.02238922
# 26 0.1951248 0.01241741 0.1762280 0.008122438 0.5098291 0.03725224 0.02436731
# 27 0.1905624 0.01192311 0.1755770 0.009249137 0.5066606 0.03576932 0.02774741
# 28 0.1931373 0.01057653 0.1801495 0.006348085 0.5030671 0.03172960 0.01904425
# 29 0.4926162 1.03789859       NaN          NA       NaN 3.11369576         NA

# there's something odd with blank 3 and 29 (probably an addition or something)
# so let's remove Them from the statistics.
hclSdAvg <- c(mean(blankSummary$hclBlnkSd[-c(3,29)]), 
               sd(blankSummary$hclBlnkSd[-c(3,29)]))


hclLodAvg <- c(mean(blankSummary$hclLod[-c(3,29)]), 
               sd(blankSummary$hclLod[-c(3,29)]))

cimsLodAvg <- c(mean(blankSummary$clno2Lod[-c(3,29)]),
                sd(blankSummary$clno2Lod[-c(3,29)]))

dfBg2 <- do.call(rbind, listBgFilter)


TIME1 <- ymd_hms("2024-09-04 03:30:00")
TIME2 <- ymd_hms("2024-09-04 08:30:00")

dfBg2 %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hcl)) +
  geom_point()+
  scale_y_continuous(name = "hcl")+
  theme_bw()


# We have extracted the blanks, now let's try to correct ambient data
blnkDf <- blankSummary[,c(1,2,4)][-c(3,29),]


dfAmbient <- dfAnal %>% 
  filter(ValveW == 0) 

listAmb <- split.groups(dfAmbient, gap = 10)
listAmb <- listAmb[which(sapply(listAmb,nrow) > 30)]
listAmbFilter <- lapply(listAmb, function(x){x[(31:(nrow(x)-10)),]}) 

dfAmb2 <- do.call(rbind,listAmbFilter)%>% 
  full_join(blnkDf) %>% 
  mutate(hclBlnkInterp = na.approx(hclBlnkAvg, x =ts,rule = 2)) %>%
  mutate(hclCorr = hcl - hclBlnkInterp) %>% 
  mutate(clno2BlnkInterp = na.approx(clno2BlnkAvg, x =ts,rule = 2)) %>% 
  mutate(clno2Corr = clno2Cal - clno2BlnkInterp) 

write.csv(dfAmb2,
          "C:/Users/jh2949/OneDrive - University of York/Desktop/fieldcorrecteddataclno2.csv",
          row.names=FALSE,quote=FALSE)

dfAmb60s <- dfAmb2 %>% 
  select(ts,clno2Corr,hclCorr) %>% 
  mutate(ts = floor_date(ts, "60 seconds")) %>% 
  group_by(ts) %>% 
  summarize_all(mean, na.rm=TRUE) %>% 
  mutate(tildascimsratio = hclCorr/clno2Corr)

write.csv(dfAmb60s,
          "C:/Users/jh2949/OneDrive - University of York/Desktop/fieldcorrecteddataclno2-60savg.csv",
          row.names=FALSE,quote=FALSE)

dfCal <- dfAnal %>% 
  filter(ValveW %in% c(0)) %>% 
  mutate(tildascimsratio = hcl / clno2Cal)

TIME1 <- ymd_hms("2024-09-03 18:00:00")
TIME2 <- ymd_hms("2024-09-04 08:30:00")

dfAmb60s %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=tildascimsratio)) +
  geom_point()+
  scale_y_continuous(name = "tildascimsratio",limits=c(0,10))+
  theme_bw()



# June Field Data ---------------------------------------------------------


# Across 07 - 10 June, we sampled outside air on CIMS and TILDAS

# Let's see how well the measurements compared.


# Load CIMS data ----------------------------------------------------------

# 
# ffCims <- list.files("C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2/Field/CIMS",
#                      full.names=TRUE, pattern = ".csv")

# fileListCims <- lapply(ffCims,read_csv)
#cimsCsv <- read_csv("C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2/Field/cimsHrAltNames.csv")
cimsCsv <- read_csv("C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2/Field/20240627-20240628/Export/20240627.csv")
dfCims <- cimsCsv %>% 
  replace(. < 0, 0) %>% 
  mutate(valve = ifelse(I < 390e3,0,1)) %>% 
  #mutate(time = time - 3600) %>% 
  mutate(clno2norm = ifelse(I!=0,IClNO2/(I + IH2O),NA)) %>% 
  #mutate(clno2norm = replace(clno2norm, I < 2e5,NA)) %>% 
  mutate(iratio = ifelse(I!=0, IH2O / I, NA))  
  #mutate(iratio = ifelse(iratio>=1e-2,IH2O / I, NA)) %>% 
  # filter(between(ts,ymd_hms("2024-06-07 16:00:00"),
  #                ymd_hms("2024-06-10 09:00:00")))


dfCims <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/iodide/Export/IcimsHRDf-calthru20240628.csv") %>% 
  mutate(time = dmy_hm(time))

TIME1 <- ymd_hms("2024-06-27 18:00:00")
TIME2 <- ymd_hms("2024-06-28 08:30:00")
dfCims %>% 
  filter(between(time, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=time,y=clno2norm)) +
  geom_line()+
  scale_y_continuous(name = "IClNO2")+
  theme_bw()

write.csv(dfCims, "C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2/Field/20240627-20240628/Export/20240627-normalized.csv",
          quote =FALSE,row.names = FALSE)


# Load TILDAS data --------------------------------------------------------

load.tildas.data(START = "2024-06-28", STOP = "2024-07-01",
                 STC = TRUE, HCLONLY = FALSE)



TIME1 = ymd_hms("2024-06-28 20:00:00")
TIME2 = ymd_hms("2024-07-01 09:00:00")


CT <- df.str[which(df.stc$ValveW == 0),]
CTBG <- df.str[which(df.stc$ValveW == 2),]

CT60 <- CT %>%
  filter(between(ts,TIME1,TIME2)) %>%
  mutate(ts10 = floor_date(ts,"10 min")) %>%
  group_by(ts10) %>%
  slice(61:n()) %>%
  ungroup() %>%
  select(-ts10) %>%
  mutate(ts = floor_date(ts,"1 min")) %>%
  group_by(ts) %>%
  filter(n() > 30) %>%
  summarise_all(mean,na.rm = TRUE) %>%
  rename(ct = hcl)

CTBG60 <- CTBG %>%
  filter(between(ts,TIME1,TIME2)) %>%
  mutate(ts10 = floor_date(ts,"10 min")) %>%
  group_by(ts10) %>%
  slice(61:n()) %>%
  ungroup() %>%
  select(-ts10) %>%
  #mutate(ts = floor_date(ts,"1 min")) %>%
  #group_by(ts) %>%
  #filter(n() > 30) %>%
  #summarise_all(mean,na.rm = TRUE) %>%
  #rename(ctbg = hcl) %>% 
  write.csv("C:/Users/jh2949/OneDrive - University of York/Desktop/ctbgs.csv",row.names=FALSE,quote=FALSE)

CTOUT <- CT60 %>%
  full_join(CTBG60) %>%
  arrange(ts) %>%
  mutate(ctBGInterp = na.approx(ctbg, x =ts,rule = 2)) %>%
  mutate(ctOut = ct - ctBGInterp)


HT <- df.str[which(df.stc$ValveW == 48),]
HTBG <- df.str[which(df.stc$ValveW == 50),]


HT60 <- HT %>%
  filter(between(ts,TIME1,TIME2)) %>%
  mutate(ts10 = floor_date(ts,"10 min")) %>%
  group_by(ts10) %>%
  slice(61:n()) %>%
  ungroup() %>%
  select(-ts10) %>%
  mutate(ts = floor_date(ts,"1 min")) %>%
  group_by(ts) %>%
  summarise_all(mean,na.rm = TRUE) %>%
  rename(ht = hcl)

HTBG60 <- HTBG %>%
  filter(between(ts,TIME1,TIME2)) %>%
  mutate(ts10 = floor_date(ts,"10 min")) %>%
  group_by(ts10) %>%
  slice(61:n()) %>%
  ungroup() %>%
  select(-ts10) %>%
  #mutate(ts = floor_date(ts,"1 min")) %>%
  #group_by(ts) %>%
  #filter(n() > 30) %>%
  #summarise_all(mean,na.rm = TRUE) %>%
  #rename(htbg = hcl) %>% 
  write.csv("C:/Users/jh2949/OneDrive - University of York/Desktop/htbgs.csv",row.names=FALSE,quote=FALSE)


HTOUT <- HT60 %>%
  full_join(HTBG60) %>%
  arrange(ts) %>%
  mutate(htBGInterp = na.approx(htbg, x =ts,rule = 2)) %>%
  mutate(htOut = ht - htBGInterp)


tildasInterp <- CTOUT %>%
  full_join(HTOUT) %>%
  arrange(ts) %>%
  mutate(ctInterp = na.approx(ctOut, x =ts,rule = 2))

tildasScale <- tildasInterp %>% 
  filter(between(hour(ts), 12, 19)) %>% 
  mutate(scaleRatio = htOut/ctInterp)
  
  #mutate(clno2 = htOut - ctInterp)


# tildasDf <- CT60 %>%
#   full_join(HT60) %>%
#   arrange(ts) %>%
#   mutate(ctInterp = na.approx(ct, x =ts,rule = 2)) %>%
#   mutate(clno2 = ht - ctInterp)

plot(HTOUT$ts,HTOUT$htOut, type = "l", col = "red",ylim= c(0,0.2)); lines(CTOUT$ts,CTOUT$ctOut, col = "blue"); lines(tildasOUT$ts,tildasOUT$clno2, col = "salmon")

# There are 4 unique valve states:
# 0 - cold tube, ambient HCl
# 2 - cold tube bg
# 48- hot tube, ambient HCl + ClNO2
# 50 -  hot tube bg






# bg <- df.str[which(df.stc$ValveW == 2),]
# bg60 <- bg %>% 
#   filter(between(ts,TIME1,TIME2)) %>% 
#   mutate(ts10 = floor_date(ts,"10 min")) %>% 
#   group_by(ts10) %>% 
#   slice(61:n()) %>% 
#   ungroup() %>% 
#   select(-ts10) %>% 
#   mutate(ts = floor_date(ts,"1 min")) %>% 
#   group_by(ts) %>% 
#   filter(n() > 30) %>% 
#   summarise_all(mean,na.rm = TRUE) %>% 
#   rename(ct = hcl)
# 
# 
# 
# hcl <- df.str[which(df.stc$ValveW == 0),]
# 
# hcl60 <- hcl %>% 
#   filter(between(ts,TIME1,TIME2)) %>% 
#   mutate(ts10 = floor_date(ts,"10 min")) %>% 
#   group_by(ts10) %>% 
#   slice(61:n()) %>% 
#   ungroup() %>% 
#   select(-ts10) %>% 
#   mutate(ts = floor_date(ts,"1 min")) %>% 
#   group_by(ts) %>% 
#   summarise_all(mean,na.rm = TRUE) %>% 
#   rename(ht = hcl)

tildasDf <- CT60 %>% 
  full_join(HT60) %>% 
  arrange(ts) %>% 
  mutate(ctInterp = na.approx(ct, x =ts,rule = 2)) %>% 
  mutate(clno2 = ht - ctInterp)

write.csv(tildasOUT, "G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/Field/tildas-20240628-20240701.csv",
          row.names=FALSE,quote =FALSE)

plot(tildasDf$ts,tildasDf$clno2,type = "l")
