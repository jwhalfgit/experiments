
# January 11-13 Field Data ----------------------------------------------------

load.tildas.data(START = "2025-01-10", STOP = "2025-01-13",
                 STC = TRUE, HCLONLY = FALSE)

dfTILDAS <- df.str %>% 
  #mutate(ts = ts-10) %>% 
  left_join(df.stc) %>% 
  mutate(ts = round(ts)) %>%
  #mutate(dnderLoss = 0.424) %>% # from denuder losses
  mutate(dnderLoss = if_else(ts ==ymd_hms("2025-01-10 12:00:00"), .4479,
                              if_else(ts ==ymd_hms("2025-01-13 12:00:00"), .69, NA))) %>%
  mutate(dnderLossInterp = na.approx(dnderLoss, x = ts, rule =2)) %>% 
  #filter(between(ts, ymd_hms("2025-01-11 01:00:00"),ymd_hms("2025-01-13 11:30:00"))) %>% 
  mutate(hcl = hcl / (dnderLossInterp)/0.9721033 * 3/2.95) %>%  # multiply by the HCl denuder loss from dry to ambient humidity, and the amount from passivant dilution
  mutate(rh = h2o / sat.water(287.15)) %>% 
  filter(between(ts,ymd_hms("2025-01-10 00:00:00"), ymd_hms("2025-01-13 11:45:00"))) #%>% 
  # mutate(ts = floor_date(ts,unit = "1 minute")) %>%
  # group_by(ts) %>%
  # summarize_all(mean,na.rm=TRUE)

# HCl Denuder Loss Testing
# After ending sampling on 13 JAnuary, I did more denuder bypass testing:

# dnderAddn1 <- dfTILDAS %>% 
#   filter(between(ts, ymd_hm("2025-01-13 15:17"), ymd_hm("2025-01-13 15:19"))) %>% 
#   summarize(hcl = mean(hcl))
# 
# 
# dnderBg <- dfTILDAS %>% 
#   filter(between(ts, ymd_hm("2025-01-13 15:27"), ymd_hm("2025-01-13 15:29"))) %>% 
#   summarize(hcl = mean(hcl))
# 
# 
# dnderAddn2 <- dfTILDAS%>% 
#   filter(between(ts, ymd_hm("2025-01-13 15:33"), ymd_hm("2025-01-13 15:34"))) %>% 
#   summarize(hcl = mean(hcl))
# 
# 
# 
# dnderBypass <- dfTILDAS %>% 
#   filter(between(ts, ymd_hm("2025-01-13 15:47"), ymd_hm("2025-01-13 15:49"))) %>% 
#   summarize(hcl = mean(hcl))
# 
# (as.vector(unlist(c(dnderAddn1,dnderAddn2,dnderBypass))) - unlist(dnderBypass)) / unlist(dnderBypass)
#  -0.3092869 -0.3102734  0.0000000 # The difference between denuder and bypass at the end of the campaign is about 31%

# I also repeated this on 10 Jan, before the weekend of sampling:
# HCl Denuder Loss Testing
# After ending sampling on 13 JAnuary, I did more denuder bypass testing:
# 
# dnderAddn1 <- dfTILDAS %>%
#   filter(between(ts, ymd_hms("2025-01-10 11:33:30"), ymd_hms("2025-01-10 11:34:30"))) %>%
#   summarize(hcl = mean(hcl))
# # 0.552
# 
# dnderBg <- dfTILDAS %>%
#   filter(between(ts, ymd_hms("2025-01-10 11:43:30"), ymd_hms("2025-01-10 11:44:30"))) %>%
#   summarize(hcl = mean(hcl))
# # 0.0531
# 
# bypassBg <- dfTILDAS %>% 
#   filter(between(ts, ymd_hm("2025-01-10 12:28"), ymd_hm("2025-01-10 12:30"))) %>%
#   summarize(hcl = mean(hcl))
# #   0.0551
# 
# dnderBypass <- dfTILDAS%>%
#   filter(between(ts, ymd_hm("2025-01-10 12:33"), ymd_hm("2025-01-10 12:35"))) %>%
#   summarize(hcl = mean(hcl))
# # 1.23
# # 
# 
# as.vector(((unlist(c(dnderAddn1))) - unlist(c(dnderBypass)) - unlist(c(bypassBg)) - unlist(c(dnderBg))) / (unlist(dnderBypass) -unlist(c(bypassBg))))

# [1] -0.5520784  # The difference between denuder and bypass before the  the campaign is about 55%
#################################################################################
# We also have the in-experiment calibration.  Let's see what that says...
# hclBg <- dfTILDAS%>%
#      filter(between(ts, ymd_hm("2025-01-13 06:05"), ymd_hm("2025-01-13 06:09"))) %>%
#      summarize(hcl = mean(hcl))
#   
#   # 0.0921 (loss factors applied)
#   
# cimsBg <- cimsData %>% 
#   filter(between(ts, ymd_hm("2025-01-13 06:05"), ymd_hm("2025-01-13 06:09"))) %>%
#   summarize(clno2 = mean(clno2norm))
#  # 6.05 ncps (no cal factors applied)
# 
# clno2AddnHcl <- dfTILDAS%>%
#   filter(between(ts, ymd_hm("2025-01-13 06:34"), ymd_hm("2025-01-13 06:39"))) %>%
#   summarize(hcl = mean(hcl))
# # 0.517 (loss factor applied)
# 
# 
# clno2AddnCims <- cimsData%>%
#   filter(between(ts, ymd_hm("2025-01-13 06:34"), ymd_hm("2025-01-13 06:39"))) %>%
#   summarize(clno2 = mean(clno2norm))
#  848

#adhoc cal factor: (848 -6.05) / (0.517 - 0.0921) 1981.525


# WThere's also the cal point before the ClnO2 goes up around midnight Let's see what that says...
# hclBg <- dfTILDAS%>%
#      filter(between(ts, ymd_hm("2025-01-13 00:05"), ymd_hm("2025-01-13 00:09"))) %>%
#      summarize(hcl = mean(hcl))
# 
#   # 0.0382 (loss factors applied)
# 
# cimsBg <- cimsData %>%
#   filter(between(ts, ymd_hm("2025-01-13 00:05"), ymd_hm("2025-01-13 00:09"))) %>%
#   summarize(clno2 = mean(clno2norm))
#  # 3.69 ncps (no cal factors applied)
# 
# clno2AddnHcl <- dfTILDAS%>%
#   filter(between(ts, ymd_hm("2025-01-13 06:37"), ymd_hm("2025-01-13 06:39"))) %>%
#   summarize(hcl = mean(hcl))
# # 0.522 (loss factor applied)
# 
# 
# clno2AddnCims <- cimsData%>%
#   filter(between(ts, ymd_hm("2025-01-13 06:37"), ymd_hm("2025-01-13 06:39"))) %>%
#   summarize(clno2 = mean(clno2norm))
# # 855 

#adhoc cal factor: (855 -3.69) / (0.522 - 0.0382) 1759.632

# I need to separate data based on TILDAS valve states.  I think this is
# probably easiest done if I round the TILDAS time stamps and merge the data 
# at their current frequencies (TILDAS are 1 Hz, CIMS are 0.1 Hz.)

# cimsData1 <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/iodide/Export/20250111-field/I-fielddata.csv") 
# cimsData2 <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/iodide/Export/20250112-field/I-fielddata.csv") 
# cimsData3 <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/iodide/Export/20250113-field/I-fielddata.csv") 
# 
# cimsDataTotal <- rbind(cimsData1,cimsData2,cimsData3)

cimsDataTotal <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/iodide/Export/20250113-field-withclno/I-fielddata.csv") 

cimsData <- cimsDataTotal %>% 
  filter(between(time, ymd_hms("2025-01-10 00:00:00"),ymd_hms("2025-01-13 11:45:00"))) %>%
  rename(ts = time) %>% 
  mutate(ts = round(ts) - 9)%>% 
  #mutate(calFactor = iratio*2104+362) %>% 
  mutate(calFactor = 1981.5) %>% 
  mutate(clno2Cal = clno2norm / calFactor) %>% 
  mutate(hno3norm = IHNO3 / iratio) %>% 
  mutate(ncl3norm = Cl3IN / iratio,
        clnonorm = IClNO / iratio,
        chcl3norm = CCl3HI / iratio)
  #filter(between(ts,ymd_hms("2025-01-13 00:00:00"), ymd_hms("2025-01-13 11:45:00"))) #%>% 
  # mutate(ts = floor_date(ts,unit = "1 minute")) %>%
  # group_by(ts) %>%
  # summarize_all(mean,na.rm=TRUE)


# write.csv(cimsDataTotal,"G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/iodide/Export/20250110-20250113-field.csv",
#           quote = FALSE,row.names=FALSE)

dfAnal <- dfTILDAS %>% 
  left_join(cimsData)


# BG processing
dfBg <- dfAnal %>% 
  filter(ValveW == 2)

# For 60 s averaging
# listBg <- split.groups(dfBg, gap = 5)
# listBgFilter <- lapply(listBg, function(x){x[(2:5),]})

listBg <- split.groups(dfBg, gap = 60)
listBg <- listBg[which(sapply(listBg,nrow) > 250)]
listBgFilter <- lapply(listBg, function(x){x[(251:(nrow(x)-10)),]})
# the above cuts off the first minute and final ten seconds of the background
# period.  There seems to be a slight adjustment period between when the valve
# is activated and response on the instruments.  Same with final 10 seconds.


hclBgAvgList <- lapply(listBgFilter, 
                       function(x){c(ts = median(x$ts),
                                     hclBlnkAvg = mean(x$hcl),
                                     hclBlnkSd = sd(x$hcl))})
hclBgAvgDf <- do.call(rbind, hclBgAvgList)



clno2BgAvgList <- lapply(listBgFilter, 
                         function(x){c(#ts = median(x$ts),
                           clno2BlnkAvg = mean(x$clno2Cal,na.rm=TRUE),
                           clno2BlnkSd = sd(x$clno2Cal,na.rm=TRUE),
                           iratioBlnk = mean(x$iratio, na.rm = TRUE))})
clno2BgAvgDf <- do.call(rbind, clno2BgAvgList)



blankSummary <- data.frame(cbind(hclBgAvgDf,clno2BgAvgDf))
blankSummary$hclLod <- blankSummary$hclBlnkSd*3
blankSummary$clno2Lod <- blankSummary$clno2BlnkSd*3
blankSummary$ts <- as.POSIXct(blankSummary$ts, origin ="1970-01-01")



hclLodAvg <- c(mean(blankSummary$hclLod), 
               sd(blankSummary$hclLod))
# 
# hclLodAvg
# [1] 0.010047697 0.004869707

cimsLodAvg <- c(mean(blankSummary$clno2Lod,na.rm = TRUE),
                sd(blankSummary$clno2Lod,na.rm = TRUE))

dfBg2 <- do.call(rbind, listBgFilter)


TIME1 <- ymd_hms("2025-01-13 01:00:00")
TIME2 <- ymd_hms("2025-01-13 12:30:00")


dfBg2 %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hcl)) +
  geom_line()+
  scale_y_continuous(name = "hcl", limits = c(-0.1,0.2))+
  theme_bw()


dfAnal %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=IH2O)) +
  geom_point()+
  scale_y_continuous(name = "ClNO2(ppbv)", limits = c(0,0.2))+
  theme_bw()


# We have extracted the blanks, now let's try to correct ambient data
blnkDf <- blankSummary[,c(1,2,4)]


dfAmbient <- dfAnal %>% 
  filter(ValveW == 0) 

listAmb <- split.groups(dfAmbient, gap = 10)
listAmb <- listAmb[which(sapply(listAmb,nrow) > 30)]
listAmbFilter <- lapply(listAmb, function(x){x[(90:(nrow(x)-10)),]}) 

dfAmb2 <- do.call(rbind,listAmbFilter)%>% 
  full_join(blnkDf) %>% 
  mutate(hclBlnkInterp = na.approx(hclBlnkAvg, x =ts,rule = 2)) %>%
  mutate(hclCorr = hcl - hclBlnkInterp) %>% 
  mutate(clno2BlnkInterp = na.approx(clno2BlnkAvg, x =ts,rule = 2)) %>% 
  mutate(clno2Corr = clno2Cal - clno2BlnkInterp) %>% 
  mutate(ts = floor_date(ts, "10 seconds")) %>% 
  select(ts,clno2norm,clno2Corr,iratio,hclCorr,clnonorm,chcl3norm) %>% 
  group_by(ts) %>% 
  #filter(between(ts, ymd_hms("2025-01-13 00:00:00"),ymd_hms("2025-01-13 12:00:00"))) %>%
  summarize_all(mean, na.rm=TRUE) 
  
  

write.csv(dfAmb2,
          "C:/Users/jh2949/OneDrive - University of York/Desktop/fieldcorrecteddataclno2-20250113-withclno.csv",
          row.names=FALSE,quote=FALSE)

dfAmb60s <- dfAmb2 %>% 
  #select(ts,clno2Corr,hclCorr) %>% 
  select(ts,clno2Corr,hclCorr,clnonorm,chcl3norm) %>% 
  
  mutate(ts = floor_date(ts, "60 seconds")) %>% 
  group_by(ts) %>% 
  #filter(between(ts, ymd_hms("2025-01-13 00:00:00"),ymd_hms("2025-01-13 12:00:00"))) %>%
  summarize_all(list(mean = mean,sd=sd), na.rm=TRUE) %>%
  #na.omit() %>% 
  mutate(cimstildasratio = clno2Corr_mean/hclCorr_mean) %>% 
  na.omit() %>% 
  filter(between(ts, ymd_hms("2025-01-10 00:00:00"),ymd_hms("2025-01-13 11:45:00"))) %>% 
  mutate(combUnc = sqrt(clno2Corr_sd / clno2Corr_mean)^2 + (clno2Corr_sd / clno2Corr_mean)^2 )


write.csv(dfAmb60s,
          "C:/Users/jh2949/OneDrive - University of York/Desktop/fieldcorrecteddataclno2-20250113-60sa.csv - altbg -withClNO",
          row.names=FALSE,quote=FALSE)

# field observations - whole shebang
fieldObs <- dfAmb60s[2642:3115,] %>% 
  na.omit()

x1 <- fieldObs$hclCorr_mean
y1 <- fieldObs$clno2Corr_mean
xsd <- fieldObs$hclCorr_sd
ysd <- fieldObs$clno2Corr_sd

summary(bfsl(x = x1, y = y1,sd_x = xsd, sd_y = ysd))
# Call:
#   bfsl.default(x = x1, y = y1, sd_x = xsd, sd_y = ysd)
# 
# Residuals:
#   Min.     1st Qu.      Median        Mean     3rd Qu.  
# -2.007e-02  -5.640e-03  -7.897e-04  -4.657e-05   4.525e-03  
# Max.  
# 6.654e-02  
# 
# Coefficients:
#   Estimate  Std. Error
# Intercept  0.007598  0.002242  
# Slope      0.963190  0.027006  
# 
# Goodness of fit: 0.2239
# Chisq-statistic: 101 on 451 degrees of freedom
# Covariance of the slope and intercept: -5.522e-05
# p-value: 1

summary(lm(y1~x1))
# Adj R2 = 0.9259

# field observations - first leg
fieldObs <- dfAmb60s[2642:2979,] %>% 
  na.omit()

x1 <- fieldObs$hclCorr_mean
y1 <- fieldObs$clno2Corr_mean
xsd <- fieldObs$hclCorr_sd
ysd <- fieldObs$clno2Corr_sd

summary(bfsl(x = x1, y = y1,sd_x = xsd, sd_y = ysd))
# Call:
#   bfsl.default(x = x1, y = y1, sd_x = xsd, sd_y = ysd)
# 
# Residuals:
#   Min.     1st Qu.      Median        Mean     3rd Qu.  
# -0.0170445  -0.0056607  -0.0006086   0.0001415   0.0038069  
# Max.  
# 0.0686170  
# 
# Coefficients:
#   Estimate  Std. Error
# Intercept  0.005379  0.002347  
# Slope      0.955351  0.027340  
# 
# Goodness of fit: 0.1948
# Chisq-statistic: 62.72 on 322 degrees of freedom
# Covariance of the slope and intercept: -5.704e-05
# p-value: 1
summary(lm(y1~x1))
# R2 = 0.949

# field observations - whole shebang filtering outliers
fieldObs <- dfAmb60s[2642:3115,][-(140:149),] %>% 
  na.omit()

x1 <- fieldObs$hclCorr_mean
y1 <- fieldObs$clno2Corr_mean
xsd <- fieldObs$hclCorr_sd
ysd <- fieldObs$clno2Corr_sd

summary(bfsl(x = x1, y = y1,sd_x = xsd, sd_y = ysd))

# 
# 
# Call:
#   bfsl.default(x = x1, y = y1, sd_x = xsd, sd_y = ysd)
# 
# Residuals:
#   Min.     1st Qu.      Median        Mean     3rd Qu.  
# -0.0199154  -0.0052978  -0.0005483  -0.0001459   0.0046325  
# Max.  
# 0.0283258  
# 
# Coefficients:
#   Estimate  Std. Error
# Intercept  0.005994  0.002359  
# Slope      0.977370  0.028071  
# 
# Goodness of fit: 0.1765
# Chisq-statistic: 77.66 on 440 degrees of freedom
# Covariance of the slope and intercept: -6.07e-05
# p-value: 1

#summary(lm(y1~x1))

#R2 = 0.94

# Quick regression from60s avg of 10s averages
fieldObs <- dfAmb60s[-(134:143),] %>% 
  na.omit()

x1 <- fieldObs$hclCorr_mean
y1 <- fieldObs$clno2Corr_mean
xsd <- fieldObs$hclCorr_sd
ysd <- fieldObs$clno2Corr_sd

summary(bfsl(x = x1, y = y1,sd_x = xsd, sd_y = ysd))
summary(lm(y1~x1))

# field observations - first leg filtered
fieldObs <- dfAmb60s[2642:2980,][-(140:149),] %>% 
  na.omit()

x1 <- fieldObs$hclCorr_mean
y1 <- fieldObs$clno2Corr_mean
xsd <- fieldObs$hclCorr_sd
ysd <- fieldObs$clno2Corr_sd

summary(bfsl(x = x1, y = y1,sd_x = xsd, sd_y = ysd))
# Call:
#   bfsl.default(x = x1, y = y1, sd_x = xsd, sd_y = ysd)
# 
# Residuals:
#   Min.     1st Qu.      Median        Mean     3rd Qu.  
# -1.663e-02  -5.104e-03  -1.865e-04  -1.900e-07   4.231e-03  
# Max.  
# 3.169e-02  
# 
# Coefficients:
#   Estimate  Std. Error
# Intercept  0.003376  0.002477  
# Slope      0.970858  0.028406  
# 
# Goodness of fit: 0.115
# Chisq-statistic: 35.86 on 312 degrees of freedom
# Covariance of the slope and intercept: -6.297e-05
# p-value: 1

# summary(lm(y1~x1))
# R2 = 0.969

# field observations - last leg filtered
fieldObs <- dfAmb60s[2980:3115,] %>% 
  na.omit()

x1 <- fieldObs$hclCorr_mean
y1 <- fieldObs$clno2Corr_mean
xsd <- fieldObs$hclCorr_sd
ysd <- fieldObs$clno2Corr_sd

summary(bfsl(x = x1, y = y1,sd_x = xsd, sd_y = ysd))
# Call:
#   bfsl.default(x = x1, y = y1, sd_x = xsd, sd_y = ysd)
# 
# Residuals:
#   Min.     1st Qu.      Median        Mean     3rd Qu.  
# -0.0110646  -0.0042491  -0.0011913  -0.0003567   0.0039785  
# Max.  
# 0.0127834  
# 
# Coefficients:
#   Estimate  Std. Error
# Intercept  0.025142  0.008387  
# Slope      0.826458  0.110363  
# 
# Goodness of fit: 0.1025
# Chisq-statistic: 13.02 on 127 degrees of freedom
# Covariance of the slope and intercept: -0.0009094
# p-value: 1
# 
# R2 = 0.8031

# whole shebang, redone zeroes
fieldObs <- dfAmb60s[1802:2262,][-(134:142),] %>% 
  na.omit()

x1 <- fieldObs$hclCorr_mean
y1 <- fieldObs$clno2Corr_mean
xsd <- fieldObs$hclCorr_sd
ysd <- fieldObs$clno2Corr_sd

summary(bfsl(x = x1, y = y1,sd_x = xsd, sd_y = ysd))

# lm()
# Call:
#   bfsl.default(x = x1, y = y1, sd_x = xsd, sd_y = ysd)
# 
# Residuals:
#   Min.     1st Qu.      Median        Mean     3rd Qu.  
# -1.983e-02  -4.792e-03   8.611e-05   4.686e-04   4.842e-03  
# Max.  
# 3.007e-02  
# 
# Coefficients:
#   Estimate   Std. Error
# Intercept  0.0052067  0.0009654 
# Slope      0.9670786  0.0120410 
# 
# Goodness of fit: 0.92
# Chisq-statistic: 414 on 450 degrees of freedom
# Covariance of the slope and intercept: -1.052e-05
# p-value: 0.887

summary(lm(y1~x1))
R2 = 0.969

a <- dfAmb60s %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hclCorr_mean)) +
  geom_line()+
  scale_y_continuous(name = "HCl (ppbv)", limits = c(0,0.2))+
  theme_bw()

b <- dfAmb60s %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=clno2Corr_mean)) +
  geom_line()+
  scale_y_continuous(name = "ClNO2(ppbv)", limits = c(0,0.2))+
  theme_bw()

grid.arrange(a,b)



dfCal <- dfAnal %>% 
  filter(ValveW %in% c(3,18))

TIME1 <- ymd_hms("2025-01-13 01:00:00")
TIME2 <- ymd_hms("2025-01-13 11:30:00")

dfCal %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=clno2Cal)) +
  geom_point()+
  scale_y_continuous(name = "clno2Cal")+
  theme_bw()


hclBgField <- df.str %>% 
  filter(between(ts,ymd_hm("2025-01-13 11:32"),ymd_hm("2025-01-13 11:34"))) %>% 
  summarize(hcl = mean(hcl))

# A tibble: 1 × 1
# hcl
# <dbl>
#   1 0.0448

# hclAddnField <- df.str %>% 
#   filter(between(ts,ymd_hm("2025-01-13 12:12"),ymd_hm("2025-01-13 12:14"))) %>% 
#   summarize(hcl = mean(hcl))
# # # A tibble: 1 × 1
# # hcl
# # <dbl>
# #   1  4.54
# 
# # hcl fiel daddition net = 4.495
# 
# hclBgDry <- df.str %>% 
#   filter(between(ts,ymd_hm("2025-01-13 14:12"),ymd_hm("2025-01-13 14:14"))) %>% 
#   summarize(hcl = mean(hcl))
# 
# # hclBgDry
# # # A tibble: 1 × 1
# # hcl
# # <dbl>
# #   1 0.0658
# 
# hclAddnDry <- df.str %>% 
#   filter(between(ts,ymd_hm("2025-01-13 14:22"),ymd_hm("2025-01-13 14:24"))) %>% 
#   summarize(hcl = mean(hcl))

# hclAddnDry
# # A tibble: 1 × 1
# hcl
# <dbl>
#   1  4.69

# hclDryAddn = 4.69 - 0.0658 = 4.6242

# difference = (4.4952 - 4.6242) / 4.6242
#############################################
# Denuder
###############
# clno2BgDnderDry <- df.str %>% 
#   filter(between(ts,ymd_hm("2025-01-10 11:27"),ymd_hm("2025-01-10 11:29"))) %>% 
#   summarize(hcl = mean(hcl))
# # 
# # # A tibble: 1 × 1
# # hcl
# # <dbl>
# #   1 0.0645
# 
# clno2DnderDry <- df.str %>% 
#   filter(between(ts,ymd_hm("2025-01-10 11:32"),ymd_hm("2025-01-10 11:34"))) %>% 
#   summarize(hcl = mean(hcl))
# # 
# # > clno2DnderDry
# # # A tibble: 1 × 1
# # hcl
# # <dbl>
# #   1 0.548
# 
# clno2BgDry <- df.str %>% 
#   filter(between(ts,ymd_hm("2025-01-10 12:12"),ymd_hm("2025-01-10 12:14"))) %>% 
#   summarize(hcl = mean(hcl))
# 
# # clno2BgDry
# # # A tibble: 1 × 1
# # hcl
# # <dbl>
# #   1 0.0517
# 
# clno2Dry <- df.str %>% 
#   filter(between(ts,ymd_hm("2025-01-10 12:02"),ymd_hm("2025-01-10 12:04"))) %>% 
#   summarize(hcl = mean(hcl))
# 
# # clno2Dry
# # A tibble: 1 × 1
# hcl
# <dbl>
#   1  1.23

#clno2 Dry = 1.23 - 0.0517 = 1.178
# Clno2Denuder = 0.548 - 0.0645 = 0.4835


# Jan11 -------------------------------------------------------------------


load.tildas.data(START = "2025-01-10", STOP = "2025-01-13",
                 STC = TRUE, HCLONLY = FALSE)

dfTILDAS <- df.str %>% 
  #mutate(ts = ts-10) %>% 
  left_join(df.stc) %>% 
  mutate(ts = round(ts)) %>%
  #mutate(dnderLoss = 0.424) %>% # from denuder losses
  mutate(dnderLoss = if_else(ts ==ymd_hms("2025-01-10 12:00:00"), .4479,
                             if_else(ts ==ymd_hms("2025-01-13 12:00:00"), .69, NA))) %>%
  mutate(dnderLossInterp = na.approx(dnderLoss, x = ts, rule =2)) %>% 
  #filter(between(ts, ymd_hms("2025-01-11 01:00:00"),ymd_hms("2025-01-13 11:30:00"))) %>% 
  mutate(hcl = hcl / (dnderLossInterp)/0.9721033 * 3/2.95) %>%  # multiply by the HCl denuder loss from dry to ambient humidity, and the amount from passivant dilution
  mutate(rh = h2o / sat.water(287.15)) %>% 
 filter(between(ts,ymd_hms("2025-01-11 01:00:00"), ymd_hms("2025-01-11 11:45:00"))) #%>% 
# mutate(ts = floor_date(ts,unit = "1 minute")) %>%
# group_by(ts) %>%
# summarize_all(mean,na.rm=TRUE)

#adhoc cal factor: (780) / (0.830) 940

# I need to separate data based on TILDAS valve states.  I think this is
# probably easiest done if I round the TILDAS time stamps and merge the data 
# at their current frequencies (TILDAS are 1 Hz, CIMS are 0.1 Hz.)

cimsData1 <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/iodide/Export/20250111-field/I-fielddata.csv") 
cimsData2 <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/iodide/Export/20250112-field/I-fielddata.csv") 
cimsData3 <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/iodide/Export/20250113-field/I-fielddata.csv") 

cimsDataTotal <- rbind(cimsData1,cimsData2,cimsData3)

cimsData <- cimsDataTotal %>% 
  filter(between(time, ymd_hms("2025-01-11 01:00:00"),ymd_hms("2025-01-11 11:30:00"))) %>%
  rename(ts = time) %>% 
  mutate(ts = round(ts) - 9)%>% 
  #mutate(calFactor = iratio*2104+362) %>% 
  mutate(calFactor = 940) %>% 
  mutate(clno2Cal = clno2norm / calFactor) %>% 
  mutate(hno3norm = IHNO3 / iratio) %>% 
  mutate(ncl3norm = Cl3IN / iratio) %>% 
  mutate(ncl2hnorm = Cl2HIN / iratio) %>%
  mutate(nclh2norm = ClH2IN / iratio) %>%
  filter(between(ts,ymd_hms("2025-01-11 00:00:00"), ymd_hms("2025-01-11 11:45:00"))) #%>% 
# mutate(ts = floor_date(ts,unit = "1 minute")) %>%
# group_by(ts) %>%
# summarize_all(mean,na.rm=TRUE)


# write.csv(cimsDataTotal,"G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/iodide/Export/20250110-20250113-field.csv",
#           quote = FALSE,row.names=FALSE)

dfAnal <- dfTILDAS %>% 
  left_join(cimsData)


# BG processing
dfBg <- dfAnal %>% 
  filter(ValveW == 2)

# For 60 s averaging
# listBg <- split.groups(dfBg, gap = 5)
# listBgFilter <- lapply(listBg, function(x){x[(2:5),]})

listBg <- split.groups(dfBg, gap = 60)
listBg <- listBg[which(sapply(listBg,nrow) > 250)]
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
                         function(x){c(#ts = median(x$ts),
                           clno2BlnkAvg = mean(x$clno2Cal,na.rm=TRUE),
                           clno2BlnkSd = sd(x$clno2Cal,na.rm=TRUE),
                           iratioBlnk = mean(x$iratio, na.rm = TRUE))})
clno2BgAvgDf <- do.call(rbind, clno2BgAvgList)



blankSummary <- data.frame(cbind(hclBgAvgDf,clno2BgAvgDf))
blankSummary$hclLod <- blankSummary$hclBlnkSd*3
blankSummary$clno2Lod <- blankSummary$clno2BlnkSd*3
blankSummary$ts <- as.POSIXct(blankSummary$ts, origin ="1970-01-01")



hclLodAvg <- c(mean(blankSummary$hclLod), 
               sd(blankSummary$hclLod))
# 
# hclLodAvg
# [1] 0.010047697 0.004869707

cimsLodAvg <- c(mean(blankSummary$clno2Lod,na.rm = TRUE),
                sd(blankSummary$clno2Lod,na.rm = TRUE))

dfBg2 <- do.call(rbind, listBgFilter)


TIME1 <- ymd_hms("2025-01-13 01:00:00")
TIME2 <- ymd_hms("2025-01-13 11:30:00")


dfBg2 %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hcl)) +
  geom_line()+
  scale_y_continuous(name = "hcl", limits = c(-0.1,0.6))+
  theme_bw()


dfAnal %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=IH2O)) +
  geom_point()+
  scale_y_continuous(name = "ClNO2(ppbv)", limits = c(0,0.2))+
  theme_bw()


# We have extracted the blanks, now let's try to correct ambient data
blnkDf <- blankSummary[,c(1,2,4)]


dfAmbient <- dfAnal %>% 
  filter(ValveW == 0) 

listAmb <- split.groups(dfAmbient, gap = 10)
listAmb <- listAmb[which(sapply(listAmb,nrow) > 30)]
listAmbFilter <- lapply(listAmb, function(x){x[(90:(nrow(x)-10)),]}) 

dfAmb2 <- do.call(rbind,listAmbFilter)%>% 
  full_join(blnkDf) %>% 
  mutate(hclBlnkInterp = na.approx(hclBlnkAvg, x =ts,rule = 2)) %>%
  mutate(hclCorr = hcl - hclBlnkInterp) %>% 
  mutate(clno2BlnkInterp = na.approx(clno2BlnkAvg, x =ts,rule = 2)) %>% 
  mutate(clno2Corr = clno2Cal - clno2BlnkInterp) %>% 
  select(ts,clno2norm,clno2Corr,iratio,hclCorr,ncl3norm, ncl2hnorm,nclh2norm)

write.csv(dfAmb2,
          "C:/Users/jh2949/OneDrive - University of York/Desktop/fieldcorrecteddataclno2-20250111.csv",
          row.names=FALSE,quote=FALSE)

dfAmb60s <- dfAmb2 %>% 
  select(ts,clno2Corr,hclCorr,ncl3norm,ncl2hnorm,nclh2norm) %>% 
  mutate(ts = floor_date(ts, "60 seconds")) %>% 
  group_by(ts) %>% 
  #filter(between(ts, ymd_hms("2025-01-13 00:00:00"),ymd_hms("2025-01-13 12:00:00"))) %>%
  summarize_all(list(mean = mean,sd=sd), na.rm=TRUE) %>%
  #na.omit() %>% 
  mutate(cimstildasratio = clno2Corr_mean/hclCorr_mean) %>% 
  na.omit()


write.csv(dfAmb60s,
          "C:/Users/jh2949/OneDrive - University of York/Desktop/fieldcorrecteddataclno2-20250111-60savg.csv",
          row.names=FALSE,quote=FALSE)

# January 09-10 Field Data ----------------------------------------------------

load.tildas.data(START = "2025-01-11", STOP = "2025-01-13",
                 STC = TRUE, HCLONLY = TRUE)

dfTILDAS <- df.str %>% 
  left_join(df.stc) %>% 
  mutate(ts = round(ts)) %>%
  mutate(ts = ts) %>% 
  filter(between(ts, ymd_hms("2025-01-11 18:00:00"),ymd_hms("2025-01-13 11:30:00")))# %>% 
  #mutate(hcl = hcl / 0.83)


# From experiments on 21 Aug, I compared the measurement of ClNO2 through 
# the denuder with the measurement of ClNO2 bypassing the denuder:

# With denuder - 808 pptv (bg - 0.587 ppb, signal - 1.395 ppb)
# Without denuder - 974 pptv (bg - 0.715, signal - 1.689)


# I need to separate data based on TILDAS valve states.  I think this is
# probably easiest done if I round the TILDAS time stamps and merge the data 
# at their current frequencies (TILDAS are 1 Hz, CIMS are 0.1 Hz.)

cimsData <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/iodide/Export/20250109-field/I-fielddata.csv") %>% 
  filter(between(time, ymd_hms("2025-01-09 18:00:00"),ymd_hms("2025-01-10 09:00:00"))) %>%  
  rename(ts = time) %>% 
  mutate(ts = round(ts))%>% 
  mutate(clno2Cal = clno2norm / 800)

dfAnal <- dfTILDAS %>% 
  left_join(cimsData)


# BG processing
dfBg <- dfAnal %>% 
  filter(ValveW == 2)

listBg <- split.groups(dfBg, gap = 60)
listBg <- listBg[which(sapply(listBg,nrow) > 250)]
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
                         function(x){c(#ts = median(x$ts),
                           clno2BlnkAvg = mean(x$clno2Cal,na.rm=TRUE),
                           clno2BlnkSd = sd(x$clno2Cal,na.rm=TRUE),
                           iratioBlnk = mean(x$iratio, na.rm = TRUE))})
clno2BgAvgDf <- do.call(rbind, clno2BgAvgList)



blankSummary <- data.frame(cbind(hclBgAvgDf,clno2BgAvgDf))
blankSummary$hclLod <- blankSummary$hclBlnkSd*3
blankSummary$clno2Lod <- blankSummary$clno2BlnkSd*3
blankSummary$ts <- as.POSIXct(blankSummary$ts, origin ="1970-01-01")



hclLodAvg <- c(mean(blankSummary$hclLod), 
               sd(blankSummary$hclLod))

cimsLodAvg <- c(mean(blankSummary$clno2Lod),
                sd(blankSummary$clno2Lod))

dfBg2 <- do.call(rbind, listBgFilter)


TIME1 <- ymd_hms("2025-01-09 18:00:00")
TIME2 <- ymd_hms("2025-01-10 09:00:00")


dfBg2 %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hcl)) +
  geom_line()+
  scale_y_continuous(name = "hcl", limits = c(-0.1,0.6))+
  theme_bw()

dfAmb60s %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hclCorr)) +
  geom_line()+
  scale_y_continuous(name = "HCl (ppbv)", limits = c(0,0.2))+
  theme_bw()

dfAmb60s %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=clno2Corr+.05)) +
  geom_line()+
  scale_y_continuous(name = "ClNO2(ppbv)", limits = c(0,0.2))+
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
          "C:/Users/jh2949/OneDrive - University of York/Desktop/fieldcorrecteddataclno2-1216.csv",
          row.names=FALSE,quote=FALSE)

dfAmb60s <- dfAmb2 %>% 
  select(ts,clno2Corr,hclCorr) %>% 
  mutate(ts = floor_date(ts, "60 seconds")) %>% 
  group_by(ts) %>% 
  summarize_all(mean, na.rm=TRUE)

write.csv(dfAmb60s,
          "C:/Users/jh2949/OneDrive - University of York/Desktop/fieldcorrecteddataclno2-1216-60savg.csv",
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
# January 07-08 Field Data ----------------------------------------------------

load.tildas.data(START = "2025-01-07", STOP = "2025-01-08",
                 STC = TRUE, HCLONLY = TRUE)

dfTILDAS <- df.str %>% 
  left_join(df.stc) %>% 
  mutate(ts = round(ts)) %>%
  mutate(ts = ts - 40) %>% 
  filter(between(ts, ymd_hms("2025-01-07 18:00:00"),ymd_hms("2025-01-08 09:00:00"))) %>% 
  mutate(hcl = hcl / 0.83)


# From experiments on 21 Aug, I compared the measurement of ClNO2 through 
# the denuder with the measurement of ClNO2 bypassing the denuder:

# With denuder - 808 pptv (bg - 0.587 ppb, signal - 1.395 ppb)
# Without denuder - 974 pptv (bg - 0.715, signal - 1.689)


# I need to separate data based on TILDAS valve states.  I think this is
# probably easiest done if I round the TILDAS time stamps and merge the data 
# at their current frequencies (TILDAS are 1 Hz, CIMS are 0.1 Hz.)

cimsData <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/iodide/Export/20250107-field/I-fielddata.csv") %>% 
  filter(between(time, ymd_hms("2025-01-07 18:00:00"),ymd_hms("2025-01-08 09:00:00"))) %>%  
  rename(ts = time) %>% 
  mutate(ts = round(ts))%>% 
  mutate(clno2Cal = clno2norm / 1000)

dfAnal <- dfTILDAS %>% 
  left_join(cimsData)


# BG processing
dfBg <- dfAnal %>% 
  filter(ValveW == 2)

listBg <- split.groups(dfBg, gap = 60)
listBg <- listBg[which(sapply(listBg,nrow) > 250)]
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
                         function(x){c(#ts = median(x$ts),
                                        clno2BlnkAvg = mean(x$clno2Cal,na.rm=TRUE),
                                       clno2BlnkSd = sd(x$clno2Cal,na.rm=TRUE),
                                       iratioBlnk = mean(x$iratio, na.rm = TRUE))})
clno2BgAvgDf <- do.call(rbind, clno2BgAvgList)



blankSummary <- data.frame(cbind(hclBgAvgDf,clno2BgAvgDf))
blankSummary$hclLod <- blankSummary$hclBlnkSd*3
blankSummary$clno2Lod <- blankSummary$clno2BlnkSd*3
blankSummary$ts <- as.POSIXct(blankSummary$ts, origin ="1970-01-01")



hclLodAvg <- c(mean(blankSummary$hclLod), 
               sd(blankSummary$hclLod))

cimsLodAvg <- c(mean(blankSummary$clno2Lod),
                sd(blankSummary$clno2Lod))

dfBg2 <- do.call(rbind, listBgFilter)


TIME1 <- ymd_hms("2025-01-07 21:00:00")
TIME2 <- ymd_hms("2025-01-08 08:30:00")


dfBg2 %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hcl)) +
  geom_line()+
  scale_y_continuous(name = "hcl", limits = c(-0.1,0.6))+
  theme_bw()

dfAmb60s %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hclCorr)) +
  geom_line()+
  scale_y_continuous(name = "HCl (ppbv)", limits = c(0,0.1))+
  theme_bw()

dfAmb60s %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=clno2Corr)) +
  geom_line()+
  scale_y_continuous(name = "ClNO2(ppbv)", limits = c(0,0.1))+
  theme_bw()


# We have extracted the blanks, now let's try to correct ambient data
blnkDf <- blankSummary[,c(1,2,4)]


dfAmbient <- dfAnal %>% 
  filter(ValveW != 2) 

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
          "C:/Users/jh2949/OneDrive - University of York/Desktop/fieldcorrecteddataclno2-1216.csv",
          row.names=FALSE,quote=FALSE)

dfAmb60s <- dfAmb2 %>% 
  select(ts,clno2Corr,hclCorr) %>% 
  mutate(ts = floor_date(ts, "60 seconds")) %>% 
  group_by(ts) %>% 
  summarize_all(mean, na.rm=TRUE)

write.csv(dfAmb60s,
          "C:/Users/jh2949/OneDrive - University of York/Desktop/fieldcorrecteddataclno2-1216-60savg.csv",
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


# December 16-17 Field Data ----------------------------------------------------

load.tildas.data(START = "2025-12-16", STOP = "2024-12-17",
                 STC = TRUE, HCLONLY = TRUE)

dfTILDAS <- df.str %>% 
  left_join(df.stc) %>% 
  mutate(ts = round(ts)) %>% 
  filter(between(ts, ymd_hms("2024-12-16 17:30:00"),ymd_hms("2024-12-17 09:00:00"))) %>% 
  filter(!between(ts, ymd_hms("2024-12-16 23:30:00"),ymd_hms("2024-12-17 01:00:00")))# %>% 
  #mutate(hcl = hcl / 0.83)

# From experiments on 21 Aug, I compared the measurement of ClNO2 through 
# the denuder with the measurement of ClNO2 bypassing the denuder:

# With denuder - 808 pptv (bg - 0.587 ppb, signal - 1.395 ppb)
# Without denuder - 974 pptv (bg - 0.715, signal - 1.689)


# I need to separate data based on TILDAS valve states.  I think this is
# probably easiest done if I round the TILDAS time stamps and merge the data 
# at their current frequencies (TILDAS are 1 Hz, CIMS are 0.1 Hz.)

cimsData <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/bromide/Export/20241216/Br-fielddata.csv") %>% 
  filter(between(time, ymd_hms("2024-12-16 17:30:00"),ymd_hms("2024-12-17 09:00:00"))) %>% 
  rename(ts = time) %>% 
  mutate(ts = round(ts))%>% 
  mutate(clno2Cal = clno2norm / 1000)

dfAnal <- dfTILDAS %>% 
  left_join(cimsData)


# BG processing
dfBg <- dfAnal %>% 
  filter(ValveW == 2)

listBg <- split.groups(dfBg, gap = 60)
listBg <- listBg[which(sapply(listBg,nrow) > 250)]
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


TIME1 <- ymd_hms("2024-12-16 21:00:00")
TIME2 <- ymd_hms("2024-12-17 08:30:00")


dfBg2 %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hcl)) +
  geom_line()+
  scale_y_continuous(name = "hcl", limits = c(-0.1,0.6))+
  theme_bw()

dfAmb60s %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hclCorr)) +
  geom_line()+
  scale_y_continuous(name = "HCl (ppbv)", limits = c(0,0.3))+
  theme_bw()

dfAmb60s %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=clno2Corr)) +
  geom_line()+
  scale_y_continuous(name = "ClNO2(ppbv)", limits = c(0,0.5))+
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
          "C:/Users/jh2949/OneDrive - University of York/Desktop/fieldcorrecteddataclno2-1216.csv",
          row.names=FALSE,quote=FALSE)

dfAmb60s <- dfAmb2 %>% 
  select(ts,clno2Corr,hclCorr) %>% 
  mutate(ts = floor_date(ts, "60 seconds")) %>% 
  group_by(ts) %>% 
  summarize_all(mean, na.rm=TRUE)

write.csv(dfAmb60s,
          "C:/Users/jh2949/OneDrive - University of York/Desktop/fieldcorrecteddataclno2-1216-60savg.csv",
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
                 STC = TRUE, HCLONLY = FALSE)

dfTILDAS <- df.str %>% 
  left_join(df.stc) %>% 
  mutate(ts = round(ts)) %>% 
  filter(between(ts, ymd_hms("2024-09-03 16:00:00"),ymd_hms("2024-09-04 07:00:00"))) %>% 
  mutate(hcl = hcl / 0.89) %>% 
  mutate(rh = h2o / sat.water(287.15)) #%>% 
  # mutate(ts = floor_date(ts,unit = "1 minute")) %>%
  # group_by(ts) %>%
  # summarize_all(mean,na.rm=TRUE)
  

# From experiments on 21 Aug, I compared the measurement of ClNO2 through 
# the denuder with the measurement of ClNO2 bypassing the denuder:

# With denuder - 808 pptv (bg - 0.587 ppb, signal - 1.395 ppb)
# Without denuder - 928 pptv (bg - 0.715, signal - 1.643)

# I did a similar experiment on 31 May ()
# without denuder - 1.948 (bg - 0.380 ppb, signal 2.328 ppb)
# with denuder - 1.77 ppb (bg - 0.243 ppb, signal 2.013 ppb)

# This leads to an average loss of 11% across these two experiments 
# (9.1% vs 12.9%)

# I need to separate data based on TILDAS valve states.  I think this is
# probably easiest done if I round the TILDAS time stamps and merge the data 
# at their current frequencies (TILDAS are 1 Hz, CIMS are 0.1 Hz.)
cimsData <- read_csv("C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2/averaged-iodide/20240903-field-refit/I-caldata.csv") %>% 
# cimsData <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/iodide/Export/I-caldata.csv") %>% 
  filter(between(time, ymd_hms("2024-09-03 16:00:00"),ymd_hms("2024-09-04 10:00:00"))) %>% 
  rename(ts = time) %>% 
  mutate(ts = round(ts)) %>% 
  mutate(clno2Cal = clno2norm / 673) %>% # or 673
  mutate(hno3norm = IHNO3 / iratio) %>% 
 mutate(ncl3norm = Cl3IN / iratio) #%>%
# mutate(ts = floor_date(ts,unit = "1 minute")) %>%
# group_by(ts) %>%
# summarize_all(mean,na.rm=TRUE)
  
  #mutate(clno2Cal = clno2norm/839)

dfAnal <- dfTILDAS %>% 
  left_join(cimsData)


# BG processing
dfBg <- dfAnal %>% 
  filter(ValveW == 2)

# for 60 s avg
# listBg <- split.groups(dfBg, gap = 5)
# listBgFilter <- lapply(listBg, function(x){x[(2:5),]}) 

# for 1s TILDAS / 10 s CIMS
listBg <- split.groups(dfBg, gap = 10)
listBgFilter <- lapply(listBg, function(x){x[(121:(nrow(x)-30)),]})
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
# 
# ts hclBlnkAvg   hclBlnkSd clno2BlnkAvg  clno2BlnkSd iratioBlnk     hclLod    clno2Lod
# 1  2024-09-03 16:03:15  0.2492939 0.010891326 0.0002188051 0.0008474283  0.4749830 0.03267398 0.002542285
# 2  2024-09-03 16:33:14  0.2371808 0.010905448 0.0011456146 0.0024223788  0.4753133 0.03271634 0.007267136
# 3  2024-09-03 17:03:15  0.2305284 0.011521277 0.0006577318 0.0025473842  0.4850585 0.03456383 0.007642153
# 4  2024-09-03 17:33:15  0.2250522 0.009709089 0.0008485191 0.0018795406  0.4853660 0.02912727 0.005638622
# 5  2024-09-03 18:33:15  0.2195658 0.010921784 0.0002565253 0.0009935182  0.4955241 0.03276535 0.002980555
# 6  2024-09-03 19:03:15  0.2157679 0.010609238 0.0077512526 0.0107710011  0.4947318 0.03182772 0.032313003
# 7  2024-09-03 19:33:15  0.2096646 0.011255358 0.0030408435 0.0056600132  0.5012395 0.03376607 0.016980040
# 8  2024-09-03 20:03:15  0.2124367 0.011518989 0.0030770852 0.0049944099  0.5014261 0.03455697 0.014983230
# 9  2024-09-03 20:33:15  0.2135340 0.012123815 0.0020674382 0.0045088371  0.5025123 0.03637145 0.013526511
# 10 2024-09-03 21:03:15  0.2177689 0.011525727 0.0031869594 0.0042589971  0.5073446 0.03457718 0.012776991
# 11 2024-09-03 21:33:15  0.2174918 0.012990627 0.0046400603 0.0053245024  0.5065177 0.03897188 0.015973507
# 12 2024-09-03 22:03:16  0.2165411 0.010217093 0.0042439981 0.0060760395  0.5132488 0.03065128 0.018228118
# 13 2024-09-03 22:33:15  0.2103930 0.012740179 0.0030208188 0.0054151049  0.5097026 0.03822054 0.016245315
# 14 2024-09-03 23:03:15  0.2088582 0.012772453 0.0044320142 0.0060941139  0.5109096 0.03831736 0.018282342
# 15 2024-09-03 23:33:15  0.2084920 0.011846798 0.0067351208 0.0070380675  0.5098217 0.03554039 0.021114203
# 16 2024-09-04 00:33:15  0.2115851 0.010948913 0.0078559743 0.0085533953  0.5104967 0.03284674 0.025660186
# 17 2024-09-04 01:03:16  0.2152375 0.010811604 0.0120163784 0.0078302376  0.5159165 0.03243481 0.023490713
# 18 2024-09-04 01:33:15  0.2068452 0.010804111 0.0071313231 0.0090554458  0.5102464 0.03241233 0.027166338
# 19 2024-09-04 02:03:16  0.2063306 0.011281577 0.0095877785 0.0095387043  0.5103706 0.03384473 0.028616113
# 20 2024-09-04 02:33:15  0.1978416 0.010674568 0.0061079470 0.0070771454  0.5082811 0.03202370 0.021231436
# 21 2024-09-04 03:03:16  0.1993474 0.010846904 0.0078366527 0.0083308917  0.5110992 0.03254071 0.024992675
# 22 2024-09-04 03:33:15  0.2078991 0.010817588 0.0078924477 0.0091080080  0.5100737 0.03245276 0.027324024
# 23 2024-09-04 04:03:16  0.2037078 0.012240087 0.0034304241 0.0050346078  0.5082947 0.03672026 0.015103824
# 24 2024-09-04 04:33:15  0.1873719 0.012280508 0.0015231508 0.0028874402  0.5065706 0.03684153 0.008662321
# 25 2024-09-04 05:03:16  0.1822957 0.010309289 0.0027789840 0.0054718831  0.5098575 0.03092787 0.016415649
# 26 2024-09-04 05:33:15  0.1755871 0.011017857 0.0006950410 0.0014457716  0.5086502 0.03305357 0.004337315
# 27 2024-09-04 06:33:15  0.1706766 0.009870249 0.0019897351 0.0053500779  0.5061577 0.02961075 0.016050234
# 28 2024-09-04 06:48:12  0.1768533 0.009434299 0.0021712174 0.0026950635  0.5061291 0.02830290 0.008085190
# 29 2024-09-04 07:35:14  0.4848007 1.039226348          NaN           NA        NaN 3.11767904          NA

# there's something odd with blank 3 and 29 (probably an addition or something)
# so let's remove Them from the statistics.
hclSdAvg <- c(mean(blankSummary$hclBlnkSd[-c(3,29)]), 
               sd(blankSummary$hclBlnkSd[-c(3,29)]))


hclLodAvg <- c(mean(blankSummary$hclLod[-c(3,29)],na.rm=TRUE), 
               sd(blankSummary$hclLod[-c(3,29)],na.rm=TRUE))
# 60 s avg 
# hclLodAvg
# [1] 0.005441438 0.003536278

cimsLodAvg <- c(mean(blankSummary$clno2Lod[-c(3,29)],na.rm=TRUE),
                sd(blankSummary$clno2Lod[-c(3,29)],na.rm=TRUE))
# 60 s avg
# cimsLodAvg
# [1] 0.008115981 0.006322863

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
blnkDf <- blankSummary[,c(1,2,4)][-c(29),]


dfAmbient <- dfAnal %>% 
  filter(ValveW == 0) 

listAmb <- split.groups(dfAmbient, gap = 10)
listAmb <- listAmb[which(sapply(listAmb,nrow) > 30)]
listAmbFilter <- lapply(listAmb, function(x){x[(31:(nrow(x)-10)),]}) 



dfAmb2 <- do.call(rbind,listAmbFilter)%>% 
  full_join(blnkDf) %>% 
  filter(between(ts, ymd_hm("2024-09-03 17:45"),ymd_hm("2024-09-04 06:45"))) %>% 
  mutate(hclBlnkInterp = na.approx(hclBlnkAvg, x =ts,rule = 2)) %>%
  mutate(hclCorr = hcl - hclBlnkInterp - 0.0206904) %>% # see below comment
  mutate(clno2BlnkInterp = na.approx(clno2BlnkAvg, x =ts,rule = 2)) %>% 
  mutate(clno2Corr = clno2Cal - clno2BlnkInterp) 


# 20241204 in comparing TILDAS ClNO2 and CIMS, TILDAS seems to have something like a 13 ppt offset.
#  Looking at the delta between ambient and overblow in the mid-late afternoon, there is a small delta.
# I would assume this is an offset I need to account for, so I will subtract the average residual from 
# this period from TILDAS measurements (16:00-19:00) .

# oblowOffset <- dfAmb2 %>% 
#   filter(between(ts, ymd_hm("2024-09-03 16:30"),ymd_hm("2024-09-03 20:00"))) %>% 
#   mutate(ts = floor_date(ts, unit = "30 minutes")) %>% 
#   group_by(ts) %>% 
#   summarize(hclAvg = mean(hclCorr,na.rm=TRUE),cimsAvg = mean(clno2Corr,na.rm=TRUE))

# As seen below, the CIMS blanks are pretty steady and I don't think warrant correction.
#  TILDAS though , I can see an argument for using the lowest value blanks as representing
# the truest zero, or using the average around it as a measure of the variability.
# Using the average of values from 16:30 - 19:30 gives result of 21 ± 6, which 
# looks better overall than using the lowest point of 12.8 ppt.  So let's go with
# that.  
# 
# ts                     hclAvg    cimsAvg
# <dttm>                  <dbl>      <dbl>
#   1 2024-09-03 16:30:00.00 0.0212  0.0000953
# 2 2024-09-03 17:00:00.00 0.0284 -0.000128 
# 3 2024-09-03 17:30:00.00 0.0246  0.00151  
# 4 2024-09-03 18:00:00.00 0.0243  0.00140  
# 5 2024-09-03 18:30:00.00 0.0128 -0.00149  
# 6 2024-09-03 19:00:00.00 0.0158 -0.00213  
# 7 2024-09-03 19:30:00.00 0.0178  0.00140  


write.csv(dfAmb2,
          "C:/Users/jh2949/OneDrive - University of York/Desktop/fieldcorrecteddataclno2.csv",
          row.names=FALSE,quote=FALSE)

dfAmb60s <- dfAmb2 %>% 
  select(ts,clno2Corr,hclCorr,ncl3norm,hno3norm) %>% 
  mutate(ts = floor_date(ts, "60 seconds")) %>% 
  group_by(ts) %>% 
  summarize_all(list(mean = mean,sd=sd), na.rm=TRUE) %>%
  na.omit() %>% 
  mutate(cimstildasratio = clno2Corr_mean/hclCorr_mean)

dfAmb60s <- dfAmb60s[-c(15,595),]


qcDfAmb <- dfAmb60s %>% 
  filter(hclCorr > 0.005 & clno2Corr > 0.008) %>% 
  filter(between(ts, ymd_hm("2024-09-03 17:45"),ymd_hm("2024-09-04 05:45")))

summary(lm(clno2Corr_mean[1:304]~hclCorr_mean[1:304],data = dfAmb60s))
summary(lm(clno2Corr_mean[444:617]~hclCorr_mean[444:617],data = dfAmb60s))


summary(bfsl(x =dfAmb60s$hclCorr_mean[1:304]*1000, y=dfAmb60s$clno2Corr_mean[1:304]*1000,
      sd_x = dfAmb60s$hclCorr_sd[1:304]*1000,sd_y = dfAmb60s$clno2Corr_sd[1:304]*1000))

summary(bfsl(x =dfAmb60s$hclCorr_mean[444:617], y=dfAmb60s$clno2Corr_mean[444:617],
     sd_x = dfAmb60s$hclCorr_sd[444:617],sd_y = dfAmb60s$clno2Corr_sd[444:617]))

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
  
  ggplot(aes(x=ts,y=(tildascimsratio^-1))) +
  geom_point()+
  scale_y_continuous(name = "tildascimsratio")+
  theme_bw()

oblowOffset %>% 
  
  ggplot(aes(x=ts,y=hclAvg)) +
  geom_point()+
  scale_y_continuous(name = "hcl")+
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
