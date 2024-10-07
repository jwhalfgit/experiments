# September Field Data ----------------------------------------------------

load.tildas.data(START = "2024-09-03", STOP = "2024-09-04",
                 STC = TRUE, HCLONLY = TRUE)

dfTILDAS <- df.str %>% 
  left_join(df.stc) %>% 
  mutate(ts = round(ts)) %>% 
  filter(between(ts, ymd_hms("2024-09-03 16:00:00"),ymd_hms("2024-09-04 10:00:00")))
  
# I need to separate data based on TILDAS valve states.  I think this is
# probably easiest done if I round the TILDAS time stamps and merge the data 
# at their current frequencies (TILDAS are 1 Hz, CIMS are 0.1 Hz.)

cimsData <- read_csv("G:/My Drive/Experiments/lab/clno2cal/2024lab/TILDAS-CIMS/ClNO2/CIMS/iodide/Export/I-caldata.csv") %>% 
  filter(between(time, ymd_hms("2024-09-03 16:00:00"),ymd_hms("2024-09-04 10:00:00"))) %>% 
  rename(ts = time)

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
                       function(x){c(hclAvg = mean(x$hcl),
                                     hclSd = sd(x$hcl))})
hclBgAvgDf <- do.call(rbind, hclBgAvgList)



clno2BgAvgList <- lapply(listBgFilter, 
                         function(x){c(clno2Avg = mean(x$clno2norm,na.rm=TRUE),
                                       clno2Sd = sd(x$clno2norm,na.rm=TRUE))})
clno2BgAvgDf <- do.call(rbind, clno2BgAvgList)



blankSummary <- data.frame(cbind(hclBgAvgDf,clno2BgAvgDf))
blankSummary$hclLod <- blankSummary$hclSd*3
blankSummary$clno2Lod <- blankSummary$clno2Sd*3


dfBg2 <- do.call(rbind, listBgFilter)
  




TIME1 <- ymd_hms("2024-09-04 03:30:00")
TIME2 <- ymd_hms("2024-09-04 08:30:00")

dfBg2 %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hcl)) +
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

dfCims %>% 
  #filter(between(time, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=clno2norm)) +
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
