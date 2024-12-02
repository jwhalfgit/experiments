
# Load files ----------------------------------------------------------------

library(openair)

OSCADATADIR <- file.path(EXPTDIR, "OSCA","data")

timespan <- data.frame(date = seq(as.POSIXct("2021-06-10"),
                                  as.POSIXct("2021-07-21"), by = 60))


tildas <- read_csv(file.path(OSCADATADIR,"2021", "tildas.csv")) %>% 
  rename(date = ts) %>% 
  mutate(hcl = hcl) # ocnvert to ppbv for CIMS consistency
  

clno2 <- read_csv(file.path(OSCADATADIR,"2021", "cims60sAvg.csv")) %>% 
  rename(date = ts, clno2 = ClNO2_ppt) %>% 
  select(!contains(c("N2O5", "Cl2"))) %>% 
  mutate(across(!date, ~.x/1000))


  
# specrad <-read_csv(file.path(OSCADATADIR,"2021", "specrad.csv")) %>% 
#   rename(date = datetime) %>% 
#   mutate(date = dmy_hms(date) )

no2 <- read_csv(file.path(OSCADATADIR,"2021","no2.csv")) %>% 
  rename(date = datetime, no2 = `NO2 (ppb)`) %>% 
  select(!`NO2_qc_flags`)

nonoy <- read_csv(file.path(OSCADATADIR,"2021","nox-noy.csv")) %>% 
  rename(date = datetime, no=`NO (ppb)`, noy = `NOy (ppb)`)

n2o5_cl2 <- read_csv(file.path(OSCADATADIR,"2021", "cims-n2o5-cl2.csv")) %>% 
  rename(date = ts, cl2 = "Cl2_ppt*", n2o5 = "N2O5_ppt*") %>% 
  mutate(across(!date, ~.x/1000))

o3 <- read_csv(file.path(OSCADATADIR,"2021", "o3.csv")) %>% 
  rename(date = datetime, o3 = "Ozone (ppb)") %>% 
  select(!contains("qc"))
  

so2 <- read_csv(file.path(OSCADATADIR,"2021", "noy_so2_yrk.csv")) %>% 
  select(date, contains("so2")) %>% 
  filter(qc_flag_so2_43i %in% c(0,1,2)) %>% 
  rename(so2 = so2_43i) %>% 
  select(date,so2)

voc <- read_csv(file.path(OSCADATADIR,"2021", "gc.csv")) %>% 
  filter(type == "Sample") %>% 
  select(!contains("flag")) %>% 
  select(!contains("uncertainty")) %>% 
  select(!contains("channel")) %>% 
  full_join(timespan) %>% 
  arrange(date) %>% 
  mutate(across(!(type), ~na.approx(.))) %>% 
  select(!contains("type")) %>% 
  mutate(date = as.POSIXct(date, origin="1970-01-01", tz = "UTC"))
                
                
#                approx(~.x, n = n())$y))


# This is the estimated BL height in m
pblh <- read_csv(file.path(OSCADATADIR,"2021","maqs.csv")) %>% 
  select(ts, "Estimated.Boundary.Layer.Height..m.","JO1D..s.1.","JNO2..s.1.") %>% 
  rename(date = ts, pblh_m = "Estimated.Boundary.Layer.Height..m.",
         jo1d= "JO1D..s.1.", jno2 = "JNO2..s.1.")

met <- read_csv(file.path(OSCADATADIR,"2021", "fidas-met.csv")) %>% 
  mutate(datetime = dmy_hm(datetime)) %>% 
  select(!contains("flag"))
names(met) = c("date","tempC","press","rh","tempK")



acsm <- read_csv(file.path(OSCADATADIR,"2021", "acsm.csv")) %>% 
  mutate(datetime = dmy_hm(datetime)) %>% 
  rename(date = datetime, pcl_1 = `Chl (ug/m3)`, pnh4 = `NH4 (ug/m3)`,
         pno3 = `NO3 (ug/m3)`, pso4 = `SO4 (ug/m3)`) %>% 
  select(!contains("qc_Flag"))

xact2_5 <- read_csv(file.path(OSCADATADIR,"2021", "xact2_5.csv")) %>% 
  mutate(datetime = ymd_hms(datetime)) %>% 
  #select(c(datetime,contains("Cl"))) %>%
  select(!contains("flag")) %>% 
  select(!contains("Uncert")) %>% 
  rename(date = datetime)
  #rename(date = datetime, pcl_2_5 = "PM2.5_Cl 17 (ug/m3)",
  #       pcl_2_5_unc = "PM2.5_Cl Uncert (ug/m3)") 


xact10 <- read_csv(file.path(OSCADATADIR,"2021", "xact10.csv")) %>% 
  mutate(datetime = dmy_hm(datetime)) %>% 
  #select(c(datetime,contains("Cl"))) %>% 
  select(!contains("flag")) %>% 
  select(!contains("Uncert")) %>% 
  
  rename(date = datetime)
  #rename(date = datetime, pcl_10 = "PM10_Cl 17 (ug/m3)",
  #       pcl_10_unc = "PM10_Cl Uncert (ug/m3)")

smps <- read_csv(file.path(OSCADATADIR,"2021", "smps.csv")) %>% 
  mutate(datetime = dmy_hm(datetime)) %>% 
  rename(date = datetime)


dfMaster <- tildas %>% 
  full_join(clno2) %>% 
  full_join(no2) %>% 
  full_join(nonoy) %>% 
  full_join(n2o5_cl2) %>% 
  full_join(o3) %>% 
  full_join(so2) %>% 
  full_join(voc) %>% 
  full_join(pblh) %>% 
  full_join(met) %>% 
  full_join(acsm) %>% 
  full_join(xact2_5) %>% 
  full_join(xact10) %>% 
  full_join(smps) %>% 
  arrange(date) %>% 
  select(!contains("Diff")) %>% 
  select(!contains("qc"))
  
  
  
dfMasterOut <- dfMaster %>%
  filter(between(date, ymd("2021-06-10"), ymd("2021-07-22"))) %>% 
  mutate(date = floor_date(date,"10 min")) %>% 
  group_by(date) %>% 
  summarise_all(mean,na.rm=TRUE)
  
write_csv(dfMasterOut, file.path(OSCADATADIR, "2021", "dfMaster.csv"))


  
namesForAnal <- names(dfMaster)[2:ncol(dfMaster)]

forDiurnal <- dfMaster %>% 
  filter(between(date,ymd("2021-06-11"),ymd("2021-06-18")))


dfDiurnal<- timeVariation(forDiurnal,
                             pollutant = namesForAnal,plot = FALSE)$data$hour %>% 
  mutate(Upper = Upper - Mean) %>% 
  mutate(Lower = Mean - Lower) 
  
  
dfDiurnalOut <- dfDiurnal %>%  
  group_by(variable) %>% 
  select(!c("default","ci")) %>% 
  arrange(variable) %>%
  pivot_wider(names_from=variable,values_from = c(Mean, Lower, Upper))

  
write_csv(dfDiurnalOut, file.path(OSCADATADIR, "2021", "dfMasterDiurnal.csv"))


justMeans <- dfDiurnalOut %>% 
  select(hour,contains("Mean"))
names(justMeans) <- gsub("Mean_","",names(justMeans))

write_csv(justMeans, file.path(OSCADATADIR, "2021", "dfMasterDiurnal_justmeans.csv"))

  


#maqs <- read_csv(file.path(OSCADATADIR,"2021", "maqs.csv"))
  
  

# test <- met %>% 
#   filter(between(hour(date), 8, 20))
# 292 K

# Diurnals ----------------------------------------------------------------

hclDiurnal <- timeVariation(tildas,pollutant = "hcl")$data$hour %>%
  mutate(Upper = Upper - Mean) %>% 
  mutate(Lower = Mean - Lower) %>% 
  write.csv(file.path(OSCADATADIR,"2021","hcl-diurnal.csv"),
            quote = FALSE,row.names = FALSE)




clno2Diurnal<- timeVariation(clno2,pollutant = "ClNO2_ppt")$data$hour %>%
  mutate(Upper = Upper - Mean) %>% 
  mutate(Lower = Mean - Lower) %>% 
  write.csv(file.path(OSCADATADIR,"2021","clno2-diurnal.csv"),
            quote = FALSE,row.names = FALSE)



plot(hclDiurnal$mean,clno2$diurnal)

timeVariation(cims,pollutant = "Cl2_cps")$data$hour %>%
  mutate(Upper = Upper - Mean) %>% 
  mutate(Lower = Mean - Lower) %>% 
  write.csv(file.path(OSCADATADIR,"2021","cl2-diurnal.csv"),
            quote = FALSE,row.names = FALSE)



timeVariation(cims,pollutant = "N2O5_cps")$data$hour %>%
  mutate(Upper = Upper - Mean) %>% 
  mutate(Lower = Mean - Lower) %>% 
  write.csv(file.path(OSCADATADIR,"2021","n2o5-diurnal.csv"),
            quote = FALSE,row.names = FALSE)



timeVariation(specrad,pollutant = "JNO2 (s-1)")$data$hour %>%
  mutate(Upper = Upper - Mean) %>% 
  mutate(Lower = Mean - Lower) %>% 
  write.csv(file.path(OSCADATADIR,"2021","jno2-diurnal.csv"),
            quote = FALSE,row.names = FALSE)



# Cl-Budget ---------------------------------------------------------------
# Question of whether to interpolate aerosol or round everything to 1 hr.  
# I think probably best to start with hourly averages... and go from there.
# I think that's all ISORROPIA can handle anyway.

timeAvg <- function(x, TIME){
  varName <- as.character(deparse(substitute(x)))
  
  xAvg <- x %>% 
    mutate(date = floor_date(x$date, TIME)) %>% 
    group_by(date) %>% 
    add_tally(name = paste("n_",varName,sep="")) %>%
    summarise_all(mean, na.rm=TRUE)
  return(xAvg)
}



tildasAvg <- timeAvg(tildas, "1 hour")
clno2Avg <- timeAvg(clno2, "1 hour")  
n2o5_cl2Avg <- timeAvg(n2o5_cl2, "1 hour")
acsmAvg <- timeAvg(acsm, "1 hour")
xact2_5Avg <- timeAvg(xact2_5, "1 hour")
xact10Avg <- timeAvg(xact10, "1 hour")
metAvg <- timeAvg(met, "1 hour")
vocAvg <- timeAvg(voc, "1 hour")
specAvg <- timeAvg(specrad,"1 hour")

dfMaster<- tildasAvg %>% 
  left_join(clno2Avg) %>% 
  left_join(n2o5_cl2Avg) %>% 
  left_join(acsmAvg) %>% 
  left_join(xact2_5Avg) %>% 
  mutate(pcl_2_5 = na.approx(pcl_2_5)) %>% 
  replace_na(list(pcl_2_5_unc = 0)) %>% 
  left_join(xact10Avg) %>% 
  mutate(pcl_10 = na.approx(pcl_10,na.rm=FALSE)) %>% 
  replace_na(list(pcl_10_unc = 0)) %>% 
  select(!contains("n_")) %>% 
  na.omit()%>% 
  left_join(metAvg) %>% 
  mutate(nDensAir = press*100 * 6.022e23/8.314/tempK) %>% 
  mutate(hcl_ug_m3 = hcl*10^-12 * nDensAir /6.02e23*(35.45+1.008)*10^6) %>% 
  mutate(cl_hcl_ug_m3 = hcl*10^-12 * nDensAir /6.02e23*(35.45)*10^6) %>% 
  mutate(clno2_ug_m3 = ClNO2_ppt*10^-12 * nDensAir /6.02e23*(35.45+14.01+16*2)*10^6) %>%
  mutate(cl_clno2_ug_m3 = ClNO2_ppt*10^-12 * nDensAir /6.02e23*(35.45)*10^6) %>% 
  mutate(cl2_ug_m3 = Cl2_ppt*10^-12 * nDensAir /6.02e23*(35.45*2)*10^6) %>% 
  mutate(cl_cl2_ug_m3 = Cl2_ppt*10^-12 *2* nDensAir /6.02e23*(35.45)*10^6) %>% 
  mutate(cl_sum = rowSums(select(., cl_hcl_ug_m3, cl_cl2_ug_m3,cl_clno2_ug_m3,
                                 pcl_1, pcl_2_5,pcl_10))) %>% 
  mutate(hcl_pct = cl_hcl_ug_m3 / cl_sum *100) %>% 
  mutate(cl2_pct = cl_cl2_ug_m3 / cl_sum *100) %>% 
  mutate(clno2_pct = cl_clno2_ug_m3 / cl_sum *100) %>% 
  mutate(pcl1_pct = pcl_1 / cl_sum *100) %>% 
  mutate(pcl2.5_pct = pcl_2_5 / cl_sum *100) %>% 
  mutate(pcl10_pct = pcl_10 / cl_sum *100) %>% 
  add_row(date = ymd_hms("2021-06-18 18:00:00")) %>% 
  add_row(date = ymd_hms("2021-06-23 09:00:00")) %>% 
  add_row(date = ymd_hms("2021-06-29 14:00:00")) %>% 
  arrange(date)
  
write.csv(dfMaster, "G:/My Drive/Experiments/OSCA/data/2021/dfMaster-1hr-completecases.csv",
          quote = FALSE, row.names= FALSE)

test <- dfMaster %>% 
  select(!c(date, pcl_2_5_unc, pcl_10_unc, tempC, n_met, nDensAir,cl_sum)) %>% 
  select(!contains("_pct"))
pairs(test)


dfMasterFullJoin<- tildasAvg %>% 
  full_join(clno2Avg) %>% 
  full_join(n2o5_cl2Avg) %>% 
  full_join(acsmAvg) %>% 
  full_join(xact2_5Avg) %>% 
  full_join(xact10Avg) %>% 
  left_join(metAvg) %>% 
  mutate(tempK = na.approx(tempK,na.rm = FALSE)) %>% 
  mutate(press = na.approx(press, na.rm = FALSE)) %>% 
  mutate(nDensAir = press*100 * 6.022e23/8.314/tempK) %>% 
  mutate(hcl_ug_m3 = hcl*10^-12 * nDensAir /6.02e23*(35.45+1.008)*10^6) %>% 
  mutate(cl_hcl_ug_m3 = hcl*10^-12 * nDensAir /6.02e23*(35.45)*10^6) %>% 
  mutate(clno2_ug_m3 = ClNO2_ppt*10^-12 * nDensAir /6.02e23*(35.45+14.01+16*2)*10^6) %>%
  mutate(cl_clno2_ug_m3 = ClNO2_ppt*10^-12 * nDensAir /6.02e23*(35.45)*10^6) %>% 
  mutate(cl2_ug_m3 = Cl2_ppt*10^-12 * nDensAir /6.02e23*(35.45*2)*10^6) %>% 
  mutate(cl_cl2_ug_m3 = Cl2_ppt*10^-12 *2* nDensAir /6.02e23*(35.45)*10^6)

write.csv(dfMasterFullJoin, "G:/My Drive/Experiments/OSCA/data/2021/dfMaster-1hr-fulljoin.csv",
          quote = FALSE, row.names= FALSE)



# ClNO2 and HCl production ------------------------------------------------
# From the overall campaign diurnals, it looks as though 
# the HCl formation rate might be extremely similar to the
# ClNO2 loss rate.  So There's a question of whether we can 
# tease out the ClNo2 influence from HCl's thermodynamic
# equilibrium 




tildasAvg <- timeAvg(tildas, "10 min")
clno2Avg <- timeAvg(clno2, "10 min") 

TIME1 = ymd("2021-06-15")
TIME2 = ymd("2021-06-16")


dfAnal <- tildasAvg %>% 
  left_join(clno2Avg) %>% 
  filter(between(date,TIME1,TIME2)) %>% 
  filter(between(hour(date) + minute(date)/60, 7, 9))
  

dclno2 <- diff(dfAnal$ClNO2_ppt)
dhcl<- diff(dfAnal$hcl)

plot(dhcl,dclno2,pch=19)


P1 <- dfAnal %>% 
  ggplot(aes(x=date,y=hcl)) +
  geom_point()+
  scale_y_continuous(name = "HCl")


P2 <- dfAnal %>% 
  ggplot(aes(x=date,y=ClNO2_ppt)) +
  geom_point()+
  scale_y_continuous(name = "ClNO2")
grid.arrange(P1,P2)


