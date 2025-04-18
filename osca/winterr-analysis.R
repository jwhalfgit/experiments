
# Load files ----------------------------------------------------------------

library(openair)
library(ncdf4)
library(lubridate)
OSCADATADIR <- file.path(EXPTDIR, "OSCA","data")

tildas <- read_csv(file.path(OSCADATADIR,"2022", "tildas.csv")) %>% 
  rename(date = ts) %>% 
  mutate(hcl = hcl*1000) # ocnvert to pptv for CIMS consistency

clno2 <- read_csv(file.path(OSCADATADIR,"2022", "cims60sAvg.csv")) %>% 
  rename(date = ts)%>% 
  select(!contains(c("N2O5", "Cl2")))

n2o5_cl2 <- read_csv(file.path(OSCADATADIR,"2022", "cims-n2o5-cl2.csv")) %>% 
  rename(date = ts) %>% 
  select(!contains("HCN"))


no2 <- read_csv(file.path(OSCADATADIR,"2022","no2.csv")) %>% 
  rename(date = datetime, no2 = `NO2 (ppb)`) %>% 
  select(!`NO2_qc_flags`)

nonoy <- read_csv(file.path(OSCADATADIR,"2022","nox-noy.csv")) %>% 
  rename(date = datetime, no=`NO (ppb)`, noy = `NOy (ppb)`)

o3 <- read_csv(file.path(OSCADATADIR,"2022", "o3.csv")) %>% 
  rename(date = datetime, o3 = "Ozone (ppb)") %>% 
  select(!contains("qc"))


so2 <- read_csv(file.path(OSCADATADIR,"2022", "noy_so2_yrk.csv")) %>% 
  select(date, contains("so2")) %>% 
  filter(qc_flag_so2_43i %in% c(0,1,2)) %>% 
  rename(so2 = so2_43i) %>% 
  select(date,so2)



voc <- read_csv(file.path(OSCADATADIR,"2022", "gc.csv")) %>% 
  mutate(date = dmy_hm(date)) %>% 
  select(!contains("flag")) %>% 
  select(!contains("_w")) %>% 
  filter(type == "Sample") %>% 
  select(!contains("uncertainty")) %>% 
  select(-c("...1"))



# This is the estimated BL height in m
pblh <- read_csv(file.path(OSCADATADIR,"2022","maqs.csv")) %>% 
  select(ts, "Estimated Boundary Layer Height (m)") %>% 
  rename(date = ts, pblh = "Estimated Boundary Layer Height (m)")

specrad <-read_csv(file.path(OSCADATADIR,"2022", "specrad.csv")) %>% 
  rename(date = datetime, jo1d = "JO1D (s-1)", jno2 = "JNO2 (s-1)")



acsm <- read_csv(file.path(OSCADATADIR,"2022", "acsm.csv")) %>% 
  mutate(datetime = dmy_hm(datetime)) %>% 
  rename(date = datetime, pcl_1 = `Chl (ug/m3)`, pnh4 = `NH4 (ug/m3)`,
         pno3 = `NO3 (ug/m3)`, pso4 = `SO4 (ug/m3)`)%>% 
  select(!contains("qc_Flag"))
acsm$pnh4[acsm$pnh4 < -2] = 0

xact2_5 <- read_csv(file.path(OSCADATADIR,"2022", "xact2_5.csv")) %>% 
  mutate(datetime = ymd_hms(datetime)) %>% 
  select(c(datetime,contains("Cl"))) %>% 
  select(!contains("flag")) %>% 
  rename(date = datetime, pcl_2_5 = "PM2.5_Cl 17 (ug/m3)",
         pcl_2_5_unc = "PM2.5_Cl Uncert (ug/m3)") 


xact10 <- read_csv(file.path(OSCADATADIR,"2022", "xact10.csv")) %>% 
  mutate(datetime = dmy_hm(datetime)) %>% 
  select(c(datetime,contains("Cl"))) %>% 
  select(!contains("flag")) %>% 
  rename(date = datetime, pcl_10 = "PM10_Cl 17 (ug/m3)",
         pcl_10_unc = "PM10_Cl Uncert (ug/m3)")

smps <- read_csv(file.path(OSCADATADIR,"2022", "smps.csv")) %>% 
  mutate(datetime = dmy_hm(datetime)) %>% 
  rename(date = datetime)




voc <- read_csv(file.path(OSCADATADIR,"2022", "gc.csv")) %>% 
  mutate(date = dmy_hms(date)) %>% 
  select(!contains("flag")) %>% 
  select(!contains("_w")) %>% 
  filter(type == "Sample") %>% 
  select(!contains("uncertainty")) %>% 
  select(-c("...1"))


met <- read_csv(file.path(OSCADATADIR,"2022", "fidas-met.csv")) %>% 
  mutate(datetime = dmy_hm(datetime)) %>% 
  select(!contains("flag"))
names(met) = c("date","tempC","press","rh","tempK")



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
  full_join(specrad) %>% 
  full_join(smps) %>% 
  arrange(date) %>% 
  select(!contains("Diff")) %>% 
  select(!contains("qc"))

dfMasterOut <- dfMaster %>%
  filter(between(date, ymd("2022-02-01"), ymd("2022-02-28"))) %>% 
  mutate(date = floor_date(date,"10 min")) %>% 
  group_by(date) %>% 
  summarise_all(mean,na.rm=TRUE)



write_csv(dfMasterOut, file.path(OSCADATADIR, "2022", "dfMaster-10min.csv"))









# Specrad -----------------------------------------------------------------

specradFeb <- nc_open(file.path(OSCADATADIR,"2022","specrad-feb.nc"))
ncVar <- names(specradFeb$var)

for(i in 1:length(ncVar)) {
  assign(ncVar[i], ncvar_get(specradFeb, ncVar[i]))
}

sTime <- as.POSIXct(paste(year, day_of_year, hour, minute, second, sep = " "),
                   format = "%Y %j %H %M %S",tz = "UTC")



# met %>%
#   filter(between(hour(date), 8, 20))
# 
# median(met$tempK)
# 276 K

# Diurnals ----------------------------------------------------------------


timeVariation(tildas,pollutant = "hcl")$data$hour %>%
  mutate(Upper = Upper - Mean) %>% 
  mutate(Lower = Mean - Lower) %>% 
  write.csv(file.path(OSCADATADIR,"2022","diurnals","hcl-diurnal.csv"),
            quote = FALSE,row.names = FALSE)


timeVariation(cims,pollutant = "ClNO2_ppt")$data$hour %>%
  mutate(Upper = Upper - Mean) %>% 
  mutate(Lower = Mean - Lower) %>% 
  write.csv(file.path(OSCADATADIR,"2022","diurnals","clno2-diurnal.csv"),
            quote = FALSE,row.names = FALSE)


timeVariation(cims,pollutant = "Cl2_cps")$data$hour %>%
  mutate(Upper = Upper - Mean) %>% 
  mutate(Lower = Mean - Lower) %>% 
  write.csv(file.path(OSCADATADIR,"2022","diurnals","cl2-diurnal.csv"),
            quote = FALSE,row.names = FALSE)



timeVariation(cims,pollutant = "N2O5_cps")$data$hour %>%
  mutate(Upper = Upper - Mean) %>% 
  mutate(Lower = Mean - Lower) %>% 
  write.csv(file.path(OSCADATADIR,"2022","diurnals","n2o5-diurnal.csv"),
            quote = FALSE,row.names = FALSE)



timeVariation(specrad,pollutant = "JNO2 (s-1)")$data$hour %>%
  mutate(Upper = Upper - Mean) %>% 
  mutate(Lower = Mean - Lower) %>% 
  write.csv(file.path(OSCADATADIR,"2022","diurnals","jno2-diurnal.csv"),
            quote = FALSE,row.names = FALSE)



# Cl-Budget ---------------------------------------------------------------
# Question of whether to interpolate aerosol or round everything to 1 hr.  
# I think probably best to start with hourly averages... and go from there.
# I think that's all ISORROPIA can handle anyway.

hr.avg <- function(x){
  varName <- as.character(deparse(substitute(x)))
  
  xAvg <- x %>% 
    mutate(date = floor_date(x$date, "1 hour")) %>% 
    group_by(date) %>% 
    add_tally(name = paste("n_",varName,sep="")) %>%
    summarise_all(mean, na.rm=TRUE)
  return(xAvg)
}


tildasAvg <- hr.avg(tildas)
clno2Avg <- hr.avg(clno2)  
n2o5_cl2Avg <- hr.avg(n2o5_cl2)
acsmAvg <- hr.avg(acsm)
xact2_5Avg <- hr.avg(xact2_5)
xact10Avg <- hr.avg(xact10)
metAvg <- hr.avg(met)

dfMaster<- tildasAvg %>% 
  left_join(clno2Avg) %>% 
  left_join(n2o5_cl2Avg) %>% 
  left_join(acsmAvg) %>% 
  left_join(xact2_5Avg) %>% 
  mutate(pcl_2_5 = na.approx(pcl_2_5, na.rm = FALSE)) %>% 
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
  add_row(date = ymd_hms("2022-02-09 04:00:00")) %>% 
  add_row(date = ymd_hms("2022-02-11 04:00:00")) %>% 
  arrange(date)


write.csv(dfMaster, "G:/My Drive/Experiments/OSCA/data/2022/dfMaster-1hr-completecases.csv",
          quote = FALSE, row.names= FALSE)


# for regression analysis:
test <- dfMaster %>% 
  select(!c(date, pcl_2_5_unc, pcl_10_unc, tempC, n_met, nDensAir,cl_sum)) %>% 
  select(!contains("_pct"))


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
  mutate(cl_cl2_ug_m3 = Cl2_ppt*10^-12 *2* nDensAir /6.02e23*(35.45)*10^6) %>% 
  arrange(date)

write.csv(dfMasterFullJoin, "G:/My Drive/Experiments/OSCA/data/2022/dfMaster-1hr-fulljoin.csv",
          quote = FALSE, row.names= FALSE)




