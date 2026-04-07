library(tidyverse)
library(openair)
library(zoo)


BLEACHDATADIR <- "G:/My Drive/Experiments/BLEACH/data"

YEAR = 2022

ozone <- read_csv(file.path(BLEACHDATADIR,YEAR, "ozone.csv")) %>% 
  rename(date = time)
  
met <- read_csv(file.path(BLEACHDATADIR,YEAR, "met.dat")) %>% 
  rename(date = TIMESTAMP) %>% 
  select(!contains("PumpStatus")) %>% 
  mutate(date = floor_date(date,"1 minute")) %>% 
  group_by(date) %>% 
  summarize_all(mean) # appears to already be local time



nox <- read_csv(file.path(BLEACHDATADIR,YEAR, "nox.csv")) %>% 
  rename(date = time_utc) %>% 
  mutate(date = floor_date(date,"1 minute")) %>% 
  mutate(date = date - 3600*3) # convert to local time (UTC-3)




pops <- read_csv(file.path(BLEACHDATADIR,YEAR, "pops.csv")) %>% 
  filter(between(ts, ymd("2022-06-07"), ymd("2022-06-30"))) %>% 
  rename(date = ts)
  

  
asol <- read_csv(file.path(BLEACHDATADIR,YEAR, "aerosol.csv")) %>% 
  rename(pcl = `Chloride (mg/m3)`, pno3 = `Nitrate (mg/m3)`,
         date = `Date_Bulk`)



# 
# br2 <- read_csv(file.path(BLEACHDATADIR,YEAR, "br2.csv")) %>% 
#   rename(date= date) %>% 
#   mutate(date = dmy_hm(ts))

cl2 <- read_csv(file.path(BLEACHDATADIR,YEAR, "cl2.csv")) %>% 
  filter(status == "sample") %>% 
  mutate(date = ymd_hms(date)) %>% 
  #left_join(pops) %>% 
  #mutate(POPS_N_particles = na.approx(POPS_N_particles, rule = 2)) %>%
  #mutate(POPS_SurfaceArea = na.approx(POPS_SurfaceArea, rule = 2)) %>%
  #select(!POPS_Temp) %>%
  #left_join(nox) %>%
  mutate(ncl3ppt = ncl3ncps / 3.6) %>% 
  mutate(cl2ppt = ifelse(cl2ppt > 1.5  & `isotope pass` != "no", NA, cl2ppt)) %>%
  select(!c(contains("pass"),"status")) %>% 
  mutate(date = floor_date(date,"1 minute")) %>% 
  group_by(date) %>% 
  summarize_all(mean) #%>% 
  #filter(date > ymd("2022-06-14"))







dfAnalClean <- met %>% 
  left_join(nox) %>% 
  left_join(cl2) %>% 
   filter(ifelse(is.na(ncl3ppt),
                 (ResultantMean_WD>150 & ResultantMean_WD <340) & NOx_pptv<= 25,
                 #(ncl3ppt<= 0.2 & NO_pptv<= 4.87))) %>%
                 (ncl3ppt<= 0.5 & NO_pptv<= 4.87))) %>%
  #filter(ResultantMean_WD>150 & ResultantMean_WD <340) %>% 
  filter(between(date, ymd("2022-06-01"),ymd("2022-07-01"))) #%>% 
  #mutate(DAY = floor_date(date, "1 day")) %>% 
  #group_by(DAY) %>% 
  #summarize(ct = n()/1440)



dfAnalDirty <- met %>% 
  left_join(nox) %>% 
  left_join(cl2) %>% 
  # filter(ifelse(is.na(ncl3ppt),
  #               (ResultantMean_WD>150 & ResultantMean_WD <340) & NO_pptv<= 4.87,
  #               (ncl3ppt<= 0.2 & NO_pptv<= 4.87))) %>%
  filter(!(ResultantMean_WD>150 & ResultantMean_WD <340)) %>% 
  #filter(ResultantMean_WD > 150 & ResultantMean_WD < 340) %>% 
  filter(between(date, ymd("2022-06-14"),ymd("2022-07-01")))




cl2CleanWind <- ggplot(data = dfAnalClean, aes(x= cl2ppt))+
  geom_histogram(binwidth = 0.25, fill = "gray", color = "black")+
  ylim(c(0,200))+
  xlim(c(0,3))+
  #scale_x_datetime(breaks=breaks_width("1 day"))+
  labs(x = expression(Cl[2]*" (ppt)"), y = "Count")+
  theme_minimal()+
  theme(axis.title = element_text(size = 20))

cl2DirtyWind <- ggplot(data = dfAnalDirty, aes(x = cl2ppt))+
  geom_histogram(binwidth = 0.25, fill = "gray", color = "black")+
  ylim(c(0,200))+
  xlim(c(0,3))+
  
  #scale_x_datetime(breaks=breaks_width("1 day"))+
  labs(x = expression(Cl[2]*" (ppt)"), y = "Count")+
  theme_minimal()+
  theme(axis.title = element_text(size = 20))



cl2DistOut <- ggarrange(cl2CleanWind, cl2DirtyWind)
ggsave("G:/My Drive/Experiments/BLEACH/cl2analysis/cl2distros.png",
       width = 12.80,
       height = 7.68,
       units = "in")






noxCleanWind <- dfAnalClean %>% 
  filter(NOx_pptv < 500) %>% 
  ggplot(aes(x= NOx_pptv))+
  geom_histogram(binwidth = 1, fill = "gray", color = "black")+
  ylim(c(0,200))+
  #scale_x_datetime(breaks=breaks_width("1 day"))+
  labs(x = expression(NO[x]*" (ppt)"), y = "Count")+
  theme_minimal()+
  theme(axis.title = element_text(size = 20))

noxDirtyWind <-  dfAnalDirty %>% 
  filter(NOx_pptv < 500) %>% 
  ggplot(aes(x =NOx_pptv))+
  geom_histogram(binwidth = 1, fill = "gray", color = "black")+
  ylim(c(0,200))+
  #scale_x_datetime(breaks=breaks_width("1 day"))+
  labs(x = expression(NO[x]*" (ppt)"), y = "Count")+
  theme_minimal()+
  theme(axis.title = element_text(size = 20))



noxDistOut <- ggarrange(noxCleanWind, noxDirtyWind)
ggsave("G:/My Drive/Experiments/BLEACH/cl2analysis/noxdistros.png",
       width = 12.80,
       height = 7.68,
       units = "in")



ncl3CleanWind <- dfAnalClean %>% 
  #filter(ncl3ppt < 500) %>% 
  ggplot(aes(x= ncl3ppt))+
  geom_histogram(binwidth = 0.25, fill = "gray", color = "black")+
  ylim(c(0,350))+
  xlim(c(0,10))+
  #scale_x_datetime(breaks=breaks_width("1 day"))+
  labs(x = expression(NCl[3]*" (ppt*)"), y = "Count")+
  theme_minimal()+
  theme(axis.title = element_text(size = 20))

ncl3DirtyWind <-  dfAnalDirty %>% 
  #filter(ncl3_pptv < 500) %>% 
  ggplot(aes(x =ncl3ppt))+
  geom_histogram(binwidth = 0.25, fill = "gray", color = "black")+
  ylim(c(0,350))+
  xlim(c(0,10))+
  #scale_x_datetime(breaks=breaks_width("1 day"))+
  labs(x = expression(NCl[3]*" (ppt*)"), y = "Count")+
  theme_minimal()+
  theme(axis.title = element_text(size = 20))



ncl3DistOut <- ggarrange(ncl3CleanWind, ncl3DirtyWind)
ggsave("G:/My Drive/Experiments/BLEACH/cl2analysis/ncl3distros.png",
       width = 12.80,
       height = 7.68,
       units = "in")



library(ggpubr)



ggplot(data = cl2, aes(x = date, y = cl2ppt))+
  #geom_point()+
  geom_line()+
  theme_minimal()


pphr <- cl2 %>%  # 
  mutate(date = hour(date)) %>% 
  group_by(date) %>% 
  summarize(ct = n())

barplot(height = pphr$ct, names.arg = pphr$date,xlab = "Hour of Day",
        ylab = "No. of Observations")



cl2Filter <- cl2 %>% 
  left_join(met) %>% 
  left_join(nox) %>% 
  filter(ifelse(is.na(ncl3ppt),
                (ResultantMean_WD>150 & ResultantMean_WD <340) & NOx_pptv<= 100,
                #(ncl3ppt<= 0.2 & NO_pptv<= 4.87))) %>%
                (ncl3ppt<= 2.5 & NOx_pptv<= 100))) %>%
  #filter(ResultantMean_WD>150 & ResultantMean_WD <340) %>% 
  filter(between(date, ymd("2022-06-01"),ymd("2022-07-01"))) %>%  #%>% 
#mutate(DAY = floor_date(date, "1 day")) %>% 
#group_by(DAY) %>% 
#summarize(ct = n()/1440)

  # filter(ifelse(is.na(NO_pptv),
  #               (ResultantMean_WD >150 & ResultantMean_WD <340) & ncl3ppt<= 0.2,
  #               (ncl3ppt<= 0.2 & NO_pptv<= 4.87))) %>%

  # #mutate(date = floor_date(date, "1 day")) %>%
  group_by(date) %>%
  summarize_all(mean, na.rm = TRUE) #%>%
  #full_join(asol) %>% 
  #filter(date > ymd("2022-06-14"))




  #filter(NO_pptv<= 1.6) %>%
  #na.omit() #%>%  
  
library(scales)

ggplot()+
  geom_line(data = cl2, aes(x = date, y = cl2ppt), color = "grey")+
  geom_line(data = cl2Filter, aes(x = date, y = cl2ppt), color = "black")+
  scale_x_datetime(breaks=breaks_width("1 day"))+
  theme_minimal()
ggsave("G:/My Drive/Experiments/BLEACH/cl2analysis/cl2-nox100-ncl3-2-5-filter.png", 
       width = 12.80, height = 7.68, units = "in")


png("G:/My Drive/Experiments/BLEACH/cl2analysis/cl2diurnal-nox100-ncl3-filter-2-5.png",
    width = 12.80, height = 7.68, units = "in", res = 150)
ggx <- timeVariation(cl2Filter, pollutant = "cl2ppt",plot = FALSE)
ggx$plot$hour
dev.off()
#timeVariation(nox, pollutant = "NO2_ppt_filt")
#timeVariation(met,pollutant = "AirTemp_Avg")

pphr <- cl2Filter %>%  # 
  mutate(date = hour(date)) %>% 
  mutate() %>% 
  group_by(date) %>% 
  summarize(ct = n())

barplot(height = pphr$ct, names.arg = pphr$date,xlab = "Hour of Day",
        ylab = "No. of Observations")



library(ggpmisc)

p1 <- ggplot(data = cl2Filter)+
  geom_point( aes(x = date, y = cl2ppt))+
  theme_minimal()

p2 <- ggplot(data = cl2Filter)+
  geom_point( aes(x = date, y = pcl))+
  theme_minimal()

p3 <- ggplot(data = cl2Filter)+
  geom_point( aes(x = date, y = pno3))+
  theme_minimal()

plotgridOut <- ggpmisc::plot_grid(p1,p2,p3, nrow = 3)

plotgridOut
# 
# 
# clamines <-  read_csv(file.path(BLEACHDATADIR,YEAR, "cl2.csv")) %>% 
#   filter(status == "sample") %>% 
#   mutate(date = ymd_hms(date)) %>% 
#   mutate(ncl3ppt = ncl3ncps / 3.6) %>% 
#   mutate(date = floor_date(date,"1 minute")) %>% 
#   group_by(date) %>% 
#   summarize_all(mean)
#   

