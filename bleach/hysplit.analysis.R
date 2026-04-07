# load hysplits
library(openair)

CODEDIR <- "C:/Users/jh2949/OneDrive - University of York/Documents/work/Code/R/experiments/hysplit"
source(file.path(CODEDIR,"hysplit.load.R"))

TRAJDIR <- "G:/My Drive/Experiments/BLEACH/hysplit/" # Overwrite TRAJDIR from DEFRA analysis

bleach_traj <- trajectory_read_alt_Par(TRAJDIR,2022) %>% 
   rename(hour.inc = hour_along,
         date2 = traj_dt,
         date = traj_dt_i) %>%
   mutate(siteid = receptor)



# ensure the cl2 data are loaded from data.load.R
cl2Avg <- cl2Filter %>% 
  mutate(date = floor_date(date, "3 hours")) %>% 
  group_by(date) %>% 
  summarize_all(mean,na.rm = TRUE)


bleachTrajDF <- bleach_traj %>% 
  group_by(date) %>% 
  left_join(cl2Avg)


library(openairmaps)

sbqta_ncl3 <- trajLevelMap(bleachTrajDF,
                        pollutant = "ncl3ppt",
                        statistic = "sqtba",
                        lat.inc = 0.25,
                        lon.inc = 0.25)



sbqta_cl2 <- trajLevelMap(bleachTrajDF,
                           pollutant = "cl2ppt",
                           statistic = "sqtba",
                           lat.inc = 0.25,
                           lon.inc = 0.25)



sbqta_nox <- trajLevelMap(bleachTrajDF,
                          pollutant = "NOx_pptv",
                          statistic = "sqtba",
                          lat.inc = 0.25,
                          lon.inc = 0.25)


sbqta_pops <- trajLevelMap(bleachTrajDF,
                          pollutant = "POPS_SurfaceArea",
                          statistic = "sqtba",
                          lat.inc = 0.25,
                          lon.inc = 0.25)

