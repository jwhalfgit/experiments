library(worldmet)
library(openair)
library(tidyverse)


#import_ghcn_stations(return = "map")

# Birmingham Ladywood is the closest site to BAQS, so I'll
# pull met for it
# 
# baqs_sites <- importMeta(source = "aurn", all = TRUE) %>% 
#   dplyr::filter(grepl("Birmingham Ladywood",site))

baqs_met <- importUKAQ(site = "bmld", 
                       year = 2019:2025, 
                       source = "aurn") %>% 
  select(date, pm10, pm2.5, ws, wd, air_temp) %>% 
  mutate(temp = air_temp + 273.15)

# Manchester Sharston is closest to MAQS with overlapping data.
# Probably should use local data at some point
maqs_sites <- importMeta(source = "aurn", all=TRUE) %>%
  dplyr::filter(grepl("Manchester Town",site))


maqs_met <- importUKAQ(site = "mahg", 
                       year = 2019:2025, 
                       source = "aurn") %>% 
  select(date, pm10, pm2.5, ws, wd, air_temp) %>% 
  mutate(temp = air_temp + 273.15)



# HOP has an actual entry in AURN
# laqs_sites <- importMeta(source = "aurn") %>%
#   dplyr::filter(grepl("London",site))

laqs_met <- importUKAQ(site = "hp1", 
                       year = 2019:2025, 
                       source = "aurn") %>% 
  select(date, pm10, pm2.5, ws, wd, air_temp) %>% 
  mutate(temp = air_temp + 273.15)



# Marylebone Rd has an entry as well
mbone_met <- importUKAQ(site = "my1", 
                       year = 2019:2025, 
                       source = "aurn") %>% 
  select(date, pm10, pm2.5, ws, wd, air_temp) %>% 
  mutate(temp = air_temp + 273.15)

# Kensington
kenston_met <- importUKAQ(site = "kc1", 
                        year = 2019:2025, 
                        source = "aurn") %>% 
  select(date, pm10, pm2.5, ws, wd, air_temp) %>% 
  mutate(temp = air_temp + 273.15)





# Chilbolton
chilb_met <- importUKAQ(site = "chbo", 
                        year = 2019:2025, 
                        source = "aurn") %>% 
  select(date, pm10, pm2.5, ws, wd, air_temp) %>% 
  mutate(temp = air_temp + 273.15)



#Harwell
har_met <- importUKAQ(site = "har", 
                        year = 2015:2025, 
                        source = "aurn") %>% 
  select(date, pm10, pm2.5, ws, wd, air_temp) %>% 
  mutate(temp = air_temp + 273.15)




