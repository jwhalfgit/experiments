library(xts)
library(dplyr)
library(openair)
library(lubridate)
library(ggplot2)
library(scales)
library(tidyverse)
library(ssh)
library(RColorBrewer)

myPalette <- brewer.pal(8,"Set2")

OSCADATADIR <- file.path(EXPTDIR,"osca","data")

setwd(OSCADATADIR)

YEAR = 2022
AVGTIME <- "60 min"


# WINTER
marga <- read_csv(file.path(OSCADATADIR, "2022","marga.csv")) %>% 
  select(!contains("Status")) %>% 
  rename(pm10ca = `calcium in PM10`, pm10cl = `chloride in PM10`,
         pm10mg = `magnesium in PM10`, pm10na = `sodium in PM10`,
         pm10k = `potassium in PM10`, pm10nh4 = `ammonium in PM10`,
         pm10no3 = `nitrate in PM10`, pm10so4 = `sulphate in PM10`,
         pm25ca = `calcium in PM2.5`, pm25cl = `chloride in PM2.5`,
         pm25mg = `magnesium in PM2.5`,pm25na = `sodium in PM2.5`,
         pm25k = `potassium in PM2.5`, pm25nh4 = `ammonium in PM2.5`,
         pm25no3 = `nitrate in PM2.5`, pm25so4 = `sulphate in PM2.5`,
         hcl = `gaseous hydrochloric acid`, hno2 = `gaseous nitrous acid`,
         hno3 = `gaseous nitric acid`, nh3 = `gaseous ammonia`,
         so2 = `gaseous sulphur dioxide`,
         date = ts) %>% 
  select(!contains("pm10"))
marga[,2:ncol(marga)] <- sapply(marga[,2:ncol(marga)],as.numeric)


marga4iso <- marga %>% 
  mutate(pm25ca_umol = pm25ca / 40.078,
         pm25cl_umol = pm25cl / 35.452,
         pm25k_umol = pm25k / 39.098,
         pm25mg_umol = pm25mg / 24.305,
         pm25na_umol = pm25na / 22.99,
         pm25nh4_umol = pm25nh4 / (1.008*4+14.01),
         pm25no3_umol = pm25no3 / (16*3 + 14.01),
         pm25so4_umol = pm25so4 / (32.068 + 16*4),
         hcl_umol= hcl / 36.458,
         hno2_umol = hno2 / (1.008 + 32 + 14.01),
         hno3_umol = hno3 / (1.008 + 16*3 + 14.01),
         nh3_umol = nh3 / (14.01 + 1.008*3)) %>% 
  left_join(margaWeather) %>% 
  filter(complete.cases(.)) %>% 
  filter(between(date, TIME1, TIME2))


margaPie <- marga4iso %>% 
  select(contains("pm")) %>%
  select(contains("umol")) %>% 
  na.omit() %>% 
  summarize_all(mean) %>% 
  mutate_if(is.numeric, round, 5)

labelSpecies = c("Ca", "Cl", "K", "Mg", "Na", "NH4", "NO3", "SO4")
labelOut <- paste0(labelSpecies,"-",margaPie)


png(filename = "G:/My Drive/Experiments/OSCA/data/2022/MARGA-aerosolDist-mean-umol.png",
    width = 12.78,height = 7.68, units = "in", res = 300)

pie(unlist(margaPie,use.names= FALSE), labels = labelOut,
    col = myPalette, border = "white", 
    #main = expression("Aucencorth Moss Feb 2022\n(ug m"^-3*")"))
    main = "Aucencorth Moss Feb 2022\n(umol m-3)")
dev.off()




# Let's do bar comparisons between AM and MAQS:
# dfMasterAer <- dfMaster %>% 
#   select(pca, pcl, pk, pnh4, pno3, pso4) %>% 
#   summarize_all(median) %>% 
#   rename(pm25ca=pca, pm25cl = pcl, pm25k = pk, pm25nh4 = pnh4, 
#          pm25no3 = pno3, pm25so4 = pso4) %>% 
#   mutate(loc = "maqs")

dfMasterAer <- dfMaster %>% 
  select(pca_umol_m3, pcl_umol_m3, pk_umol_m3,
         pnh4_umol_m3, pno3_umol_m3, pso4_umol_m3) %>%
  summarize_all(median) %>% 
  rename(pm25ca_umol = pca_umol_m3, 
         pm25cl_umol = pcl_umol_m3, 
         pm25k_umol = pk_umol_m3, 
         pm25nh4_umol = pnh4_umol_m3, 
         pm25no3_umol = pno3_umol_m3, 
         pm25so4_umol = pso4_umol_m3) %>% 
  mutate(loc = "maqs")



margaPie <- margaPie %>% 
  mutate(loc = "am")

forBar <- bind_rows(dfMasterAer, margaPie) %>% 
  pivot_longer(contains("p"), names_to = "species", values_to = "conc") %>% 
  group_by(species)

ggplot(data = forBar, aes(x = species, y = conc,fill = loc))+
  geom_bar(position = "dodge", stat = "identity")+
  labs(y = expression("Concentration (umol m"^-3*")"), x = "Species",
       fill = "Location")+
  scale_x_discrete(labels=c("Ca","Cl","K","Mg","Na","NH4","NO3","SO4"))+
  scale_fill_discrete(labels = c("Auchencorth Moss", "Manchester Supersite"))+
  theme_minimal(base_size = 20)+
  theme(legend.position = "bottom")
  
ggsave("G:/My Drive/Experiments/OSCA/data/2022/aerosolDist-maqs-vs-am-umol.png",
       width = 10.24, height = 7.68, units = "in",bg="white")







YEAR = 2021
AVGTIME <- "60 min"


# SUMMER
marga <- read_csv(file.path(OSCADATADIR, YEAR,"marga.csv")) %>% 
  select(!contains("Status")) %>% 
  rename(pm10ca = `calcium in PM10`, pm10cl = `chloride in PM10`,
         pm10mg = `magnesium in PM10`, pm10na = `sodium in PM10`,
         pm10k = `potassium in PM10`, pm10nh4 = `ammonium in PM10`,
         pm10no3 = `nitrate in PM10`, pm10so4 = `sulphate in PM10`,
         pm25ca = `calcium in PM2.5`, pm25cl = `chloride in PM2.5`,
         pm25mg = `magnesium in PM2.5`,pm25na = `sodium in PM2.5`,
         pm25k = `potassium in PM2.5`, pm25nh4 = `ammonium in PM2.5`,
         pm25no3 = `nitrate in PM2.5`, pm25so4 = `sulphate in PM2.5`,
         hcl = `gaseous hydrochloric acid`, hno2 = `gaseous nitrous acid`,
         hno3 = `gaseous nitric acid`, nh3 = `gaseous ammonia`,
         so2 = `gaseous sulphur dioxide`,
         date = ts) %>% 
  select(!contains("pm10"))
marga[,2:ncol(marga)] <- sapply(marga[,2:ncol(marga)],as.numeric)

margaWeather <- read_delim("G:/My Drive/Experiments/OSCA/data/2021/marga-weather.csv",
                           skip = 152,delim=",") %>% 
  select(c(`1`,`6`,`8`)) %>% 
  rename(date = `1`, temp = `6`, rh = `8`) %>% 
  mutate(date = dmy_hm(date), temp = as.numeric(temp),
         temp = temp + 273.15) %>% 
  mutate(date = floor_date(date, "hours")) %>% 
  group_by(date) %>% 
  summarize_all(mean)



marga4iso <- marga %>% 
  mutate(pm25ca_umol = pm25ca / 40.078,
         pm25cl_umol = pm25cl / 35.452,
         pm25k_umol = pm25k / 39.098,
         pm25mg_umol = pm25mg / 24.305,
         pm25na_umol = pm25na / 22.99,
         pm25nh4_umol = pm25nh4 / (1.008*4+14.01),
         pm25no3_umol = pm25no3 / (16*3 + 14.01),
         pm25so4_umol = pm25so4 / (32.068 + 16*4),
         hcl_umol= hcl / 36.458,
         hno2_umol = hno2 / (1.008 + 32 + 14.01),
         hno3_umol = hno3 / (1.008 + 16*3 + 14.01),
         nh3_umol = nh3 / (14.01 + 1.008*3)) %>% 
  left_join(margaWeather) %>% 
  filter(complete.cases(.)) %>% 
  filter(between(date, TIME1, TIME2))


margaPie <- marga4iso %>% 
  select(contains("pm")) %>%
  select(contains("umol")) %>% 
  na.omit() %>% 
  summarize_all(mean) %>% 
  mutate_if(is.numeric, round, 5)


labelSpecies = c("Ca", "Cl", "K", "Mg", "Na", "NH4", "NO3", "SO4")
labelOut <- paste0(labelSpecies,"-",margaPie)


png(filename = "G:/My Drive/Experiments/OSCA/data/2021/MARGA-aerosolDist-umol.png", width = 12.78,
    height = 7.68, units = "in", res = 300)

pie(unlist(margaPie,use.names= FALSE), labels = labelOut,
    col = myPalette, border = "white", 
    #main = expression("Aucencorth Moss Feb 2022\n(ug m"^-3*")"))
    main = "Aucencorth Moss June-July 2021\n(umol m-3)")
dev.off()






# Let's do bar comparisons between AM and MAQS:
dfMasterAer <- dfMaster %>% 
  select(pca_umol_m3, pcl_umol_m3, pk_umol_m3,
         pnh4_umol_m3, pno3_umol_m3, pso4_umol_m3) %>%
    summarize_all(median) %>% 
  rename(pm25ca_umol=pca_umol_m3, 
         pm25cl_umol = pcl_umol_m3, 
         pm25k_umol = pk_umol_m3, 
         pm25nh4_umol = pnh4_umol_m3, 
         pm25no3_umol = pno3_umol_m3, 
         pm25so4_umol = pso4_umol_m3) %>% 
  mutate(loc = "maqs")


margaPie <- margaPie %>% 
  mutate(loc = "am")

forBar <- bind_rows(dfMasterAer, margaPie) %>% 
  pivot_longer(contains("p"), names_to = "species", values_to = "conc") %>% 
  group_by(species)


ggplot(data = forBar, aes(x = species, y = conc,fill = loc))+
  geom_bar(position = "dodge", stat = "identity")+
  labs(y = expression("Concentration (umol m"^-3*")"), x = "Species",
       fill = "Location")+
  scale_x_discrete(labels=c("Ca","Cl","K","Mg","Na","NH4","NO3","SO4"))+
  scale_fill_discrete(labels = c("Auchencorth Moss", "Manchester Supersite"))+
  theme_minimal(base_size = 20)+
  theme(legend.position = "bottom")


ggsave("G:/My Drive/Experiments/OSCA/data/2021/aerosolDist-maqs-vs-am-umol.png",
       width = 10.24, height = 7.68, units = "in", bg="white")

