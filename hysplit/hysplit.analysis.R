library(splitr)
library(openair)
library(gridExtra)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggallin)
library(parallel)
library(foreach)

library(foreach)
library(doParallel)
registerDoParallel(cores= 4)


CODEDIR <- "C:/Users/jh2949/OneDrive - University of York/Documents/work/Code/R/experiments/hysplit"
source(file.path(CODEDIR,"hysplit.load.R"))
source(file.path(CODEDIR,"hysplit.analysis.funs.R"))



sites <- as.data.frame(matrix(data = c(53.44,-2.21,"Manchester Air Quality Site",
                                       52.45, -1.93, "Birmingham Super Site",
                                       54.98, -1.61, "Newcastle Center",
                                       55.95, -3.20, "Edinburgh Center",
                                       54.60, -5.93, "Belfast Center",
                                       51.48, -3.18, "Cardiff Center",
                                       51.45, -0.04,  "London Super Site",
                                       51.08, -4.10, "Barnstaple Center",
                                       55.79, -3.24, "Auchencorth Moss",
                                       51.15, -1.44, "Chilbolton Observatory"),
                              ncol=3,byrow=TRUE,dimnames = list(c(),c("lat","lon","loc"))))

sites$lat <- as.numeric(sites$lat)
sites$lon <- as.numeric(sites$lon)


# Load the trajDF.Rds workspace saved from hysplit.load.R
load("G:/My Drive/Experiments/DEFRA/hysplit/trajDF_oa.Rds")

# Generate individual site data.frames
maqs <- trajDF_oa %>% filter(receptor == 1) 
bss <- trajDF_oa %>% filter(receptor == 2)
nc <- trajDF_oa %>% filter(receptor == 3)
ec <- trajDF_oa %>% filter(receptor == 4)
bc <- trajDF_oa %>% filter(receptor == 5)
cc <- trajDF_oa %>% filter(receptor == 6)
lc <- trajDF_oa %>% filter(receptor == 7)
barnc <- trajDF_oa %>% filter(receptor == 8)
am <- trajDF_oa %>% filter(receptor == 9)
chilb <- trajDF_oa %>% filter(receptor == 10)



################################################################################
# FREQUENCY PLOTS
################################################################################


DATE1 <- "01/03/2024"
DATE2 <- "31/05/2024"

maqsFreq <- plotTrajBin(DF = maqs,DATE1 = DATE1, DATE2 = DATE2)
bssFreq <- plotTrajBin(DF = bss,DATE1 = DATE1, DATE2 = DATE2)
lcFreq <- plotTrajBin(DF = lc,DATE1 = DATE1, DATE2 = DATE2)
barncFreq <- plotTrajBin(DF = barnc,DATE1 = DATE1, DATE2 = DATE2)
ccFreq <- plotTrajBin(DF = cc,DATE1 = DATE1, DATE2 = DATE2)
bcFreq <- plotTrajBin(DF = bc,DATE1 = DATE1, DATE2 = DATE2)
ecFreq <- plotTrajBin(DF = ec,DATE1 = DATE1, DATE2 = DATE2)
ncFreq <- plotTrajBin(DF = nc,DATE1 = DATE1, DATE2 = DATE2)
amFreq <- plotTrajBin(DF = am,DATE1 = DATE1, DATE2 = DATE2)
chilbFreq <- plotTrajBin(DF = chilb,DATE1 = DATE1, DATE2 = DATE2)

# Difference Plots --------------------------------------------------------

# Look at seasonal differences 


seasDates <- data.frame(START = c("01/01/2021","01/04/2021","01/07/2021", "01/10/2021"),
                        STOP = c("31/03/2021","30/06/2021","30/09/2021", "31/12/2021"))
 
# Seasonal dates
maqsBss <- list()
for(ix in 1:nrow(seasDates)){
  maqsBss[[ix]] <- calcDiff(SITE1 = maqs, SITE2 = bss,
                            DATE1 = seasDates[ix,1],DATE2 = seasDates[ix,2], 
                            PLOT = TRUE)
}


ncBss <- list()
for(ix in 1:nrow(seasDates)){
  ncBss[[ix]] <- calcDiff(SITE1 = nc, SITE2 = bss,
                          DATE1 = seasDates[ix,1],DATE2 = seasDates[ix,2], 
                          PLOT = TRUE)
}



# Aggregate Difference Plots ----------------------------------------------


# Aggregated Dates
maqsBss <- calcDiff(SITE1 = maqs, SITE2 = bss,
                          DATE1 = DATE1,DATE2 = DATE2, 
                          PLOT = TRUE)



ncBss <- calcDiff(SITE1 = nc, SITE2 = bss,
                    DATE1 = DATE1,DATE2 = DATE2, 
                    PLOT = TRUE)


ecBss <- calcDiff(SITE1 = ec, SITE2 = bss,
                  DATE1 = DATE1,DATE2 = DATE2, 
                  PLOT = TRUE)


bcBss <- calcDiff(SITE1 = bc, SITE2 = bss,
                  DATE1 = DATE1,DATE2 = DATE2, 
                  PLOT = TRUE)

ccBss <- calcDiff(SITE1 = cc, SITE2 = bss,
                  DATE1 = DATE1,DATE2 = DATE2, 
                  PLOT = TRUE)

ecLc <- calcDiff(SITE1 = ec, SITE2 = lc,
                 DATE1 = DATE1,DATE2 = DATE2, 
                 PLOT = TRUE)

bssLc <-  calcDiff(SITE1 = bss, SITE2 = lc,
                   DATE1 = DATE1,DATE2 = DATE2, 
                   PLOT = TRUE)

barncLc <-  calcDiff(SITE1 = barnc, SITE2 = lc,
                   DATE1 = DATE1,DATE2 = DATE2, 
                   PLOT = TRUE)

################################################################################
# SECTOR ANALYSIS
################################################################################
# From the openair book, Sect. 10.4.2.  Divides into 8 sectors based on where
# trajectories spend their time.  

maqsSector <- sectorAnal(maqs, DATE1,DATE2)
bssSector <- sectorAnal(bss, DATE1,DATE2)
barncSector <- sectorAnal(barnc, DATE1,DATE2)
lcSector <- sectorAnal(lc, DATE1,DATE2)
ccSector <- sectorAnal(cc, DATE1,DATE2)
ecSector <- sectorAnal(ec, DATE1,DATE2)
ncSector <- sectorAnal(nc, DATE1,DATE2)
bcSector <- sectorAnal(bc, DATE1,DATE2)

sectorList <- list(maqsSector, bssSector, barncSector,
                   lcSector, ccSector, ecSector, ncSector,
                   bcSector)

capture.output(sectorList, file = "G:/My Drive/Experiments/DEFRA/hysplit/2019-sectorlist.csv")




# Cluster Analysis --------------------------------------------------------
DATE1 <- "01/03/2024"
DATE2 <- "31/05/2024"

maqsClust <- clusterPlots(SITE = maqs, DATE1, DATE2)
lcClust <- clusterPlots(SITE = lc, DATE1, DATE2)
ecClust <- clusterPlots(SITE = ec, DATE1, DATE2)



# Source Attribution Analysis Bar Plots

DATE1 <- "01/12/2021"
DATE2 <- "28/02/2022"
HOURS <- 03:72

maqsSource <- sourceBoxes(maqs, DATE1,DATE2,HOURS)
bssSource <- sourceBoxes(bss, DATE1,DATE2, HOURS)
ncSource <- sourceBoxes(nc, DATE1,DATE2, HOURS)
ecSource <- sourceBoxes(ec, DATE1,DATE2, HOURS)
bcSource <- sourceBoxes(bc, DATE1,DATE2, HOURS)
ccSource <- sourceBoxes(cc, DATE1,DATE2, HOURS)
lcSource <- sourceBoxes(lc, DATE1,DATE2, HOURS)
barncSource <- sourceBoxes(barnc, DATE1,DATE2, HOURS)

sourceOut <- t(rbind(maqsSource, bssSource,ncSource,ecSource,
                   bcSource,ccSource,lcSource,barncSource))
sourceOutDf <- data.frame(region = row.names(sourceOut),sourceOut,row.names = NULL) %>% 
  pivot_longer(cols = -region,names_to = "source", values_to = "value")



write.csv(sourceOut, paste0("G:/My Drive/Experiments/DEFRA/hysplit/sourceregions/",
                            gsub("-","",dmy(DATE1)),gsub("-","",dmy(DATE2)),".csv"),
          row.names = TRUE,quote=FALSE)

ggplot(data = sourceOutDf, aes(x = region, y = value, fill = source))+
  geom_bar(position = "dodge", stat = "identity",color = "black", linewidth = 0.5)+
  scale_fill_brewer(palette = "Set2", labels = c("Barnstaple",
                                                 "Belfast",
                                                 "Birmingham",
                                                 "Cardiff",
                                                 "Edinburgh",
                                                 "London",
                                                 "Manchester",
                                                 "Newcastle"))+
  theme_light(base_size = 16)+
  theme( legend.position = "bottom")+
  labs(title = paste0(DATE1," thru ", DATE2, ", Hours ", HOURS[1],"-",
                      HOURS[length(HOURS)]),
       x = "Region", y= "% of trajectory endpoints",fill = "Site")+
  scale_y_continuous(limits = c(0,81))

ggsave(file = paste0("G:/My Drive/Experiments/DEFRA/hysplit/sourceregions/",
                     gsub("-","",dmy(DATE1)),gsub("-","",dmy(DATE2)),"-",HOURS[1],"-",
                     HOURS[length(HOURS)],".png"), width= 12.80, height = 7.68,
       units = "in")




# Source Attribution Analysis - TIME SERIES
DATE1 <- "01/12/2023"
DATE2 <- "28/02/2024"
HOURS <- 0:72

maqsSource <- sourceBoxes(maqs, DATE1,DATE2,HOURS, OUTPUT = "TS") %>% 
  select(date,run,region,hour.inc) %>% 
  group_by(run) %>% 
  summarize((num = n()))
  mutate(region = as.factor(region))
  
ggplot(data = maqsSource, aes(fill = region, x = date, y = hour.inc*1))+
  geom_raster()+
  scale_fill_manual(values = c("#0072b2", # Marine Atlantic
                    "darksalmon", # N America
                    "peru", # Africa
                    "forestgreen",#Europe
                    "azure2", #Greenland
                    "azure4", #Iceland
                    "darkblue", #N Atlantic
                    "royalblue4", # N Sea
                    "red4", # UK
                    "springgreen4", #N Europe
                    "olivedrab4", # S Europe
                    "grey20"),
                    breaks = 1:12,
                    labels = c("Atlantic", "N America", "Africa", "Europe",
                               "Greenland","Iceland","N Atlantic","North Sea","UK", 
                               "N Europe","S Europe","uncategorized")) + #uncategorized
  labs(x = "Date (UTC)", y = "Hours Backward", fill = "Region",
       title = paste0(DATE1," thru ", DATE2, ", Hours ", HOURS[1],"-",
                      HOURS[length(HOURS)]))




