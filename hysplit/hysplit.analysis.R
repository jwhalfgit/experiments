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


DATE1 <- "05/02/2021"
DATE2 <- "20/02/2021"

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
DATE1 <- "01/01/2017"
DATE2 <- "31/05/2017"
HOURS <- 0:72

barncSource <- sourceBoxes(barnc, DATE1,DATE2,HOURS, OUTPUT = "TS")
bcSource <- sourceBoxes(bc, DATE1,DATE2,HOURS, OUTPUT = "TS")
bssSource <- sourceBoxes(bss, DATE1,DATE2,HOURS, OUTPUT = "TS")
ccSource <- sourceBoxes(cc, DATE1,DATE2,HOURS, OUTPUT = "TS")
ecSource <- sourceBoxes(ec, DATE1,DATE2,HOURS, OUTPUT = "TS")
lcSource <- sourceBoxes(lc, DATE1,DATE2,HOURS, OUTPUT = "TS")
maqsSource <- sourceBoxes(maqs, DATE1,DATE2,HOURS, OUTPUT = "TS")
ncSource <- sourceBoxes(nc, DATE1,DATE2,HOURS, OUTPUT = "TS")

grid.arrange(barncSource,bcSource,bssSource,ccSource,
             ecSource,lcSource,maqsSource,ncSource)




lc17 <- sourceBoxes(lc, DATE1 ="01/01/2017" ,DATE2 ="31/05/2017",
                    HOURS, OUTPUT = "TS")
lc18 <- sourceBoxes(lc, DATE1 ="01/01/2018" ,DATE2 ="31/05/2018",
                    HOURS, OUTPUT = "TS")
lc19 <- sourceBoxes(lc, DATE1 ="01/01/2019" ,DATE2 ="31/05/2019",
                    HOURS, OUTPUT = "TS")
lc20 <- sourceBoxes(lc, DATE1 ="01/01/2020" ,DATE2 ="31/05/2020",
                    HOURS, OUTPUT = "TS")
lc21 <- sourceBoxes(lc, DATE1 ="01/01/2021" ,DATE2 ="31/05/2021",
                    HOURS, OUTPUT = "TS")
lc22 <- sourceBoxes(lc, DATE1 ="01/01/2022" ,DATE2 ="31/05/2022",
                    HOURS, OUTPUT = "TS")
lc23 <- sourceBoxes(lc, DATE1 ="01/01/2023" ,DATE2 ="31/05/2023",
                    HOURS, OUTPUT = "TS")
lc24 <- sourceBoxes(lc, DATE1 ="01/01/2024" ,DATE2 ="31/05/2024",
                    HOURS, OUTPUT = "TS")

outPlot <- arrangeGrob(lc17,lc18,lc19,lc20,lc21,lc22,lc23,lc24)
ggsave(file = paste0("G:/My Drive/Experiments/DEFRA/hysplit/sourceregions/",
                     gsub("-","",dmy(DATE1)),gsub("-","",dmy(DATE2)),"-",HOURS[1],"-",
                     HOURS[length(HOURS)],"lcSourceTimeSeries.png"),plot = outPlot, 
       width= 19.20, height = 10.8,
       units = "in")



ec17 <- sourceBoxes(ec, DATE1 ="01/01/2017" ,DATE2 ="31/05/2017",
                    HOURS, OUTPUT = "TS")
ec18 <- sourceBoxes(ec, DATE1 ="01/01/2018" ,DATE2 ="31/05/2018",
                    HOURS, OUTPUT = "TS")
ec19 <- sourceBoxes(ec, DATE1 ="01/01/2019" ,DATE2 ="31/05/2019",
                    HOURS, OUTPUT = "TS")
ec20 <- sourceBoxes(ec, DATE1 ="01/01/2020" ,DATE2 ="31/05/2020",
                    HOURS, OUTPUT = "TS")
ec21 <- sourceBoxes(ec, DATE1 ="01/01/2021" ,DATE2 ="31/05/2021",
                    HOURS, OUTPUT = "TS")
ec22 <- sourceBoxes(ec, DATE1 ="01/01/2022" ,DATE2 ="31/05/2022",
                    HOURS, OUTPUT = "TS")
ec23 <- sourceBoxes(ec, DATE1 ="01/01/2023" ,DATE2 ="31/05/2023",
                    HOURS, OUTPUT = "TS")
ec24 <- sourceBoxes(ec, DATE1 ="01/01/2024" ,DATE2 ="31/05/2024",
                    HOURS, OUTPUT = "TS")

outPlot <- arrangeGrob(ec17,ec18,ec19,ec20,ec21,ec22,ec23,ec24)
ggsave(file = paste0("G:/My Drive/Experiments/DEFRA/hysplit/sourceregions/",
                     gsub("-","",dmy(DATE1)),gsub("-","",dmy(DATE2)),"-",HOURS[1],"-",
                     HOURS[length(HOURS)],"ecSourceTimeSeries.png"), plot = outPlot,
       width= 19.20, height = 10.8,
       units = "in")





chilb17 <- sourceBoxes(chilb, DATE1 ="01/01/2017" ,DATE2 ="31/05/2017",
                    HOURS, OUTPUT = "TS")
chilb18 <- sourceBoxes(chilb, DATE1 ="01/01/2018" ,DATE2 ="31/05/2018",
                    HOURS, OUTPUT = "TS")
chilb19 <- sourceBoxes(chilb, DATE1 ="01/01/2019" ,DATE2 ="31/05/2019",
                    HOURS, OUTPUT = "TS")
chilb20 <- sourceBoxes(chilb, DATE1 ="01/01/2020" ,DATE2 ="31/05/2020",
                    HOURS, OUTPUT = "TS")
chilb21 <- sourceBoxes(chilb, DATE1 ="01/01/2021" ,DATE2 ="31/05/2021",
                    HOURS, OUTPUT = "TS")
chilb22 <- sourceBoxes(chilb, DATE1 ="01/01/2022" ,DATE2 ="31/05/2022",
                    HOURS, OUTPUT = "TS")
chilb23 <- sourceBoxes(chilb, DATE1 ="01/01/2023" ,DATE2 ="31/05/2023",
                    HOURS, OUTPUT = "TS")
chilb24 <- sourceBoxes(chilb, DATE1 ="01/01/2024" ,DATE2 ="31/05/2024",
                    HOURS, OUTPUT = "TS")

outPlot <- grid.arrange(chilb17,chilb18,chilb19,chilb20,chilb21,chilb22,chilb23,chilb24)


###########################################################################
# Equivalence Data --------------------------------------------------------
###########################################################################
barncEq <- read_csv("G:/My Drive/Experiments/DEFRA/data/barnc.csv") %>% 
  rename(ts = `Date and Time`,
         digitel = `Barnstaple Digitel`,
         fidas = `Barnstaple Fidas 215`) %>% 
  select(ts, digitel, fidas) %>% 
  mutate(ts = dmy_hm(ts),
         digitel = as.numeric(digitel),
         fidas = as.numeric(fidas)) %>% 
  drop_na() %>% 
  mutate(delta = fidas - digitel)
  
bss1Eq <-  read_csv("G:/My Drive/Experiments/DEFRA/data/bss1.csv") %>% 
  rename(ts = `Date and Time`,
         digitel = `Birmingham A4540 Digitel`,
         fidas = `Birmingham A4540 Fidas 215`) %>% 
  select(ts, digitel, fidas) %>% 
  mutate(ts = dmy_hm(ts),
         digitel = as.numeric(digitel),
         fidas = as.numeric(fidas)) %>% 
  drop_na() %>% 
  mutate(delta = fidas - digitel)


bss2Eq <-  read_csv("G:/My Drive/Experiments/DEFRA/data/bss2.csv") %>% 
  rename(ts = `Date and Time`,
         psol = `Birmingham University Partisol`,
         fidas = `Birmingham University Fidas 215`) %>% 
  select(ts, psol, fidas) %>% 
  mutate(ts = dmy_hm(ts),
         psol = as.numeric(psol),
         fidas = as.numeric(fidas)) %>% 
  drop_na() %>% 
  mutate(delta = fidas - psol)

chilbEq <- read_csv("G:/My Drive/Experiments/DEFRA/data/chilb.csv") %>% 
  rename(ts = `Date and Time`,
         digitel = `Chilbolton Digitel`,
         fidas = `Chilbolton Fidas 215`) %>% 
  select(ts, digitel, fidas) %>% 
  mutate(ts = dmy_hm(ts),
         digitel = as.numeric(digitel),
         fidas = as.numeric(fidas)) %>% 
  drop_na() %>% 
  mutate(delta = fidas - digitel)


lcEq <- read_csv("G:/My Drive/Experiments/DEFRA/data/lc.csv") %>% 
  rename(ts = `Date and Time`,
         digitel = `Marylebone Digitel`,
         fidas = `Marylebone Fidas 215`) %>% 
  select(ts, digitel, fidas) %>% 
  mutate(ts = dmy_hm(ts),
         digitel = as.numeric(digitel),
         fidas = as.numeric(fidas)) %>% 
  drop_na() %>% 
  mutate(delta = fidas - digitel)


maqsEq <- read_csv("G:/My Drive/Experiments/DEFRA/data/maqs.csv") %>% 
  rename(ts = `Date and Time`,
         psol = `Manchester University Partisol`,
         fidas = `Manchester University Fidas 215`) %>% 
  select(ts, psol, fidas) %>% 
  mutate(ts = dmy_hm(ts),
         psol = as.numeric(psol),
         fidas = as.numeric(fidas)) %>% 
  drop_na() %>% 
  mutate(delta = fidas - psol)




