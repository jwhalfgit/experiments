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


DATE1 <- "01/01/2017"
DATE2 <- "31/12/2024"

maqsFreq <- plotTrajBin(DF = maqs,DATE1 = DATE1, DATE2 = DATE2)
bssFreq <- plotTrajBin(DF = bss,DATE1 = DATE1, DATE2 = DATE2)
lcFreq <- plotTrajBin(DF = lc,DATE1 = DATE1, DATE2 = DATE2)
barncFreq <- plotTrajBin(DF = barnc,DATE1 = DATE1, DATE2 = DATE2)
ccFreq <- plotTrajBin(DF = cc,DATE1 = DATE1, DATE2 = DATE2)
bcFreq <- plotTrajBin(DF = bc,DATE1 = DATE1, DATE2 = DATE2)
ecFreq <- plotTrajBin(DF = ec,DATE1 = DATE1, DATE2 = DATE2)
ncFreq <- plotTrajBin(DF = nc,DATE1 = DATE1, DATE2 = DATE2)



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

sectorAnal(maqs)
sectorAnal(bss)
sectorAnal(barnc)
sectorAnal(lc)
sectorAnal(cc)
sectorAnal(ec)
sectorAnal(nc)
sectorAnal(bc)
# MAQS
# # A tibble: 9 × 3
# sector           n percent
# <chr>        <int>   <dbl>
#   1 E            30158   7.36 
# 2 N             3683   0.899
# 3 NE            6502   1.59 
# 4 NW           10881   2.66 
# 5 S             4693   1.15 
# 6 SE            5413   1.32 
# 7 SW           30522   7.45 
# 8 W           101586  24.8  
# 9 unallocated 216060  52.8  

# Birmingham Super Site
# A tibble: 9 × 3
# sector           n percent
# <chr>        <int>   <dbl>
#   1 E            27183   6.78 
# 2 N             3253   0.811
# 3 NE            8838   2.20 
# 4 NW           10512   2.62 
# 5 S             4909   1.22 
# 6 SE            3935   0.981
# 7 SW           28576   7.13 
# 8 W           100232  25.0  
# 9 unallocated 213589  53.3  

# Barnstaple Center
# A tibble: 9 × 3
# sector           n percent
# <chr>        <int>   <dbl>
#   1 E            23488   5.68 
# 2 N             3476   0.841
# 3 NE           11719   2.83 
# 4 NW           14735   3.56 
# 5 S             3167   0.766
# 6 SE            4096   0.991
# 7 SW           27786   6.72 
# 8 W           116881  28.3  
# 9 unallocated 208089  50.3  


# Belfast Center
# > sectorAnal(bc)
# # A tibble: 9 × 3
# sector           n percent
# <chr>        <int>   <dbl>
#   1 E            29816    7.19
# 2 N             5603    1.35
# 3 NE            6968    1.68
# 4 NW           12500    3.01
# 5 S             7617    1.84
# 6 SE            6000    1.45
# 7 SW           32656    7.87
# 8 W           103712   25.0 
# 9 unallocated 209962   50.6 

# Cardiff Center
# > sectorAnal(cc)
# # A tibble: 9 × 3
# sector           n percent
# <chr>        <int>   <dbl>
#   1 E            22830   5.52 
# 2 N             3945   0.953
# 3 NE           12795   3.09 
# 4 NW           14324   3.46 
# 5 S             3249   0.785
# 6 SE            3604   0.871
# 7 SW           29082   7.03 
# 8 W           111888  27.0  
# 9 unallocated 212230  51.3  

# Edinburgh Center
# > sectorAnal(ec)
# # A tibble: 9 × 3
# sector           n percent
# <chr>        <int>   <dbl>
#   1 E            32223   7.81 
# 2 N             3431   0.832
# 3 NE            8679   2.10 
# 4 NW            6505   1.58 
# 5 S             4818   1.17 
# 6 SE            7966   1.93 
# 7 SW           35699   8.65 
# 8 W            96768  23.5  
# 9 unallocated 216448  52.5  


# Newcastle Center
# > sectorAnal(nc)
# # A tibble: 9 × 3
# sector           n percent
# <chr>        <int>   <dbl>
#   1 E            32955    7.99
# 2 N             5325    1.29
# 3 NE            5602    1.36
# 4 NW            5948    1.44
# 5 S             4823    1.17
# 6 SE            6666    1.62
# 7 SW           32327    7.83
# 8 W           108213   26.2 
# 9 unallocated 210844   51.1 

# # London Center
# > sectorAnal(lc)
# # A tibble: 9 × 3
# sector           n percent
# <chr>        <int>   <dbl>
#   1 E            25675   6.20 
# 2 N             7582   1.83 
# 3 NE           10511   2.54 
# 4 NW            9980   2.41 
# 5 S             3648   0.882
# 6 SE            4144   1.00 
# 7 SW           29752   7.19 
# 8 W           106038  25.6  
# 9 unallocated 216481  52.3  



# Cluster Analysis --------------------------------------------------------


