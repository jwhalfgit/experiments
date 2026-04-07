library(splitr)
library(openair)
library(gridExtra)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggallin)
library(parallel)
library(foreach)
library(gtable)
library(ggpubr)
library(ggpmisc)
library(grid)
library(foreach)
library(doParallel)
library(caret)

registerDoParallel(cores= 4)


CODEDIR <- "C:/Users/jh2949/OneDrive - University of York/Documents/work/Code/R/experiments/hysplit"
source(file.path(CODEDIR,"hysplit.load.R"))
source(file.path(CODEDIR,"hysplit.analysis.funs.R"))

# Sites of interest
sites <- as.data.frame(matrix(data = c(53.44,-2.21,"Manchester Air Quality Site","maqs",
                                       52.45, -1.93, "Birmingham Super Site","bss",
                                       54.98, -1.61, "Newcastle Center","newcastle",
                                       55.95, -3.20, "Edinburgh Center","edinburgh",
                                       54.60, -5.93, "Belfast Center","bc",
                                       51.48, -3.18, "Cardiff Center","cardiff",
                                       51.45, -0.04,  "London Super Site","lss",
                                       51.08, -4.10, "Barnstaple Center","barnc",
                                       51.15, -1.44, "Chilbolton Observatory","chilb",
                                       55.79, -3.24, "Auchencorth Moss","auchencorthmoss",
                                       52.61, 1.30, "Norwich Center","norwc",
                                       53.75, -0.34, "Hull Center","hull",
                                       50.81, 0.27, "Eastbourne","eastbourne",
                                       50.91, -1.40, "Southampton","southampton",
                                       53.03, -2.18, "Stoke On Trent","stokeontrent",
                                       50.37, -4.14, "Plymouth","plymouth",
                                       51.46, 0.63, "Rochester Stoke","rochesterstoke"),
                              ncol=4,byrow=TRUE,dimnames = list(c(),c("lat","lon","loc","code"))))


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
norw <-  trajDF_oa %>% filter(receptor == 11)
hull <- trajDF_oa %>% filter(receptor == 12)


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
DATE1 <- seq.Date(as.Date("2017-03-01"), as.Date("2024-09-01"), by = "3 months")
DATE2 <- seq.Date(as.Date("2017-06-01"), as.Date("2024-12-01"), by = "3 months")-1

# 
 DATE1 <- "2017-03-01"
 DATE2 <- "2024-12-31"
seasonDF <- data.frame(DATE1, DATE2)

sectorDfList <- list()
sectorPlotList <- list()

library(outliers)

for(ix in 1:length(DATE1)){
#for(ix in 1:1){
    
  maqsSector <- sectorAnal(maqs, DATE1[ix],DATE2[ix])
  bssSector <- sectorAnal(bss, DATE1[ix],DATE2[ix])
  ncSector <- sectorAnal(nc, DATE1[ix],DATE2[ix])
  ecSector <- sectorAnal(ec, DATE1[ix],DATE2[ix])
  bcSector <- sectorAnal(bc, DATE1[ix],DATE2[ix])
  ccSector <- sectorAnal(cc, DATE1[ix],DATE2[ix])
  lcSector <- sectorAnal(lc, DATE1[ix],DATE2[ix])
  barncSector <- sectorAnal(barnc, DATE1[ix],DATE2[ix])
  chilbSector <- sectorAnal(chilb,DATE1[ix], DATE2[ix])
  amSector <- sectorAnal(am,DATE1[ix], DATE2[ix])
  
  
  sectorList <- list(maqsSector, bssSector, ncSector,
                     ecSector,bcSector,ccSector, lcSector,
                     barncSector,chilbSector,amSector)
  
  sectorListTrun <- lapply(sectorList,function(x){
    mutate(x, trajTotal = sum(nTraj)) %>% 
    select(sector, percentTraj,trajTotal) %>% 
      mutate(dates = paste(DATE1[ix],DATE2[ix], sep = "-"))
  })
  
  for(iy in 1:length(sectorListTrun)){
    names(sectorListTrun[[iy]])[2] <-sites[iy,3]
  }
  
  siteOrder <- c("Auchencorth Moss",
                  "Barnstaple Center",
                  "Belfast Center",
                  "Birmingham Super Site",
                  "Cardiff Center",
                  "Chilbolton Observatory",
                  "Edinburgh Center",
                  "London Super Site",
                  "Manchester Air Quality Site",
                  "Newcastle Center")
  
  
  
  sectorOutDF <- Reduce(function(...) merge(..., all = T), sectorListTrun)
  #capture.output(sectorList, file = "G:/My Drive/Experiments/DEFRA/hysplit/2021MAM-sectorlist.csv")
  pivotSectorOut <- sectorOutDF %>% 
    pivot_longer(cols = -c("sector", "dates", "trajTotal"),
                 names_to = "site",
                 values_to = "percentTraj") %>% 
    mutate(site = factor(site, levels = siteOrder, order = TRUE))
  sectorDfList[[ix]] <-sectorOutDF
  
  

  # Make Plot
  siteColors <- c("Auchencorth Moss" = "#AAAAFF",
                  "Barnstaple Center" = "#ff0000",
                  "Belfast Center" = "#0072B2",
                  "Birmingham Super Site" = "#e69f00",
                  "Cardiff Center" = "#56b4e9",
                  "Chilbolton Observatory" = "#aa0000",
                  "Edinburgh Center" = "#009e73",
                  "London Super Site" = "#d55e00",
                  "Manchester Air Quality Site" = "#000000",
                  "Newcastle Center" = "#cc79a7")
  
  sectorPlotList[[ix]] <- ggplot(data = pivotSectorOut, aes(x = factor(sector, 
                                     level = c(  "NE", "E", "SE",
                                                 "S", "SW", "W",
                                                 "NW", "N", "unallocated")),
                          y = percentTraj, fill=site))+ 
    geom_bar(position = position_dodge(width = 0.9, preserve = "single"),
             stat = "identity", alpha = 0.5, 
             aes(color = site), width = 0.75)+
    scale_fill_manual(values = siteColors, name = "Sites")+
    scale_color_manual(values = siteColors, guide = "none")+
    ylim(c(0,70))+
    labs(x = "Sector", y = "% of Trajectories",
         title = paste0(DATE1[ix],"/",DATE2[ix],", N = ", 
                        unique(sectorDfList[[ix]]$trajTotal)))+
    theme_light()
  
}
library(ggpubr)
# Spring 
ggarrange(sectorPlotList[[1]],#2017
             sectorPlotList[[5]],#2018
             sectorPlotList[[9]],#2019
             sectorPlotList[[13]],#2020
             sectorPlotList[[17]],#2021
             sectorPlotList[[21]],#2022
             sectorPlotList[[25]],#2023
             sectorPlotList[[29]],#2024
          common.legend = TRUE,
          legend= "bottom",
          ncol = 2,nrow = 4)#2024
# # 
# ggsave("G:/My Drive/Experiments/DEFRA/presentations/sectorBars-MAM-all.png",
#        width = 12.8, height = 7.68, units = "in", plot = sectorPlotList[[1]])



sectorMeans <- lapply(sectorDfList,function(x){apply(x[,4:13],1,mean,na.rm = TRUE)})
sectorSds <- lapply(sectorDfList,function(x){apply(x[,4:13],1,sd, na.rm = TRUE)})
sectorMeanSds <- mapply(function(x,y){data.frame(dir=sectorDfList[[1]][,1], 
                                                 means = round(x,1),
                                                 sds = round(y,1))}, y = sectorSds, x = sectorMeans,SIMPLIFY = FALSE)
sectorMeanSdsSorted <- lapply(sectorMeanSds, function(x){x[c(3,1,6,5,7,9,4,2,8),]}) # sort by direciton on plots

mean(sapply(sectorMeanSdsSorted,function(x){x$sd[which(x$dir != "unallocated")]}))

sectorMeansString <- lapply(sectorMeanSdsSorted, function(x){
  x$Percent = paste0(x$means, " ± ", x$sds); return(x)})  # create nice string for plots

sectorMeans4Table <- bind_cols(sectorMeansString) %>% 
  select(contains("dir")[1], contains("Percent"))

names(sectorMeans4Table) <- c("Direction",paste0(DATE1,"/",DATE2))
write.csv(sectorMeans4Table,"G:/My Drive/Experiments/DEFRA/sectorMeanSds.csv",
          quote = FALSE,row.names = FALSE)

#sectorCvs <- mapply(function(x,y){x / y *100}, x = sectorSds, y = sectorMeans)



Gtest <- lapply(sectorDfList, function(x){ grubbs.rowwise(x[,4:13])})
lapply(Gtest,write.table,
       file = "G:/My Drive/Experiments/DEFRA/hysplit/sectoranalysis/grubbs.csv",
       append = TRUE, row.names= FALSE, quote = FALSE,sep=",")


Gtest <- sectorOutDF[,4:13]
Gtest2 <- grubbs.rowwise(Gtest)






ggsave("G:/My Drive/Experiments/DEFRA/presentations/sectorBars-MAM.png",
       width = 8.3, height = 11.7, units = "in")
    
ggarrange(sectorPlotList[[2]],#2017
          sectorPlotList[[6]],#2018
          sectorPlotList[[10]],#2019
          sectorPlotList[[14]],#2020
          sectorPlotList[[18]],#2021
          sectorPlotList[[22]],#2022
          sectorPlotList[[26]],#2023
          sectorPlotList[[30]],#2024
          common.legend = TRUE,
          legend= "bottom",
          ncol = 2,nrow = 4)#2024


ggsave("G:/My Drive/Experiments/DEFRA/presentations/sectorBars-JJA.png",
       width = 8.3, height = 11.7, units = "in")

ggarrange(sectorPlotList[[3]],#2017
          sectorPlotList[[7]],#2018
          sectorPlotList[[11]],#2019
          sectorPlotList[[15]],#2020
          sectorPlotList[[19]],#2021
          sectorPlotList[[23]],#2022
          sectorPlotList[[27]],#2023
          sectorPlotList[[31]],#2024
          common.legend = TRUE,
          legend= "bottom",
          ncol = 2,nrow = 4)#2024


ggsave("G:/My Drive/Experiments/DEFRA/presentations/sectorBars-SON.png",
       width = 8.3, height = 11.7, units = "in")


ggarrange(sectorPlotList[[4]],#2017
          sectorPlotList[[8]],#2018
          sectorPlotList[[12]],#2019
          sectorPlotList[[16]],#2020
          sectorPlotList[[20]],#2021
          sectorPlotList[[24]],#2022
          sectorPlotList[[28]],#2023
          common.legend = TRUE,
          legend= "bottom",
          ncol = 2,nrow = 4)#2024


ggsave("G:/My Drive/Experiments/DEFRA/presentations/sectorBars-DJF.png",
       width = 8.3, height = 11.7, units = "in")




# Cluster Analysis --------------------------------------------------------
DATE1 <- "01/03/2024"
DATE2 <- "31/05/2024"

maqsClust <- clusterPlots(SITE = maqs, DATE1, DATE2)
lcClust <- clusterPlots(SITE = lc, DATE1, DATE2)
ecClust <- clusterPlots(SITE = ec, DATE1, DATE2)



# Source Attribution Analysis Bar Plots

DATE1 <- "01/12/2021"
DATE2 <- "28/02/2022"
HOURS <- 0:72

maqsSource <- sourceBoxes(maqs, DATE1,DATE2,HOURS)
bssSource <- sourceBoxes(bss, DATE1,DATE2, HOURS)
ncSource <- sourceBoxes(nc, DATE1,DATE2, HOURS)
ecSource <- sourceBoxes(ec, DATE1,DATE2, HOURS)
bcSource <- sourceBoxes(bc, DATE1,DATE2, HOURS)
ccSource <- sourceBoxes(cc, DATE1,DATE2, HOURS)
lcSource <- sourceBoxes(lc, DATE1,DATE2, HOURS)
barncSource <- sourceBoxes(barnc, DATE1,DATE2, HOURS)
amSource <- sourceBoxes(barnc, DATE1,DATE2, HOURS)
chilbSource <- sourceBoxes(chilb,DATE1,DATE2, HOURS)
norwSource <- sourceBoxes(norw, DATE1,DATE2, HOURS)
hullSource <- sourceBoxes(hull,DATE1,DATE2,HOURS)

sourceOut <- t(rbind(maqsSource, bssSource,ncSource,ecSource,
                   bcSource,ccSource,lcSource,barncSource))
sourceOutDf <- data.frame(region = row.names(sourceOut),sourceOut,
                          row.names = NULL) %>% 
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

############################################################################
#
#
#
#
#
#
#
#
#
#
#
# BY SITE BY SEASON -------------------------------------------------------
# I want to repeat the above by site and by season. 

sourceList <- list()
# Define date ranges
DATE1s <- seq(as.Date("2017-03-01"), as.Date("2024-09-01"), by = "3 months")
DATE2s <- seq(as.Date("2017-06-01"), as.Date("2024-12-01"), by = "3 months") - 1
# Create a list containing data for each site


DATE1s = ("2017-03-01")
DATE2s = ("2024-12-28")
site_data <- list(maqs = maqs, 
                  bss = bss, 
                  nc = nc, 
                  ec = ec, 
                  bc = bc,
                  cc = cc, 
                  lc = lc, 
                  barnc = barnc, 
                  am = am, 
                  chilb = chilb)

# Use Map to iterate over DATE1s and DATE2s simultaneously
sourceList <- Map(function(START, STOP) {
  # For each date range, run sourceBoxes on every site in the list
  lapply(names(site_data), function(SITE){
    sourceBoxes(site_data[[SITE]], 
                DATE1 = START, 
                DATE2=STOP, 
                HOURS = 0:72, 
                OUTPUT = "TS", 
                PLOT = FALSE) %>%
      mutate(site = SITE,
             atlantic = ifelse(region %in%c(1,7), 1, 0)) # Add site name as a column for later sorting
  })
}, DATE1s, DATE2s)

atl.pct <- lapply(sourceList[[1]],function(x){
  c(sum(x$atlantic)/nrow(x))
})

atl.sd <- sd(unlist(atl.pct))
atl.relsd <- sd(unlist(atl.pct)) / mean(unlist(atl.pct))

# Define a lookup for your labels to avoid manual typing in the loop
region_labels <- c("Atlantic", "Americas", "Africa", "Europe", "Greenland", 
                   "Iceland", "N Atlantic", "North Sea", "UK", "N Europe", 
                   "S Europe", "uncategorized")


# grpData is a function to group the HYSPLIT box values
grpData <- function(LIST){
  bind_rows(LIST) %>%
    mutate(region = factor(region, levels = 1:12)) %>%
    group_by(site, region, .drop = FALSE) %>%
    summarize(n = n(), .groups = "drop_last") %>%
    # Handle the "uncategorized" (Region 12 + NAs) logic
    mutate(n = if_else(region == 12, n + sum(n[is.na(region)], na.rm=T), n)) %>%
    filter(!is.na(region)) %>%
    mutate(nPct = n / sum(n) * 100,
           labels = region_labels[as.numeric(region)]) %>% 
    arrange(labels)
}

regionPctObsDF <- lapply(sourceList, grpData)


# regionPctObsDF <- do.call(rbind, regionPctObs) %>% 
#   arrange(labels) 
regionPctObsDFSD <- lapply(regionPctObsDF,function(x){
  x %>% group_by(labels) %>% 
    summarize(labels = unique(labels), avg = mean(nPct), sds = round(sd(nPct),2),relSds = round(sd(nPct),2) / round(mean(nPct),2)) %>% 
    arrange(labels)
})


# regionPctObsDFSD <- regionPctObsDF %>% 
#   group_by(labels) %>% 
#   summarize(labels = unique(labels), sds = round(sd(nPct),2)) %>% 
#   arrange(labels)

# The following gives plots based on the surface type against starting location.

locColors =c("#D7B5A6", # Africa
              "#FF6F00", # N America
              "#72D9FF", # Marine Atlantic
              "#42B540",#Europe
              "azure2", #Greenland
              "azure4", #Iceland
              "#0072b2", #N Atlantic
              "springgreen4", #N Europe
              "#264DFF", # N Sea
              "#C5E1A5", # S Europe
              "#9D7660", # UK
              "grey20")


# Create plots for each season
seasonPlotList <- list()
masterPlotList <- list()
for(iy in 1:length(regionPctObsDF)){
  for(ix in 1:length(unique(regionPctObsDF[[iy]]$labels))){
    seasonPlotList[[ix]] <-regionPctObsDF[[iy]] %>% 
                    filter(labels == (unique(regionPctObsDF[[iy]]$labels)[ix])) %>% 
      ggplot(aes(x = site, y = nPct, fill = region))+
      geom_bar(stat= "identity", position = "dodge")+
      ylab("Percent of Observation Points\n(%)")+
      xlab("Site")+
      labs(title = paste0(unique(regionPctObsDF[[iy]]$labels)[ix],
           ", SD = ",regionPctObsDFSD[[iy]]$sds[ix]))+
      scale_fill_manual(values = locColors[ix])+
      theme_minimal()+
      scale_x_discrete(guide = guide_axis(n.dodge = 3),
                       labels = c("Auchencorth Moss",
                                  "Barnstaple",
                                  "Belfast",
                                  "Birmingham",
                                  "Cardiff",
                                  "Chilbolton",
                                  "Edinburgh",
                                  "London",
                                  "Manchester",
                                  "Newcastle"))+
      theme(text= element_text(size = 20))
    
    
  }
  masterPlotList[[iy]] <- seasonPlotList
}

for(iy in 1:length(masterPlotList)){
  regionPlot <- ggarrange(plotlist = masterPlotList[[iy]], legend = "none")
  ggsave(paste0("G:/My Drive/Experiments/DEFRA/hysplit/sourceRegionGrid-byregion",
                DATE1s[iy],"-",DATE2s[iy],".png"),
         plot = regionPlot, width = 33, height = 20, units = "in",
         bg = "white")
}

########################################################################
#
#
#
#
#
#
#
########################################################################
# The above is great for generating plots of individual seasons, but I want
# to get yearly averages.
# Let's assign each season a tag

dfMaster <- bind_rows(regionPctObsDF, .id = "ix")

# Extract the month from the original DATE1s to define the season
# Since DATE1s are 03-01, 06-01, 09-01, 12-01:
season_map <- c("3" = "spring", "6" = "summer", "9" = "autumn", "12" = "winter")
dfMaster <- dfMaster %>%
  mutate(
    # Get the starting month of that specific date range
    startMonth = month(DATE1s[as.numeric(ix)]),
    # Assign the season name
    seasonName = season_map[as.character(startMonth)]
  )
# now calculate seasonal averages for the time range..
seasonal_averages <- dfMaster %>%
  group_by(seasonName, site, labels) %>%
  summarize(
    avg_nPct = mean(nPct, na.rm = TRUE),
    sd_nPct = sd(nPct, na.rm = TRUE),
    ci95 = t.test(nPct)$conf.int[2] - t.test(nPct)$conf.int[1],
    .groups = "drop"
  )


# I need to create some dummy data that I can use to set limits on the y-axes
# of the grouped plots.
facet_limits <- data.frame(
  labels = c("Atlantic", "Europe", "Americas", "Africa", "Greenland", 
             "Iceland", "N Atlantic", "North Sea", "UK", "N Europe", 
             "S Europe", "uncategorized"),
  avg_nPct = c(80, 20, 2.5, 0.4, 2, 2, 17, 17.5, 45, 8, 6, 2), # Set your desired max here
  site = seasonal_averages$site[1] # Assign to an existing site so it aligns on the x-axis
)


# Gemini plot
# Loop through the 4 seasons and save a plot for each
for(iy in c("spring", "summer", "autumn", "winter")) {
#  for(iy in c("winter")) {
    
  plot_data <- seasonal_averages %>% filter(seasonName == iy)
  
  p <- ggplot(plot_data, aes(x = site, y = avg_nPct, fill = labels)) +
    geom_col(position = "dodge") +
    geom_blank(data = facet_limits, aes(y = avg_nPct)) +
    geom_errorbar(
      aes(min = pmax(0, avg_nPct - ci95), # pmax ensures the bar stops at 0
        ymax = avg_nPct + ci95),
      position = "dodge",
      width = 0.25,      # Width of the horizontal "whiskers"
      color = "grey30"   # Subtle color so it doesn't overwhelm the plot
    ) +
    # This creates a separate box for each Region within the Season plot
    facet_wrap(~labels, scales = "free_y") + 
    scale_fill_manual(values = locColors) +
    # scale_x_discrete(guide = guide_axis(n.dodge = 3),
    #                  labels = c("Auchencorth Moss",
    #                             "Barnstaple",
    #                             "Belfast",
    #                             "Birmingham",
    #                             "Cardiff",
    #                             "Chilbolton",
    #                             "Edinburgh",
    #                             "London",
    #                             "Manchester",
    #                             "Newcastle"))+
    theme_minimal() +
    labs(
      title = paste("Multi-Year Average:", iy, "(2017-2024)"),
      y = "Average Percent of Observations (%)",
      x = "Site"
    ) +
    theme(
      text = element_text(size = 18),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  print(iy)
  ggsave(paste0("G:/My Drive/Experiments/DEFRA/hysplit/sourceRegionGrid-Seasonal_Avg_", iy, "-CIs.png"), 
         plot = p, width = 16, height = 10, 
         units = "in")
}




# regionPctSite <- lapply(sourceList, function(x){
#   x %>% 
#     #filter(!hour.inc%in%c(-5:0)) %>% # toss out the first 5 hours of the traj
#     group_by(region) %>% 
#     summarize(n = n()) %>% 
#     mutate(nPct = n/sum(n)*100) %>% 
#     arrange(as.numeric(as.character(region))) %>% 
#     mutate(labels = c("Atlantic", "N America", "Africa", "Europe",
#                       "Greenland","Iceland","N Atlantic","North Sea","UK", 
#                       "N Europe","S Europe","uncategorized", "NA"))
# })


plotListObs <- list()
for(ix in 1:length(regionPctObs)){
  plotListObs[[ix]] <- ggplot(data = regionPctObs[[ix]], 
                           aes(x = fct_inorder(labels), 
                               y = nPct, 
                               fill = fct_inorder(region)))+
    geom_bar(stat = "identity", position = "dodge")+
    ylim(c(0,75))+
    theme_minimal()+
    scale_fill_manual(values = c("#72D9FF", # Marine Atlantic
                                 "#FF6F00", # N America
                                 "#D7B5A6", # Africa
                                 "#42B540",#Europe
                                 "azure2", #Greenland
                                 "azure4", #Iceland
                                 "#0072b2", #N Atlantic
                                 "#264DFF", # N Sea
                                 "#9D7660", # UK
                                 "springgreen4", #N Europe
                                 "#C5E1A5", # S Europe
                                 "grey20"))+
    # labels = c("Atlantic", "N America", "Africa", "Europe",
    #            "Greenland","Iceland","N Atlantic","North Sea","UK", 
    #            "N Europe","S Europe","uncategorized")) +
    labs(x = "Region", y = "Percent (%)", title = sites[ix, 3])+
    theme(legend.position = "none", text = element_text(size = 18))+
    scale_x_discrete(guide = guide_axis(n.dodge = 3))
  
}

regionGrid <- do.call(grid.arrange,c(plotListObs, ncol = 3))

ggsave("G:/My Drive/Experiments/DEFRA/hysplit/sourceRegionGrid-bysite.png",
       plot = regionGrid, width = 19.20, height = 17.68, units = "in")



regionPctTraj <- lapply(sourceList, function(x){
  x %>% 
    #filter(!hour.inc%in%c(-5:0)) %>% # toss out the first 5 hours of the traj
    group_by(run, region) %>% 
    summarize(n = n()) %>% 
    mutate(nPct = n/72*100) %>% 
    arrange(as.numeric(as.character(region))) 
    # mutate(labels = c("Atlantic", "N America", "Africa", "Europe",
    #                   "Greenland","Iceland","N Atlantic","North Sea","UK", 
    #                   "N Europe","S Europe","uncategorized", "NA"))
})







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



############################################################################
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
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
  mutate(delta = fidas - digitel)%>% 
  mutate(deltadelta = mean(delta) - delta) %>% 
  rename(date = ts)


# plotEq(barncEq,barnc, DATE1 = "2022-07-01", DATE2 = "2022-12-31")
# plotEq(barncEq,barnc, DATE1 = "2023-01-01", DATE2 = "2023-08-31")

# Bham A4540  
bss1Eq <-  read_csv("G:/My Drive/Experiments/DEFRA/data/bss1.csv") %>% 
  rename(ts = `Date and Time`,
         digitel = `Birmingham A4540 Digitel`,
         fidas = `Birmingham A4540 Fidas 215`) %>% 
  select(ts, digitel, fidas) %>% 
  mutate(ts = dmy_hm(ts),
         digitel = as.numeric(digitel),
         fidas = as.numeric(fidas)) %>% 
  drop_na() %>% 
  mutate(delta = fidas - digitel)%>% 
  mutate(deltadelta = mean(delta) - delta)%>% 
  rename(date = ts)

# 
# plotEq(bss1Eq,bss, DATE1 = "2023-03-01", DATE2 = "2023-05-31")
# 
# plotEq(bss1Eq,bss, DATE1 = "2023-01-01", DATE2 = "2023-08-31")

#BhamU
bss2Eq <-  read_csv("G:/My Drive/Experiments/DEFRA/data/bss2.csv") %>% 
  rename(ts = `Date and Time`,
         psol = `Birmingham University Partisol`,
         fidas = `Birmingham University Fidas 215`) %>% 
  select(ts, psol, fidas) %>% 
  mutate(ts = dmy_hm(ts),
         psol = as.numeric(psol),
         fidas = as.numeric(fidas)) %>% 
  drop_na() %>% 
  mutate(delta = fidas - psol)%>% 
  mutate(deltadelta = mean(delta) - delta)%>% 
  rename(date = ts)


# plotEq(bss2Eq,bss, DATE1 = "2023-03-01", DATE2 = "2023-05-31")
# plotEq(bss2Eq,bss, DATE1 = "2023-01-01", DATE2 = "2023-05-31")
# 

chilbEq <- read_csv("G:/My Drive/Experiments/DEFRA/data/chilb.csv") %>% 
  rename(ts = `Date and Time`,
         digitel = `Chilbolton Digitel`,
         fidas = `Chilbolton Fidas 215`) %>% 
  select(ts, digitel, fidas) %>% 
  mutate(ts = dmy_hm(ts),
         digitel = as.numeric(digitel),
         fidas = as.numeric(fidas)) %>% 
  drop_na() %>% 
  mutate(delta = fidas - digitel)%>% 
  mutate(deltadelta = mean(delta) - delta)%>% 
  rename(date = ts)

# 
# plotEq(chilbEq,chilb, DATE1 = "2022-06-01", DATE2 = "2022-12-31")
# plotEq(chilbEq, chilb, DATE1 = "2023-01-01", DATE2 = "2023-05-31")


lcEq <- read_csv("G:/My Drive/Experiments/DEFRA/data/lc.csv") %>% 
  rename(ts = `Date and Time`,
         digitel = `Marylebone Digitel`,
         fidas = `Marylebone Fidas 215`) %>% 
  select(ts, digitel, fidas) %>% 
  mutate(ts = dmy_hm(ts),
         digitel = as.numeric(digitel),
         fidas = as.numeric(fidas)) %>% 
  drop_na() %>% 
  mutate(delta = fidas - digitel) %>% 
  mutate(deltadelta = mean(delta) - delta)%>% 
  rename(date = ts)

# 
# plotEq(lcEq,lc)
# plotEq(lcEq,lc, DATE1 = "2022-12-01", DATE2 = "2023-02-28")
# plotEq(lcEq, lc, DATE1 = "2023-03-01", DATE2 = "2023-05-30")


hopEq <- read_csv("G:/My Drive/Experiments/DEFRA/data/hop.csv") %>% 
  rename(ts = `Date and Time`,
         psol = `HOP Partisol`,
         fidas = `HOP Fidas 215`) %>% 
  select(ts, psol, fidas) %>% 
  mutate(ts = ymd(ts),
         psol = as.numeric(psol),
         fidas = as.numeric(fidas)) %>% 
  drop_na() %>% 
  mutate(delta = fidas - psol) %>% 
  mutate(deltadelta = mean(delta) - delta)%>% 
  rename(date = ts)


maqsEq <- read_csv("G:/My Drive/Experiments/DEFRA/data/maqs.csv") %>% 
  rename(ts = `Date and Time`,
         psol = `Manchester University Partisol`,
         fidas = `Manchester University Fidas 215`) %>% 
  select(ts, psol, fidas) %>% 
  mutate(ts = dmy_hm(ts),
         psol = as.numeric(psol),
         fidas = as.numeric(fidas)) %>% 
  drop_na() %>% 
  mutate(delta = fidas - psol) %>% 
  mutate(deltadelta = mean(delta) - delta)%>% 
  rename(date = ts)



# plotEq(maqsEq,maqs)
# plotEq(maqsEq,maqs, DATE1 = "2023-03-01", DATE2 = "2023-05-31")
# plotEq(maqsEq, maqs, DATE1 = "2023-01-01", DATE2 = "2023-05-31")

# For this analysis, I'm wondering how problematic near-zero (but not zero)
# variance predictors are.  Let's check how much of a problem this is:


# Summer JJA
DATEJJA1 = "2022-05-20"
DATEJJA2 = "2022-08-30"

# Autumn SON
DATESON1 = "2022-09-01"
DATESON2 = "2022-11-30"

# Winter DJF
DATEDJF1 = "2022-12-01"
DATEDJF2 = "2023-02-28"

#Spring MAM
DATEMAM1 = "2023-03-01"
DATEMAM2 = "2023-05-31"


dateMat <- matrix(c(DATEJJA1,DATEJJA2,DATESON1,DATESON2,
                    DATEDJF1,DATEDJF2,DATEMAM1,DATEMAM2,
                    DATEJJA1, DATEMAM2),
                  ncol = 2, byrow = TRUE)


barncPlot <- eqMVRLoc(locEq = barncEq, locBox = barnc)
maqsPlot <- eqMVRLoc(locEq = maqsEq, locBox = maqs)
lcPlot <- eqMVRLoc(locEq = lcEq, locBox = lc)
chilbPlot <- eqMVRLoc(locEq = chilbEq, locBox = chilb)
bss1Plot <- eqMVRLoc(locEq = bss1Eq, bss)
bss2Plot <- eqMVRLoc(locEq = bss2Eq, bss)


allSitesPlot <- ggarrange(barncPlot + rremove("ylab") + rremove("xlab"),
                          maqsPlot + rremove("ylab") + rremove("xlab"),
                          lcPlot + rremove("ylab") + rremove("xlab"),
                          chilbPlot + rremove("ylab") + rremove("xlab"),
                          bss1Plot + rremove("ylab") + rremove("xlab"),
                          bss2Plot + rremove("ylab") + rremove("xlab"),
             ncol =3, nrow = 2, common.legend = TRUE, legend = "bottom")


allSitesPlotOut <- annotate_figure(allSitesPlot, 
                                   left = textGrob(expression("Predictor (ug m" ^-3* ")"),rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                                   bottom = textGrob("All Seasons",gp = gpar(cex = 1.3)))



ggsave("G:/My Drive/Experiments/DEFRA/hysplit/seasonal_mvr/allSites_bar.png",
       plot = allSitesPlotOut,
       width = 14.68, 
       height = 10.24, 
       units = "in",
       bg = "white")



# Summer JJA
DATEJJA1 = "2022-05-20"
DATEJJA2 = "2022-08-30"

# Autumn SON
DATESON1 = "2022-09-01"
DATESON2 = "2022-11-30"

# Winter DJF
DATEDJF1 = "2022-12-01"
DATEDJF2 = "2023-02-28"

#Spring MAM
DATEMAM1 = "2023-03-01"
DATEMAM2 = "2023-05-31"


dateMat <- matrix(c(#DATEJJA1,DATEJJA2,DATESON1,DATESON2,
  #DATEDJF1,DATEDJF2,DATEMAM1,DATEMAM2),
  DATEJJA1, DATEMAM2),
  ncol = 2, byrow = TRUE)


# MAQS ACSM ---------------------------------------------------------------
acsm <- read_csv("G:/My Drive/Experiments/DEFRA/data/maqs/acsm/acsm.csv") %>% 
  select(!contains("qc")) %>% 
  mutate(datetime = floor_date(datetime, unit = "1 day")) %>% 
  group_by(datetime) %>% 
  summarize_all(mean)

dfMerge <- acsm %>% 
  left_join(maqsEq, by = join_by(datetime == date)) %>% 
  na.omit() %>% 
  mutate(massSum = cl+nh4 + no3+so4+organic,
         clPct = cl / massSum,
         nh4Pct = nh4 / massSum,
         no3Pct = no3 / massSum,
         so4Pct = so4 / massSum,
         orgPct = organic /massSum)




# Summer JJA
DATEJJA1 = "2022-05-20"
DATEJJA2 = "2022-08-30"

# Autumn SON
DATESON1 = "2022-09-01"
DATESON2 = "2022-11-30"

# Winter DJF
DATEDJF1 = "2022-12-01"
DATEDJF2 = "2023-02-28"

#Spring MAM
DATEMAM1 = "2023-03-01"
DATEMAM2 = "2023-05-31"


dateMat <- matrix(c(DATEJJA1,DATEJJA2,DATESON1,DATESON2,
  DATEDJF1,DATEDJF2,DATEMAM1,DATEMAM2,
  DATEJJA1, DATEMAM2),
  ncol = 2, byrow = TRUE)

maqsCompPlots <- comp.regression(ACSM = acsm, EQDATA= maqsEq,
                                 DATES = dateMat)




testDF <- eventLocator(dfMerge, SIGN = "neg")
allReg <- summary(lm(deltadelta~ cl + nh4 + no3+ so4 + organic, data = testDF))
# Summer reg
sumReg <- testDF %>% 
  filter(between(datetime, ymd("2022-06-01"), ymd("2022-08-30"))) %>% 
  lm(data = ., formula =deltadelta~ cl + no3+ nh4+ so4 + organic)

autReg <- testDF %>% 
  filter(between(datetime, ymd("2022-09-01"), ymd("2022-11-30"))) %>% 
  lm(data = ., formula =deltadelta~ cl + no3+ nh4+ so4 + organic)

winReg <- testDF %>% 
  filter(between(datetime, ymd("2022-12-01"), ymd("2023-02-28"))) %>% 
  lm(data = ., formula =abs(deltadelta)~ cl + no3+ nh4+ so4 + organic)

# Spring
sprReg <- testDF %>% 
  filter(between(datetime, ymd("2023-03-01"), ymd("2023-05-31"))) %>% 
  lm(data = ., formula =deltadelta~ cl + no3+ nh4+ so4 + organic)




write.csv(dfMerge,"G:/My Drive/Experiments/DEFRA/data/maqs/acsm/eq-acsm.csv" ,
          row.names = FALSE, quote = FALSE)


# MVR

allReg <- summary(lm(deltadelta~ cl + nh4 + no3+ so4 + organic, data = dfMerge))

# 
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9.1147 -0.6109  0.1756  0.7020  4.8622 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.01934    0.15825  -0.122  0.90283    
# cl          -0.90873    0.66518  -1.366  0.17279    
# nh4          3.82900    1.66848   2.295  0.02234 *  
#   no3         -1.44976    0.54536  -2.658  0.00822 ** 
#   so4         -1.59367    0.57522  -2.771  0.00590 ** 
#   organic      0.17976    0.03779   4.757  2.9e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.445 on 343 degrees of freedom
# Multiple R-squared:  0.09414,	Adjusted R-squared:  0.08094 
# F-statistic: 7.129 on 5 and 343 DF,  p-value: 2.316e-06
# 
# 
# 
# 
allRegPct <- summary(lm(deltadelta~ clPct + no3Pct+ nh4Pct+ so4Pct + orgPct,
                        data = dfMerge))
# Call:
#   lm(formula = deltadelta ~ clPct + nh4Pct + no3Pct + so4Pct + 
#        orgPct, data = dfMerge)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -8.6333 -0.5817  0.1377  0.6195  5.1856 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   1.4899     0.2841   5.245 2.73e-07 ***
#   clPct       -10.0070     3.1769  -3.150  0.00178 ** 
#   nh4Pct       10.7367     7.2553   1.480  0.13983    
# no3Pct      -10.6037     2.5790  -4.112 4.92e-05 ***
#   so4Pct       -4.9781     2.6215  -1.899  0.05840 .  
# orgPct            NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.403 on 344 degrees of freedom
# Multiple R-squared:  0.1438,	Adjusted R-squared:  0.1338 
# F-statistic: 14.44 on 4 and 344 DF,  p-value: 6.524e-11


# Summer reg %
sumRegPct <- dfMerge %>% 
  filter(between(datetime, ymd("2022-06-01"), ymd("2022-08-30"))) %>% 
  lm(data = ., formula =deltadelta~ clPct + no3Pct+ nh4Pct+ so4Pct + orgPct)
# R2 = 0.16

# Summer reg
sumReg <- dfMerge %>% 
  filter(between(datetime, ymd("2022-06-01"), ymd("2022-08-30"))) %>% 
  lm(data = ., formula =deltadelta~ cl + no3+ nh4+ so4 + organic)
# R2 = 0.28


# autumn reg%
autRegPct <- dfMerge %>% 
  filter(between(datetime, ymd("2022-09-01"), ymd("2022-11-30"))) %>% 
  lm(data = ., formula =deltadelta~ clPct + no3Pct+ nh4Pct+ so4Pct + orgPct)
# R2 = 0.256

autReg <- dfMerge %>% 
  filter(between(datetime, ymd("2022-09-01"), ymd("2022-11-30"))) %>% 
  lm(data = ., formula =deltadelta~ cl + no3+ nh4+ so4 + organic)
#R2  = 0.41

# Winter
winRegPct <- dfMerge %>% 
  filter(between(datetime, ymd("2022-12-01"), ymd("2023-02-28"))) %>% 
  lm(data = ., formula =deltadelta~ clPct + no3Pct+ nh4Pct+ so4Pct + orgPct)
# R2 = 0.06

winReg <- dfMerge %>% 
  filter(between(datetime, ymd("2022-12-01"), ymd("2023-02-28"))) %>% 
  lm(data = ., formula =abs(deltadelta)~ cl + no3+ nh4+ so4 + organic)
# R2 = 0.2


# Spring
sprReg <- dfMerge %>% 
  filter(between(datetime, ymd("2023-03-01"), ymd("2023-05-31"))) %>% 
  lm(data = ., formula =deltadelta~ cl + no3+ nh4+ so4 + organic)
# R2 = 0.19

sprRegPct <- dfMerge %>% 
  filter(between(datetime, ymd("2023-03-01"), ymd("2023-05-31"))) %>% 
  lm(data = ., formula =deltadelta~ clPct + no3Pct+ nh4Pct+ so4Pct + orgPct)
# 0.1



# London ACSM
acsmLondon <- list.files(path = "G:/My Drive/Experiments/DEFRA/data/london/",
                            pattern = ".csv",full.names = TRUE)%>%
  lapply(read_csv) %>%
  bind_rows()%>%
  mutate(datetime = dmy(`measurement end date`) + hms(`measurement end time`)) %>%
  rename("site" = "Station name") %>%
  filter(Validity_id == 1) %>%
  select(c("datetime", "site", "measurement", "parameter_id")) %>% 
  filter(site == "London Marylebone Road") %>% 
  filter(between(datetime,ymd("2022-06-01"), ymd("2023-06-01"))) %>% 
  pivot_wider(#id_cols = date, 
            names_from = parameter_id,
            values_from = measurement, 
            values_fill = NA) %>% 
  select(!site) %>% 
  rename(nh4 = ANH4_PM1, no3 = ANO3_PM1, organic = AOCM_PM1, so4 = ASO4_PM1) %>% 
  arrange(datetime) %>% 
  mutate(datetime = floor_date(datetime, unit = "1 day")) %>% 
  group_by(datetime) %>% 
  summarize_all(mean, na.rm = TRUE)

# 
# dfMerge <- acsmLondon %>% 
#   left_join(lcEq, by = join_by(datetime == ts)) %>% 
#   mutate(massSum = nh4 + no3+so4+organic,
#          nh4Pct = nh4 / massSum,
#          no3Pct = no3 / massSum,
#          so4Pct = so4 / massSum,
#          orgPct = organic /massSum) %>% 
#   na.omit()

londonCompPlots <- comp.regression(ACSM = acsmLondon, EQDATA= lcEq,
                                 DATES = dateMat)


testDF <- eventLocator(dfMerge, SIGN = "neg")
allReg <- summary(lm(deltadelta~ nh4 + no3+ so4 + organic, data = testDF))

sumReg <- testDF %>% 
  filter(between(datetime, ymd("2022-06-01"), ymd("2022-08-30"))) %>% 
  lm(data = ., formula =deltadelta~ no3+ nh4+ so4 + organic)

autReg <- testDF %>% 
  filter(between(datetime, ymd("2022-09-01"), ymd("2022-11-30"))) %>% 
  lm(data = ., formula =deltadelta~ no3+ nh4+ so4 + organic)

winReg <- testDF %>% 
  filter(between(datetime, ymd("2022-12-01"), ymd("2023-02-28"))) %>% 
  lm(data = ., formula =abs(deltadelta)~ no3+ nh4+ so4 + organic)

# Spring
sprReg <- testDF %>% 
  filter(between(datetime, ymd("2023-03-01"), ymd("2023-05-31"))) %>% 
  lm(data = ., formula =deltadelta~no3+ nh4+ so4 + organic)






write_csv(dfMerge, "G:/My Drive/Experiments/DEFRA/data/london/mbone_acsm.csv")


# MVR - 
# reg <- summary(lm(deltadelta~ nh4 + no3+ so4 + organic, data = dfMerge))
# Call:
#   lm(formula = deltadelta ~ nh4 + no3 + so4 + organic, data = dfMerge)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.9621 -0.9331 -0.0954  0.8296  4.9578 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.6144     0.2316   2.653  0.00844 ** 
#   nh4          -4.7598     0.7799  -6.103 3.50e-09 ***
#   no3           1.4244     0.2995   4.755 3.20e-06 ***
#   so4           0.4159     0.3022   1.376  0.16987    
# organic       0.3226     0.0403   8.005 3.31e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.413 on 277 degrees of freedom
# Multiple R-squared:  0.2991,	Adjusted R-squared:  0.289 
# F-statistic: 29.56 on 4 and 277 DF,  p-value: < 2.2e-16



# allRegPct <- summary(lm(deltadelta~  no3Pct+ nh4Pct+ so4Pct + orgPct,
#                         data = dfMerge))
# Call:
#   lm(formula = deltadelta ~ clPct + nh4Pct + no3Pct + so4Pct + 
#        orgPct, data = dfMerge)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -8.6333 -0.5817  0.1377  0.6195  5.1856 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   1.4899     0.2841   5.245 2.73e-07 ***
#   clPct       -10.0070     3.1769  -3.150  0.00178 ** 
#   nh4Pct       10.7367     7.2553   1.480  0.13983    
# no3Pct      -10.6037     2.5790  -4.112 4.92e-05 ***
#   so4Pct       -4.9781     2.6215  -1.899  0.05840 .  
# orgPct            NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.403 on 344 degrees of freedom
# Multiple R-squared:  0.1438,	Adjusted R-squared:  0.1338 
# F-statistic: 14.44 on 4 and 344 DF,  p-value: 6.524e-11


# Summer reg %
sumRegPct <- dfMerge %>% 
  filter(between(datetime, ymd("2022-06-01"), ymd("2022-08-30"))) %>% 
  lm(data = ., formula =deltadelta~ no3Pct+ nh4Pct+ so4Pct + orgPct)
# R2 = 0.16

# Summer reg
sumReg <- dfMerge %>% 
  filter(between(datetime, ymd("2022-06-01"), ymd("2022-08-30"))) %>% 
  lm(data = ., formula =deltadelta~ no3+ nh4+ so4 + organic)
# R2 = 0.28


# autumn reg%
autRegPct <- dfMerge %>% 
  filter(between(datetime, ymd("2022-09-01"), ymd("2022-11-30"))) %>% 
  lm(data = ., formula =deltadelta~ no3Pct+ nh4Pct+ so4Pct + orgPct)
# R2 = 0.256

autReg <- dfMerge %>% 
  filter(between(datetime, ymd("2022-09-01"), ymd("2022-11-30"))) %>% 
  lm(data = ., formula =deltadelta~ no3+ nh4+ so4 + organic)
#R2  = 0.41

# Winter
winRegPct <- dfMerge %>% 
  filter(between(datetime, ymd("2022-12-01"), ymd("2023-02-28"))) %>% 
  lm(data = ., formula =deltadelta~ no3Pct+ nh4Pct+ so4Pct + orgPct)
# R2 = 0.06

winReg <- dfMerge %>% 
  filter(between(datetime, ymd("2022-12-01"), ymd("2023-02-28"))) %>% 
  lm(data = ., formula =deltadelta~ no3+ nh4+ so4 + organic)
# R2 = 0.2


# Spring
sprReg <- dfMerge %>% 
  filter(between(datetime, ymd("2023-03-01"), ymd("2023-05-31"))) %>% 
  lm(data = ., formula =deltadelta~ no3+ nh4+ so4 + organic)
# R2 = 0.19

sprRegPct <- dfMerge %>% 
  filter(between(datetime, ymd("2023-03-01"), ymd("2023-05-31"))) %>% 
  lm(data = ., formula =deltadelta~ no3Pct+ nh4Pct+ so4Pct + orgPct)
# 0.1


# Chilb ACSM
acsmChilb <- list.files(path = "G:/My Drive/Experiments/DEFRA/data/london/",
                         pattern = ".csv",full.names = TRUE)%>%
  lapply(read_csv) %>%
  bind_rows()%>%
  mutate(datetime = dmy(`measurement end date`) + hms(`measurement end time`)) %>%
  rename("site" = "Station name") %>%
  filter(Validity_id == 1) %>%
  select(c("datetime", "site", "measurement", "parameter_id")) %>% 
  filter(site == "Chilbolton Observatory") %>% 
  filter(between(datetime,ymd("2022-06-01"), ymd("2023-06-01"))) %>% 
  pivot_wider(#id_cols = date, 
    names_from = parameter_id,
    values_from = measurement, 
    values_fill = NA) %>% 
  select(!site) %>% 
  rename(nh4 = ANH4_PM2.5, no3 = ANO3_PM2.5, organic = AOCM_PM2.5, so4 = ASO4_PM2.5) %>% 
  arrange(datetime) %>% 
  mutate(datetime = floor_date(datetime, unit = "1 day")) %>% 
  group_by(datetime) %>% 
  summarize_all(mean, na.rm = TRUE)
# 
# dfMerge <- acsmChilb %>% 
#   left_join(hopEq, by = join_by(datetime == ts)) %>% 
#   mutate(massSum = nh4 + no3+so4+organic,
#          nh4Pct = nh4 / massSum,
#          no3Pct = no3 / massSum,
#          so4Pct = so4 / massSum,
#          orgPct = organic /massSum) %>% 
#   na.omit()
# 


chilbCompPlots <- comp.regression(ACSM = acsmChilb, EQDATA= chilbEq,
                                   DATES = dateMat)


testDF <- eventLocator(dfMerge, SIGN = "neg")
allReg <- summary(lm(deltadelta~ nh4 + no3+ so4 + organic, data = testDF))

sumReg <- testDF %>% 
  filter(between(datetime, ymd("2022-06-01"), ymd("2022-08-30"))) %>% 
  lm(data = ., formula =deltadelta~ no3+ nh4+ so4 + organic)

autReg <- testDF %>% 
  filter(between(datetime, ymd("2022-09-01"), ymd("2022-11-30"))) %>% 
  lm(data = ., formula =deltadelta~ no3+ nh4+ so4 + organic)

winReg <- testDF %>% 
  filter(between(datetime, ymd("2022-12-01"), ymd("2023-02-28"))) %>% 
  lm(data = ., formula =abs(deltadelta)~ no3+ nh4+ so4 + organic)

# Spring
sprReg <- testDF %>% 
  filter(between(datetime, ymd("2023-03-01"), ymd("2023-05-31"))) %>% 
  lm(data = ., formula =deltadelta~no3+ nh4+ so4 + organic)


write_csv(dfMerge, "G:/My Drive/Experiments/DEFRA/data/london/chilb_acsm.csv")

# MVR
reg <- summary(lm(deltadelta~ nh4 + no3+ so4 + organic, data = dfMerge))

# 
# Call:
#   lm(formula = deltadelta ~ nh4 + no3 + so4 + organic, data = dfMerge)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.7594 -0.6001  0.1029  0.8978  5.6821 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.98053    0.24745   3.963 9.49e-05 ***
#   nh4         -3.89307    0.70419  -5.528 7.60e-08 ***
#   no3          0.87139    0.30663   2.842  0.00483 ** 
#   so4          2.47677    0.56161   4.410 1.49e-05 ***
#   organic      0.13425    0.05748   2.336  0.02024 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.424 on 271 degrees of freedom
# Multiple R-squared:  0.2773,	Adjusted R-squared:  0.2666 
# F-statistic:    26 on 4 and 271 DF,  p-value: < 2.2e-16

allRegPct <- summary(lm(deltadelta~  no3Pct+ nh4Pct+ so4Pct + orgPct,
                        data = dfMerge))
# Summer reg %
sumRegPct <- dfMerge %>% 
  filter(between(datetime, ymd("2022-06-01"), ymd("2022-08-30"))) %>% 
  lm(data = ., formula =deltadelta~ no3Pct+ nh4Pct+ so4Pct + orgPct)
# R2 = 0.16

# Summer reg
sumReg <- dfMerge %>% 
  filter(between(datetime, ymd("2022-06-01"), ymd("2022-08-30"))) %>% 
  lm(data = ., formula =deltadelta~ no3+ nh4+ so4 + organic)
# R2 = 0.28


# autumn reg%
autRegPct <- dfMerge %>% 
  filter(between(datetime, ymd("2022-09-01"), ymd("2022-11-30"))) %>% 
  lm(data = ., formula =deltadelta~ no3Pct+ nh4Pct+ so4Pct + orgPct)
# R2 = 0.256

autReg <- dfMerge %>% 
  filter(between(datetime, ymd("2022-09-01"), ymd("2022-11-30"))) %>% 
  lm(data = ., formula =deltadelta~ no3+ nh4+ so4 + organic)
#R2  = 0.41

# Winter
winRegPct <- dfMerge %>% 
  filter(between(datetime, ymd("2022-12-01"), ymd("2023-02-28"))) %>% 
  lm(data = ., formula =deltadelta~ no3Pct+ nh4Pct+ so4Pct + orgPct)
# R2 = 0.06

winReg <- dfMerge %>% 
  filter(between(datetime, ymd("2022-12-01"), ymd("2023-02-28"))) %>% 
  lm(data = ., formula =deltadelta~ no3+ nh4+ so4 + organic)
# R2 = 0.2


# Spring
sprReg <- dfMerge %>% 
  filter(between(datetime, ymd("2023-03-01"), ymd("2023-05-31"))) %>% 
  lm(data = ., formula =deltadelta~ no3+ nh4+ so4 + organic)
# R2 = 0.19

sprRegPct <- dfMerge %>% 
  filter(between(datetime, ymd("2023-03-01"), ymd("2023-05-31"))) %>% 
  lm(data = ., formula =deltadelta~ no3Pct+ nh4Pct+ so4Pct + orgPct)
# 0.1








# London ACSM
acsmHOP <- list.files(path = "G:/My Drive/Experiments/DEFRA/data/london/",
                         pattern = ".csv",full.names = TRUE)%>%
  lapply(read_csv) %>%
  bind_rows()%>%
  mutate(datetime = dmy(`measurement end date`) + hms(`measurement end time`)) %>%
  rename("site" = "Station name") %>%
  filter(Validity_id == 1) %>%
  select(c("datetime", "site", "measurement", "parameter_id")) %>% 
  filter(site == "London Honor Oak Park") %>% 
  filter(between(datetime,ymd("2022-06-01"), ymd("2023-06-01"))) %>% 
  pivot_wider(#id_cols = date, 
    names_from = parameter_id,
    values_from = measurement, 
    values_fill = NA) %>% 
  select(!site) %>% 
  rename(nh4 = ANH4_PM2.5, 
         no3 = ANO3_PM2.5, 
         organic = AOCM_PM2.5, 
         so4 = ASO4_PM2.5) %>% 
  arrange(datetime) %>% 
  mutate(datetime = floor_date(datetime, unit = "1 day")) %>% 
  group_by(datetime) %>% 
  summarize_all(mean, na.rm = TRUE)

# dfMerge <- acsmHOP %>% 
#   left_join(hopEq, by = join_by(datetime == ts)) %>% 
#   mutate(massSum = nh4 + no3+so4+organic,
#          nh4Pct = nh4 / massSum,
#          no3Pct = no3 / massSum,
#          so4Pct = so4 / massSum,
#          orgPct = organic /massSum) %>% 
#   na.omit()



HOPCompPlots <- comp.regression(ACSM = acsmHOP, EQDATA= hopEq,
                                  DATES = dateMat)



write_csv(dfMerge, "G:/My Drive/Experiments/DEFRA/data/london/hop_acsm.csv")

testDF <- eventLocator(dfMerge, SIGN = "neg")
allReg <- summary(lm(deltadelta~ nh4 + no3+ so4 + organic, data = testDF))

sumReg <- testDF %>% 
  filter(between(datetime, ymd("2022-06-01"), ymd("2022-08-30"))) %>% 
  lm(data = ., formula =deltadelta~ no3+ nh4+ so4 + organic)

autReg <- testDF %>% 
  filter(between(datetime, ymd("2022-09-01"), ymd("2022-11-30"))) %>% 
  lm(data = ., formula =deltadelta~ no3+ nh4+ so4 + organic)

winReg <- testDF %>% 
  filter(between(datetime, ymd("2022-12-01"), ymd("2023-02-28"))) %>% 
  lm(data = ., formula =abs(deltadelta)~ no3+ nh4+ so4 + organic)

# Spring
sprReg <- testDF %>% 
  filter(between(datetime, ymd("2023-03-01"), ymd("2023-05-31"))) %>% 
  lm(data = ., formula =deltadelta~no3+ nh4+ so4 + organic)





reg <- summary(lm(deltadelta~ nh4 + no3+ so4 + organic, data = dfMerge))
# reg
# 
# Call:
#   lm(formula = deltadelta ~ nh4 + no3 + so4 + organic, data = dfMerge)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.4405 -0.6081  0.1574  0.7123  3.7851 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.60229    0.25776   2.337   0.0203 *  
#   nh4         -2.07022    0.88048  -2.351   0.0195 *  
#   no3         -0.33948    0.36376  -0.933   0.3516    
# so4          0.78331    0.37257   2.102   0.0366 *  
#   organic      0.24673    0.03927   6.283 1.55e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.361 on 240 degrees of freedom
# (64 observations deleted due to missingness)
# Multiple R-squared:   0.29,	Adjusted R-squared:  0.2782 
# F-statistic: 24.51 on 4 and 240 DF,  p-value: < 2.2e-16

allRegPct <- summary(lm(deltadelta~  no3Pct+ nh4Pct+ so4Pct + orgPct,
                        data = dfMerge))

sumRegPct <- dfMerge %>% 
  filter(between(datetime, ymd("2022-06-01"), ymd("2022-08-30"))) %>% 
  lm(data = ., formula =deltadelta~ no3Pct+ nh4Pct+ so4Pct + orgPct)
# R2 = 0.16

# Summer reg
sumReg <- dfMerge %>% 
  filter(between(datetime, ymd("2022-06-01"), ymd("2022-08-30"))) %>% 
  lm(data = ., formula =deltadelta~ no3+ nh4+ so4 + organic)
# R2 = 0.28


# autumn reg%
autRegPct <- dfMerge %>% 
  filter(between(datetime, ymd("2022-09-01"), ymd("2022-11-30"))) %>% 
  lm(data = ., formula =deltadelta~ no3Pct+ nh4Pct+ so4Pct + orgPct)
# R2 = 0.256

autReg <- dfMerge %>% 
  filter(between(datetime, ymd("2022-09-01"), ymd("2022-11-30"))) %>% 
  lm(data = ., formula =deltadelta~ no3+ nh4+ so4 + organic)
#R2  = 0.41

# Winter
winRegPct <- dfMerge %>% 
  filter(between(datetime, ymd("2022-12-01"), ymd("2023-02-28"))) %>% 
  lm(data = ., formula =deltadelta~ no3Pct+ nh4Pct+ so4Pct + orgPct)
# R2 = 0.06

winReg <- dfMerge %>% 
  filter(between(datetime, ymd("2022-12-01"), ymd("2023-02-28"))) %>% 
  lm(data = ., formula =deltadelta~ no3+ nh4+ so4 + organic)
# R2 = 0.2


# Spring
sprReg <- dfMerge %>% 
  filter(between(datetime, ymd("2023-03-01"), ymd("2023-05-31"))) %>% 
  lm(data = ., formula =deltadelta~ no3+ nh4+ so4 + organic)
# R2 = 0.19

sprRegPct <- dfMerge %>% 
  filter(between(datetime, ymd("2023-03-01"), ymd("2023-05-31"))) %>% 
  lm(data = ., formula =deltadelta~ no3Pct+ nh4Pct+ so4Pct + orgPct)
# 0.1



comp_mvr_out <- ggarrange(maqsCompPlots, 
                          londonCompPlots,
                          HOPCompPlots,
                          chilbCompPlots, 
                          common.legend = TRUE,
                          legend = "bottom")

ggsave(paste0("G:/My Drive/Experiments/DEFRA/hysplit/seasonal_mvr/composition-groupedsites-nh4.png"),
       plot = comp_mvr_out,
       width = 19.8, height = 14.8, units = "in",
       bg = "white")


###############################################################################
#
# BAM
#
###############################################################################
# Summer JJA
DATEJJA1 = "2022-05-20"
DATEJJA2 = "2022-08-30"

# Autumn SON
DATESON1 = "2022-09-01"
DATESON2 = "2022-11-30"

# Winter DJF
DATEDJF1 = "2022-12-01"
DATEDJF2 = "2023-02-28"

#Spring MAM
DATEMAM1 = "2023-03-01"
DATEMAM2 = "2023-05-31"



dateMat <- matrix(c(DATEJJA1,DATEJJA2,DATESON1,DATESON2,
                    DATEDJF1,DATEDJF2,DATEMAM1,DATEMAM2,
                    DATEJJA1, DATEMAM2),
                  ncol = 2, byrow = TRUE)


maqsBAMEQ <- read_csv("G:/My Drive/Experiments/defra/data/bam/maqs.csv") %>% 
  mutate(date = parse_date_time(date, orders = "%Y-%m-%d")) %>% # converts date to datetime
  mutate(across(!date, as.numeric),
         deltaRefBAM = bam_corr - psol,
         deltadelta_entire = bam_corr - mean(deltaRefBAM,na.rm = TRUE),
         season = case_when(
           date >= ymd(DATEJJA1) & date <= ymd(DATEJJA2) ~ "JJA",
           date >= ymd(DATESON1) & date <= ymd(DATESON2) ~ "SON",
           date >= ymd(DATEDJF1) & date <= ymd(DATEDJF2) ~ "DJF",
           date >= ymd(DATEMAM1) & date <= ymd(DATEMAM2) ~ "MAM",
           TRUE ~ NA_character_  # For dates that don't fall into these ranges
         )) %>%
  filter(!is.na(season)) %>%
  # Group by season
  group_by(season) %>%
  # Calculate the average across data columns
  mutate(ref_avg_season = median(psol, na.rm = TRUE),
         bam_uncorr_avg_season = median(bam_uncorr, na.rm = TRUE),
         bam_corr_avg_season = median(bam_corr, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(deltadeltaSeason = bam_corr - (bam_corr_avg_season-ref_avg_season)) %>% 
  filter(complete.cases(.))
                   

# Note that changing deltadelta affects the intercept on a seasonal basis,
# it doesn't affect the regression. However, it will affect the overall
# terms when considered across the entire year, to a small extent.  

maqsEqFullSeasons <- maqsBAMEQ %>% 
  rename(deltadelta = deltadelta_entire)

test <- eqMVRLoc(locEq = maqsEqFullSeasons, 
                 locBox = maqs, 
                 DATES= dateMat, 
                 TAG = "BAM-totalMean")



maqsSeasonAvg <- maqsBAMEQ %>% 
  rename(deltadelta = deltadeltaSeason)


test2<-   eqMVRLoc(locEq = maqsSeasonAvg,
                   locBox = maqs, DATES= dateMat, TAG = "BAM-seasonalMean")




chilbBAMEQ <- read_csv("G:/My Drive/Experiments/defra/data/bam/chilb.csv") %>% 
  mutate(date = parse_date_time(date, orders = "%Y-%m-%d")) %>% # converts date to datetime
  mutate(across(!date, as.numeric),
         deltaRefBAM = bam_corr - digitel,
         deltadelta_entire = bam_corr - mean(deltaRefBAM,na.rm = TRUE),
         season = case_when(
           date >= ymd(DATEJJA1) & date <= ymd(DATEJJA2) ~ "JJA",
           date >= ymd(DATESON1) & date <= ymd(DATESON2) ~ "SON",
           date >= ymd(DATEDJF1) & date <= ymd(DATEDJF2) ~ "DJF",
           date >= ymd(DATEMAM1) & date <= ymd(DATEMAM2) ~ "MAM",
           TRUE ~ NA_character_  # For dates that don't fall into these ranges
         )) %>%
  filter(!is.na(season)) %>%
  # Group by season
  group_by(season) %>%
  # Calculate the average across data columns
  mutate(ref_avg_season = median(digitel, na.rm = TRUE),
         bam_uncorr_avg_season = median(bam_uncorr, na.rm = TRUE),
         bam_corr_avg_season = median(bam_corr, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(deltadeltaSeason = bam_corr - (bam_corr_avg_season-ref_avg_season)) %>% 
  filter(complete.cases(.))



chilbEqFullSeasons <- chilbBAMEQ %>% 
  rename(deltadelta = deltadelta_entire)

test <- eqMVRLoc(locEq = chilbEqFullSeasons, 
                 locBox = chilb, 
                 DATES= dateMat, 
                 TAG = "BAM-totalMean")



chilbSeasonAvg <- chilbBAMEQ %>% 
  rename(deltadelta = deltadeltaSeason)


test2<-   eqMVRLoc(locEq = chilbSeasonAvg,
                   locBox = chilb, 
                   DATES= dateMat, 
                   TAG = "BAM-seasonalMean")



