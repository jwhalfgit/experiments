library(xts)
library(dplyr)
library(openair)
library(lubridate)
library(ggplot2)
library(scales)
library(tidyverse)
library(ssh)



BLEACHDATADIR <- file.path(EXPTDIR,"BLEACH","isorropia")

setwd(BLEACHDATADIR)

# bleachData <- read_csv(file.path(BLEACHDATADIR, "input_obs.csv")) %>% 
#   select(!contains(c("Bulk","Coarse","Fine"))) 
# bleachData[bleachData <0] <- 0

# bleachData <- read_csv(file.path(BLEACHDATADIR, "input_model.csv")) %>% 
#   mutate(rh = qair2rh(sh/1000,temp = temp-273.15)*100)
# bleachData[bleachData <0] <- 0

#gc <- "GEOSCHEM_OUTPUT_BLEACH_SULFATE_CONCURRENT_AEROSOL_GAS_MET_UTC_DOWNSAMPLED_WNH3NH4.csv"
#gc <- "GEOSCHEM_OUTPUT_BLEACH_HNO3_CONCURRENT_AEROSOL_GAS_MET_UTC_DOWNSAMPLED_WNH3NH4.csv"
gc <- "GEOSCHEM_OUTPUT_CONCURRENT_AEROSOL_GAS_MET_UTC_DOWNSAMPLED_WNH3NH4.csv"

obs <- "BLEACH_WINTER_OBSERVATIONS_CONCURRENT_AEROSOL_GAS_MET_UTC_DOWNSAMPLED_WNH3NH4.csv"

# this should work for the model df's...
bleachData <- read_csv(file.path(BLEACHDATADIR, gc)) %>% 
  select(!contains(c("Bulk","Coarse","...1"))) %>% 
  rename(temp = `Temperature / K`,
         sh = `Specific humidity / g kg-1`) %>% 
  mutate(rh = qair2rh(sh / 1000, temp = temp-273.15)*100)
bleachData[bleachData <0] <- 0
names(bleachData) <- gsub(pattern = "Fine mode ",
                          replacement = "",
                          x = names(bleachData))
names(bleachData) <- gsub(pattern = " \\(ug/m3\\)",
                          replacement = "",
                          x = names(bleachData))



# and this should work for the obs df's...
bleachData <- read_csv(file.path(BLEACHDATADIR, obs)) %>% 
  select(!contains(c("Bulk","Coarse","...1"))) %>% 
  rename(temp = `Temperature / K`,
         rh = `Relative humidity / %`)
bleachData[bleachData <0] <- 0
names(bleachData) <- gsub(pattern = "Fine mode ",
                          replacement = "",
                          x = names(bleachData))
names(bleachData) <- gsub(pattern = " \\(ug/m3\\)",
                          replacement = "",
                          x = names(bleachData))






TIME1 = ymd("2023-01-26")
TIME2 = ymd("2023-02-11")


dfMaster <- bleachData %>% 
  filter(between(timestamp, TIME1,TIME2)) %>%
  mutate( nDensAir = (1*100 * 6.022e23/8.314/temp)) %>%  # press is 1mbar converted to 100 Pa
  drop_na() %>% 
  mutate(TSO4 = sulfate) %>%
  mutate(TNH3 = ammonium + NH3) %>%
  mutate(TNO3 = nitrate + HNO3) %>%
  mutate(TCl = chloride + HCl) %>%
  mutate(temp = temp) %>% 
  # mutate(hcl_umol_m3 = hcl*10^-9 * nDensAir /6.02e23*10^6) %>%
  # mutate(pcl_umol_m3 = pcl / 35.45) %>%
  # mutate(nh3_umol_m3 = nh3 *10^-9 * nDensAir /6.02e23*10^6) %>%
  # mutate(pnh4_umol_m3 = pnh4 / (1.008*4 + 14.01)) %>%
  # mutate(hno3_umol_m3 = hno3 *10^-9 * nDensAir /6.02e23*10^6) %>%
  # mutate(pno3_umol_m3 = pno3 / (14.01 + 16*3)) %>%
  # mutate(pk_umol_m3 = pk / 39.0983) %>%
  # mutate(pca_umol_m3 = pca / 40.078) %>%
  # mutate(TSO4 = pso4/(32.08 + 16*4)) %>%
  # mutate(TNH3 = pnh4_umol_m3 + nh3_umol_m3) %>%
  # mutate(TNO3 = pno3_umol_m3 + hno3_umol_m3) %>%
  # mutate(TCl = pcl_umol_m3 + hcl_umol_m3) %>%
  relocate(rh, .after = TCl) %>% 
  relocate(temp, .after = rh) %>% 
  replace(. < 0, 0)

dfMaster$timestamp <- as.character(format(dfMaster$timestamp))


fn <-  paste("G:/My Drive/Experiments/BLEACH/isorropia/inputfiles/",
             gsub("-","",TIME1),"-",gsub("-","",TIME2),"ug-BLEACH-obs",
             sep = "")

write.csv(dfMaster,paste0(fn,".csv"),quote=FALSE,row.names = FALSE)


# Create and write ISORROPIA Input ---------------------------------------

outDF <- data.frame("Na" = dfMaster$sodium,
                    #"Na" = dfMaster$pna * 0.397/0.306,
                    "SO4" = dfMaster$TSO4,
                    #"NH3" = 0,
                    "NH3" = dfMaster$TNH3,
                    "NO3" = dfMaster$TNO3,
                    "Cl" = dfMaster$TCl,
                    #"Ca" = 0,
                    "Ca" = dfMaster$calcium, 
                    "K" = 0,
                    #"K" = dfMaster$pk_umol_m3, 
                    "Mg" = dfMaster$magnesium,
                    #"Mg" = 0,
                    "RH" = dfMaster$rh/100,
                    "TEMP" = dfMaster$temp)

outDF <- round(outDF, digits = 5)


fileConn <- file(paste0(fn,".INP"), "w")
writeLines(c("Input units (0=umol/m3, 1=ug/m3) ; sample input file",
             "1",
             "",
             "Problem type (0=forward, 1=reverse); Phase state (0=solid+liquid, 1=metastable)",
             "0, 1",
             ""),sep = "\n", fileConn)

write.table(outDF, fileConn, quote= FALSE, row.names = FALSE, sep = "\t",
            append = TRUE)

close(fileConn)



# Copy ISORROPIA Input to Viking ------------------------------------------
session = ssh_connect("jh2949@viking.york.ac.uk")
viking_location = paste0("./scratch/isorropia/")
fp <- paste0(fn,".INP")
scp_upload(session, fp, to = viking_location)
ssh_disconnect(session)









# Retrieve ISORROPIA output from Viking -----------------------------------
session = ssh_connect("jh2949@viking.york.ac.uk")
viking_location = paste0("./scratch/isorropia/")
local_destination = file.path(sub("input", "output", fn))
dir.create(local_destination)

scp_download(session, paste0(viking_location,basename(fn),".dat"), to = local_destination)
scp_download(session, paste0(viking_location,basename(fn),".txt"), to = local_destination)

ssh_disconnect(session)


# Re-attach timestamps to ISORROPIA output --------------------------------
# load input dataframe (may already be loaded as dfMaster if working under
# one workflow

dfMasterTime <- read_csv(paste0(fn, ".csv"),col_select = 1)

fpOut <- file.path(sub("input", "output", fn), paste0(basename(fn),".dat"))
modelOutputNames <- read.table(fpOut,sep="",nrow=1)
model <- read.table(fpOut,sep = "", strip.white = TRUE,skip = 1)
names(model) <- modelOutputNames
model$CASE <- sub("\\s+$", "", model$CASE) # remove trailing whitespace
# The default ISORROPIA output is in units of ug m3.  I will
# convert some columns back to umol/m3, but will include units
# to make it explicit.
model <- as_tibble(cbind(dfMasterTime, model)) %>% 
  mutate(GHCL_umol = GHCL /36.458,
         GHNO3_umol = GHNO3 / 63.018,
         GNH3_umol = GNH3 / 17.034,
         CLLIQ_umol = CLLIQ / 35.45,
         NH4LIQ_umol = NH4LIQ / 18.33,
         NO3LIQ_umol = NO3LIQ / 62.01,
         NALIQ_umol = NALIQ / 22.99,
         NH4LIQ_umol = NH4LIQ / (14.01 + 1.008*4),
         CaLIQ_umol = CaLIQ / (40.078),
         KLIQ_umol = KLIQ / 39.098,
         MgLIQ_umol = MgLIQ / 24.3,
         SO4LIQ_umol = SO4LIQ / (48+32.068),
         HLIQ_mol = as.numeric(HLIQ) / 1.008 / 1000,
         WATER_mol = WATER / 18.02 / 1000,
         ph = -log10(HLIQ_mol / WATER_mol * 55.55))

model$ts <- as.character(format(model$timestamp))

write.csv(model, sub(".dat",".csv",fpOut), quote = FALSE, row.names = FALSE)



