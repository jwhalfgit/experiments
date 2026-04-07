library(xts)
library(dplyr)
library(openair)
library(lubridate)
library(ggplot2)
library(scales)
library(tidyverse)
library(ssh)
library(plotly)
library(geomtextpath)
library(RColorBrewer)
library(metR)

OSCADATADIR <- file.path(EXPTDIR,"osca","data")

setwd(OSCADATADIR)

# load your data.  At present, let's just load all the data (files aren't that big).
s_dfMaster <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/inputfiles/20210611-20210721dfMaster.csv")
s_dfMarga <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/inputfiles/20210611-20210721umol-1hr-marga-pm25.csv")

w_dfMaster <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/inputfiles/20220205-20220221dfMaster.csv")
w_dfMarga <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/inputfiles/20220205-20220221-1hr-marga-pm25.csv")

all_dfMarga <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/marga/marga_all_2020-2024_4iso.csv")

calnex_dfMaster <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/calnex/calnex-carbAnions.csv")

hk_dfMaster  <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/hk/hkust-forIso.csv") 
yl_dfMaster <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/yl/yl-forIso.csv")

soas_dfMaster <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/soas/soas-forIso.csv")

dfModel_yl_act <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/outputfiles/20161003-20161130umol-1hr-YL-base/20161003-20161130umol-1hr-YL-base-act.csv")

# use the following function to determine dfMaster according to SEASON and SOURCE
create_dfMaster <-  function(SEASON, SOURCE){
  if(SEASON == "summer"){
    if(SOURCE == "OSCA"){
      df = s_dfMaster
    }else if(SOURCE == "MARGA"){
      df = s_dfMarga
    }
    
  }else if(SEASON == "winter"){
    if(SOURCE == "OSCA"){
      df = w_dfMaster
    }else if(SOURCE == "MARGA"){
      df = w_dfMarga
    }
    
  }else if(SEASON == "other"){
    if(SOURCE == "HK"){
      df = hk_dfMaster
    }else if(SOURCE == "YL"){
      df = yl_dfMaster
    }else if(SOURCE == "SOAS"){
      df = soas_dfMaster
    }else if(SOURCE == "CALNEX"){
      df = calnex_dfMaster
    }else if(SOURCE == "MARGA"){
      df = all_dfMarga
    }else{
      return(print("SOURCE value is unknown.  Please check you've 
                   used the right abbreviation."))
    }
  }
  return(df)
}




generateISOinput <- function(SEASON, SOURCE, fn_tag = NULL){
  if(is.null(fn_tag)){
    return(print("Give the argument fn_tag a descriptive character string"))
  }
  
  dfMaster = create_dfMaster(SEASON,SOURCE)
    
  if(SEASON == "summer"){
    TIME1 = ymd_hms("2021-06-11 00:00:00")
    TIME2 = ymd_hms("2021-07-21 00:00:00")
    
  }else if(SEASON == "winter"){
    TIME1 = ymd_hms("2022-02-05 00:00:00")
    TIME2 = ymd_hms("2022-02-21 00:00:00")
    
  }else if(SEASON == "other"){
    TIME1 = as.Date(dfMaster$date[1])
    TIME2 = as.Date(dfMaster$date[length(dfMaster$date)])
  }
  
  
  if(SOURCE == "OSCA"){
    outDF <- data.frame(
      "Na" = dfMaster$pcl_2_5_umol_m3,
      "SO4" = dfMaster$pso4_umol_m3,
      "NH3" = dfMaster$nh3_umol_m3 + dfMaster$pnh4_umol_m3,
      "NO3" = dfMaster$pno3_umol_m3 + dfMaster$hno3_umol_m3,
      "Cl" = dfMaster$hcl_umol_m3 + dfMaster$pcl_umol_m3 + dfMaster$pcl_2_5_umol_m3,
      "Ca" = dfMaster$pca_umol_m3,
      "K" = dfMaster$pk_umol_m3, 
      "Mg" = 0,
      "RH" = dfMaster$rh/100,
      "TEMP" = dfMaster$tempK)

  }else if(SOURCE == "MARGA"){
    outDF <- data.frame(
      "Na" = dfMaster$pm25na_umol,
      "SO4" = dfMaster$pm25so4_umol,
      "NH3" = dfMaster$pm25nh4_umol+dfMaster$nh3_umol,
      "NO3" = dfMaster$pm25no3_umol+dfMaster$hno3_umol,
      "Cl" = dfMaster$pm25cl_umol+dfMaster$hcl_umol,
      "Ca" = dfMaster$pm25ca_umol,
      "K" = dfMaster$pm25k_umol,
      "Mg" = dfMaster$pm25mg_umol,
      "RH" =  dfMaster$rh/100,
      "TEMP" =dfMaster$temp)

    # HK = Hong Kong,
    # YL = ???
  }else if(SOURCE == "HK" | SOURCE == "YL" | SOURCE == "SOAS"){
    outDF <- data.frame(
      "Na" = dfMaster$pna_umol,
      "SO4" = dfMaster$pso4_umol,
      "NH3" = dfMaster$pnh4_umol+dfMaster$nh3_umol,
      "NO3" = dfMaster$pno3_umol+dfMaster$hno3_umol,
      "Cl" = dfMaster$pcl_umol+dfMaster$hcl_umol,
      "Ca" = dfMaster$pca_umol,
      "K" = dfMaster$pk_umol,
      "Mg" = dfMaster$pmg_umol,
      "RH" =  dfMaster$rh,
      #"RH" =  dfModel_yl_act$DRKCL,
      "TEMP" =dfMaster$temp)
    
  }else if(SOURCE == "CALNEX"){
    outDF <- data.frame(
      "Na" = dfMaster$pm25na_umol,
      "SO4" = dfMaster$pm25so4_umol,
      "NH3" = dfMaster$pm25nh4_umol+dfMaster$nh3_umol,
      "NO3" = dfMaster$pm25no3_umol+dfMaster$hno3_umol,
      "Cl" = dfMaster$pm25cl_umol+dfMaster$hcl_umol,
      "Ca" = 0,
      "K" = dfMaster$pm25k_umol,
      "Mg" = 0,
      "RH" = dfMaster$rh/100,
      #"RH" = rhConcs[ix]/100,
      "TEMP" =dfMaster$temp)
  }
  outDF <- round(outDF, digits = 5)
  
  
  # Create and write ISORROPIA Input ---------------------------------------
  inputdir <-  paste("G:/My Drive/Experiments/OSCA/isorropia/wes/inputfiles",
                     sep = "")
  fn <- file.path(inputdir, paste0(gsub("-","",TIME1),"-",gsub("-","",TIME2),
                                   "umol-1hr-",SOURCE,"-",fn_tag))
  
  if(file.exists(paste0(fn,".INP"))){
    print("WARNING: This file already exists, printing fn anyway")
    return(print(fn))
  }
  
  
  fileConn <- file(paste0(fn,".INP"), "w")
  writeLines(c("Input units (0=umol/m3, 1=ug/m3) ; sample input file",
               "0",
               "",
               "Problem type (0=forward, 1=reverse); Phase state (0=solid+liquid, 1=metastable)",
               "0, 1",
               ""),sep = "\n", fileConn)
  
  write.table(outDF, fileConn, quote= FALSE, row.names = FALSE, sep = "\t",
              append = TRUE)
  
  close(fileConn)
  
  
  return(print(fn))
}



generateISOinput_Rev <- function(SEASON, SOURCE, fn_tag = NULL){
  if(is.null(fn_tag)){
    return(print("Give the argument fn_tag a descriptive character string"))
  }
  
  dfMaster = create_dfMaster(SEASON,SOURCE)
  
  if(SEASON == "summer"){
    TIME1 = ymd_hms("2021-06-11 00:00:00")
    TIME2 = ymd_hms("2021-07-21 00:00:00")
    
  }else if(SEASON == "winter"){
    TIME1 = ymd_hms("2022-02-05 00:00:00")
    TIME2 = ymd_hms("2022-02-21 00:00:00")
    
  }else if(SEASON == "other"){
    TIME1 = as.Date(dfMaster$date[1])
    TIME2 = as.Date(dfMaster$date[length(dfMaster$date)])
  }
  
  
  if(SOURCE == "OSCA"){
    outDF <- data.frame(
      "Na" = dfMaster$pcl_2_5_umol_m3,
      "SO4" = dfMaster$pso4_umol_m3,
      "NH3" = dfMaster$pnh4_umol_m3,
      "NO3" = dfMaster$pno3_umol_m3,
      "Cl" = dfMaster$pcl_umol_m3 + dfMaster$pcl_2_5_umol_m3,
      "Ca" = dfMaster$pca_umol_m3,
      "K" = dfMaster$pk_umol_m3, 
      "Mg" = 0,
      "RH" = dfMaster$rh/100,
      "TEMP" = dfMaster$tempK)
    
  }else if(SOURCE == "MARGA"){
    outDF <- data.frame(
      "Na" = dfMaster$pm25na_umol,
      "SO4" = dfMaster$pm25so4_umol,
      "NH3" = dfMaster$pm25nh4_umol,
      "NO3" = dfMaster$pm25no3_umol,
      "Cl" = dfMaster$pm25cl_umol,
      "Ca" = dfMaster$pm25ca_umol,
      "K" = dfMaster$pm25k_umol,
      "Mg" = dfMaster$pm25mg_umol,
      "RH" =  dfMaster$rh/100,
      "TEMP" =dfMaster$temp)
    
    # HK = Hong Kong,
    # YL = ???
  }else if(SOURCE == "HK" | SOURCE == "YL" | SOURCE == "SOAS"){
    outDF <- data.frame(
      "Na" = dfMaster$pna_umol,
      "SO4" = dfMaster$pso4_umol,
      "NH3" = dfMaster$pnh4_umol,
      "NO3" = dfMaster$pno3_umol,
      "Cl" = dfMaster$pcl_umol,
      "Ca" = dfMaster$pca_umol,
      "K" = dfMaster$pk_umol,
      "Mg" = dfMaster$pmg_umol,
      "RH" =  dfMaster$rh,
      #"RH" =  dfModel_yl_act$DRKCL,
      "TEMP" =dfMaster$temp)
    
  }else if(SOURCE == "CALNEX"){
    outDF <- data.frame(
      "Na" = dfMaster$pm25na_umol,
      "SO4" = dfMaster$pm25so4_umol,
      "NH3" = dfMaster$pm25nh4_umol,
      "NO3" = dfMaster$pm25no3_umol,
      "Cl" = dfMaster$pm25cl_umol,
      "Ca" = 0,
      "K" = dfMaster$pm25k_umol,
      "Mg" = 0,
      "RH" = dfMaster$rh/100,
      #"RH" = rhConcs[ix]/100,
      "TEMP" =dfMaster$temp)
  }
  outDF <- round(outDF, digits = 5)
  
  
  # Create and write ISORROPIA Input ---------------------------------------
  inputdir <-  paste("G:/My Drive/Experiments/OSCA/isorropia/wes/inputfiles",
                     sep = "")
  fn <- file.path(inputdir, paste0(gsub("-","",TIME1),"-",gsub("-","",TIME2),
                                   "umol-1hr-REVERSE",SOURCE,"-",fn_tag))
  
  if(file.exists(paste0(fn,".INP"))){
    print("WARNING: This file already exists, printing fn anyway")
    return(print(fn))
  }
  
  
  fileConn <- file(paste0(fn,".INP"), "w")
  writeLines(c("Input units (0=umol/m3, 1=ug/m3) ; sample input file",
               "0",
               "",
               "Problem type (0=forward, 1=reverse); Phase state (0=solid+liquid, 1=metastable)",
               "1, 1",
               ""),sep = "\n", fileConn)
  
  write.table(outDF, fileConn, quote= FALSE, row.names = FALSE, sep = "\t",
              append = TRUE)
  
  close(fileConn)
  
  
  return(print(fn))
}


generateISOinputIteration <- function(SEASON, SOURCE, fn_tag = NULL){
  if(is.null(fn_tag)){
    return(print("Give the argument fn_tag a descriptive character string"))
  }
  
  outList <- list()
  outListIx <- 1
  
  dfMaster = create_dfMaster(SEASON,SOURCE)
  
  if(SEASON == "summer"){
    TIME1 = ymd_hms("2021-06-11 00:00:00")
    TIME2 = ymd_hms("2021-07-21 00:00:00")
    
  }else if(SEASON == "winter"){
    TIME1 = ymd_hms("2022-02-05 00:00:00")
    TIME2 = ymd_hms("2022-02-21 00:00:00")
    
  }else if(SEASON == "other"){
    TIME1 = as.Date(dfMaster$date[1])
    TIME2 = as.Date(dfMaster$date[length(dfMaster$date)])
  }
  
  rhConcs <- c(round(seq(10,80,5)),round(seq(80,100,1)))
  
  for(ix in 1: length(rhConcs)){
    if(SOURCE == "OSCA"){
      outDF <- data.frame(
        "Na" = dfMaster$pcl_2_5_umol_m3,
        "SO4" = dfMaster$pso4_umol_m3,
        "NH3" = dfMaster$nh3_umol_m3 + dfMaster$pnh4_umol_m3,
        "NO3" = dfMaster$pno3_umol_m3 + dfMaster$hno3_umol_m3,
        "Cl" = dfMaster$hcl_umol_m3 + dfMaster$pcl_umol_m3 + dfMaster$pcl_2_5_umol_m3,
        "Ca" = dfMaster$pca_umol_m3,
        "K" = dfMaster$pk_umol_m3, 
        "Mg" = 0,
        #"RH" = dfMaster$rh/100,
        "RH" = rhConcs[ix]/100,
        "TEMP" = dfMaster$tempK)
      
    }else if(SOURCE == "MARGA"){
      outDF <- data.frame(
        "Na" = dfMaster$pm25na_umol,
        "SO4" = dfMaster$pm25so4_umol,
        "NH3" = dfMaster$pm25nh4_umol+dfMaster$nh3_umol,
        "NO3" = dfMaster$pm25no3_umol+dfMaster$hno3_umol,
        "Cl" = dfMaster$pm25cl_umol+dfMaster$hcl_umol,
        "Ca" = dfMaster$pm25ca_umol,
        "K" = dfMaster$pm25k_umol,
        "Mg" = dfMaster$pm25mg_umol,
        #"RH" = dfMaster$rh/100,
        "RH" = rhConcs[ix]/100,
        "TEMP" =dfMaster$temp)
      
      # HK = Hong Kong,
      # YL = ???
    }else if(SOURCE == "HK" | SOURCE == "YL"| SOURCE == "SOAS"){
      outDF <- data.frame(
        "Na" = dfMaster$pna_umol,
        "SO4" = dfMaster$pso4_umol,
        "NH3" = dfMaster$pnh4_umol+dfMaster$nh3_umol,
        "NO3" = dfMaster$pno3_umol+dfMaster$hno3_umol,
        "Cl" = dfMaster$pcl_umol+dfMaster$hcl_umol,
        "Ca" = dfMaster$pca_umol,
        "K" = dfMaster$pk_umol,
        "Mg" = dfMaster$pmg_umol,
        #"RH" =  dfMaster$rh/100,
        "RH" = rhConcs[ix]/100,
        "TEMP" =dfMaster$temp)
      
    }else if(SOURCE == "CALNEX"){
      outDF <- data.frame(
        "Na" = dfMaster$pm25na_umol,
        "SO4" = dfMaster$pm25so4_umol,
        "NH3" = dfMaster$pm25nh4_umol+dfMaster$nh3_umol,
        "NO3" = dfMaster$pm25no3_umol+dfMaster$hno3_umol,
        "Cl" = dfMaster$pm25cl_umol+dfMaster$hcl_umol,
        "Ca" = 0,
        "K" = dfMaster$pm25k_umol,
        "Mg" = 0,
        #"RH" = dfMaster$rh/100,
        "RH" = rhConcs[ix]/100,
        "TEMP" =dfMaster$temp)
    }
    outDF <- round(outDF, digits = 5)
    
    outList[[outListIx]] <- outDF
    
    outListIx <- outListIx +1
  }
  outDF <- do.call(rbind,outList)
  
  
  # Create and write ISORROPIA Input ---------------------------------------
  inputdir <-  paste("G:/My Drive/Experiments/OSCA/isorropia/wes/iterations/",
                     sep = "")
  fn <- file.path(inputdir, paste0(gsub("-","",TIME1),"-",gsub("-","",TIME2),
                                   "umol-1hr-",SOURCE,"-",fn_tag))
  
  if(file.exists(paste0(fn,".INP"))){
    print("WARNING: This file already exists, printing fn anyway")
    return(print(fn))
  }
  
  
  fileConn <- file(paste0(fn,".INP"), "w")
  writeLines(c("Input units (0=umol/m3, 1=ug/m3) ; sample input file",
               "0",
               "",
               "Problem type (0=forward, 1=reverse); Phase state (0=solid+liquid, 1=metastable)",
               "0, 1",
               ""),sep = "\n", fileConn)
  
  write.table(outDF, fileConn, quote= FALSE, row.names = FALSE, sep = "\t",
              append = TRUE)
  
  close(fileConn)
  
  
  return(print(fn))
}
# Re-attach timestamps to ISORROPIA output --------------------------------
# load input dataframe (may already be loaded as dfMaster if working under
# one workflow



createModelOutCSV <- function(FN = fn){
  
  fpOut <- file.path(sub("input", "output", fn), paste0(basename(fn),".dat"))
  modelOutputNames <- read.table(fpOut,sep="",nrow=1)
  model <- read.table(fpOut,sep = "", strip.white = TRUE,skip = 1)
  names(model) <- modelOutputNames
  model$CASE <- sub("\\s+$", "", model$CASE) # remove trailing whitespace
  # The default ISORROPIA output is in units of ug m3.  I will
  # convert some columns back to umol/m3, but will include units
  # to make it explicit.
  model <- as_tibble(cbind(dfMaster$date, model)) %>% 
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
           CLTOT_umol = CLTOT / 35.45,
           CNACL_umol = CNACL / (22.99 + 35.45),
           CNH4CL_umol = CNH4CL / (14.01 + 4*1.008 + 35.45),
           CCACL2_umol = CCACL2 / (110.98),
           CKCL_umol = CKCL / 74.5513,
           CMGCL2_umol= CMGCL2 / 95.211,
           NATOT_umol = NATOT / 22.99,
           SO4TOT_umol = SO4TOT / 96.06,
           NH4TOT_umol = NH4TOT / 18.04,
           NO3TOT_umol = NO3TOT / 62,
           CATOT_umol = CATOT / 40.078,
           KTOT_umol = KTOT / 39.0983,
           MGTOT_umol = MGTOT/24.305,
           
           # Sodium compounds
           CNACL_umol = CNACL / (22.99 + 35.45),   # NaCl
           CNANO3_umol = CNANO3 / (22.99 + 3*16 + 14.01),  # NaNO₃
           CNA2SO4_umol = CNA2SO4 / (2*22.99 + 32.06 + 4*16),  # Na₂SO₄
           CNAHSO4_umol = CNAHSO4 / (22.99 + 32.06 + 4*16 + 1.008),  # NaHSO₄
           CNH4CL_umol = CNH4CL / (14.01 + 4*1.008 + 35.45),  # NH₄Cl
           CNH4NO3_umol = CNH4NO3 / (14.01 + 4*1.008 + 14.01 + 3*16),  # NH₄NO₃
           CNH42S4_umol = CNH42S4 / (2*(14.01 + 4*1.008) + 32.06 + 4*16),  # (NH₄)₂SO₄
           CNH4HS4_umol = CNH4HS4 / ((14.01 + 4*1.008) + 32.06 + 4*16 + 1.008),  # NH₄HSO₄
           
           # Calcium compounds
           CCASO4_umol = CCASO4 / (40.08 + 32.06 + 4*16),  # CaSO₄
           CCANO32_umol = CCANO32 / (40.08 + 2*14.01 + 6*16),  # Ca(NO₃)₂
           CCACL2_umol = CCACL2 / (40.08 + 2*35.45),  # CaCl₂
           
           # Potassium compounds
           CK2SO4_umol = CK2SO4 / (2*39.09 + 32.06 + 4*16),  # K₂SO₄
           CKHSO4_umol = CKHSO4 / (39.09 + 32.06 + 4*16 + 1.008),  # KHSO₄
           CKNO3_umol = CKNO3 / (39.09 + 3*16 + 14.01),  # KNO₃
           CKCL_umol = CKCL / (39.09 + 35.45),  # KCl
           
           # Magnesium compounds
           CMGSO4_umol = CMGSO4 / (24.305 + 32.06 + 4*16),  # MgSO₄
           CMGNO32_umol = CMGNO32 / (24.305 + 2*(14.01 + 3*16)),  # Mg(NO₃)₂
           CMGCL2_umol = CMGCL2 / (24.305 + 2*35.45),  # MgCl₂
           
           # Liquid phase compounds
           HLIQ_umol = HLIQ / 1.008,  # H⁺
           NALIQ_umol = NALIQ / 22.99,  # Na⁺
           NH4LIQ_umol = NH4LIQ / (14.01 + 4*1.008),  # NH₄⁺
           CLLIQ_umol = CLLIQ / 35.45,  # Cl⁻
           SO4LIQ_umol = SO4LIQ / (32.06 + 4*16),  # SO₄²⁻
           HSO4LIQ_umol = HSO4LIQ / (32.06 + 4*16 + 1.008),  # HSO₄⁻
           NO3LIQ_umol = NO3LIQ / (14.01 + 3*16),  # NO₃⁻
           CaLIQ_umol = CaLIQ / 40.08,  # Ca²⁺
           KLIQ_umol = KLIQ / 39.09,  # K⁺
           MgLIQ_umol = MgLIQ / 24.305,  # Mg²⁺
           
           # Aerosol compounds
           NH4AER_umol = NH4AER / (14.01 + 3*1.008),  # NH3⁺
           CLAER_umol = CLAER / (35.45+1.008),  # HCl
           NO3AER_umol = NO3AER / (14.01 + 3*16+1.008), # HNO3
           HLIQ_umol = HLIQ / 1.008,
           WATER_umol = WATER / 18.02,
           ph = -log10(HLIQ_umol / WATER_umol * 55.55)) 
  names(model)[1] <- "date"
  
  model$date <- as.character(format(model$date))
  
  write.csv(model, sub(".dat",".csv",fpOut), quote = FALSE, row.names = FALSE)
  return(model)
}
 


# using generateISOinput will generate the ISORROPIA input into the "inputfiles"
# folder and return the filepath for that to use in the below code for uploading
# to Viking
season = "summer"
src = "OSCA"

dfMaster <- create_dfMaster(SEASON=season,SOURCE = src)
# 
fn <- generateISOinput(SEASON = season,
                       SOURCE = src,
                       fn_tag = "lowerVol-90red")

# fn <- generateISOinputIteration(SEASON = season,
#                                 SOURCE = src,
#                                 fn_tag = "rhIter-1")

# Copy ISORROPIA Input to Viking ------------------------------------------
session = ssh_connect("jh2949@viking.york.ac.uk")
viking_location = paste0("./scratch/wes_isorropia/")
fp <- paste0(fn,".INP")
ssh_exec_wait(session, command = paste0("cd ",viking_location,"; rm my_*"))
ssh_exec_wait(session, command = paste0("cd ",viking_location,"; rm 20*"))
ssh_exec_wait(session, command = paste0("cd ",viking_location,"; rm act*"))
scp_upload(session, fp, to = viking_location)
ssh_exec_wait(session, command = paste0("cd ",viking_location,"; sbatch jobscript.job"))





# Retrieve ISORROPIA output from Viking -----------------------------------
local_destination = file.path(sub("input", "output", fn))
dir.create(local_destination)

scp_download(session, paste0(viking_location,basename(fn),".dat"), to = local_destination)
scp_download(session, paste0(viking_location,basename(fn),".txt"), to = local_destination)
scp_download(session, paste0(viking_location,"act.dat"), to = local_destination)
ssh_exec_wait(session, command = paste0("cd ",viking_location,"; rm my_*"))
ssh_exec_wait(session, command = paste0("cd ",viking_location,"; rm 20*"))
ssh_exec_wait(session, command = paste0("cd ",viking_location,"; rm act*"))

ssh_disconnect(session)

 
model = createModelOutCSV(FN = fn)
fpOut <- file.path(sub("input", "output", fn))

act <- read_table(file.path(fpOut,"act.dat"))
bigTable <- cbind(model, act)
write.csv(bigTable, paste0(fpOut,"/",basename(fpOut),"-act.csv"), 
          quote = FALSE, row.names = FALSE)



############################################################################################


forPlot_yl <- dfMaster_yl %>% 
  left_join(model, by = join_by(date))
forPlot_yl$date <- as.character(format(forPlot_yl$date))
write.csv(forPlot_yl,sub(".dat","-forPlot.csv",fpOut), quote = FALSE,
            row.names = FALSE)


forPlot_hk <- dfMaster_hk %>% 
  left_join(model, by = join_by(date))
forPlot_hk$date <- as.character(format(forPlot_hk$date))
write.csv(forPlot_hk,sub(".dat","-forPlot.csv",fpOut), quote = FALSE,
          row.names = FALSE)


forPlot_marga <- model%>% 
  mutate(date = ymd_hms(date)) %>% 
  left_join(all_dfMarga, by = join_by(date))
forPlot_marga$date <- as.character(format(forPlot_marga$date))
write.csv(forPlot_marga,paste0(fpOut,"/",basename(fpOut),"-forPlot.csv"), quote = FALSE,
          row.names = FALSE)

#
#
#
#
#
##
#
#

#
#
#
#

# dfMaster_soas  <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/soas/soas.csv") 
# dfMaster_soasWeather <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/soas/soas-weather.csv") 
# 
# dfMaster_soas <- dfMaster_soas %>% 
#   left_join(dfMaster_soasWeather) %>% 
#   mutate(temp = temp + 273.15,
#          date = floor_date(date,unit = "1 hour")) %>% 
#   group_by(date) %>% 
#   summarize_all(mean, na.rm = TRUE) %>%  
#   mutate(pca_umol = pca / 40.078,
#          pcl_umol = pcl / 35.452,
#          pk_umol = pk / 39.098,
#          pmg_umol = pmg / 24.305,
#          pna_umol = pna / 22.99,
#          pnh4_umol = pnh4 / (1.008*4+14.01),
#          pno3_umol = pno3 / (16*3 + 14.01),
#          pso4_umol = pso4 / (32.065 + 16*4),
#          hcl_umol= hcl / 36.458,
#          hno3_umol = hno3 / (1.008 + 16*3 + 14.01),
#          nh3_umol = nh3 / (14.01 + 1.008*3)) %>%
#   filter(complete.cases(.)) %>% 
#   write_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/soas/soas-forIso.csv")
# 
# 
# 
# soas_acsm <-  read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/soas/soas-acsm.csv") %>% 
#   mutate(ts_utc = ts_cdt + 60*60*5) %>% 
#   mutate(ts_utc = floor_date(ts_utc, "1 hour")) %>% 
#   group_by(ts_utc) %>% 
#   summarize_all(mean,na.rm=TRUE) %>% 
#   ungroup() %>% 
#   select(-ts_cdt) %>% 
#   relocate(ts_utc, .before = pcl) %>% 
#   rename(date = ts_utc) %>% 
#   write_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/soas/soas-acsm-forIso.csv")
# # 
# # 
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


# marga_ff <- list.files("G:/My Drive/Experiments/OSCA/isorropia/wes/marga/defra_data",
#                        pattern = ".csv", full.names = TRUE)
# 
# margaList_am <- lapply(marga_ff, read_csv, skip=10, col_select= c(1:44))
# margaDf_am<- do.call(rbind, margaList_am) %>%
#   as_tibble() %>%
#   dplyr::select(-contains("PM10"),
#                 -contains("Status")) %>%
#                 # -Status...4,
#                 # -Status...6,
#                 # -Status...8,
#                 # -Status...10,
#                 # -Status...12,
#                 # -Status...14,
#                 # -Status...16,
#                 # -Status...18) %>%
#   mutate(Date = as.Date(Date),
#          date = ymd_hms(paste(Date,Time))) %>%
#   select(-Date,-Time) %>%
#   relocate(date) %>%
#   arrange(date) %>%
#   rename(pm25ca = contains("calcium"),
#          pm25cl = contains("chloride"),
#          pm25k = contains("potassium"),
#          pm25mg = contains("magnesium"),
#          pm25na = contains("sodium"),
#          pm25nh4= contains("ammonium"),
#          pm25no3 = contains("nitrate"),
#          pm25so4 = contains("sulphate"),
#          hcl = contains("hydrochloric acid"),
#          hno3 = contains("nitric acid"),
#          nh3 = contains("ammonia"))
# # convert to numeric because it's loaded as characters for who knows what reason.
# margaDf_am[,2:ncol(margaDf_am)] <- apply(margaDf_am[,2:ncol(margaDf_am)],2,as.numeric)
# # generate mol versions of data
# margaDf_am <- margaDf_am %>% 
#   mutate(pm25ca_umol = pm25ca / 40.078,
#          pm25cl_umol = pm25cl / 35.452,
#          pm25k_umol = pm25k / 39.098,
#          pm25mg_umol = pm25mg / 24.305,
#          pm25na_umol = pm25na / 22.99,
#          pm25nh4_umol = pm25nh4 / (1.008*4+14.01),
#          pm25no3_umol = pm25no3 / (16*3 + 14.01),
#          pm25so4_umol = pm25so4 / (32.065 + 16*4),
#          hcl_umol= hcl / 36.458,
#          hno3_umol = hno3 / (1.008 + 16*3 + 14.01),
#          nh3_umol = nh3 / (14.01 + 1.008*3))
# 
# # get complete cases
# margaDf_am <- margaDf_am[complete.cases(margaDf_am),]
# # and let's save
# margaDf_am$date <- as.character(format(margaDf_am$date))
# write.csv(margaDf_am,
#           "G:/My Drive/Experiments/OSCA/isorropia/wes/marga/marga2020-2025_am.csv",
#           quote = FALSE, row.names = FALSE)
# # 
# 
# 
# margaList_chilb <- lapply(marga_ff, read_csv, skip=10, col_select= c(1,2,45:86))
# margaDf_chilb<- do.call(rbind, margaList_chilb) %>%
#   as_tibble() %>%
#   dplyr::select(-contains("PM10"),
#                 -contains("Status")) %>%
#   # -Status...4,
#   # -Status...6,
#   # -Status...8,
#   # -Status...10,
#   # -Status...12,
#   # -Status...14,
#   # -Status...16,
#   # -Status...18) %>%
#   mutate(Date = as.Date(Date),
#          date = ymd_hms(paste(Date,Time))) %>%
#   select(-Date,-Time) %>%
#   relocate(date) %>%
#   arrange(date) %>%
#   rename(pm25ca = contains("calcium"),
#          pm25cl = contains("chloride"),
#          pm25k = contains("potassium"),
#          pm25mg = contains("magnesium"),
#          pm25na = contains("sodium"),
#          pm25nh4= contains("ammonium"),
#          pm25no3 = contains("nitrate"),
#          pm25so4 = contains("sulphate"),
#          hcl = contains("hydrochloric acid"),
#          hno3 = contains("nitric acid"),
#          nh3 = contains("ammonia")) 
# # convert to numeric because it's loaded as characters for who knows what reason.
# margaDf_chilb[,2:ncol(margaDf_chilb)] <- apply(margaDf_chilb[,2:ncol(margaDf_chilb)],2,as.numeric)
# # get complete cases
# margaDf_chilb <- margaDf_chilb[complete.cases(margaDf_chilb),]
# # and let's save
# margaDf_chilb$date <- as.character(margaDf_chilb$date)
# write.csv(margaDf_chilb,
#           "G:/My Drive/Experiments/OSCA/isorropia/wes/marga/marga2020-2025_chilb.csv",
#           quote = FALSE, row.names = FALSE)
# 
# 

##############################################################################
##
#
#
#
#
#
#
#
#
#
###############################################################################
# CALNEX
hclhno3 <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/calnex/acids.csv") %>% 
  rename(date = ts,
         hcl_lod = HCl_DL,
         hcl_ppb = HCl_ppb,
         hcl_sd = HCl_stdev,
         hno3_lod = HNO3_DL,
         hno3_ppb = HNO3_ppb,
         hno3_sd = HNO3_stdev) %>% 
  mutate(date = floor_date(date, "1 hour")) %>% 
  group_by(date) %>% 
  summarize_all(mean,na.rm=TRUE)
  
# Not sure currently if the time given is the start or end time.
carbAnions <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/calnex/carb_anions.csv") %>% 
  rename(date = ts,
         pm25cl_ug_m3 = GPIC_Chloride,
         pm25no3_ug_m3 = GPIC_Nitrate,
         pm25so4_ug_m3 = GPIC_Sulfate) %>% 
  mutate(date = floor_date(date, "1 hour"))
  
anions <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/calnex/anions.csv") %>% 
  rename(date = ts,
         pm25cl_ug_m3 = Chloride_Dionex_IC_PM2_5,
         pm25no3_ug_m3 = Nitrate_Dionex_IC_PM2_5,
         pm25so4_ug_m3 = Sulfate_Dionex_IC_PM2_5)%>%
  
  mutate(pm25cl_ug_m3 = ifelse(pm25cl_ug_m3 < -0.1,NA,pm25cl_ug_m3),
         pm25no3_ug_m3 = ifelse(pm25no3_ug_m3 < -0.1,NA,pm25no3_ug_m3),
         pm25so4_ug_m3 = ifelse(pm25so4_ug_m3 < -0.1,NA,pm25so4_ug_m3))%>% 
  mutate(date = floor_date(date, "1 hour")) %>% 
  group_by(date) %>% 
  summarize_all(mean,na.rm=TRUE)

cations <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/calnex/cations.csv") %>% 
  rename(date = ts,
         pm25nh4_ug_m3 = ammonium_PM2_5,
         pm25k_ug_m3 = potassium_PM2_5,
         pm25na_ug_m3 = sodium_PM2_5)%>% 
  mutate(pm25nh4_ug_m3 = ifelse(pm25nh4_ug_m3 < -0.1,NA,pm25nh4_ug_m3),
         pm25k_ug_m3 = ifelse(pm25k_ug_m3 < -0.1,NA,pm25k_ug_m3),
         pm25na_ug_m3 = ifelse(pm25na_ug_m3 < -0.1,NA,pm25na_ug_m3))%>% 
  mutate(date = floor_date(date, "1 hour")) %>% 
  group_by(date) %>% 
  summarize_all(mean,na.rm=TRUE)

nh3 <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/calnex/nh3.csv") %>% 
  rename(date = ts_utc,
         nh3_ppb = NH3_ppb)%>% 
  mutate(date = floor_date(date, "1 hour")) %>% 
  group_by(date) %>% 
  summarize_all(mean,na.rm=TRUE)

met <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/calnex/met.csv") %>% 
  rename(date = Time_G31_UTC,
         temp = Temp_G31_C, 
         rh = RelHum_G31_pct,
         press = Press_G31_mbar) %>% 
  mutate(temp = temp + 273.15,
        nDensAir = press/83.14/temp*6.02e23/1000)%>%# should get us to molecules / cm3 
  mutate(date = floor_date(date, "1 hour")) %>% 
  group_by(date) %>% 
  summarize_all(mean,na.rm=TRUE)


calnex <- cations %>% 
  left_join(anions) %>%
  #left_join(carbAnions) %>% 
  left_join(hclhno3) %>% 
  left_join(nh3) %>% 
  left_join(met) %>% 
  filter(complete.cases(.)) %>% 
  mutate(hcl_umol = hcl_ppb*1e-9 * nDensAir / 6.02e23 *10^6 * 100^3,#umol/m3
          nh3_umol = nh3_ppb*1e-9 * nDensAir / 6.02e23 *10^6 * 100^3,#umol/m3
         hno3_umol = hno3_ppb*1e-9 * nDensAir / 6.02e23 *10^6 * 100^3,#umol/m3
         pm25na_umol = pm25na_ug_m3 / 22.99,
         pm25cl_umol = pm25cl_ug_m3 / 35.45,
         pm25k_umol = pm25k_ug_m3 / 39.0983,
         pm25so4_umol = pm25so4_ug_m3 / 96.06,
         pm25no3_umol = pm25no3_ug_m3 / 62.01,
         pm25nh4_umol = pm25nh4_ug_m3 / 18.04,
         nh3_umol = ifelse(nh3_umol < 0, 0, nh3_umol)) %>% 
  filter(pm25k_umol > -0.1)

write.csv(calnex,"G:/My Drive/Experiments/OSCA/isorropia/wes/calnex/calnex.csv",
          quote = FALSE,
          row.names= FALSE)










