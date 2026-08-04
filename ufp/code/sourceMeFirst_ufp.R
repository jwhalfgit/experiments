# Please source first to load required packages, define directory 
# and data locations.

library(tidyverse)
library(readxl)
library(ggpubr)
library(openair)
library(openairmaps)
library(htmlwidgets)
library(leaflet)
library(base64enc)
library(trend)     # sens.slope(), mk.test() for custom Theil-Sen plots

Sys.setenv(TZ='UTC')

MYNAME <- system("hostname", intern = TRUE)

ROOT <- switch(MYNAME,
               "Computer" = "~/Documents/experiments/ufp", 
               "UOY21Y093" = "C:/Users/jh2949/One Drive - University of York/Documents/work/Code/R/experiments/ufp",
               NULL) 

DATADIR <- switch(MYNAME,
                  "Computer" = "~/Documents/experiments/ufp/data", 
                  "UOY21Y093" = "G:/My Drive/Experiments/DEFRA/ufp/data",
                  NULL) 

PLOTDIR <- switch(MYNAME,
                  "Computer" = "~/Documents/experiments/ufp/plots", 
                  "UOY21Y093" = "G:/My Drive/Experiments/DEFRA/ufp/plots",
                  NULL) 


setwd(file.path(ROOT, "code"))
# Entry point: sources all component scripts in dependency order.
# Each file can also be sourced individually after sourceMeFirst_ufp.R.

source("load_ufp.R")   # SMPS_SCALE, read_smps_files(), site data frames
source("plot_ufp.R")   # plot_smps_banana(), plot_smps_conversion()
source("npf_ufp.R")    # find_modes_*, link_mode_tracks(), detect_npf_events(), npf_prescreen()
source("npf_physics.R") # condensation_sink(), coagulation_sink(), formation_rate(), etc.
source("npf_classify.R") # NPF logbook, npf_classify(), npf_trace_mode(), npf_refit_logbook()
source("prep_external.R") # for preparing for PyNSD (James Brean software)