# Please source first to load required packages, define directory 
# and data locations.

library(tidyverse)
library(ggpubr)
library(openair)

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


setwd(ROOT)
# Entry point: sources all component scripts in dependency order.
# Each file can also be sourced individually after sourceMeFirst_ufp.R.

source("load_ufp.R")   # SMPS_SCALE, read_smps_files(), site data frames
source("plot_ufp.R")   # plot_smps_banana(), plot_smps_conversion()
source("npf_ufp.R")    # find_modes_*, link_mode_tracks(), detect_npf_events()
