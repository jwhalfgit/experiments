# Please source first to load required packages, define directory 
# and data locations.

library(tidyverse)

Sys.setenv(TZ='UTC')

MYNAME <- system("hostname", intern = TRUE)

ROOT <- switch(MYNAME,
               "Computer" = "~/Documents/experiments/ufp", 
               "nas-10-240-199-223.york.ac.uk" = "/Users/amylees/Documents/PhD",
               "UOY21Y093" = "G:/My Drive",
               NULL) 

DATADIR <- file.path(ROOT, "data")