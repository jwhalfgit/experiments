# Each site has produced their data differently, so I will wrangle into 
# a unified data frame.


# BAQS --------------------------------------------------------------------
ff_baqsCPC <- list.files(file.path(DATADIR,"baqs"),
                         pattern = "CPC",
                         full.names = TRUE)


baqsCPC <- lapply(ff_baqsCPC, read_csv) %>% 
  bind_rows()
summary(baqsCPC)



ff_baqsSMPS <- list.files(file.path(DATADIR,"baqs"),
                          pattern = "SMPS",
                          full.names = TRUE)



baqsSMPS <- lapply(ff_baqsSMPS, read_csv) %>% 
  bind_rows() %>% 
  filter(complete.cases(.))



###########################################################################
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
# MAQS --------------------------------------------------------------------
ff_maqsCPC <- list.files(file.path(DATADIR,"maqs", "cpc"),
                         pattern = "maqs-CPC",
                         full.names = TRUE)


maqsCPC <-lapply(ff_maqsCPC, read_csv) %>% 
  bind_rows()
summary(maqsCPC)


# I note that the MAQS SMPS data are in units of d(N) / d(log(Dp))
ff_maqsSMPS <- list.files(file.path(DATADIR,"maqs", "smps"),
                         pattern = "maqs-SMPS",
                         full.names = TRUE)


smps1 <- read_csv(ff_maqsSMPS[1]) 
smps2 <- read_csv(ff_maqsSMPS[2])
smps3 <- read_csv(ff_maqsSMPS[3]) 
smpsn <- read_csv(ff_maqsSMPS[length(ff_maqsSMPS)])


identical(names(smps1), names(smps3))

maqsSMPS <- lapply(ff_maqsSMPS, read_csv) %>% 
  bind_rows() %>% 
  replace_na(0)


summary(maqsSMPS)



###############################################################
###
#
#
#
##

# London Honor Oak Park ---------------------------------------------------









