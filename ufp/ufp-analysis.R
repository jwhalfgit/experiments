# UFP Analysis file
# this is essentially a scratch pad for using the functions in 
# load_ufp.R
# plot_ufp.R
# npf_ufp.R

# This will allow me to keep the function files clean.
source("sourceMeFirst_ufp.R")


# BAQS (Birmingham) -------------------------------------------------------
# Load BAQS SMPS and CPC

ff_baqsCPC <- list.files(file.path(DATADIR, "baqs", "cpc"),
                         pattern = "CPC",
                         full.names = TRUE)

ff_baqsSMPS <- list.files(file.path(DATADIR, "baqs", "smps"),
                          pattern = "SMPS",
                          full.names = TRUE)

baqsCPC <- lapply(ff_baqsCPC, read_csv) %>%
  bind_rows() %>% 
  mutate(date = dmy_hm(date))

baqsSMPS <- read_smps_files(ff_baqsSMPS)


# MAQS (Manchester) -------------------------------------------------------
# CPC: 1-minute resolution; two instrument models (CPC-3750, CPC-3772) with
# overlapping periods — deduplication/instrument selection not yet applied.
ff_maqsCPC <- list.files(file.path(DATADIR, "maqs", "cpc"),
                         pattern = "maqs-CPC",
                         full.names = TRUE)
# SMPS: 5-minute resolution
ff_maqsSMPS <- list.files(file.path(DATADIR, "maqs", "smps"),
                          pattern = "maqs-SMPS",
                          full.names = TRUE)

maqsCPC <- lapply(ff_maqsCPC, read_csv) %>%
  bind_rows() %>% 
  rename(date = datetime) %>% 
  mutate(date = floor_date(date, unit = "1 hour")) %>% 
  group_by(date) %>% 
  summarize_all(mean,na.rm = TRUE)




maqsSMPS <- read_smps_files(ff_maqsSMPS) %>% 
  #rename(date = datetime) %>% 
  mutate(date = floor_date(date, unit = "1 hour")) %>% 
  group_by(date) %>% 
  summarize_all(mean,na.rm = TRUE)


#############################################################################
# we need to see how close together hte overlapping MAQS CPC data are:

plot_cpc_ts(cpc_data = maqsCPC, start = "2022-09-01", end = "2022-11-01")
maqsCPC %>% 
  filter(between(date,ymd("2022-09-01"), ymd("2022-11-01" ))) %>% 
  arrange(date)


