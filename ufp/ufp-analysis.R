# UFP Analysis file
# this is essentially a scratch pad for using the functions in 
# load_ufp.R
# plot_ufp.R
# npf_ufp.R

# This will allow me to keep the function files clean.
source("sourceMeFirst_ufp.R")

# CPC
# BAQS (Birmingham) -------------------------------------------------------
# Load BAQS SMPS and CPC

ff_baqsCPC  <- find_site_files(file.path(DATADIR, "baqs", "cpc"),  
                               pattern = "CPC")
# baqsCPC <- read_cpc_files(ff_baqsCPC) %>%
#   mutate(date = floor_date(date, unit = "1 hour")) %>%
#   filter(complete.cases(.)) %>%
#   group_by(date) %>%
#   summarize_all(mean, na.rm = TRUE)
# 
# write_working_csv(baqsCPC, file.path(DATADIR, "baqs", "cpc", "baqs_cpc.csv"))
# save(baqsCPC, file = "baqsCPC.Rds")
load("baqsCPC.Rds")



# MAQS (Manchester) -------------------------------------------------------
# CPC: 1-minute resolution; two instrument models (CPC-3750, CPC-3772) with
# overlapping periods — CPC-3750 takes priority (handled in read_cpc_files()).
ff_maqsCPC  <- find_site_files(file.path(DATADIR, "maqs", "cpc"), 
                               pattern = "CPC")

# 
# maqsCPC <- read_cpc_files(ff_maqsCPC) %>%
#   mutate(date = floor_date(date, unit = "1 hour")) %>%
#   filter(complete.cases(.)) %>%
#   group_by(date) %>%
#   summarize_all(mean, na.rm = TRUE)
#  write_working_csv(maqsCPC, file.path(DATADIR, "maqs", "cpc", "maqs_cpc.csv"))
# save(maqsCPC, file = "maqsCPC.Rds")
load("maqsCPC.Rds")



#############################################################################
maqsCPC_monthly <- maqsCPC %>% 
  mutate(mo = floor_date(date,"month")) %>% 
  group_by(mo) %>% 
  summarize(date = mean(mo), 
            concMean = mean(conc,na.rm = TRUE), 
            concSd = sd(conc,na.rm = TRUE))


baqsCPC_monthly <- baqsCPC %>% 
  mutate(mo = floor_date(date,"month")) %>% 
  group_by(mo) %>% 
  summarize(date = mean(mo), 
            concMean = mean(conc,na.rm = TRUE), 
            concSd = sd(conc, na.rm = TRUE))


cpc_monthly <- bind_rows(
  mutate(maqsCPC_monthly, site = "Manchester"),
  mutate(baqsCPC_monthly, site = "Birmingham")
)

cpcTs <- ggplot(cpc_monthly, aes(x = date, y = concMean, colour = site, fill = site)) +
  geom_ribbon(aes(ymin = concMean - concSd, ymax = concMean + concSd), alpha = 0.2, colour = NA) +
  geom_line() +
  scale_colour_manual(values = c("Manchester" = "steelblue", "Birmingham" = "tomato")) +
  scale_fill_manual(  values = c("Manchester" = "steelblue", "Birmingham" = "tomato")) +
  labs(x = NULL, y = "Particle concentration (#/cm³)", colour = NULL, fill = NULL) +
  theme_minimal() +
  theme(text = element_text(size = 22))


cpcBoxWhisk <- bind_rows(
  mutate(maqsCPC, site = "Manchester"),
  mutate(baqsCPC,  site = "Birmingham")
) %>%
  mutate(year_mo = as.Date(floor_date(date, "month"))) %>%
  ggplot(aes(x = year_mo, y = conc, colour = site, fill = site,
             group = interaction(year_mo, site))) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA,
               position = position_dodge(width = 20), width = 15) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_colour_manual(values = c("Manchester" = "steelblue", "Birmingham" = "tomato")) +
  scale_fill_manual(  values = c("Manchester" = "steelblue", "Birmingham" = "tomato")) +
  labs(x = NULL, y = "Particle concentration (#/cm³)", colour = NULL, fill = NULL) +
  theme_minimal() +
  theme(text = element_text(size = 22), axis.text.x = element_text(angle = 45, hjust = 1))+
  ylim(c(0,40000))


cpc_ts_plot <- ggarrange(cpcTs, cpcBoxWhisk,nrow = 2, common.legend = TRUE,
                         legend = "right")
ggsave( file.path(PLOTDIR, "cpc_ts_plot.png"),plot = cpc_ts_plot,
       width = 12.8, height =10.76, units = "in", bg = "white")


# SMPS number concentration -----------------------------------------------

maqsSMPS_N <- smps_number_conc(maqsSMPS)
baqsSMPS_N <- smps_number_conc(baqsSMPS)

maqsSMPS_N_monthly <- maqsSMPS_N %>%
  mutate(mo = floor_date(date, "month")) %>%
  group_by(mo) %>%
  summarize(date   = mean(mo),
            concMean = mean(N, na.rm = TRUE),
            concSd   = sd(N,   na.rm = TRUE))

baqsSMPS_N_monthly <- baqsSMPS_N %>%
  mutate(mo = floor_date(date, "month")) %>%
  group_by(mo) %>%
  summarize(date   = mean(mo),
            concMean = mean(N, na.rm = TRUE),
            concSd   = sd(N,   na.rm = TRUE))

smps_N_monthly <- bind_rows(
  mutate(maqsSMPS_N_monthly, site = "Manchester"),
  mutate(baqsSMPS_N_monthly, site = "Birmingham")
)

smpsTs <- ggplot(smps_N_monthly, aes(x = date, y = concMean, colour = site, fill = site)) +
  geom_ribbon(aes(ymin = concMean - concSd, ymax = concMean + concSd), alpha = 0.2, colour = NA) +
  geom_line() +
  scale_colour_manual(values = c("Manchester" = "steelblue", "Birmingham" = "tomato")) +
  scale_fill_manual(  values = c("Manchester" = "steelblue", "Birmingham" = "tomato")) +
  labs(x = NULL, y = "SMPS N (#/cm³)", colour = NULL, fill = NULL) +
  theme_minimal() +
  theme(text = element_text(size = 22))

smpsBoxWhisk <- bind_rows(
  mutate(maqsSMPS_N, site = "Manchester"),
  mutate(baqsSMPS_N,  site = "Birmingham")
) %>%
  mutate(year_mo = as.Date(floor_date(date, "month"))) %>%
  ggplot(aes(x = year_mo, y = N, colour = site, fill = site,
             group = interaction(year_mo, site))) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA,
               position = position_dodge(width = 20), width = 15) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_colour_manual(values = c("Manchester" = "steelblue", "Birmingham" = "tomato")) +
  scale_fill_manual(  values = c("Manchester" = "steelblue", "Birmingham" = "tomato")) +
  labs(x = NULL, y = "SMPS N (#/cm³)", colour = NULL, fill = NULL) +
  theme_minimal() +
  theme(text = element_text(size = 22), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(c(0, 40000))

smps_ts_plot <- ggarrange(smpsTs, smpsBoxWhisk, nrow = 2, common.legend = TRUE,
                          legend = "right")
ggsave(file.path(PLOTDIR, "smps_ts_plot.png"), plot = smps_ts_plot,
       width = 12.8, height = 10.76, units = "in", bg = "white")




# SMPS --------------------------------------------------------------------
# ff_baqsSMPS <- find_site_files(file.path(DATADIR, "baqs", "smps","raw"),
#                                pattern = "SMPS")
# 
# baqsSMPSList <- read_smps_files(ff_baqsSMPS)
# baqsSMPSdf <- smps_interpolate(baqsSMPSList,new_scale = SMPS_SCALE)
# plot_smps_banana(baqsSMPSdf)
# 
# # write_working_csv(baqsSMPS, file.path(DATADIR, "baqs", "smps", "baqs_smps.csv"))
# # save(baqsSMPS, file = "baqsSMPS.Rds")
# #load("baqsSMPS.Rds")
# 
# baqsSMPS <- prep_smps_external(ff_baqsSMPS)
# sapply(names(baqsSMPS),
#        function(x){write.csv(baqsSMPS[[x]],
#                              file = paste(x,"forPyNSD", "csv", sep = "."),
#                              row.names = FALSE,
#                              quote = FALSE)})
# 
# 
# 
# ff_maqsSMPS_raw <- find_site_files(file.path(DATADIR, "maqs", "smps","raw"),
#                                pattern = "SMPS")
# 
# ff_maqsSMPS_rat <-  find_site_files(file.path(DATADIR, "maqs", "smps",
#                                                    "ratified"),
#                                          pattern = "SMPS")
# 

# We also want to include the raw SMPS data from MAQS provided by James Allan.
# It has been QC'd
# 
# maqsSMPS_raw <- prep_smps_external(ff_maqsSMPS_raw) %>%
#   bind_rows() %>% 
#   write_csv("~/Documents/experiments/ufp/data/maqs/smps/forPyNSD/2023-2025_maqs_smps_raw.forPyNSD.csv")
# 
# 
# maqsSMPS_raw %>%
#   mutate(date = ymd_hms(date)) %>%
#   #filter(between(date,ymd("2025-04-09"), ymd("2025-04-10"))) %>%
#   #filter(!dates < 1e12) %>%
#   ggplot()+
#   geom_line(aes(x = ymd_hms(date), y = `32.20`))
# 
# 
# maqsSMPS_rat <- prep_smps_external(ff_maqsSMPS_rat)

# sapply(names(maqsSMPS_rat),
#        function(x){write.csv(maqsSMPS_rat[[x]],
#                              file = paste(x,"forPyNSD", "csv", sep = "."),
#                              row.names = FALSE,
#                              quote = FALSE)})




# SMPS: ratified 5-minute + raw AIM format
ff_maqsSMPS <- find_site_files(file.path(DATADIR, "maqs", "smps", "forPyNSD"), 
                               pattern = "forPyNSD")
maqsSMPS <- read_smps_files(ff_maqsSMPS, TIME_AVG = "1 hour")
maqsSMPS_splined <- smps_spline(maqsSMPS) # harmonize size bins

#   mutate(date = floor_date(date, unit = "1 hour")) %>%
#   group_by(date) %>%
#   summarize_all(mean, na.rm = TRUE)
write_working_csv(maqsSMPS_splined, file.path(DATADIR, "maqs", "smps", "maqs_smps_R.csv"))
save(maqsSMPS_splined, file = "maqsSMPS_splined.Rds")
load("maqsSMPS_splined.Rds")

plot_smps_banana(maqsSMPS_splined, show_NSD = TRUE, NSD_avg = "month")




