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

# baqsCPC <- read_cpc_files(ff_baqsCPC) %>%
#   mutate(date = floor_date(date, unit = "1 hour")) %>%
#   filter(complete.cases(.)) %>% 
#   group_by(date) %>%
#   summarize_all(mean,na.rm = TRUE)
# 
# save(baqsCPC, file = "baqsCPC.Rds")
load("baqsCPC.Rds")



# baqsSMPS <- read_smps_files(ff_baqsSMPS)
# save(baqsSMPS, file = "baqsSMPS.Rds")
load("baqsSMPS.Rds")



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

# maqsCPC <- read_cpc_files(ff_maqsCPC) %>%
#   mutate(date = floor_date(date, unit = "1 hour")) %>%
#   filter(complete.cases(.)) %>% 
#   group_by(date) %>%
#   summarize_all(mean,na.rm = TRUE)
# save(maqsCPC, file = "maqsCPC.Rds")
load("maqsCPC.Rds")



#
# maqsSMPS <- read_smps_files(ff_maqsSMPS) %>%
#   #rename(date = datetime) %>%
#   mutate(date = floor_date(date, unit = "1 hour")) %>%
#   group_by(date) %>%
#   summarize_all(mean,na.rm = TRUE)
#
# save(maqsSMPS, file = "maqsSMPS.Rds")
load("maqsSMPS.Rds")


#############################################################################
# we need to see how close together hte overlapping MAQS CPC data are:
START = ymd("2022-09-01")
END = ymd("2022-11-01")


plot_cpc_ts(cpc_data = maqsCPC, start = START, end = END)


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

