# We want to estimate the lifetime of HCl as it relates to its uptake on
# aerosol.  From Seinfeld and Pandis, this should be estimated as...

# R = 1/4 * uptake_coef * mean_speed_HCl * particle_SA/vol * num_dens_HCl

# where k = 1/4 * uptake_coef * mean_speed_HCl * particle_SA/vol
# mean_speed_HCl = (8* kB * T /pi / mass/molecule) )

# From Alfie's thesis, I see a different version of this:
# k = particle_SA / ((particle_r/Diffusion_gas) + 4/mean_speed_Hcl*uptake_coef)

#where mean speed = (3RT / molar mass HCl)^0.5


# IUPAC (doi:10.5194/acp-13-8045-2013) says these uptake coefficnets by 
# Van Doren are actually
# mass accomodation coefficients. However, Finlayson-Pitts and Pitts
# says (p.163) that if you have fast gas transport, solubility, and reaction
# that 1 / alpha approximates 1 / gamma.  
# uc274 = 0.177
# uc285 = 0.135
# uc294 = 0.064


# Get surface area:
get.SMPS.SA <- function(SMPS){
  smps <- SMPS
  
  asol_d = as.numeric(names(smps[,2:107])) / 1e9 *100 # convert to cm
  asol_SA = 4*pi*(asol_d/2)^2 # corresponding SA for each diameter in nm^2
  
  # average number densitiies
  asol_nbar <- smps[,2:107]
  asol_SA_n <- t(t(asol_nbar) * asol_SA)  # cm^2 / cc
  total_SA <- rowSums(asol_SA_n)  # cm^2 / cc
  total_sub500 <- rowSums(asol_SA_n[,1:98]) # cm^2 / cc
  total_super <- rowSums(asol_SA_n[,99:106])   # cm^2 / cc
  total_fidasoverlap <- rowSums(asol_SA_n[,84:95])  # 
  outDF <- data.frame(date = SMPS$ts, 
                      smps_SA = total_SA,
                      smps_sub500 = total_sub500, 
                      smps_super = total_super, 
                      smps_fidas = total_fidasoverlap)
  return(outDF)
}



get.FIDAS.SA <- function(FIDAS){
  fidas <- FIDAS
  colnames(fidas)[2:66] <- gsub(pattern = " um (##/cm^3)",
                                replacement="",
                                x = names(fidas)[2:66],
                                fixed = TRUE)
  
  asol_d = as.numeric(names(fidas[,2:66])) / 1e6 * 100
  asol_SA = 4*pi*(asol_d/2)^2 # corresponding SA for each diameter in um^2
  
  # average number densitiies
  asol_nbar <- fidas[,2:66]
  asol_SA_n <- t(t(asol_nbar) * asol_SA) # cm^2 / cc # from micrograms
  total_SA_pm25 <- rowSums(asol_SA_n[,1:46]) # cm^2 / cc
  #total_submicron <- rowSums(asol_SA_n[,1:31]) # cm^2 / cc # submicron
  #total_super <- rowSums(asol_SA_n[,32:65]) # cm^2 / cc # supermicron
  total_SMPS <-rowSums(asol_SA_n[,16:21]) # cm^2 / cc # sub500nm
  total_SA_500_2500 <- rowSums(asol_SA_n[,24:45])
  
  outDF <- data.frame(date = fidas$date, 
                      fidas_pm25 = total_SA_pm25,
                      #sub = total_submicron,
                      fidas_smpsoverlap = total_SMPS, 
                      #super = total_super,
                      fidas_pm500_2500 = total_SA_500_2500)
  return(outDF)
}


# For F0AM ----------------------------------------------------------------
get.hcl.kdep <- function(TEMP = dfAnal$tempK,SA = dfAnal$pm2_5_combined){
  alpha <- 4.4e-6 * exp(2898/TEMP) # from Van Doren et al (see top section of this file)
  kB = 1.380649e-23 # J / K (Boltzman Constant)
  #p_diameter = SMPS$"Median (nm)" * 1e-9 
  
  mean_speed_hcl = sqrt((8*kB * TEMP) / (pi*36.458 / 1000 /(6.02e23))) # m/s
  Dhcl = (118 / ((296/TEMP)^1.75)) / 760
  # Diffusivity from Tang et al 2014 (doi:10.5194/acp-14-9233-2014)
  
  # surface area must be cm^2 / cc for units to cancel. 
  # If using PM2.5 from SMPS and FIDAS, it should have already been converted
  total_SA <- SA 
  
  # asol_d = as.numeric(names(smps[,2:107]))
  # asol_SA = 4*pi*(asol_d/2)^2 # corresponding SA for each diameter
  # 
  # # average number densitiies
  # asol_nbar <- smps[,2:107]
  # asol_SA_n <- t(t(asol_nbar) * asol_SA) * (10^-9 * 100)^2 # cm^2 / cc
  # total_SA <- rowSums(asol_SA_n) # cm^2 / cc
  # 
  
  kHCl <- 1/4 * alpha * (mean_speed_hcl*100)*total_SA
  
  #mean_speed_alfie = sqrt(3*8.314 * TEMP / 0.036458)*100 # cm/s - HCl specific
  #kHCl = total_SA / ((p_diameter*100/2 / Dhcl)+(4/(mean_speed_alfie*alpha)))

  outDF <- data.frame(ts = dfAnal$ts, khcl = kHCl,tau = 1/kHCl, SA = total_SA)
  return(outDF)
}

# 
# get.hcl.kdep.pm25 <- function(TEMP = dfAnal$tempK,SMPS = dfAnal){
#   alpha <- 4.4e-6 * exp(2898/TEMP) # from Van Doren et al (see top section of this file)
#   kB = 1.380649e-23 # J / K (Boltzman Constant)
#   p_diameter = SMPS$"Median (nm)" * 1e-9 
#   
#   #mean_speed_hcl = sqrt((8*kB * TEMP) / (pi*36.458 / 1000 /(6.02e23))) # m/s
#   Dhcl = (118 / ((296/TEMP)^1.75)) / 760
#   # Diffusivity from Tang et al 2014 (doi:10.5194/acp-14-9233-2014)
#   
#   smps <- SMPS
#   
#   asol_d = as.numeric(names(smps[,2:107]))
#   asol_SA = 4*pi*(asol_d/2)^2 # corresponding SA for each diameter
#   
#   # average number densitiies
#   asol_nbar <- smps[,2:107]
#   asol_SA_n <- asol_nbar * asol_SA * (10^-9 * 100)^2 # cm^2 / cc
#   total_SA <- rowSums(asol_SA_n) # cm^2 / cc
#   
#   # kHCl <- 1/4 * alpha * (mean_speed_hcl*100)*total_SA
#   
#   mean_speed_alfie = sqrt(3*8.314 * TEMP / 0.036458)*100 # cm/s - HCl specific
#   kHCl = total_SA / ((p_diameter*100/2 / Dhcl)+(4/(mean_speed_alfie*alpha)))
#   
#   outDF <- data.frame(ts = SMPS$ts, khcl = kHCl,tau = 1/kHCl, SA = total_SA)
#   return(outDF)
# }
# 



# Unlike the smps data, the FIDAS data is already in units of 
# number concentration.  
fidas2021 <- read_csv(file.path(OSCADATADIR, "2021", "fidas-conc.csv")) %>% 
  mutate(date = ymd_hms(datetime)) %>% 
  filter(between(date, ymd_hm("2021-06-10 00:00"),ymd_hm("2021-07-21 00:00"))) %>% 
  mutate(date = floor_date(date,unit = "5 minute")) %>% 
  select(!datetime) %>% 
  group_by(date) %>% 
  summarize_all(mean, na.rm = TRUE)

# The smps output gives dN / dlogDp data, which means that the actual
# number concentration for these data need to be divided by the bin
# width, which is 64.

smps2021 <- read_csv(file.path(OSCADATADIR, "2021", "smps.csv")) %>% 
  mutate(datetime = dmy_hm(datetime)) %>% 
  rename(ts = datetime) %>% 
  filter(between(ts, ymd_hm("2021-06-10 00:00"),ymd_hm("2021-07-21 00:00"))) %>% 
  mutate(ts = floor_date(ts,unit = "5 minute")) %>% 
  group_by(ts) %>% 
  summarize_all(mean, na.rm = TRUE)
smps2021[,2:107] <- smps2021[,2:107]/64 # the bin size in the SMPS is 1/64

met2021 <- read_csv(file.path(OSCADATADIR,"2021", "fidas-met.csv")) %>% 
  mutate(datetime = dmy_hm(datetime)) %>% 
  select(!contains("flag")) %>% 
  rename(ts = datetime) %>% 
  mutate(ts = floor_date(ts,unit = "5 minute")) %>% 
  group_by(ts) %>% 
  summarize_all(mean,na.rm = TRUE)
names(met2021) = c("ts","tempC","press","rh","tempK")

fidasSA <- get.FIDAS.SA(FIDAS=fidas2021)
sa <- get.SMPS.SA(SMPS = smps2021)

pmSA <- sa %>% 
  left_join(fidasSA) %>% 
  rename(ts = date) %>% 
  mutate(pm2_5_combined = smps_sub500 + fidas_pm500_2500,
         sub500Fraction = smps_sub500 / pm2_5_combined)

dfAnal <- pmSA %>% 
  left_join(met2021)


kdep21 <- get.hcl.kdep()


write.csv(kdep21, "G:/My Drive/Experiments/OSCA/data/2021/kdep.csv",row.names=FALSE,
          quote = FALSE)

##########################################

smps2022 <- read_csv("G:/My Drive/Experiments/OSCA/data/2022/smps.csv") %>% 
  mutate(datetime = dmy_hm(datetime)) %>% 
  rename(ts = datetime) %>% 
  filter(between(ts, ymd_hm("2022-01-23 00:00"),ymd_hm("2022-02-28 00:00"))) %>% 
  mutate(ts = floor_date(ts,unit = "5 minutes")) %>% 
  group_by(ts) %>% 
  summarize_all(mean,na.rm = TRUE)
smps2022[,2:107] <- smps2022[,2:107] / 64
#

fidas2022 <- read_csv(file.path(OSCADATADIR, "2022", "fidas-conc.csv")) %>% 
  mutate(date = dmy_hm(datetime)) %>% 
  filter(between(date, ymd_hm("2022-01-23 00:00"),ymd_hm("2022-02-28 00:00"))) %>% 
  mutate(date = floor_date(date,unit = "5 minutes")) %>% 
  select(!datetime) %>% 
  group_by(date) %>% 
  summarize_all(mean, na.rm = TRUE)

met2022 <- read_csv(file.path(OSCADATADIR,"2022", "fidas-met.csv")) %>% 
  mutate(datetime = dmy_hm(datetime)) %>% 
  select(!contains("flag")) %>% 
  rename(ts = datetime) %>% 
  mutate(ts = floor_date(ts,unit = "5 minutes")) %>% 
  group_by(ts) %>% 
  summarize_all(mean,na.rm = TRUE)
names(met2022) = c("ts","tempC","press","rh","tempK")

fidasSA <- get.FIDAS.SA(FIDAS=fidas2022)
sa <- get.SMPS.SA(SMPS = smps2022)

pmSA <- sa %>% 
  left_join(fidasSA) %>% 
  rename(ts = date) %>% 
  mutate(pm2_5_combined = smps_sub500 + fidas_pm500_2500,
         sub500Fraction = smps_sub500 / pm2_5_combined)

dfAnal <- pmSA %>% 
  left_join(met2022)


kdep22 <- get.hcl.kdep()


kdep22 <- get.hcl.kdep()
write.csv(kdep22, "G:/My Drive/Experiments/OSCA/data/2022/kdep.csv",
          row.names=FALSE,quote = FALSE)

# OLD ---------------------------------------------------------------------
get.hcl.kdep <- function(TEMP = dfAnal$tempK,SA){
  alpha <- 4.4e-6 * exp(2898/TEMP) # from Van Doren et al (see top section of this file)
  kB = 1.380649e-23 # J / K (Boltzman Constant)
  #p_diameter = SMPS$"Median (nm)" * 1e-9 
  
  mean_speed_hcl = sqrt((8*kB * TEMP) / (pi*36.458 / 1000 /(6.02e23))) # m/s
  Dhcl = (118 / ((296/TEMP)^1.75)) / 760
  # Diffusivity from Tang et al 2014 (doi:10.5194/acp-14-9233-2014)
  
  smps <- SA
  
  asol_d = as.numeric(names(smps[,2:107]))
  asol_SA = 4*pi*(asol_d/2)^2 # corresponding SA for each diameter
  
  # average number densitiies
  asol_nbar <- smps[,2:107]
  asol_SA_n <- t(t(asol_nbar) * asol_SA) * (10^-9 * 100)^2 # cm^2 / cc
  total_SA <- rowSums(asol_SA_n) # cm^2 / cc
  
  kHCl <- 1/4 * alpha * (mean_speed_hcl*100)*total_SA
  
  mean_speed_alfie = sqrt(3*8.314 * TEMP / 0.036458)*100 # cm/s - HCl specific
  #kHCl = total_SA / ((p_diameter*100/2 / Dhcl)+(4/(mean_speed_alfie*alpha)))
  
  outDF <- data.frame(ts = SMPS$ts, khcl = kHCl,tau = 1/kHCl, SA = total_SA)
  return(outDF)
}



TEMP = 298# K
alpha <- 4.4e-6 * exp(2898/TEMP) # uptake coefficient from Van Doren et al (above)

kB = 1.380649e-23 # J / K
p_diameter = 67

mean_speed_hcl = sqrt((8*kB * TEMP) / (pi*36.458 / 1000 /(6.02e23))) # m/s

# Diffusivity from Tang et al 2014 (doi:10.5194/acp-14-9233-2014)
Dhcl = (122 / ((296/TEMP)^1.75)) / 760

# conducatance equation from p 160 of Finlayson Pitts and Pitts
conductance = (mean_speed_hcl * p_diameter / (8 * Dhcl) - 1/2)^-1

smps <- smps2021

# asol_d is aerosol diameters in nm.  We are assuming the relevant data
# are number per cc 
asol_d = as.numeric(names(smps[,2:107]))
asol_SA = 4*pi*(asol_d/2)^2

# I think Jordan has taken the average surface areas across the campaign.  He
# first did this by calculating the average number density of particles for each
# diameter across the campaign.

# average number densitiies
asol_nbar <- colMeans(smps[,2:107])
asol_SA_n <- asol_nbar * asol_SA * (10^-9 * 100)^2 # cm^2 / cc
total_SA <- cumsum(asol_SA_n)[length(asol_SA)] # cm^2 / cc

kHCl <- 1/4 * alpha * (mean_speed_hcl*100)*total_SA
# 1/kHCl = 25.43 s



# asol_d is aerosol diameters in nm.  We are assuming the relevant data
# are number per cc 


# I think Jordan has taken the average surface areas across the campaign.  He
# first did this by calculating the average number density of particles for each
# diameter across the campaign.

# average number densitiies
asol_nbar <- colMeans(smps[,2:107])
asol_SA_n <- asol_nbar * asol_SA * (10^-9 * 100)^2 # cm^2 / cc
total_SA <- cumsum(asol_SA_n)[length(asol_SA)] # cm^2 / cc

kHCl <- 1/4 * alpha * (mean_speed_hcl*100)*total_SA
# 1/kHCl = 25.43 s

###################
# Alfie's method

mean_speed_alfie = sqrt(3*8.314 * TEMP / 0.036458)*100 # cm/s 
kHCl = total_SA / ((67e-9*100/2 / Dhcl)+(4/(mean_speed_alfie*alpha)))

# 1/kHCl = 24.13 s



# FIDAS -------------------------------------------------------------------

# smps2021 <- read_csv("G:/My Drive/Experiments/OSCA/data/2021/smps.csv") %>% 
#   mutate(datetime = dmy_hm(datetime)) %>% 
#   rename(ts = datetime) %>% 
#   filter(between(ts, ymd_hm("2021-06-10 00:00"),ymd_hm("2021-06-18 00:00"))) %>% 
#   mutate(ts = floor_date(ts,unit = "hours")) %>% 
#   group_by(ts) %>% 
#   summarize_all(mean, na.rm = TRUE)
# smps2021[,2:107] <- smps2021[,2:107] / 64
