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



TEMP = 273 # K
alpha <- 4.4e-6 * exp(2898/TEMP)

kB = 1.380649e-23 # J / K
p_diameter = 67

mean_speed_hcl = sqrt((8*kB * TEMP) / (pi*36.458 / 1000 /(6.02e23))) # m/s

# Diffusivity from Tang et al 2014 (doi:10.5194/acp-14-9233-2014)
Dhcl = (122 / ((296/TEMP)^1.75)) / 760

# conducatance equation from p 160 of Finlayson Pitts and Pitts
conductance = (mean_speed_hcl * p_diameter / (8 * Dhcl) - 1/2)^-1


smps2021 <- read_csv("G:/My Drive/Experiments/OSCA/data/2021/smps.csv") %>% 
  mutate(datetime = dmy_hm(datetime)) %>% 
  rename(ts = datetime) %>% 
  filter(between(ts, ymd_hm("2021-06-10 00:00"),ymd_hm("2021-07-22 00:00")))

smps2022 <- read_csv("G:/My Drive/Experiments/OSCA/data/2022/smps.csv") %>% 
  mutate(datetime = dmy_hm(datetime)) %>% 
  rename(ts = datetime) %>% 
  filter(between(ts, ymd_hm("2022-01-23 00:00"),ymd_hm("2022-02-28 00:00")))

smps <- smps2022
TEMP = 278

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

###################
# Alfie's method

mean_speed_alfie = sqrt(3*8.314 * 294 / 0.036458)*100 # cm/s 
kHCl = total_SA / ((67e-9*100 / Dhcl)+(4/(mean_speed_alfie*alpha)))

# 1/kHCl = 24.13 s
