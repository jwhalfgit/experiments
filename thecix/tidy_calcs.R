# cleaner denuder calcs

library(openair)
library(plotly) #interactive plot 
library(htmlwidgets) #save as widget

load.tildas.data(START = "2023-07-31", STOP = "2023-08-20",
                 STC = TRUE, HCLONLY= FALSE)

dfAnal <- df.str %>% 
  left_join(df.stc)  %>% 
  filter(!between(ts,ymd_hm("2023-08-01 16:15"), ymd_hm("2023-08-01 19:39"))) %>% 
  
  filter(!between(ts,ymd_hm("2023-08-16 12:09"), ymd_hm("2023-08-16 12:15")))

dfAnal <- dfAnal %>% 
  mutate(ValveW = replace(ValveW,
                          between(ts,
                                  ymd_hm("2023-08-01 14:30"),
                                  ymd_hm("2023-08-02 16:20"))&ValveW == 8,0)) %>% 
  mutate(ValveW = replace(ValveW,
                          between(ts,
                                  ymd_hm("2023-08-16 04:05"),
                                  ymd_hm("2023-08-16 12:00"))&ValveW == 8,0)) %>%
  mutate(ValveW = replace(ValveW,
                          between(ts,
                                  ymd_hm("2023-08-16 21:20"),
                                  ymd_hm("2023-08-17 18:58")) & ValveW == 8,0))
######  ***************** I added the line above this ******************************############

#below counts data from 5 seconds after a change in valve state has occurred (6s removed in total)
valveChangeix <- which(c(0,diff(dfAnal$ValveW))!=0)
valveChangeixTail <- unique(unlist(lapply(valveChangeix,function(x){seq(x,x+5)})))
valveChangeixTail <- valveChangeixTail[!valveChangeixTail > nrow(dfAnal)]

dfAnal$flag <- 0
dfAnal$flag[valveChangeixTail] <- 1


#first filter dataframe for only overblow and denuder settings

do_df <- dfAnal %>%
  filter(ValveW == 0 |ValveW == 8) %>%
  mutate(period = cumsum(flag !=lag(flag, default = flag[1])) ) %>% 
  filter(flag != 1) %>%
  group_by(period,ValveW) %>%
  summarise(start = min(ts),
            end  = max(ts),
            mean_hcl = mean(hcl),
            sd_hcl = sd(hcl),
            ymin = mean_hcl-sd_hcl,
            ymax = mean_hcl+sd_hcl)%>%
  mutate(sector = case_when(ValveW == 8 ~ "Overblow",
                            ValveW == 0 ~ "Denuder"),)

means <- ggplot(do_df, 
       aes(x = start,
           y = mean_hcl,
           color = sector)) +
  geom_point()+ 
  labs( x = "Date", 
        y = "Mean HCl", 
        color = "Valve State",
        title = "Overblow and Denuder mean blanks")+
  scale_x_datetime(date_breaks = "1 day", date_labels = "%b %d")+
  scale_color_manual(values = c("steelblue1", "navy"))+
geom_errorbar(aes(x = start, ymin = ymin, ymax = ymax), width = 0.25)
#last line above adds the standard deviation +- to each plot 

# "G:/Shared drives/Trop-ClOC experiments/TILDAS Data"
setwd("C:/Users/grace/Documents/Mchem_project")
#remember to change back to "G:/Shared drives/Trop-ClOC experiments/TILDAS Data"
ggsave(means, filename = "C:/Users/grace/Documents/Mchem_project/means.png")

setwd("G:/Shared drives/Trop-ClOC experiments/TILDAS Data")

#interactive plot 
int_means<-ggplotly(means)

#save as widget
htmlwidgets::saveWidget(
  widget = int_means,
  file = "C:/Users/grace/Documents/Mchem_project/int_means.html",
  selfcontained = TRUE
)

######### now want to remove dodgy data every 3 hours 
dat <- dfAnal %>% 
  filter(ValveW == 0 |ValveW == 8) %>%
  mutate(period = cumsum(flag !=lag(flag, default = flag[1])) ) %>%
  filter(flag != 1) %>%
  group_by(period,ValveW) %>%
  mutate(sector = case_when(ValveW == 8 ~ "Overblow",
                            ValveW == 0 ~ "Denuder"),)

dat$hour <- hour(dat$ts)
dat$mins <- minute(dat$ts)
dat$sec <- second(dat$ts)

#CLEAN UP:
#this should give all the data points want to remove for dodgy overblow measurements
#should I remove the two massive outlier sections ? ask Wes first. can come back to later
df_clean1 <- dat %>% filter(!(hour %in% c(3,6,9,12,15,18,21) & mins >= 3 & mins <=8) ) %>%
  rename(date = ts) %>% 
  pivot_wider(names_from = sector, values_from = hcl)

#Now take difference of average per hour 
#timeAverage function requires date/ time to be labelled as date
# last line above changes format so that don't get duplicate dates when timeAverage
#then takes the difference between denuder and overblow

hravg_clean <-df_clean1 %>% group_by(date) %>% timeAverage(avg.time = "1 hour", statistic = "mean") %>%
  select(date, Overblow, Denuder,Traw,) %>% 
  mutate(diff = abs(Denuder - Overblow))

diff_plot<- ggplot(hravg_clean, 
       aes(x = date,
           y = diff)) +
  geom_point(color = "deeppink4")+ 
  labs( x = "Date", 
        y = "Absolute Difference",
        title = " Absolute difference between Overblow and Denuder Hour Averages")+
  scale_x_datetime(date_breaks = "5 day", date_labels = "%b %d")+
  scale_y_continuous(breaks = c(0.05,0.1,0.15,0.2,0.25,0.3,0.35))

ggsave(diff_plot, filename = "C:/Users/grace/Documents/Mchem_project/absolute_difference1point.png")

#this is for the hour avg denuder and overblow plot for comparison
hrdo <- hravg_clean %>% pivot_longer(cols = c(Overblow, Denuder), names_to = "ValveSt", values_to = "hr_hcl")

hrplot<-ggplot(hrdo, 
       aes(x = date,
           y = hr_hcl,
           color = ValveSt)) +
  geom_point()+ 
  labs( x = "Date", 
        y = "Mean HCl", 
        color = "Valve State",
        title = "Overblow and Denuder mean blanks")+
  scale_x_datetime(date_breaks = "2 day", date_labels = "%b %d")+
  scale_color_manual(values = c("steelblue1", "navy"))

ggsave(hrplot, filename = "C:/Users/grace/Documents/Mchem_project/hr_do.png")

#find day and night variations 
#filtered to remove data from 16th Aug onwards
diff_tv <-hravg_clean %>% filter(date <= as.POSIXct("2023-08-16 01:00:00"))%>%
  timeVariation(pollutant = c("diff"), ylab = "Absolute difference", name.pol = "", col = "deeppink4", main = "")


png("C:/Users/grace/Documents/Mchem_project/tv_didiff_16th_filt.png",
    res = 400, width = 2000, height = 1600)
diff_tv$plot$hour
dev.off()

#(correct)temperature variation (NOT Traw)

setwd("C:/Users/grace/Documents/Mchem_project/Calcs_and_data/Met")
filepath <- "C:/Users/grace/Documents/Mchem_project/Calcs_and_data/Met"
met_raw <- read_csv("C:/Users/grace/Documents/Mchem_project/Calcs_and_data/Met/THECIX_Met_Data_July24_Aug21.csv",
                    skip = 1)
#its finally working !!! had to restart R
#- in future make sure dtaa not open in notes

#this uses the NE met station measurements only
met_ne <- met_raw[,9:15] %>%
  mutate(date = as.POSIXct(date,format="%Y-%m-%d %H:%M",tz ="EST" )) %>% with_tz(tzone = "UTC")%>%
  timeAverage(avg.time = "1 hour", statistic = "mean")
#set date and tz , UTC -5 is the same as EST, Eastern Standard Time. I hope
#timeAverage for 1 hour to match denuder/overblow averages merging with

hr_temp <- left_join(hravg_clean,met_ne, by = "date") %>%
  filter(!between(date, as.POSIXct("2023-07-31 10:00:00"),
                  as.POSIXct("2023-07-31 18:00:00"))) %>%
  select(!Traw)%>%
  rename(xdiff = diff, Temp = "AirTemp_NE (ºC)") %>%
  pivot_longer(cols = c(Temp, xdiff),
               names_to = "var", values_to = "value")

#plot as geom_line because it looks clearer than geom_point in this case
temp_plot <-ggplot(hr_temp) + 
  aes(x = date, y = value)+
  labs(x = "Date", y = "", color = "", title = "Absolute difference and temperature over campaign")+
  geom_line(aes(color = var))+
  scale_color_manual(values = c("seagreen3","deeppink4"), labels = c("Temperature (ºC)", "Absolute difference")) +
  theme(strip.text = element_blank())+
  facet_wrap(~var,ncol = 1, scale = "free_y")

setwd("C:/Users/grace/Documents/Mchem_project")
ggsave(temp_plot, filename = "C:/Users/grace/Documents/Mchem_project/NE_tempPlot.png")

#next look at h2o

hrdat_clean <- dat %>%
filter(!(hour %in% c(3,6,9,12,15,18,21) & mins >= 3 & mins <=8) ) %>%
  rename(date = ts)%>%
  pivot_wider(names_from = sector, values_from = hcl)%>%
  group_by(date) %>% 
  timeAverage(avg.time = "1 hour", statistic = "mean")%>%
  select(date, ch3oh, h2co, no2, h2o, ch4, Overblow, Denuder) %>% 
  mutate(diff = abs(Denuder - Overblow))

hr_h2o <- hrdat_clean %>% 
  pivot_longer(cols = c(ch3oh, h2co, no2, h2o, ch4, diff), 
               names_to = "pollutant", values_to = "value" )

hr_speciesplot <-ggplot(hr_h2o, 
       aes(x = date,
           y = value,
           color = pollutant)) +
  geom_point()+ 
  labs( x = "Date", 
        y = "conc ", 
        color = "pollutant",
        title = "Absolute difference vs other species measured")+
  scale_x_datetime(date_breaks = "3 days", date_labels = "%b %d")+
  scale_color_manual(values = c("#F8766D","#D89000","#39B600","#00BFC4","#00B0F6","#E76BF3" ),labels = c("ch3oh","ch4","Absolute difference", "h2co","h2o","no2"))+
  theme(strip.text = element_blank(), legend.title = element_blank())+
  facet_wrap(~pollutant,ncol = 1, scale = "free_y")

ggsave(hr_speciesplot, filename = "C:/Users/grace/Documents/Mchem_project/species_plot2.png", 
       width = 10.5, height = 13.5)

cut_h2o <- hrdat_clean %>% select(date, diff, ch4, h2co) %>% pivot_longer(cols = c(h2co,ch4, diff), names_to = "pollutant", values_to = "value" )
#these species have the most similar patterns 
#but still no strong trend, let's move on shall we
ch4_plot <-ggplot(cut_h2o, 
       aes(x = date,y = value, color = pollutant)) +
  geom_point()+ 
  labs( x = "Date",y = "ppb",color = "pollutant",title = "Absolute difference vs other species measured")+
  scale_x_datetime(date_breaks = "2 days", date_labels = "%b %d")+
  theme(strip.text = element_blank())+
  facet_wrap(~pollutant,ncol = 1, scale = "free_y")+
  scale_color_manual(values = c("#D89000","#39B600","#00BFC4"), labels = c("ch4","Absolute difference", "h2co"))+
  theme(legend.title = element_blank())

ggsave(ch4_plot, filename = "C:/Users/grace/Documents/Mchem_project/ch4_plot.png", 
       width = 6, height = 8)

#total HCl plot (ValveW ==2)
library(openair)
hcl_df <- dfAnal %>% 
  filter(ValveW == 0| ValveW == 8|ValveW == 2) %>%
  mutate(period = cumsum(flag !=lag(flag, default = flag[1])) ) %>%
  filter(flag != 1) %>%
  mutate(sector = case_when(ValveW == 8 ~ "Overblow",
                            ValveW == 0 ~ "Denuder",
                            ValveW == 2 ~ "Total_hcl"),)%>%
  mutate(hour = hour(ts), mins = minute(ts)) %>%
  filter(!(hour %in% c(3,6,9,12,15,18,21) & mins >= 3 & mins <=8) ) %>%
  rename(date = ts) %>% 
  pivot_wider(names_from = sector, values_from = hcl) %>%
  select(date, Total_hcl, Denuder, Overblow)%>%
  timeAverage(avg.time = "1 hour", statistic = "mean") %>%
  mutate(diff = abs(Denuder - Overblow), f_hcl = diff/Total_hcl)

hcl_plot<-ggplot(hcl_df, 
       aes(x = date,
           y = f_hcl)) +
  geom_point()+ 
  labs( x = "Date", 
        y = "difference/ total HCl",
        title = "Absolute difference as a fraction of total HCl")+
  scale_x_datetime(date_breaks = "2 days", date_labels = "%b %d")+
  theme(strip.text = element_blank())+
  scale_y_continuous(breaks = seq(-100,400, by = 4))

ggsave(hcl_plot, filename = "C:/Users/grace/Documents/Mchem_project/total_hcl_fraction1.1.png",
       width = 6, height = 10)

#a tad hard to see so will filter points that are massively off so the rest is clearer
# mention to Wes
hcl <- hcl_df %>% filter(!f_hcl >= 100, f_hcl >=-50 )

filt_hcl<-ggplot(hcl, 
       aes(x = date,y = f_hcl)) +
  geom_point()+ 
  labs( x = "Date",  y = "difference/ total HCl",title = "Absolute difference as a fraction of total HCl")+
  scale_x_datetime(date_breaks = "2 days", date_labels = "%b %d")+
  theme(strip.text = element_blank())+
  scale_y_continuous(breaks = seq(-50,50, by = 2))

ggsave(filt_hcl, filename = "C:/Users/grace/Documents/Mchem_project/filt_total_hcl_fraction1.1.png",
       width = 6,height= 8)

#save as interactive plot

int_hcl<-ggplotly(hcl_plot)

#save as widget
htmlwidgets::saveWidget(
  widget = int_hcl,
  file = "C:/Users/grace/Documents/Mchem_project/int_hcl_sep2.html",
  selfcontained = TRUE)
)
#next is total chlorine :
#timezone is UTC -4 --> set to "America/New_York"
hal_raw <- read_csv("C:/Users/grace/Documents/Mchem_project/Calcs_and_data/totalhalogen/0906_picarro_0728-0821_data.csv")

#have used the edited total chlorine values - ask Wes if that's right one
#convert all timezones to UTC with with_tz before join to df (in UTC)
hal<- hal_raw %>% mutate(date = as.POSIXct(ts, format = "%Y-%m-%d %H:%M" , tz = "America/New_York")) %>%
  with_tz(ts,tzone = "UTC") %>% 
  rename(TCl = TClg_edited) %>% 
  select(date, TCl)%>%
  timeAverage(avg.time = "1 hour", statistic = "mean")

#combine dfs, and remove total_hcl column so don't get confused for this plot
totcl_df <- left_join(hcl_df,hal, by = "date")%>%
  select(!Total_hcl)%>%
  mutate(Ratio = diff/TCl)%>%
  rename(Difference = diff, "Total chlorine" = TCl)%>%
  pivot_longer(cols = c(Difference,"Total chlorine", Ratio), names_to = "type", values_to = "value")

totcl_plot<-ggplot(totcl_df) + 
  aes(x = date, y = value)+
  labs(x = "Date", y = "", color = "", title = "Absolute difference and total chlorine over campaign")+
  geom_line(aes(color = type))+
  scale_color_manual(values = c("deeppink4", "blue", "green"))+
  facet_wrap(~type,ncol= 1, scale = "free_y")+
  theme(legend.position = "none")

ggsave(totcl_plot, filename = "C:/Users/grace/Documents/Mchem_project/Total_Cl_fn2.png",
       width = 6, height = 8)


#Particle data 
#ill come back to
# Cl- data


#calculate Reynolds number 

#temp in degrees, pressure = kPa, flowrate = litres per minute
#(these are converted later on)
sampleLineFlow <- data.frame(temp_C = rep(25, times = 81), pressure_kPa = rep(101.325, times=  81), flow_rate_slpm = seq(0,80, by = 1))
sampleLineFlow <- data.frame(temp_C = 25, pressure_kPa = rep(101.325, times=  81), flow_rate_slpm = 70)
##### this is Will's code
# From here if you care: W. Sutherland. “The viscosity of gases and molecular force”. Philosophical Magazine 5 (1893), 507–531.
# https://web.stanford.edu/~cantwell/AA210A_Course_Material/AA210A_Resources/Sutherland_1893_Paper_on_Viscosity.pdf
sutherland_viscosity = function(temp){
  ((1.458e-6)*(temp^(3/2)))/(temp+110.4)
}

# https://en.wikipedia.org/wiki/Reynolds_number
reynolds = function(density,temp,transit_speed,inDiam){
  (density*transit_speed*inDiam)/sutherland_viscosity(temp)
}

#will use to work out the flow rate required for a specific Reynolds number 
transit = function(density,temp, ReyNum,inDiam){
  (ReyNum*sutherland_viscosity(temp))/(density*inDiam)
}

#internal diameter of flow tube
inDiam = 0.024 # m

tubeArea = (pi*(inDiam/2)^2) # m^2

# flow rate is in standard liters per minute,
# the constant converts into something sensible like m^3 s^-1
#this UNITS are actually m/s for transit speed because divided by area 
#*
sampleLineParsed = sampleLineFlow %>%
  mutate(temp = temp_C+273.15, # C -> K
         transit_speed = (flow_rate_slpm*1.666667e-05)/tubeArea,
         pressure_Pa = pressure_kPa*1e3) %>% # kPa -> Pa
  select(flow_rate_slpm,transit_speed,pressure_Pa,temp) %>%   #removed date from select
  mutate(density = pressure_Pa/(287.058*temp)) # this bit calculates the density from the line pressure and temperature

sampleLineParsed$RenNum = reynolds(density = sampleLineParsed$density,
                                   temp = sampleLineParsed$temp,
                                   transit_speed = sampleLineParsed$transit_speed,
                                   inDiam = inDiam)

# transit velocity (should churn out same as transit_speed)
sampleLineParsed$calTransit = transit(density = sampleLineParsed$density,
                                  temp = sampleLineParsed$temp,
                                   ReyNum = sampleLineParsed$RenNum,
                                   inDiam = inDiam )

TransCrit = print(transit(density = 1.183892, temp = 298.15, ReyNum = 4000, inDiam= inDiam))
Flowcrit = print(((transit(density = 1.183892, temp = 298.15, ReyNum = 4000, inDiam= inDiam))*tubeArea)/1.666667e-05)

#flow crit = 70.020446 litres per minute   ##expected for standard litres per second, but ask wes if pressure is reduced
#nvm wes is not interested in the reynolds number
#try 200 == 35lpm
#aerocalc - used to measure turbulence
#transit speed crit  = 2.586433 m/s
#70 lmp = 3988.35096 Reynolds number , transit speed = 2.57890006 m/s
#71 lpm = 4045.32740, transit speed = 2.61574149 m/s

rey_plot<-ggplot(sampleLineParsed)+
  aes(x = flow_rate_slpm, y  =RenNum)+
  labs(x = "flow rate lpm", y = "Reynolds Number", title = "Reynolds Number Plot", subtitle = "Turbulent flow at flow rate >= 70.020 lpm (2.586 m/s)", caption  = "Temp = 298.15K, Pressure = 101325 Pa, Internal diameter = 0.024 m")+
  geom_line()+
  geom_hline(yintercept = 4000, linetype = "dashed", color = "red")+
  scale_x_continuous(breaks = seq(0,80, by = 5))

ggsave(rey_plot, filename = "C:/Users/grace/Documents/Mchem_project/Reynolds_plot.png")

# looking at suspected biomass burning cause of HCl plume
#CO and ACN data
#CO data in EDT
CO_raw <- read_csv("C:/Users/grace/Documents/Mchem_project/Calcs_and_data/CO_hollywood.csv", )
CO <- CO_raw %>% force_tz(date, tz = "America/New_York")%>% with_tz(date, tz = "UTC") %>% rename(CO_ppm = "Mixing Ratio (ppm)", date = DateTime ) %>% timeAverage(avg.time = "hour")

#combine with hcl dataframe to plot against HCl
comb_hcl <- left_join(hcl_df, CO, by = "date") 

#need to divide ACN by 63 to get in ppb
#ACN timezone is UTC - which is set to automatically
ACN_raw <- read_csv("C:/Users/grace/Documents/Mchem_project/Calcs_and_data/Acetonitrile raw data.csv")
ACN <- ACN_raw %>% rename(ACN = "'C2H4N+'", date = t_start_Buf) %>%  
  mutate(ACN_ppb = ACN/63)%>% 
  select(date, ACN_ppb) %>% timeAverage(avg.time = "hour")

comb_all_hcl <- left_join(comb_hcl,ACN, by = "date") %>% mutate(enhancement = ACN_ppb/CO_ppm)%>%
  pivot_longer(cols = c("Total_hcl","CO_ppm", "ACN_ppb", "enhancement"),names_to = "Species", values_to = "conc")

all_enh<-ggplot(comb_all_hcl)+
  aes(x = date, y = conc, color = Species ) +
  geom_line()+ facet_wrap(~Species,ncol= 1, scale = "free_y")

comb_all_hcl_filt <- comb_all_hcl %>% filter(between(date,ymd("2023-07-31"), ymd("2023-08-07")))
filt_enh <- ggplot(comb_all_hcl_filt)+
  aes(x = date, y = conc, color = Species ) +
  geom_line()+ facet_wrap(~Species,ncol= 1, scale = "free_y")

ggsave(all_enh, filename = "C:/Users/grace/Documents/Mchem_project/all_enh_plots.png", height = 10, width = 9)
ggsave(filt_enh, filename = "C:/Users/grace/Documents/Mchem_project/filt_enhancement_plots.png", height = 10, width = 9)

#split graphs up into fewer variables

ACN_CO <- left_join(comb_hcl,ACN, by = "date") %>% mutate(enhancement = ACN_ppb/CO_ppm)%>%
  pivot_longer(cols = c("Total_hcl","CO_ppm", "ACN_ppb"),names_to = "Species", values_to = "conc")

ACN_CO_plot<-ggplot(ACN_CO)+
  aes(x = date, y = conc, color = Species ) +
  geom_line()+ facet_wrap(~Species,ncol= 1, scale = "free_y")

ggsave(ACN_CO_plot, filename= "C:/Users/grace/Documents/Mchem_project/ACN_CO_plot.png")

enh <- left_join(comb_hcl,ACN, by = "date") %>% mutate(enhancement = ACN_ppb/CO_ppm)%>%
  pivot_longer(cols = c("Total_hcl","enhancement"),names_to = "Species", values_to = "conc")

ggplot(enh)+
  aes(x = date, y = conc, color = Species ) +
  geom_line()+ facet_wrap(~Species,ncol= 1, scale = "free_y")

enh_plot<-ggplot(comb_all_hcl)+
  aes(x = date, y = enhancement ) +
  geom_line()+
  scale_y_continuous(breaks = seq(-25,300, by = 10))

ggsave(enh_plot, filename = "C:/Users/grace/Documents/Mchem_project/enhancement.png")



# okay lets do the delta vs particle and gas phase chloride
#AIMIC time is in local (EDT)
AIMIC_raw <- read_csv("C:/Users/grace/Documents/Mchem_project/Calcs_and_data/23July_AIMIC_obs3.csv")

AIMIC <- AIMIC_raw %>% select(Time, "gCl-", "pCl-") %>% rename(date = Time) %>% 
  mutate(date = as.POSIXct(date,format="%d/%m/%Y %H:%M",tz ="America/New_York" )) %>% with_tz(date, tz = "UTC")

#Need to convert UNITS !!! gCl and pCl are in micrograms per meter cubed
#for the sake of ease will convert diff and total hcl to ppb **maybe meant micrograms/m3 (?)
#Concentration (µg/m3) = molecular weight x concentration (ppb) ÷ 24.45
# hcl molecular weight  = 36.46 g/mol
# 36.46/24.45 * concentration
chloride <- left_join(hcl_df, AIMIC, by = "date") %>%
  mutate(delta = (diff*(36.46/24.45)), Total_hcl = (Total_hcl*(36.46/24.45))) %>%
  pivot_longer(cols = c(delta,Total_hcl,"gCl-","pCl-"), names_to = "name", values_to = "value")

library(wesanderson)
chloride_plot<-ggplot(chloride)+
  aes(x = date, y = value, color = name ) +
  geom_line()+ facet_wrap(~name,ncol= 1, scale = "free_y")+
  labs(y=  "conc (µg/m³)", fill = "", color = "")+
  scale_color_manual(values=wes_palette(n=4, name="Darjeeling1"))

ggsave(chloride_plot, filename = "C:/Users/grace/Documents/Mchem_project/chloride_comb_hcl.png", height = 10, width = 9)



# next plot pCL on one axis, delta on another axis

pCl_df <- left_join(hcl_df, AIMIC, by = "date") %>% rename(pCl = "pCl-", gCl = "gCl-") %>%
  mutate(delta = (diff*(36.46/24.45)), ratio  = delta/pCl, Total_hcl = (Total_hcl*(36.46/24.45))) %>%
  filter(pCl <= 0.2, gCl <=0.35)

p_filt <- pCl_df %>% filter(pCl <=0.1, delta <=0.3)

p<-ggplot(data = pCl_df, aes(x = delta, y = pCl,  color = pCl)) +
  geom_point(color = "#d19c2f")+
  labs(y = "pCl- (µg/m3)", x = "delta (µg/m3)")+
  scale_y_continuous(breaks = seq(0,0.17, by = 0.05))

ggsave(p, filename = "C:/Users/grace/Documents/Mchem_project/pCl_delt_correlation.png", height = 7, width =10)
ggsave(p_plot_filt, filename = "C:/Users/grace/Documents/Mchem_project/pCl_delt_correlation_filt.png", height = 7, width =10)

p_plot_filt<- ggplot(data = p_filt, aes(x = delta, y = pCl,  color = pCl)) +
  geom_point(color = "#d19c2f")+
  labs(y = "pCl- (µg/m3)", x = "delta (µg/m3)")+
  scale_y_continuous(breaks = seq(0,0.17, by = 0.05))+ labs(caption = "filtered: pCl < 0.1, delta < 0.3")

g <-ggplot(data = pCl_df, aes(x = delta, y = gCl,  color = gCl)) +
  geom_point(color = "#49997c")+
  labs(y = "gCl- (µg/m3)", x = "delta (µg/m3)")+
  scale_y_continuous(breaks = seq(0,0.5, by = 0.05))

ggsave(g, filename = "C:/Users/grace/Documents/Mchem_project/gCl_delt_correlation.png", height = 7, width =10)



#plot relative humidity from met data vs Hcl data


met_raw <- read_csv("C:/Users/grace/Documents/Mchem_project/Calcs_and_data/Met/THECIX_Met_Data_July24_Aug21.csv",
                    skip = 1)

#this uses the NE met station measurements only
met_ne <- met_raw[,9:15] %>%
  mutate(date = as.POSIXct(date,format="%Y-%m-%d %H:%M",tz ="EST" )) %>% with_tz(tzone = "UTC")%>%
  timeAverage(avg.time = "1 hour", statistic = "mean")
#set date and tz , UTC -5 is the same as EST, Eastern Standard Time. I hope
#timeAverage for 1 hour to match denuder/overblow averages merging with

hcl_temp <- left_join(hcl_df,met_ne, by = "date") %>% rename(Humidity = "RH_NE (%)", delta = diff)%>%
  pivot_longer(cols = c(Total_hcl, delta, Humidity), names_to  = "var", values_to = "conc")

# plot humidity vs hcl
#also plot against delta ig incase Wes wants that


ggplot(hcl_temp)+
  aes(x = date, y = conc, color = var)+
  geom_line()+ facet_wrap(~var,ncol= 1, scale = "free_y")


h_plot <-ggplot(hcl_temp, aes(x = date, y = delta)) +
  geom_line(aes(color = "delta")) +
  geom_line(aes(y = Humidity/500, color = "humidity")) +
  scale_x_continuous(breaks = seq(0, 336, 24)) +
  scale_y_continuous(sec.axis = sec_axis(~.*500, name="Relative Humidity (%)")) +
  labs(x = "date", y = "Delta", color = "", fill = "") +
  scale_color_manual(values = c("orange2", "gray30"))+
  scale_fill_discrete(labels=c('delta', 'Relative Humididy (%)'))

ggsave(h_plot, filename = "C:/Users/grace/Documents/Mchem_project/humidity_delta.png", height = 7, width =10)

#this plot isn't really as important
hcl_hum_plot<-ggplot(hcl_temp, aes(x = date, y = Total_hcl)) +
  geom_line(aes(color = "HCl")) +
  geom_line(aes(y = Humidity/500, color = "humidity")) +
  scale_x_continuous(breaks = seq(0, 336, 24)) +
  scale_y_continuous(sec.axis = sec_axis(~.*500, name="Relative Humidity (%)")) +
  labs(x = "date", y = "Total HCl", color = "", fill = "") +
  scale_color_manual(values = c("purple3", "gray30"))+
  scale_fill_discrete(labels=c('delta', 'Relative Humididy (%)'))


#**** theme(legend.position =  "bottom")   - puts legend under the graph
#*# legend.title = element_blank()  - will get rid of legend titles

ggsave(hcl_hum_plot, filename = "C:/Users/grace/Documents/Mchem_project/humidity_hcl.png", height = 7, width =10)


# prep for plot of delta vs H2O

TILDAS_df <- dfAnal %>% 
  filter(ValveW == 0| ValveW == 8) %>%
  mutate(period = cumsum(flag !=lag(flag, default = flag[1])) ) %>%
  filter(flag != 1) %>%
  mutate(sector = case_when(ValveW == 8 ~ "Overblow",
                            ValveW == 0 ~ "Denuder"),)%>%
  mutate(hour = hour(ts), mins = minute(ts)) %>%
  filter(!(hour %in% c(3,6,9,12,15,18,21) & mins >= 3 & mins <=8) ) %>%
  rename(date = ts)%>% 
  pivot_wider(names_from = sector, values_from = hcl)%>%
  timeAverage(avg.time = "1 hour", statistic = "mean") %>%
  mutate(diff = abs(Denuder - Overblow)) %>%
  select(date, h2o, Denuder, Overblow, diff)

h2o_plot<-ggplot(TILDAS_df, aes(x = date, y = diff)) +
  geom_line(aes(color = "delta")) +
  geom_line(aes(y = h2o/80000000, color = "h2o")) +
  scale_y_continuous(sec.axis = sec_axis(~.*80000000, name="H2O")) +
  labs(x = "date", y = "delta", color = "", fill = "") +
  scale_color_manual(values = c("steelblue", "gray30"))+
  scale_fill_discrete(labels=c('delta', 'H2O'))

ggsave(h2o_plot, filename = "C:/Users/grace/Documents/Mchem_project/h2o_delta.png", height = 7, width =10)

##
#####Particle composition - this all loads in nicely now but don't really need it at the moment
##

file_names = list.files("C:/Users/grace/Documents/Mchem_project/Calcs_and_data/SMPS",
                        full.names = T,
                        recursive = T,
                        pattern = ".csv")

#if i skip out row with row names, wont load in (i.e skip = 26)
particle_raw = file_names %>% 
  map_dfr(read_csv, col_names = F, skip = 25)

#assign column names as same as file i edited w/ all the nonsense at start removed
example<- read_csv("C:/Users/grace/Documents/Mchem_project/Calcs_and_data/new_SMPS_example.csv")
colnames(particle_raw) <- colnames(example)

#check all column names are correctly labelled
colnames(example)

#this removes the rows with the column names in the by searching for rows
#in the date column that say "Date" and only selecting  rows that don't have "Date" in them
#negate = TRUE returns elements that don't match conditions i.e what we want :)
particle_filt <-particle_raw %>%
  filter(str_detect(Date, "Date", negate = TRUE))




#back to AIMIC data 
AIMIC_raw <- read_csv("C:/Users/grace/Documents/Mchem_project/Calcs_and_data/23July_AIMIC_obs3.csv")

AIMIC_full <- AIMIC_raw %>% rename(date = Time) %>% 
  mutate(date = as.POSIXct(date,format="%d/%m/%Y %H:%M",tz ="America/New_York" )) %>% with_tz(date, tz = "UTC")
 # rename(pCl = "pCl-", gCl = "gCl-")
         #,NH4 = "NH4+...7", DMA = "DMA+...8", DEA = "DEA+...9", SO4 = "SO42-...13", Oxalate = "Oxalate...14",)

pCl_df <- left_join(hcl_df, AIMIC_full, by = "date") %>% rename(pCl = "pCl-", gCl = "gCl-") %>%
  mutate(delta = (diff*(36.46/24.45)), ratio  = delta/pCl, Total_hcl = (Total_hcl*(36.46/24.45)))
   
rm(testing)

p<-ggplot(data = pCl_df, aes(x = delta, y = pCl,  color = pCl)) +
  geom_point(color = "#d19c2f")+
  labs(y = "pCl- (µg/m3)", x = "delta (µg/m3)")+
  scale_y_continuous(breaks = seq(0,0.17, by = 0.05))


#  filter(pCl <= 0.2, gCl <=0.35)

ggplot(data = pCl_df, aes(x = delta, y = Cl, color = Cl)) +
  geom_point(color = "#d19c2f")+
  labs(y = "pCl- (µg/m3)", x = "delta (µg/m3)")+
  scale_y_continuous(breaks = seq(0,0.17, by = 0.05))

#next want to find the date/times when delta was greater than 0.2 to search other 
#species concs at the corresponding times

#library(latex2exp)  
#lab(y = latex2exp::TeX("$O_3$))  or for superscript "$O^3$" , for more than one number $O^{32}$
#source scripts that load in the data - e.g all the stuff that would load a desired df
#create an r script and save separately for these thins - could save time in the long run
#what is Viking? look up
#also look at using quattro /r markdown - allows you to keep text and plots alongside the raw code
# can use read sheets for excel files with more than one sheet ???
#read.table() has lots of variables e.g one that ignores blank space before a dataframe

pCl_filt2 <- pCl_df %>%
  mutate(sector = case_when( delta > 0.15 & between(date, ymd("2023-08-01"),ymd("2023-08-04")) ~ "deeppink",
                             delta > 0.15 & between(date, ymd("2023-08-04"),ymd("2023-08-05")) ~ "steelblue2",
                             delta > 0.15 & between(date, ymd("2023-08-05"),ymd("2023-08-07")) ~ "seagreen4",
                             delta > 0.15 & between(date, ymd("2023-08-07"),ymd("2023-08-10")) ~ "red",
                             delta > 0.15 & between(date, ymd("2023-08-14"),ymd("2023-08-18")) ~ "blue",
                             delta > 0.15 & between(date, ymd("2023-08-18"), ymd("2023-08-22")) ~ "green3",
                             is.na(delta) & is.numeric(pCl) ~ "black",
                             TRUE ~ "#d19c2f"))  %>%
  filter(pCl <= 0.2, gCl <=0.35)

date2_plot <- ggplot(data = pCl_filt2, aes(x = date, y = pCl, color = sector)) +
  geom_point()+
  labs(y = "pCl- (µg/m3)", x = "date")+
  scale_y_continuous(breaks = seq(0,0.20, by = 0.02))+
  scale_color_manual(
    values = c( "deeppink" = "deeppink","steelblue2" = "steelblue2","seagreen4"="seagreen4","red" = "red",
      "blue" = "blue","green3" = "green3","black"= "black","#d19c2f" = "#d19c2f"),
    breaks = c("deeppink", "steelblue2","seagreen4","red", "blue", "green3","black", "#d19c2f"),
    labels = c("3am Aug 1 - 1pm Aug 3", "5am -3pm Aug 4","10pm Aug 5 - 6am Aug 6", "2-3am Aug 8", "8pm Aug 15- 10pm Aug 17 ", " 12am - 11pm Aug 20","delta = NA","delta < 0.15"),
    guide = guide_legend(title = NULL)) +
  scale_x_datetime(date_breaks = "2 day", date_labels = "%b %d")+
  theme(legend.position =  "bottom") 

ggsave(date2_plot, filename = "C:/Users/grace/Documents/Mchem_project/new_DATE_large_delt_pCl.png", height = 10, width= 14)


delta2 <-ggplot(data = pCl_filt2, aes(x = delta, y = pCl, color = sector)) +
  geom_point()+
  labs(y = "pCl- (µg/m3)", x = "delta (µg/m3)")+
  scale_y_continuous(breaks = seq(0,0.20, by = 0.02))+
  scale_color_manual(
    values = c( "deeppink" = "deeppink","steelblue2" = "steelblue2","seagreen4"="seagreen4","red" = "red",
                "blue" = "blue","green3" = "green3","#d19c2f" = "#d19c2f"),
    breaks = c("deeppink", "steelblue2","seagreen4","red", "blue", "green3", "#d19c2f"),
    labels = c("3am Aug 1 - 1pm Aug 3", "5am -3pm Aug 4","10pm Aug 5 - 6am Aug 6", "2-3am Aug 8", "8pm Aug 15- 10pm Aug 17 ", " 12am - 11pm Aug 20","delta < 0.15"),
    guide = guide_legend(title = NULL))+
  theme(legend.position =  "bottom")

ggsave(delta2, filename = "C:/Users/grace/Documents/Mchem_project/newsimpler_delt_pCl.png", height = 10, width= 14)

#this short data frame summaries all the coloured in data points
#useful when  labeling sections of plots
delt_filt <-pCl_filt2 %>% 
  filter(sector != "#d19c2f") %>% 
  select(date, pCl, diff,Total_hcl, sector)


#next look at the other species for the given points when delta > 0.2
#(adjust delt_filt for this) & look at creative ways to plot, but first do bar charts ?
#i would prefer time series, there aren't many datapoints...
#ask wes if look at particle phase species measurements only?



new_Cl<-pCl_df%>% select(1,84, 11:37) %>% 
  rename(gNH4 = "NH4+...7", gDMA = "DMA+...8", gDEA = "DEA+...9", gNO2 = "NO2-...11",
         gNO3 = "NO3-...12", gSO4 = "SO42-...13", gOxalate= "Oxalate...14", gFormic = "Formic acid",
         gAcetic = "Acetic acid", pNa = "Na+", pNH4 = "NH4+...19", pNO2 = "NO2-...26",
         pNO3 = "NO3-...27", pSO4 = "SO42-...28", pOxolate = "Oxalate...29",
         pPhosphate = "Phosphate...30",pFormate = "Formate...31",pAcetate = "Acetate...32" ) %>%
  select(!Gas) %>% select(!Particle) %>% select(!pAcetate) %>% select(!pFormate) %>% select(!pPhosphate) 

new1_Cl <- new_Cl %>%
  pivot_longer(cols = 3:24, names_to = "species", values_to = "conc") %>% filter(delta > 0.15)

print(colnames(new_Cl))

ggplot(new1_Cl,aes(x = date, y = conc, color = species))+
  geom_point()+
  facet_wrap(~species,ncol = 4, scale = "free_y")+
  theme(legend.position =  "bottom")

#now need to separate out the chunks into
#maybe separate by particle and gas phase measurements?
#look at bar charts for wes

particle_spec <- new_Cl %>% select(1,2, 13:24) %>% 
  pivot_longer(cols = 3:14, names_to = "species", values_to = "conc") %>% 
  filter(delta > 0.15)

particle_plot<-ggplot(particle_spec,aes(x = date, y = conc, color = species))+
  geom_point()+
  facet_wrap(~species,ncol = 4, scale = "free_y")+
  theme(legend.position =  "bottom")

ggsave(particle_plot, filename = "C:/Users/grace/Documents/Mchem_project/particle_spec_delta.png", height = 10, width= 20)

gas_spec <- new_Cl %>% select(1,2, 3:12) %>% 
  pivot_longer(cols = 3:12, names_to = "species", values_to = "conc") %>% 
  filter(delta > 0.15)

gas_plot<-ggplot(gas_spec,aes(x = date, y = conc, color = species))+
  geom_point()+
  facet_wrap(~species,ncol = 5, scale = "free_y")+
  theme(legend.position =  "bottom")

ggsave(gas_plot, filename = "C:/Users/grace/Documents/Mchem_project/gas_spec_delta.png", height = 10, width= 20)

