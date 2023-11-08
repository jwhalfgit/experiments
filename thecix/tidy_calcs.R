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
  mutate(date = as.POSIXct(date,format="%Y-%m-%d %H:%M",tz ="EST" )) %>%
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

library("readxl")
AMIC_ra<- read_excel("C:/Users/grace/Documents/Mchem_project/Calcs_and_data/23July_AIMIC_observation.xlsx")

#will come back to this once have spoken to Wes



#calculate Reynolds number 

#temp in degrees, pressure = kPa, flowrate = litres per minute
#(these are converted later on)
sampleLineFlow <- data.frame(temp_C = rep(25, times = 81), pressure_kPa = rep(101.325, times=  81), flow_rate_slpm = seq(0,80, by = 1))

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

#flow crit = 70.020446 litres per minute
#transit speed crit  = 2.586433 m/s
#70 lmp = 3988.35096 Reynolds number , transit speed = 2.57890006 m/s
#71 lpm = 4045.32740, transit speed = 2.61574149 m/s

rey_plot<-ggplot(sampleLineParsed)+
  aes(x = flow_rate_slpm, y  =RenNum)+
  labs(x = "flow rate lpm", y = "Reynolds Number", subtitle = "Reynolds Number: Temp = 298.15K, Pressure = 101325 Pa, Internal diameter = 0.024 m")+
  geom_line()+
  geom_hline(yintercept = 4000, linetype = "dashed", color = "red")+
  scale_x_continuous(breaks = seq(0,80, by = 5))

ggsave(rey_plot, filename = "C:/Users/grace/Documents/Mchem_project/Reynolds_plot.png")
