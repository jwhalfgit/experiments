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
diff_tv <-timeVariation(hravg_clean, pollutant = c("diff"), ylab = "Absolute difference", name.pol = "", col = "deeppink4", main = "")

png("C:/Users/grace/Documents/Mchem_project/tv_didiff.png",
    res = 400, width = 2000, height = 1600)
diff_tv$plot$hour
dev.off()

#(correct)temperature variation (NOT Traw)

setwd("C:/Users/grace/Documents/Mchem_project/Calcs_and_data/Met")
met_raw <- read_csv("C:/Users/grace/Documents/Mchem_project/Calcs_and_data/Met/THECIX_Met_Data_July24_Aug21.csv",
                    skip = 1)
#its finally working !!! had to restart R

#this uses the SW met station measurements only
met_sw <- met_raw[,1:7] %>%
  mutate(date = as.POSIXct(Date,format="%Y-%m-%d %H:%M",tz ="EST" )) %>%
  select(!Date)%>%
  timeAverage(avg.time = "1 hour", statistic = "mean")
#set date and tz , UTC -5 is the same as EST, Eastern Standard Time. I hope
#timeAverage for 1 hour to match denuder/overblow averages merging with

hr_temp <- left_join(hravg_clean,met_sw, by = "date") %>%
  filter(!between(date, as.POSIXct("2023-07-31 10:00:00"),
                  as.POSIXct("2023-07-31 18:00:00"))) %>%
  select(!Traw)%>%
  rename(xdiff = diff, Temp = "AirTemp_SW (ºC)") %>%
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
ggsave(temp_plot, filename = "C:/Users/grace/Documents/Mchem_project/correct_tempPlot.png")

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
  mutate(hour = hour(hcl_raw$ts), mins = minute(hcl_raw$ts)) %>%
  filter(!(hour %in% c(3,6,9,12,15,18,21) & mins >= 3 & mins <=8) ) %>%
  rename(date = ts) %>% 
  pivot_wider(names_from = sector, values_from = hcl) %>%
  select(date, Total_hcl, Denuder, Overblow)%>%
  timeAverage(avg.time = "1 hour", statistic = "mean") %>%
  mutate(diff = abs(Denuder - Overblow))

hcl_plot<-ggplot(hcl_df, 
       aes(x = date,
           y = f_hcl)) +
  geom_point()+ 
  labs( x = "Date", 
        y = "difference/ total HCl",
        title = "Absolute difference as a fraction of total HCl")+
  scale_x_datetime(date_breaks = "2 days", date_labels = "%b %d")+
  theme(strip.text = element_blank())

ggsave(hcl_plot, filename = "C:/Users/grace/Documents/Mchem_project/total_hcl_fraction.png")

#a tad hard to see so will filter point that's massively off
# mention to Wes
hcl <- hcl_df %>% filter(!f_hcl >= 100)

filt_hcl<-ggplot(hcl, 
       aes(x = date,y = f_hcl)) +
  geom_point()+ 
  labs( x = "Date",  y = "difference/ total HCl",title = "Absolute difference as a fraction of total HCl")+
  scale_x_datetime(date_breaks = "2 days", date_labels = "%b %d")+
  theme(strip.text = element_blank())

ggsave(filt_hcl, filename = "C:/Users/grace/Documents/Mchem_project/filt_total_hcl_fraction.png")
                            