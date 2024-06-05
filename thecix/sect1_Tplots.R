# Toronto intro plots


#load.tildas.data(START = "2023-07-31", STOP = "2023-08-20",
STC = TRUE, HCLONLY= FALSE)

dfAnalT <- df.str %>% 
  left_join(df.stc)  %>% 
  filter(!between(ts,ymd_hm("2023-08-01 16:15"), ymd_hm("2023-08-01 19:39"))) %>% 
  
  filter(!between(ts,ymd_hm("2023-08-16 12:09"), ymd_hm("2023-08-16 12:15")))

dfAnalT <- dfAnalT %>% 
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
                                  ymd_hm("2023-08-17 18:58")) & ValveW == 8,0)) %>%
  mutate(ts= floor_date(ts, "seconds"))%>%
  filter(ts>ymd_hm("2023-07-31 20:00"))

##NOTE above, filtered to date > 8pm 31st july just for TimeAverage function purposes

valveChangeix <- which(c(0,diff(dfAnalT$ValveW))!=0)
valveChangeixTail <- unique(unlist(lapply(valveChangeix,function(x){seq(x,x+5)})))
valveChangeixTail <- valveChangeixTail[!valveChangeixTail > nrow(dfAnalT)]

dfAnalT$flag <- 0
dfAnalT$flag[valveChangeixTail] <- 1


### this for 
datfx <- dfAnalT %>% 
  filter(ValveW == 0| ValveW == 8|ValveW == 2) %>%
  mutate(period = cumsum(flag !=lag(flag, default = flag[1])) ) %>%
  filter(flag != 1) %>%
  mutate(sector = case_when(ValveW == 8 ~ "Overblow",
                            ValveW == 0 ~ "Denuder",
                            ValveW == 2 ~ "Total_hcl"),)%>%
  mutate(hour = hour(ts), mins = minute(ts)) %>%
  filter(!(hour %in% c(3,6,9,12,15,18,21) & mins >= 3 & mins <=8) ) %>%
  rename(date = ts) %>%
  group_by(period)%>% 
  #filter(between(date,ymd_hm("2024-04-23 09:25"), ymd_hm("2024-04-23 14:20")))%>%
  mutate(cell_temperature = Traw - 273.15)%>%
  reframe(hcl = mean(hcl), h2o = mean(h2o),DATE = min(date),
          pressure_in_cell=mean(Praw), cell_temp = mean(cell_temperature),
          sector = unique(sector), ValveW = unique(ValveW), AD8 = mean(AD8))%>%
  rename(date = DATE, HCl = hcl,)%>%
  #filter(sector!= "Total_hcl") 
  pivot_longer(cols = c(HCl, cell_temp), 
               names_to = "variable", values_to = "value" )
#pivot_longer(cols = c(h2o, HCl, Inlet_temperature,pressure_in_cell), 
#names_to = "variable", values_to = "value" )
#aa<-


library(openair)

df<- dfAnalT %>% 
  filter(ValveW == 0| ValveW == 8|ValveW == 2) %>%
  mutate(period = cumsum(flag !=lag(flag, default = flag[1])) ) %>%
  filter(flag != 1) %>%
  mutate(sector = case_when(ValveW == 8 ~ "Overblow",
                            ValveW == 0 ~ "Denuder",
                            ValveW == 2 ~ "Total_hcl"),)%>%
  mutate(hour = hour(ts), mins = minute(ts)) %>%
  filter(!(hour %in% c(3,6,9,12,15,18,21) & mins >= 3 & mins <=8) ) %>%
  rename(date = ts) 
  
deltam <- df %>% 
  pivot_wider(names_from = "sector", values_from = "hcl")%>%
  timeAverage(avg.time = "15 min", statistic = "mean")%>%
  pivot_longer(cols = c(Overblow,Denuder), names_to = "sector", values_to = "hcl")


#aa<-
ggplot(deltam, 
       aes(x = date,
           y = hcl,
           color = sector)) +
  geom_point(size = 0.75)+ 
  labs( x = "", 
        y = " HCl (ppb)", 
        color = "")+
  scale_x_datetime(date_breaks = "2 day", date_labels = "%b %d")+
  scale_y_continuous(breaks=seq(0,2,by = 0.25))+
  scale_color_manual(values = c( "red","green","#619CFF"),name = "",
                     limits=c("Total_hcl", "Denuder", "Overblow"),
                     labels = c("Ambient","Denuder", "Overblow"))+
  theme_classic()+theme(legend.position = "top",legend.text=element_text(size=12),
                        axis.text=element_text(size=12),
                        axis.title=element_text(size=12))+
  guides(color = guide_legend(override.aes = list(size = 3)))

ggsave(aa, filename = "C:/Users/grace/Documents/Mchem_project/Experiments_data_plots/Report_plots/Toronto_hcl_time.png", height = 7, width = 11)

#Denuder and Overblow only
aa.3<-
  ggplot(deltam, 
       aes(x = date,
           y = hcl,
           color = sector)) +
  geom_point(size = 0.8)+ 
  labs( x = "", 
        y = " HCl (ppb)", 
        color = "")+
  scale_x_datetime(date_breaks = "2 day", date_labels = "%b %d")+
  scale_y_continuous(breaks=seq(0,2,by = 0.25))+
  scale_color_manual(values = c( "green","#619CFF"),name = "",
                     limits=c("Denuder", "Overblow"),
                     labels = c("Denuder", "Overblow"))+
  theme_classic()+theme(legend.position = "top",legend.text=element_text(size=12),
                        axis.text=element_text(size=12),
                        axis.title=element_text(size=12))+
  guides(color = guide_legend(override.aes = list(size = 3)))


ggsave(aa.3, filename = "C:/Users/grace/Documents/Mchem_project/Experiments_data_plots/Report_plots/Toronto_zeros_time_point.png", height = 7, width = 11)

?geom_line()


# raw section
filt_raw <- df %>% filter(between(date,ymd_hm("2023-08-09 09:15"), ymd_hm("2023-08-09 10:30")))

aa.2<-
  ggplot(filt_raw, aes(x = date,
           y = hcl,
           color = sector)) +
  geom_point(size = 0.75)+ 
  labs( x = "9th August", 
        y = " HCl (ppb)", 
        color = "")+
  scale_x_datetime(date_breaks = "15 min", date_labels ="%H:%M")+
  scale_y_continuous(breaks=seq(0,2,by = 0.25))+
  scale_color_manual(values = c( "red","green","#619CFF"),name = "",
                     limits=c("Total_hcl", "Denuder", "Overblow"),
                     labels = c("Ambient","Denuder", "Overblow"))+
  theme_classic()+theme(legend.position = "top",legend.text=element_text(size=12),
                        axis.text=element_text(size=12),
                        axis.title=element_text(size=12))+
  guides(color = guide_legend(override.aes = list(size = 3)))

ggsave(aa.2, filename = "C:/Users/grace/Documents/Mchem_project/Experiments_data_plots/Report_plots/Toronto_hcl_raw_time.png", height = 7, width = 9)


# now want delta H2o delta + ADDED START DATE FOR TIME AVERAGE

deltam2<- df %>% mutate(sector2= sector)%>%
  pivot_wider(names_from = "sector", values_from = "hcl")%>%
  rename(o_hcl = Overblow, d_hcl = Denuder, a_hcl = Total_hcl)%>%
  pivot_wider(names_from = "sector2", values_from = "h2o")%>%
  rename(o_h2o = Overblow, d_h2o = Denuder, a_h2o = Total_hcl)%>%
  timeAverage(avg.time = "15 min", statistic = "mean")%>%
  mutate(delta_hcl = d_hcl-o_hcl, delta_h2o = d_h2o-o_h2o)%>%
 filter(delta_h2o>-10000000)

bb<-
  ggplot(deltam2)+aes(x = delta_hcl, y = delta_h2o)+
  stat_poly_line()+
  stat_poly_eq(use_label(c("eq","R2")))+
  geom_point()+
  labs(x = "Delta HCl (ppb)", y = bquote("Delta H "[ 2]*"O (ppb)"))+
  theme_classic()+
  theme(legend.position = "top",legend.text=element_text(size=12),
                        axis.text=element_text(size=12),
                        axis.title=element_text(size=12))
  ?theme

ggsave(bb, filename = "C:/Users/grace/Documents/Mchem_project/Experiments_data_plots/Report_plots/Toronto_h2o_delta.png", height = 6, width = 8)

#plot of delta hcl vs ambient h2o shows no correlation 
#ggplot(deltam2)+aes(x = delta_hcl, y = a_h2o)+
#  stat_poly_line()+
#  stat_poly_eq(use_label(c("eq","R2")))+
#  geom_point()+
#  labs(x = "Delta HCl (ppb)", 
#       y =  bquote("H"[2]*"O"))+
#  theme_classic()


#Relative humidity

met_raw <- read_csv("C:/Users/grace/Documents/Mchem_project/Calcs_and_data/Met/THECIX_Met_Data_July24_Aug21.csv",
                    skip = 1)
#this uses the NE met station measurements only
met_ne <- met_raw[,9:15] %>%
  mutate(date = as.POSIXct(date,format="%Y-%m-%d %H:%M",tz ="EST" )) %>% with_tz(tzone = "UTC")%>%
  timeAverage(avg.time = "15 min", statistic = "mean")
#set date and tz , UTC -5 is the same as EST, Eastern Standard Time. I hope
#timeAverage for 1 hour to match denuder/overblow averages merging with

hcl_RH <- left_join(deltam2,met_ne, by = "date") %>% 
  rename(Humidity = "RH_NE (%)")%>%
  filter(delta_hcl<0.4)


cc<-
ggplot(hcl_RH)+aes(x = delta_hcl, y = Humidity)+
 # stat_poly_line()+
  stat_poly_eq(use_label(c("R2")))+
  geom_point()+
  labs(x = "Delta HCl (ppb)", y = bquote("Relative Humidity (%)"))+
  theme_classic()+
  theme()
legend.position = "top",legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12)
ggsave(cc, filename = "C:/Users/grace/Documents/Mchem_project/Experiments_data_plots/Report_plots/Toronto_delts_vs_humidity.png", height = 6, width = 8)


deltap <- deltam %>% mutate(delta = Denuder-Overblow, corrected_ambient = Total_hcl-Overblow)%>% 
  mutate( percent_diff_denuder= (Denuder/Total_hcl)*100)%>%
  mutate(diff_overblow = (Overblow/Total_hcl)*100)


#pCl plots ## DIFFERENT UNITS !!!!

AIMIC_raw <- read_csv("C:/Users/grace/Documents/Mchem_project/Calcs_and_data/23July_AIMIC_obs3.csv")

AIMIC_full <- AIMIC_raw %>% rename(date = Time) %>% 
  mutate(date = as.POSIXct(date,format="%d/%m/%Y %H:%M",tz ="America/New_York" )) %>% with_tz(date, tz = "UTC")
# rename(pCl = "pCl-", gCl = "gCl-")
#,NH4 = "NH4+...7", DMA = "DMA+...8", DEA = "DEA+...9", SO4 = "SO42-...13", Oxalate = "Oxalate...14",)

hr_df <- df %>% 
  pivot_wider(names_from = "sector", values_from = "hcl")%>%
  timeAverage(avg.time = "1 hour", statistic = "mean")%>%
  mutate(delta= Denuder-Overblow)
  #pivot_longer(cols = c(Overblow,Denuder,Total_hcl), names_to = "sector", values_to = "hcl")


pCl_df <- left_join(hr_df, AIMIC_full, by = "date") %>% rename(pCl = "pCl-", gCl = "gCl-") %>%
  mutate(delta = (delta*(36.46/24.45)), ratio  = delta/pCl, Ambient = (Total_hcl*(36.46/24.45)))%>%
  filter(delta > 0)
# filter(pCl <= 0.2, gCl <=0.35)

ee<-
  ggplot() +
    geom_rect(aes(xmin = 0.15, xmax = 0.54, ymin = -Inf, ymax = Inf), fill = "#82C341", alpha = 0.2)+
  geom_point(data = pCl_df, aes(x = delta, y = pCl,  color = pCl),color = "black")+
  labs(y =bquote("pCl"^"-"* " (µg/m" ^ "3" * ")"), x = bquote("Delta HCl (µg/m" ^ "3" * ")"))+
  scale_y_continuous(breaks = seq(0,0.17, by = 0.05))+
  theme_classic()+
  theme(legend.position = "top",legend.text=element_text(size=12),
                                                            axis.text=element_text(size=12),
                                                            axis.title=element_text(size=12))


ggsave(ee, filename = "C:/Users/grace/Documents/Mchem_project/Experiments_data_plots/Report_plots/Toronto_pCl_delta.png", height = 6, width = 7)


Cl_sector_df <- pCl_df %>%
  mutate(sector = case_when( delta > 0.15 & between(date, ymd("2023-08-01"),ymd("2023-08-04")) ~ "deeppink",
                             delta > 0.15 & between(date, ymd("2023-08-04"),ymd("2023-08-05")) ~ "steelblue2",
                             delta > 0.15 & between(date, ymd("2023-08-05"),ymd("2023-08-07")) ~ "seagreen4",
                             delta > 0.15 & between(date, ymd("2023-08-07"),ymd("2023-08-10")) ~ "red",
                             delta > 0.15 & between(date, ymd("2023-08-14"),ymd("2023-08-18")) ~ "blue",
                             delta > 0.15 & between(date, ymd("2023-08-18"), ymd("2023-08-22")) ~ "green3",
                             between(date, ymd_h("2023-08-06 7"),ymd_h("2023-08-06 13")) ~ "thistle3",
                             between(date, ymd_h("2023-08-9 07"),ymd_h("2023-08-09 12")) ~ "grey",
                             between(date, ymd_h("2023-08-10 06"),ymd_h("2023-08-10 13")) ~ "grey40",
                             between(date, ymd_h("2023-08-12 06"),ymd_h("2023-08-12 12")) ~ "plum2",
                             between(date, ymd_h("2023-08-13 06"),ymd_h("2023-08-13 10")) ~ "violetred4",
                             between(date, ymd_h("2023-08-15 06"),ymd_h("2023-08-15 10")) ~ "plum4",
                             is.na(delta) & is.numeric(pCl) ~ "black",
                             TRUE ~ "#d19c2f"))  %>%
  filter(!sector == "black")%>% 
  rename(pNa = "Na+", pNH4 = "NH4+...19", pNO2 = "NO2-...26",
         pNO3 = "NO3-...27", pSO4 = "SO42-...28", pOxolate = "Oxalate...29",
         pPhosphate = "Phosphate...30",pFormate = "Formate...31",pAcetate = "Acetate...32" ) %>% 
  select(!pAcetate) %>% select(!pFormate) %>% select(!pPhosphate)  %>%
  mutate(group = ifelse(sector %in% c("grey", "grey40", "plum2","violetred4","plum4","plum","thistle3","deeppink","steelblue2","seagreen4","red","blue","green3"), "color", "other"))%>%
  mutate(group2 = case_when(group == "color" & delta >0.15 ~ "high_delta", 
                            group == "color" & delta <0.15 ~ "low_delta",
                            group == "other" ~ "other"))

color_box = Cl_sector_df %>% 
  filter(sector != "#d19c2f") %>% 
  group_by(sector) %>% 
  summarise(start_date = min(date),
            end_date = max(date)) 

low_delt_color <- color_box %>% filter(grepl("grey|grey40|plum2|violetred4|plum4|thistle3", sector))
high_delt_color <- color_box %>% filter(!grepl("grey|grey40|plum2|violetred4|plum4|thistle3", sector))

#bquote("pCl- (µg/m" ^ "3" * ")")

ff<-
  ggplot(Cl_sector_df) +
    aes(x = date, y = pCl, color = group2) +
    geom_point( size = 1.5) +
    labs(y = bquote("pCl"^"-"* " (µg/m" ^ "3" * ")"), x = "") +
    scale_y_continuous(breaks = seq(0, 0.20, by = 0.02)) +
    scale_color_manual(values = c("#82C341", "#FAA31B", "black"), name = "", labels = c("HCl delta > 0.15", "pCl spike with HCl delta < 0.15", "HCl delta < 0.15")) +
    theme_classic()+
    theme(legend.position = "top",
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12)) 

  #(data points when HCl delta = NA filtered out)  
ggsave(ff, filename = "C:/Users/grace/Documents/Mchem_project/Experiments_data_plots/Report_plots/Toronto_pCl_time.png", height = 6, width = 7)
  




#
