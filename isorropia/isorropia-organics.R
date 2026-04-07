#
TIME1 = ymd_hms("2021-06-11 00:00:00")
TIME2 = ymd_hms("2021-07-05 00:00:00")

# Load observational data
s_dfMaster <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/inputfiles/20210611-20210721dfMaster.csv") %>% 
  mutate(pClFrac = (pcl_umol_m3/(hcl_umol_m3+pcl_umol_m3))) %>% 
  filter(between(date,TIME1,TIME2))

# I can't remember which specific study I grabbed the following from, but they
# do appear in Li et al 2019.
kap_org_upper = 0.3
kap_org_lower = 0.08
kap_org_urban = 0.13

alwc_org <- s_dfMaster %>% 
  mutate(org_vol = porg*1/(1.4), #multiplying by water density divided by organic density
         alwc_org_upper = org_vol * kap_org_upper * (rh/100) / (1-(rh/100))/18.02,
         alwc_org_lower = org_vol * kap_org_lower * (rh/100) / (1-(rh/100))/18.02,
         alwc_org_urban = org_vol * kap_org_urban * (rh/100) / (1-(rh/100))/18.02
         )

# Load base model with XACT Na and XACT Cl and ACSM Cl
s_model_base <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/outputfiles/20210611-20210721umol-1hr-XACTNa-XACTACSMCl/20210611-20210721umol-1hr-XACTNa-XACTACSMCl.csv") %>% 
  mutate(pClFrac = (CLLIQ_umol/CLTOT_umol)) %>% 
  filter(between(date,TIME1,TIME2))


# Load rh optimised model with XACT Na and XACT Cl and ACSM Cl
s_model_bestrh <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/rhbestfit-osca-summer-XACT.csv") %>% 
  mutate(pClFrac = (CLLIQ_umol/CLTOT_umol)) %>% 
  filter(between(date,TIME1,TIME2))



# Load reverse mode model (note that it includes XACT Na (as Cl) and XACT+AcSM Cl-)
s_reverse_ff <- file.path(MODELOUTDIR,
                          "20210611-20210721umol-1hr-REVERSEOSCA-test",
                          "20210611-20210721umol-1hr-REVERSEOSCA-test.csv")

s_reverse <- read_csv(s_reverse_ff) %>% 
  filter(between(date,TIME1,TIME2)) %>% 
  arrange(date) %>%   
  mutate(pclFracMod = CLLIQ_umol/(GHCL_umol + CLLIQ_umol),
         pno3FracMod = NO3LIQ_umol/(GHNO3_umol + NO3LIQ_umol),
         pnh4FracMod = NH4LIQ_umol/(GNH3_umol + NH4LIQ_umol))



s_gordonlwc <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/maqs/2021_Campaign_Processed_Data_LWC.csv") %>% 
  filter(between(date,TIME1,TIME2))
  


water_df <- data.frame(date = s_model_base$date,
  bestrh_water = s_model_bestrh$WATER_umol,
                       base_water = s_model_base$WATER_umol,
                       reverse_water = s_reverse$WATER_umol) %>% 
  mutate(delta_water = bestrh_water - base_water,
         alwc_org_upper = alwc_org$alwc_org_upper,
         alwc_org_urban = alwc_org$alwc_org_urban,
         alwc_org_lower = alwc_org$alwc_org_lower,
         rh = s_dfMaster$rh,
         pclFail =s_model_base$pClFrac/s_dfMaster$pClFrac)
water_df$date <- as.character(format(water_df$date))

test <- s_model_base$pClFrac/s_dfMaster$pClFrac

ggplot(data = water_df)+
  geom_point(aes(x = delta_water, y = alwc_org_urban, color =test),
             size = 3)+
  labs(x = "Delta Water (BestRH - BaseRH) (umol m-3)",
       y = "ALWC_org_upper (umol m-3)")+
  #ylim(c(0,1.5))+
  labs(color = "Cl- Model Fail")+
  xlim(c(0, 2.5))+
  scale_y_continuous(trans="log10", limits = c(0.005,1))+
  scale_color_viridis_c(trans = "log10", limits = c(0.01,4))+
  theme_minimal()


ggplot(data = water_df)+
  geom_point(aes(x =bestrh_water, y =  base_water+alwc_org_upper, color =test),
             size = 4)+
  labs(x = "Best water",
       y = "Base Water + Organic Water (upper limit) (umol m-3)")+
  scale_y_continuous(limits = (c(0.01,5)),trans = "log10")+
  scale_x_continuous(limits = (c(0.01,5)),trans = "log10")+
  
  #xlim(c(0, 5))+
  labs(color = "Cl- Model Fail")+
  scale_color_viridis_c(trans = "log10", limits = c(0.01,5))+
  theme(title = "")+
  theme_minimal()

ggplot(data = water_df)+
  geom_point(aes(x =bestrh_water, y =  reverse_water+alwc_org_upper, color =test),
             size = 4)+
  labs(x = "Best water",
       y = "Base Water + Organic Water (upper limit) (umol m-3)")+
  scale_y_continuous(limits = (c(0.01,5)),trans = "log10")+
  scale_x_continuous(limits = (c(0.01,5)),trans = "log10")+
  
  #xlim(c(0, 5))+
  labs(color = "Cl- Model Fail")+
  scale_color_viridis_c(trans = "log10", limits = c(0.01,5))+
  theme(title = "")+
  theme_minimal()







# using s_bestGuess from the lowVol code:
s_dfMaster <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/inputfiles/20210611-20210721dfMaster.csv") %>% 
  filter(between(date,TIME1,TIME2))%>% 
  mutate(pclFracObs = pcl_umol_m3/(hcl_umol_m3 + pcl_umol_m3),
         pno3FracObs = pno3_umol_m3/(hno3_umol_m3 + pno3_umol_m3),
         pnh4FracObs = pnh4_umol_m3/(nh3_umol_m3 + pnh4_umol_m3)) %>% 
  mutate(pclTOT =pcl_2_5_umol_m3+pcl_umol_m3) 

s_bestGuess_ff <- file.path(MODELOUTDIR, 
                            "20210611-20210721umol-1hr-XACTNa-XACTACSMCl",
                            "20210611-20210721umol-1hr-XACTNa-XACTACSMCl.csv") 

s_bestGuess <- read_csv(s_bestGuess_ff) %>% 
  filter(between(date,TIME1,TIME2)) %>% 
  #add_row(date = gap_ts) %>%
  arrange(date) %>% 
  mutate(pclFracMod = CLLIQ_umol/(GHCL_umol + CLLIQ_umol),
         pno3FracMod = NO3LIQ_umol/(GHNO3_umol + NO3LIQ_umol),
         pnh4FracMod = NH4LIQ_umol/(GNH3_umol + NH4LIQ_umol))%>% 
  left_join(s_dfMaster) %>% 
  mutate(pclFail = pclFracMod/pclFracObs,
         pno3Fail = pno3FracMod / pno3FracObs,
         pnh4Fail = pnh4FracMod / pnh4FracObs)



p1 <- ggplot()+
  geom_point(aes(x = s_gordonlwc$LWC_org, y = s_bestGuess$pclFail))+
  ylim(c(0,5))+
  xlim(c(0,5))+
  scale_y_continuous(trans = "log10")




p2 <- ggplot()+
  geom_point(aes(x = s_gordonlwc$LWC_org, y = s_bestGuess$pno3Fail))+
  ylim(c(0,5))+
  xlim(c(0,5))+
  scale_y_continuous(trans = "log10")




p3 <- ggplot()+
  geom_point(aes(x = s_gordonlwc$LWC_org, y = s_bestGuess$pnh4Fail))+
  ylim(c(0,5))+
  xlim(c(0,5))+
  scale_y_continuous(trans = "log10")


ggarrange(p1,p2,p3)

#
#

#
#
#
##
#
# MAQS  organics
ff <- list.files("G:/My Drive/Experiments/OSCA/isorropia/wes/maqs/", 
                 pattern = ".csv", full.names = TRUE)

acsmList <- lapply(ff,read_csv)
acsmDF <- do.call(rbind,acsmList) %>% 
  rename(pcl = `Chl (ug/m3)`,
         pnh4 = `NH4 (ug/m3)`,
         pno3 = `NO3 (ug/m3)`,
         pso4 = `SO4 (ug/m3)`,
         porg = `organic (ug/m3)`) %>% 
  mutate(datetime = floor_date(datetime,"1 day"),
         porgFrac = porg/(pcl + pnh4+pno3 + pso4+porg)) %>% 
  group_by(datetime) %>% 
  summarize_all(mean,na.rm = TRUE) %>% 
  ungroup()



acsmForDiurnal <- acsmDF %>% 
  select(datetime, porg,porgFrac) %>% 
  rename(date = datetime)

acsmForDiurnal$date <- format(as.character(acsmForDiurnal$date)) 
write.csv(acsmForDiurnal, "G:/My Drive/Experiments/OSCA/isorropia/wes/maqs_org.csv",
          row.names=  FALSE, quote= FALSE)

timeVariation(acsmForDiurnal, pollutant = "porg")

ggplot(data = acsmDF)+
  geom_line(aes(x = datetime, y= porg), color = "black")+
  theme_minimal()+
  ylab("Organic ug/m3")+
  ylim(c(0,50))



ggplot(data = acsmDF)+
  geom_line(aes(x = datetime, y= porgFrac), color = "black")+
  theme_minimal()+
  ylab("Organic Mass Fraction")+
  ylim(c(0,1))

timeVariation(acsmForDiurnal, pollutant = "porgFrac")



# Winter ------------------------------------------------------------------

# Model failure vs LWC organic
TIME1 = ymd_hms("2022-02-05 00:00:00")
TIME2 = ymd_hms("2022-02-21 00:00:00")

w_dfMaster <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/inputfiles/20220205-20220221dfMaster.csv") %>% 
  select(!c(NATOT_umol, NATOT)) %>% 
  mutate(pclFracObs = pcl_umol_m3/(hcl_umol_m3 + pcl_umol_m3),
         pno3FracObs = pno3_umol_m3/(hno3_umol_m3 + pno3_umol_m3),
         pnh4FracObs = pnh4_umol_m3/(nh3_umol_m3 + pnh4_umol_m3)) %>% 
  mutate(pclTOT =pcl_2_5_umol_m3+pcl_umol_m3) 


# ISORROPIA using XACT est Na and ACSM/XACT est Cl:
w_bestGuess_ff <- file.path(MODELOUTDIR, 
                            "20220205-20220221umol-1hr-XACTNa-XACTACSMCl",
                            "20220205-20220221umol-1hr-XACTNa-XACTACSMCl.csv") 

w_bestGuess <- read_csv(w_bestGuess_ff) %>% 
  left_join(w_dfMaster) %>% 
  
  filter(between(date,TIME1,TIME2)) %>% 
  #add_row(date = gap_ts) %>%
  arrange(date) %>% 
  mutate(pclFracMod = CLLIQ_umol/(GHCL_umol + CLLIQ_umol),
         pno3FracMod = NO3LIQ_umol/(GHNO3_umol + NO3LIQ_umol),
         pnh4FracMod = NH4LIQ_umol/(GNH3_umol + NH4LIQ_umol))%>% 
  mutate(pclFail = pclFracMod/pclFracObs,
         pno3Fail = pno3FracMod / pno3FracObs,
         pnh4Fail = pnh4FracMod / pnh4FracObs)


w_gordonlwc <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/maqs/2022_Campaign_Processed_Data_LWC.csv") %>% 
  filter(between(date,TIME1,TIME2)) %>% 
  select(c(date,LWC_org))




p1 <- ggplot()+
  geom_point(aes(x = w_gordonlwc$LWC_org, y = w_bestGuess$pclFail))+
  ylim(c(0,5))+
  xlim(c(0,5))+
  scale_y_continuous(trans = "log10")


p2 <- ggplot()+
  geom_point(aes(x = w_gordonlwc$LWC_org, y = w_bestGuess$pno3Fail))+
  ylim(c(0,5))+
  xlim(c(0,5))+
  scale_y_continuous(trans = "log10")




p3 <- ggplot()+
  geom_point(aes(x = w_gordonlwc$LWC_org, y = w_bestGuess$pnh4Fail))+
  ylim(c(0,5))+
  xlim(c(0,5))+
  scale_y_continuous(trans = "log10")


ggarrange(p1,p2,p3)
