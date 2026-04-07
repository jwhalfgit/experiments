#
# Load observational data
s_dfMaster <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/inputfiles/20210611-20210721dfMaster.csv") %>% 
  mutate(pClFrac = (pcl_umol_m3/(hcl_umol_m3+pcl_umol_m3)) )

# Load base model with XACT Na and XACT Cl and ACSM Cl
s_model_base <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/outputfiles/20210611-20210721umol-1hr-XACTNa-XACTACSMCl/20210611-20210721umol-1hr-XACTNa-XACTACSMCl.csv") %>% 
  mutate(pClFrac = (CLLIQ_umol/CLTOT_umol))

# Load rh optimised model with XACT Na and XACT Cl and ACSM Cl
s_model_bestrh <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/rhbestfit-osca-summer-XACT.csv") %>% 
  mutate(pClFrac = (CLLIQ_umol/CLTOT_umol))

# Load base model with increased CaSO4:
s_model_caso4 <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/outputfiles/20210611-20210721umol-1hr-OSCA-XACTNa-XACTACSMCl-CaSO4Test/20210611-20210721umol-1hr-OSCA-XACTNa-XACTACSMCl-CaSO4Test.csv") %>% 
  mutate(pClFrac = (CLLIQ_umol/CLTOT_umol))

ggplot()+
  geom_point(aes(x = s_model_base$WATER_umol, y = s_model_caso4$WATER_umol))+
  xlim(c(0,3))+
  ylim(c(0,3))


ggplot()+
  geom_line(aes(x = s_model_base$date, y = s_model_base$WATER_umol),
           color = "black")+
  geom_line(aes(x = s_model_caso4$date, y = s_model_caso4$WATER_umol), 
            color = "red")


