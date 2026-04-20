# Let's start with winter because it's the more interesting period.
library(ggpubr)
library(GGally)
library(ggpmisc)
# "w" fo# "w" fo# "w" for winter

MODELOUTDIR <- file.path(EXPTDIR,"OSCA", "isorropia","wes", "outputfiles")
ITERDIR <- file.path(EXPTDIR, "OSCA", "isorropia","wes","iterations")





dfMarga_all <- read_csv("G:/My Drive/Experiments/OSCA/isorropia/wes/marga/marga_all_2012-2024_4iso.csv")


dfMarga_all <- dfMarga_all %>% 
  mutate(pclFracObs = (pm25cl_umol)/(hcl_umol+pm25cl_umol),
         pno3FracObs = pm25no3_umol/(hno3_umol+pm25no3_umol),
         pnh4FracObs = pm25nh4_umol/(nh3_umol+pm25nh4_umol)) 



s_margaAll_ff <- file.path(MODELOUTDIR,
                           "20120101-20241231umol-1hr-MARGA-multiyear",
                           "20120101-20241231umol-1hr-MARGA-multiyear.csv")



s_margaAll <- read_csv(s_margaAll_ff) %>% 
  arrange(date) %>% 
  mutate(pclFracMod = (CLLIQ_umol/(CLLIQ_umol + GHCL_umol)))%>% 
  mutate(pno3FracMod = (NO3LIQ_umol/(NO3LIQ_umol + GHNO3_umol))) %>% 
  mutate(pnh4FracMod = (NH4LIQ_umol/(NH4LIQ_umol + GNH3_umol))) %>% 
  left_join(dfMarga_all) %>% 
  mutate(pclFrac = (CLLIQ_umol/(CLLIQ_umol + GHCL_umol))/(pm25cl_umol/(hcl_umol+pm25cl_umol))) %>% 
  mutate(pno3Frac = (NO3LIQ_umol/(NO3LIQ_umol + GHNO3_umol))/(pm25no3_umol/(hno3_umol+pm25no3_umol))) %>% 
  mutate(pnh4Frac = (NH4LIQ_umol/(NH4LIQ_umol + GNH3_umol))/(pm25nh4_umol/(nh3_umol+pm25nh4_umol))) %>% 
  mutate(inorgSumObs = pm25nh4 + pm25no3+pm25cl + pm25so4 + pm25k + pm25ca,
         inorgSumMod = NH4LIQ + NO3LIQ + CLLIQ + SO4LIQ + KLIQ + CaLIQ + NALIQ,
         #orgWat = porg / WATER,
         inorgObsWat = inorgSumObs / WATER,
         inorgModWat = inorgSumMod / WATER,
  )








# BIG MARGA Plots ---------------------------------------------------------

margaAll_pclFail <- ggplot(data = s_margaAll, aes(x = date, y = pclFrac, color = WATER_umol))+
  geom_point(size = 1.5)+
  theme_minimal()+
  scale_y_continuous(trans = "log10", lim= c(1e-3, 10))+
  scale_color_gradientn(colors = rainbow(6),limits = c(0.01,5),
                        trans = "log10", name = "Water (umol m-3)")+
  ylab("pCl Model Failure")+
  theme(text = element_text(size = 22))

# ggplot(data = s_margaAll, aes(x = date, y = pclFrac, color = RH))+
#   geom_point(size = 1.5)+
#   theme_minimal()+
#   scale_y_continuous(trans = "log10", lim= c(1e-3, 10))+
#   scale_color_gradientn(colors = rainbow(6),limits = c(0.01,1),
#                         name = "Water (umol m-3)")+
#   ylab("pCl Model Failure")


margaAll_pno3Fail <- ggplot(data = s_margaAll, aes(x = date, y = pno3Frac, color = WATER_umol))+
  geom_point(size = 1.5)+
  theme_minimal()+
  scale_y_continuous(trans = "log10", lim= c(1e-3, 10))+
  scale_color_gradientn(colors = rainbow(6),limits = c(0.01,5),trans = "log10",
                        name = "Water (umol m-3)")+
  ylab(expression("pNO"[3]* " Model Failure"))+
  theme(text = element_text(size = 22))


margaAll_pnh4Fail <- ggplot(data = s_margaAll, aes(x = date, y = pnh4Frac, color = WATER_umol))+
  geom_point(size = 1.5)+
  theme_minimal()+
  scale_y_continuous(trans = "log10", lim= c(1e-3, 10))+
  scale_color_gradientn(colors = rainbow(6),limits = c(0.01,5),trans = "log10",
                        name = "Water (umol m-3)")+
  ylab(expression("pNH"[4]* " Model Failure"))+
  theme(text = element_text(size = 22))


ggarrange(margaAll_pclFail, margaAll_pno3Fail, margaAll_pnh4Fail,
          common.legend = TRUE, legend = "right", ncol = 1,nrow= 3)

ggsave()


###########################################################################



margaAll_pclFail <- ggplot(data = s_margaAll, aes(x = date, y = pclFrac, color = RH))+
  geom_point(size = 1.5)+
  theme_minimal()+
  scale_y_continuous(trans = "log10", lim= c(1e-3, 10))+
  scale_color_gradientn(colors = rainbow(6),limits = c(0.1,1),
                        #trans = "log10", 
                        name = "RH (%)")+
  ylab("pCl Model Failure")+
  theme(text= element_text(size = 16))

# ggplot(data = s_margaAll, aes(x = date, y = pclFrac, color = RH))+
#   geom_point(size = 1.5)+
#   theme_minimal()+
#   scale_y_continuous(trans = "log10", lim= c(1e-3, 10))+
#   scale_color_gradientn(colors = rainbow(6),limits = c(0.01,1),
#                         name = "Water (umol m-3)")+
#   ylab("pCl Model Failure")


margaAll_pno3Fail <- ggplot(data = s_margaAll, aes(x = date, y = pno3Frac, color = RH))+
  geom_point(size = 1.5)+
  theme_minimal()+
  scale_y_continuous(trans = "log10", lim= c(1e-3, 10))+
  scale_color_gradientn(colors = rainbow(6),limits = c(0.1,1),#trans = "log10",
                        name = "RH (%)")+
  ylab(expression("pNO"[3]* " Model Failure"))+
  theme(text= element_text(size = 16))


margaAll_pnh4Fail <- ggplot(data = s_margaAll, aes(x = date, y = pnh4Frac, color = RH))+
  geom_point(size = 1.5)+
  theme_minimal()+
  scale_y_continuous(trans = "log10", lim= c(1e-3, 10))+
  scale_color_gradientn(colors = rainbow(6),limits = c(0.1,1),#trans = "log10",
                        name = "RH (%)")+
  ylab(expression("pNH"[4]* " Model Failure"))+
  theme(text= element_text(size = 16))


outRHPlot <- ggarrange(margaAll_pclFail, margaAll_pno3Fail, margaAll_pnh4Fail,
                       common.legend = TRUE, legend = "bottom")

ggsave("G:/My Drive/Experiments/OSCA/isorropia/wes/marga-ts-rh.png",
       plot = outRHPlot,
       width = 12.80,
       height = 10.24,
       units = "in")




# Dry vs Wet aerosol ------------------------------------------------------
ggplot(data = s_margaAll)+
  geom_point(aes(x = inorgSumObs/(inorgSumObs + WATER), y = pclFrac,
             color = as.factor(month(date))))+
  theme_minimal()+
  ylab("Cl Model Failure")+
  xlab("Dry Inorg / (Dry Inorg + Water)")+
  scale_y_continuous(trans = "log10", limits = c(1e-5,30))+
  scale_x_continuous(trans = "log10")+
  scale_color_viridis_d()

