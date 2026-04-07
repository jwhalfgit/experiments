# Make sure you load dfMaster from ISORROPIA_Prep_Script_jwh_summer-iterations.


clCpds <- dfMaster %>% 
  select(date, contains("cl")) %>% 
  mutate(cl2 = cl2 * 2) %>% 
  #mutate(ftest_umol = .00100) %>% 
  pivot_longer(cols = c("hcl","cl2","clno2", "pcl_ppbv"),
              names_to = "species", values_to = "conc") #%>% 
  #filter(date < ymd("2021-06-19"))


summary(clCpds)

cb <- ggplot(data = clCpds)+
  geom_bar(aes(x = date, y = conc, fill = species), stat = "identity", 
           position = "stack")+
  scale_fill_manual(values = c("darkorange1", # Cl2
                               "blueviolet", # ClNO2
                               "forestgreen", # Hcl
                               "hotpink"), # pCl
                    labels = c("Cl2",
                               "ClNO2",
                               "HCl",
                               "pCl-","test"))+
  labs(x = "Date (UTC)", y = expression("Cl Concentration (umol m"^-3*")"), 
       fill = "Species")+
  theme_minimal()


# ggsave("G:/My Drive/Manuscripts/OSCA/winter-cl-budget.png",
#        plot = cb, width = 12.8, height = 7.68, units = "in",
#        bg = "white")

# Diurnal
cl2Di <- timeVariation(dfMaster, pollutant = "cl2", plot = FALSE)$data$hour
clno2Di <- timeVariation(dfMaster, pollutant = "clno2", plot = FALSE)$data$hour
hclDi <- timeVariation(dfMaster, pollutant = "hcl", plot = FALSE)$data$hour
pclDi <- timeVariation(dfMaster, pollutant = "pcl_ppbv", plot = FALSE)$data$hour
# tempDi <- timeVariation(dfMaster, pollutant = "tempK", plot = FALSE)$data$hour %>% 
#   mutate(Upper = Upper - Mean, Lower = Mean - Lower) %>% 
#   write_csv("G:/My Drive/Experiments/OSCA/data/2022/diurnal/tempDi.csv")
# rhDi <- timeVariation(dfMaster, pollutant = "rh", plot = FALSE)$data$hour %>% 
#   mutate(Upper = Upper - Mean, Lower = Mean - Lower) %>% 
#   write_csv("G:/My Drive/Experiments/OSCA/data/2022/diurnal/rhDi.csv")
# 
# wsDi <- timeVariation(dfMaster, pollutant = "windspd", plot = FALSE)$data$hour %>% 
#   mutate(Upper = Upper - Mean, Lower = Mean - Lower) %>% 
#   write_csv("G:/My Drive/Experiments/OSCA/data/2022/diurnal/windspdDi.csv")
# 
# 
# gustDi <- timeVariation(dfMaster, pollutant = "gust", plot = FALSE)$data$hour %>% 
#   mutate(Upper = Upper - Mean, Lower = Mean - Lower) %>% 
#   write_csv("G:/My Drive/Experiments/OSCA/data/2021/diurnal/gustDi.csv")




clDi <- rbind(cl2Di, clno2Di, hclDi, pclDi) %>% 
  group_by(hour) %>% 
  arrange(hour)

di <- ggplot(data = clDi)+
  geom_bar(aes(x = hour, y = Mean, fill = variable), stat = "identity", 
           position = "stack")+
  scale_fill_manual(values = c("darkorange1", # Cl2
                               "blueviolet", # ClNO2
                               "forestgreen", # Hcl
                               "hotpink"),
                    labels = c(expression(Cl[2]*" x 2"),
                               expression(ClNO[2]),
                               "HCl",
                               expression(pCl^"-")))+
  # labs(x = "Hour", y = expression("Cl Concentration (umol m"^-3*")"), 
  #      fill = "Species")+
  labs(x = "Hour", y = expression("Cl Concentration (ppbv)"), 
       fill = "Species")+
  #ylim(c(0,0.0115))+
  ylim(c(0,0.3))+
  theme_bw()+
  theme(text = element_text(size = 20))

ggsave("G:/My Drive/Manuscripts/OSCA/winter-cl-budget-diurnal.png",
      plot = di, width = 12.8, height = 7.68, units = "in",
      bg = "white")





# Cl2 analysis ------------------------------------------------------------

# Does Cl2 correlate well with irradiation and nitrate?  
# Load ACSM, ,met, SMPS, and SPECRAD

get.SA <- function(TEMP = met$tempK,SMPS = smps){
  alpha <- 4.4e-6 * exp(2898/TEMP) # from Van Doren et al (see top section of this file)
  kB = 1.380649e-23 # J / K (Boltzman Constant)
  p_diameter = SMPS$"Median (nm)" * 1e-9 
  
  #mean_speed_hcl = sqrt((8*kB * TEMP) / (pi*36.458 / 1000 /(6.02e23))) # m/s
  Dhcl = (118 / ((296/TEMP)^1.75)) / 760
  # Diffusivity from Tang et al 2014 (doi:10.5194/acp-14-9233-2014)
  
  smps <- SMPS
  
  asol_d = as.numeric(names(smps[,2:107]))
  asol_SA = 4*pi*(asol_d/2)^2 # corresponding SA for each diameter in nm^2
  
  # average number densitiies
  asol_nbar <- smps[,2:107]
  asol_SA_n <- asol_nbar * asol_SA * (10^-9 * 100)^2 # cm^2 / cc
  total_SA <- rowSums(asol_SA_n) /(100^2) * (1e6)^2  # um^2 / cc
  total_sub <- rowSums(asol_SA_n[,1:98]) /(100^2) * (1e6)^2  # um^2 / cc
  total_super <- rowSums(asol_SA_n[,99:106]) /(100^2) * (1e6)^2  # um^2 / cc
  total_fidasoverlap <- rowSums(asol_SA_n[,52:98]) /(100^2) * (1e6)^2 # 
  outDF <- data.frame(date = SMPS$ts, SA = total_SA,sub = total_sub, 
                      super = total_super, fidas = total_fidasoverlap)
  return(outDF)
}

sa <- get.SA()

# Make sure you have pmSA loaded from the lifetimeanalysis.R file!
#
pmSA <- pmSA %>% 
  mutate(date = ts)

dfAnal <- dfMaster %>% # dfMaster from ISORROPIA file
  # select(date, pno3_umol_m3,cl2, cl2_umol_m3, no2,pcl_umol_m3, pcl_ppbv, rh) %>%
  left_join(pmSA) %>%
  left_join(specrad) %>%
  left_join(o3) %>%
  mutate(no3prod = pm2_5_combined * pno3_umol_m3 * actFlux) %>%
  mutate(o3prod = pm2_5_combined * (o3)* actFlux) %>%
  filter(!between(x = hour(date), 8,18)) %>%
  #filter(date < ymd("2021-06-19"))
  filter(date < ymd("2021-06-19"))

# 
# library(foqat)
# 
# df <- data.frame(
#   time=seq(as.POSIXct("2021-06-10 00:00:00",tz="UTC"), as.POSIXct("2021-07-21 00:00:00",tz="UTC"), by="hour"), 
#   gAltitude=rep(0.035, 985), 
#   mAltitude=rep(0.038, 985),
#   longitude=rep(-2.217, 985),
#   latitude=rep(53.444, 985)
# )
# # 
# tuvOut <- tuv_batch(df)
# save(tuvOut, file = "G:/My Drive/Experiments/OSCA/data/2021/tuv.Rds")
load(file = "G:/My Drive/Experiments/OSCA/data/2021/tuv.Rds")

js <- data.frame(date = seq(as.POSIXct("2021-06-10 00:00:00",tz="UTC"), 
                              as.POSIXct("2021-07-21 00:00:00",tz="UTC"), 
                              by="hour"),
                   jcl2 =  as.numeric(tuvOut[,64]), 
                   jclno2 = as.numeric(tuvOut[,71]),
                   jno2_tuv = as.numeric(tuvOut[,8]))


dfAnal2 <- dfAnal %>% 
  left_join(js) %>% 
  mutate(`JNO2 (s-1)` = ifelse(`JNO2 (s-1)` <0, 0, `JNO2 (s-1)`)) %>% 
  mutate(jno2SF = `JNO2 (s-1)` / jno2_tuv) %>% 
  mutate(jcl2Scaled = jcl2 * jno2SF,
         jclno2Scaled = jclno2 * jno2SF) %>% 
  mutate(cl2Prod = cl2 * jcl2Scaled,
         clno2Prod = clno2 * jclno2Scaled)
  #filter(pcl_ppbv <= 0.06)
  
library(ggpmisc)


# Cl2 ---------------------------------------------------------------------
p1 <- ggplot(data = dfAnal2, aes(x= pno3_umol_m3, y = cl2Prod))+
  labs(x = expression("pNO"[3]^"-"* " (\u03bcmol m"^"-3" * ")"),
       y = expression("Cl"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()

p2 <- ggplot(data = dfAnal2, aes(x= o3, y = cl2Prod))+
  labs(x = expression("O"[3] * " (ppbv s"^"-1" * ")"),
       y = expression("Cl"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()


p3 <- ggplot(data = dfAnal2, aes(x= SA, y = cl2Prod))+
  labs(x = expression("Surface area density (\u03bcm"^"2" *"cm"^"-3" * ")"),
       y = expression("Cl"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()

p4 <- ggplot(data = dfAnal2, aes(x= actFlux, y = cl2Prod))+
  labs(x = expression("Solar radiation (W m"^"-2" * ")"),
       y = expression("Cl"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()


p5 <- ggplot(data = dfAnal2, aes(x= actFlux * SA, y = cl2Prod))+
  labs(x = expression("Solar radiation x Sa"),
       y = expression("Cl"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+  
  stat_poly_line() +
  stat_poly_eq()




p6 <- ggplot(data = dfAnal2, aes(x= actFlux* o3, y = cl2Prod))+
  labs(x = expression("O"[3] * " x Solar radiation"),
       y = expression("Cl"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()


p7 <- ggplot(data = dfAnal2, aes(x= o3prod, y = cl2Prod))+
  labs(x = expression("O"[3] * "x Sa x Solar radiation"),
       y = expression("Cl"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()

p8 <- ggplot(data = dfAnal2, aes(x= no3prod, y = cl2Prod))+
  labs(x = expression("pNO"[3]^"-"* " x Sa x Solar radiation"),
       y = expression("Cl"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()


p9 <- ggplot(data = dfAnal2, aes(x= pcl_umol_m3, y = cl2Prod))+
  labs(x = expression("pCl"^"-"* " (\u03bcmol m"^"-3" * ")"),
       y = expression("Cl"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()


p10 <- ggplot(data = dfAnal2, aes(x= pcl_umol_m3 * SA, y = cl2Prod))+
  labs(x = expression("pCl"^"-"* " x Sa"),
       y = expression("Cl"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()


p11 <- ggplot(data = dfAnal2, aes(x= pcl_umol_m3 * actFlux, y = cl2Prod))+
  labs(x = expression("pCl"^"-"* " x Solar radiation"),
       y = expression("Cl"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()


p12 <- ggplot(data = dfAnal2, aes(x= pcl_umol_m3 * actFlux*SA, y = cl2Prod))+
  labs(x = expression("pCl"^"-"* " x Sa x Solar radiation"),
       y = expression("Cl"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()

plotgridOut <- plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,
          labels = paste0("(",letters[1:12],")"),
          ncol = 3, label_x = -0.02)

ggsave(filename = "G:/My Drive/Manuscripts/OSCA/cl2prod_grid.png",
       plot= plotgridOut, width = 12.80,height = 12.8, units = "in",
       bg = "white")


summary(lm(dfAnal2$cl2Prod~dfAnal2$o3 * dfAnal$actFlux ))



ggplot(data = dfAnal2, aes(x= o3prod, y = cl2Prod, color = pno3_umol_m3))+
  geom_point(aes(size = 10))+
  theme_minimal()+
  scale_color_viridis_c()

plot(dfAnal2$no3prod, dfAnal2$cl2Prod, type= "p", pch = 19,
     ylab ="Cl2 Production Rate (ppb/s)",
     xlab = "Solar Radiation x Sa x NO3- (W m-2 um-2 ug m-3)")

summary(lm(dfAnal2$cl2Prod~dfAnal2$no3prod ))


plot(dfAnal2$o3prod, dfAnal2$cl2Prod, type= "p", pch = 19,
     ylab ="Cl2 Production Rate (ppb/s)",
     xlab = "Solar Radiation x Sa x O3 (W m-2 um-2 ppbv)")

summary(lm(dfAnal2$cl2Prod~dfAnal2$o3prod ))

# ClNO2 ---------------------------------------------------------------------

p1 <- ggplot(data = dfAnal2, aes(x= pno3_umol_m3, y = clno2Prod))+
  labs(x = expression("pNO"[3]^"-"* " (\u03bcmol m"^"-3" * ")"),
       y = expression("ClNO"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()

p2 <- ggplot(data = dfAnal2, aes(x= o3, y = clno2Prod))+
  labs(x = expression("O"[3] * " (ppbv s"^"-1" * ")"),
       y = expression("ClNO"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()


p3 <- ggplot(data = dfAnal2, aes(x= SA, y = clno2Prod))+
  labs(x = expression("Surface area density (\u03bcm"^"2" *"cm"^"-3" * ")"),
       y = expression("ClNO"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()

# p4 <- ggplot(data = dfAnal2, aes(x= actFlux, y = clno2Prod))+
#   labs(x = expression("Solar radiation (W m"^"-2" * ")"),
#        y = expression("ClNO"[2] * "production rate (ppbv s"^"-1" * ")"))+
#   geom_point(size = 3)+
#   theme_minimal()+
#   scale_color_viridis_c()+
#   stat_poly_line() +
#   stat_poly_eq()

p4 <- ggplot(data = dfAnal2, aes(x= n2o5, y = clno2Prod))+
  labs(x = expression("N2O5 (ppbv"),
       y = expression("ClNO"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()



p5 <- ggplot(data = dfAnal2, aes(x= n2o5 * SA, y = clno2Prod))+
  labs(x = expression("N2O5 x Sa"),
       y = expression("ClNO"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+  
  stat_poly_line() +
  stat_poly_eq()




p6 <- ggplot(data = dfAnal2, aes(x= actFlux* o3, y = clno2Prod))+
  labs(x = expression("O"[3] * " x Solar radiation"),
       y = expression("ClNO"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()


p7 <- ggplot(data = dfAnal2, aes(x= o3prod, y = clno2Prod))+
  labs(x = expression("O"[3] * "x Sa x Solar radiation"),
       y = expression("ClNO"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()

p8 <- ggplot(data = dfAnal2, aes(x= no3prod, y = clno2Prod))+
  labs(x = expression("pNO"[3]^"-"* " x Sa x Solar radiation"),
       y = expression("ClNO"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()


p9 <- ggplot(data = dfAnal2, aes(x= pcl_umol_m3, y = clno2Prod))+
  labs(x = expression("pCl"^"-"* " (\u03bcmol m"^"-3" * ")"),
       y = expression("ClNO"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()


p10 <- ggplot(data = dfAnal2, aes(x= pcl_umol_m3 * SA, y = cl2Prod))+
  labs(x = expression("pCl"^"-"* " x Sa"),
       y = expression("ClNO"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()


p11 <- ggplot(data = dfAnal2, aes(x= pcl_umol_m3 * actFlux, y = clno2Prod))+
  labs(x = expression("pCl"^"-"* " x Solar radiation"),
       y = expression("ClNO"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()


p12 <- ggplot(data = dfAnal2, aes(x= pcl_umol_m3 * actFlux*SA, y = clno2Prod))+
  labs(x = expression("pCl"^"-"* " x Sa x Solar radiation"),
       y = expression("ClNO"[2] * "production rate (ppbv s"^"-1" * ")"))+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_viridis_c()+
  stat_poly_line() +
  stat_poly_eq()

plotgridOut <- plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,
                         labels = paste0("(",letters[1:12],")"),
                         ncol = 3, label_x = -0.02)

ggsave(filename = "G:/My Drive/Manuscripts/OSCA/clno2prod_grid.png",
       plot= plotgridOut, width = 12.80,height = 12.8, units = "in",
       bg = "white")


summary(lm(dfAnal2$cl2Prod~dfAnal2$o3 * dfAnal$actFlux ))



ggplot(data = dfAnal2, aes(x= o3prod, y = cl2Prod, color = pno3_umol_m3))+
  geom_point(aes(size = 10))+
  theme_minimal()+
  scale_color_viridis_c()

plot(dfAnal2$no3prod, dfAnal2$cl2Prod, type= "p", pch = 19,
     ylab ="Cl2 Production Rate (ppb/s)",
     xlab = "Solar Radiation x Sa x NO3- (W m-2 um-2 ug m-3)")

summary(lm(dfAnal2$cl2Prod~dfAnal2$no3prod ))


plot(dfAnal2$o3prod, dfAnal2$cl2Prod, type= "p", pch = 19,
     ylab ="Cl2 Production Rate (ppb/s)",
     xlab = "Solar Radiation x Sa x O3 (W m-2 um-2 ppbv)")

summary(lm(dfAnal2$cl2Prod~dfAnal2$o3prod ))


# ClNO2 production --------------------------------------------------------
dfAnal <- dfMaster %>% 
  filter(between(date, ymd("2021-06-11"), ymd("2021-06-18"))) %>% 
  left_join(pmSA) %>% 
  mutate(pm2_5_combined = pm2_5_combined / (100^2) * (1e6)^2) %>% 
  filter(!between(hour(date),06,19)) %>% 
  mutate(c_speed = sqrt(8*8.314*tempK/(pi*(28.02/1000 + 5*16/1000)))*3600, # 8RT/MW; m/hr
         gammaphi_est = 0.10,
         clno2_est = 0.25*c_speed*pm2_5_combined*((1e-6)^2) * 100^3 *gammaphi_est*n2o5, #um2/cm3 --> /m
         fake_k = 0.25*c_speed*pm2_5_combined*((1e-6)^2) * 100^3 *gammaphi_est,
         gammaphi_obs = clno2 / n2o5 /(0.25*c_speed*pm2_5_combined*((1e-6)^2) * 100^3))
# 0.084 


plot(dfAnal$clno2_est, dfAnal$clno2)
summary(lm(dfAnal$clno2~dfAnal$clno2_est))
abline(lm(dfAnal$clno2~dfAnal$clno2_est))

plot(dfAnal$date,dfAnal$clno2_est,type = "l")
lines(dfAnal$date,dfAnal$clno2, col = "blue")
