library(geosphere)

# Apparently, PAR is the spectral irradiance integrated from 400 - 700 nm:
# doi.org/10.1029/2023EA002948

tuv <- read_table("G:/My Drive/Experiments/THECIX/Data/TUV/20230819tuv.txt",
                  skip = 5,col_names = c("nm",as.character(seq(0.5,23.5,by=1))),
                  n_max = 156) %>% 
  filter(nm >=250& nm <=400)

tuv <- read_table("G:/My Drive/Experiments/THECIX/Data/TUV/20220618tuv.txt",
                  skip = 5,col_names = c("nm",as.character(seq(0.5,23.5,by=1))),
                  n_max = 156) %>% 
  filter(nm >=250& nm <=400) # 

jvalues <- read_table("G:/My Drive/Experiments/THECIX/Data/TUV/20220618tuv.txt",
                      skip = 174, n_max = 24,col_names = c("hr","sza","jo1d",
                                                            "jh2o2", "jno2",
                                                            "jno31","jno32",
                                                            "jhno2","jch2o1",
                                                            "jch2o2"), 
                      col_types = "dddddddddd") %>% 
  mutate(ts = seq(ymd_hm("2022-06-18 00:30"),ymd_hm("2022-06-18 23:30"), by = 3600))

# THE ABOVE TUV values are based on UTC - 4

# Use the trapezoid rule to integrate irregularly spaced data:
# https://stackoverflow.com/questions/6552844/integration-sampled-data-in-r
# https://www.r-bloggers.com/2017/08/the-trapezoidal-rule-of-numerical-integration-in-r/


yorku <- c(-79.5093503,43.7739355)


# The following for loop will integrate TUV for a given time point.  I need to 
# iterate it across all time points

parOut <- vector()
for(ix in 2:ncol(tuv)){
  wavelength <- tuv$nm
  irradiance <- tuv[ix]
  integral <- 0
  for (iy in 1:(length(wavelength)-1)) {
    # Width of each trapezoid
    delta_lambda <- wavelength[iy+1] - wavelength[iy]
    # Average height of trapezoid
    avg_irradiance <- (irradiance[iy,] + irradiance[iy+1,]) / 2
    # Add area of this trapezoid to total
    integral <- integral + (delta_lambda * avg_irradiance)
  }
  parOut[ix-1] <- integral
}
unlist(parOut) # I don't guess we need this because we are just tranlsating
# the j values to the observed PAR.

# These timestamps will be UTC - 5!!!
obsPar2022 <- read.csv("G:/My Drive/Experiments/THECIX/Data/TUV/met2022.csv") %>% 
  mutate(ts = ymd_hms(ts)+3600) %>% # convert to UTC - 4
  left_join(jvalues) %>% 
  mutate(jo1dClear = na.approx(jo1d, rule= 2),
         jh2o2Clear = na.approx(jh2o2, rule = 2),
         jno2Clear =  na.approx(jno2, rule = 2),
         jno31Clear = na.approx(jno31, rule = 2),
         jno32Clear = na.approx(jno32, rule = 2),
         jhno2Clear =  na.approx(jhno2, rule = 2),
         jch2o1Clear = na.approx(jch2o1, rule = 2),
         jch2o2Clear =  na.approx(jch2o2, rule = 2)) %>% 
  select(-hr, -sza, -ts)
obsPar <- obsPar2022[-nrow(obsPar2022),]

tcTs <- seq(ymd_hm("2023-07-25 00:00"),ymd_hm("2023-08-21 23:59"), by = 60)
jValsCIX <- do.call("rbind", replicate(n=28, obsPar,simplify = FALSE))

TUV_tcDF <- data.frame(ts = tcTs, jValsCIX) %>% 
  rename(parClear = par) %>% 
  mutate(ts = ts + 3600*4) # From EDT to UTC


obsPar2023 <- read.csv("G:/My Drive/Experiments/THECIX/Data/TUV/par2023.csv") %>% 
  mutate(ts = Time_Start_UTC, par = PAR_umolm2s_1min) %>% 
  select(ts,par) %>% 
  mutate(ts=dmy_hms(ts)) %>% 
  left_join(TUV_tcDF) %>% 
  filter(ts >= ymd_hms("2023-07-25 04:00:00")) %>% 
  mutate(parDiff = ifelse(parClear >0, (par - parClear) /parClear , NA)) %>% 
  filter(parDiff > -1 & parDiff <2) %>% 
  mutate(jo3 = jo1dClear*abs(parDiff),
         jh2o2 = jh2o2Clear*abs(parDiff),
         jno2 = jno2Clear * abs(parDiff),
         jno31 = jno31Clear*abs(parDiff),
         jno32 = jno32Clear *abs(parDiff),
         jhno2 = jhno2Clear * abs(parDiff),
         jch2o1 = jch2o1Clear * abs(parDiff),
         jch2o2 = jch2o2Clear * abs(parDiff)) %>% 
  filter(between(ts,ymd_hm("2023-07-25 00:00"),
                 ymd_hm("2023-08-21 23:00")))

  
write.csv(obsPar2023, "G:/My Drive/Experiments/THECIX/jval_est.csv",
          row.names = FALSE, quote = FALSE)

aeromma <- read.csv("G:/My Drive/Experiments/THECIX/Data/TUV/20230804-aeromma.csv") %>% 
  mutate(Time = dmy_hms(Time)) %>% 
  rename(ts = Time) %>% 
  mutate(ts = ts) 
  

distance <- vector()
for(ix in 1:nrow(aeromma)){
  distance[ix] <- distm(c(aeromma$Longitude_MetNav[ix],
                          aeromma$Latitude_MetNav[ix]),
                        yorku,fun = distHaversine)
}


aeromma$distance = distance
aerommaFilter <- aeromma %>% 
  filter(distance < 100000) %>% 
  filter(HAE_GPS_Altitude <= 500) %>% 
  mutate(across(-1, function(x){x - 0.15*x}))




# 
# obsParFiltered <- obsPar2023 %>% 
#   filter(between(ts,ymd_hm("2023-08-05 10:00"),
#                     ymd_hm("2023-08-20 23:00")))

forPlot <- obsPar2023 %>% 
  left_join(aerommaFilter) %>% 
  filter(between(ts,ymd_hm("2023-08-04 12:00"),
                 ymd_hm("2023-08-04 23:00"))) %>% 
  select(ts, jno2,jno2Clear,jNO2) %>% 
  pivot_longer(-ts, names_to = "variable", values_to = "value")


ggplot(data = forPlot,aes(x = ts, y = value, color = variable))+
  geom_line(linewidth = 1.1)+
  labs(x = "Date", y = "Photolysis Frequency (s-1)")+
  theme_minimal(base_size = 20) + 
  scale_color_manual(labels= c("jNO2 TUV","jNO2Aeromma","jNO2 clear Sky"),values = c("royalblue4","orangered3","black"))

ggsave("G:/My Drive/Experiments/THECIX/Data/TUV/jno2_aeromma.png",width = 12.8,
       height = 7.68, units = "in")
  
plot(aerommaFilter$ts,aerommaFilter$jO3,type="l")
lines(obsParFiltered$ts,obsParFiltered$jo3,col="blue")
lines(obsParFiltered$ts,obsParFiltered$jno2Clear,col="red")

