met <- read_csv(file.path(EXPTDIR,"THECIX","Data","met","met.csv")) %>% 
  mutate(DateSW = mdy_hm(DateSW)) %>% 
  rename(ts = DateSW)


cims <- read_csv(file.path(EXPTDIR,"THECIX","Data","cims","cims.csv")) %>% 
  rename(IW = iodide, IH2O = `Iodide/water`, clno2 = `Nitryl chloride`,
         cl2 = "Chlorine") %>% 
  mutate(ts = dmy_hm(ts))
  #        iratio = IH2O / IW,
  #        clno2norm = clno2/(IW + IH2O))

cimsI_corr <- read_csv(file.path(EXPTDIR,"THECIX","Data","cims","corrected_I.csv")) %>% 
  rename(ts = Time, I_corr = iodide) %>% 
  mutate(ts = dmy_hm(ts))

cims_clno2_out <- cims %>% 
  left_join(cimsI_corr) %>% 
  mutate(iratio = IH2O / I_corr,
    clno2norm = clno2/(I_corr + IH2O),
    cl2norm = cl2/(I_corr + IH2O)) %>% 
  select(ts, I_corr, IH2O, iratio, clno2,clno2norm,cl2,cl2norm) %>% 
  # mutate(clno2_sens = 2.104e-3*iratio+3.62e-4,
  #   clno2_ppb = clno2norm / clno2_sens,
  #   clno2_flag = ifelse(iratio > 0.82,1,0),
  #   cl2_sens = (-2.7059*iratio^2 + 3.7843*iratio + 0.1044)/(1e6)*1000,
  #   cl2_ppb = cl2norm / cl2_sens)
  
  mutate(clno2_sens = 2.104e-3*iratio+3.62e-4,
         clno2_ppb = clno2norm / clno2_sens,
         clno2_flag = ifelse(iratio > 0.82,1,0),
         cl2_sens = (-2.7059*iratio^2 + 3.7843*iratio + 0.1044)/(1e6)*1000,
         cl2_ppb = cl2norm / cl2_sens)
  
write.csv(cims_clno2_out, file.path(EXPTDIR,"THECIX","Data","cims","clno2-cl2-complete.csv"),
          quote = FALSE, row.names = FALSE)





load.tildas.data(START = "2023-08-01", STOP = "2023-08-02", HCLONLY = FALSE)

df.str$ts <- as.POSIXct(df.str$ts)-(3600*4)



TIME1 <- ymd_hms("2023-08-07 18:00:00")
TIME2 <- ymd_hms("2023-08-10 08:00:00")


P1 <- met %>% 
  filter(between(ts,TIME1,TIME2)) %>% 
  ggplot(aes(x=ts,y=`RH_SW (%)`)) +
  geom_line()


P2 <- cims_clno2_out %>% 
  filter(between(ts,TIME1,TIME2)) %>% 
  ggplot(aes(x=ts,y=cl2_ppb, color = flag)) +
  geom_line()+
  scale_y_continuous(name = "Cl2 (ppbv)")

P3 <- df.str %>% 
  filter(between(ts,TIME1,TIME2)) %>% 
  ggplot(aes(x=ts,y=h2o)) +
  geom_line()+
  scale_y_continuous(name = "Abs water (ppb)")


grid.arrange(P2,P3)

