# 2023-10-18 - This is for manipulating whatever peripheral OSCA data I need 
# to



# CIMS --------------------------------------------------------------------
# Emily Matthews has sent me calibrated ClNO2 data and
# Cl2 and N2O5 signals.  I will load and merge here, as well as average
# to... a minute?


# summer
clno2DF <- read_csv(file.path(EXPTDIR, "OSCA", "data","2021","cims-clno2.csv")) %>% 
  rename(ts = t_start_Buf) %>% 
  mutate(ts = dmy_hm(ts))

cl2n2o5DF <-  read_csv(file.path(EXPTDIR, "OSCA", "data","2021","cims-n2o5-cl2.csv")) %>% 
  rename(ts = t_start_Buf)
#  mutate(ts = dmy_hm(ts))
  

bigDF <- clno2DF %>% 
  left_join(cl2n2o5DF) %>% 
  mutate(ts = floor_date(ts,"1 minute")) %>% 
  group_by(ts) %>% 
  summarize_all(mean, na.rm = TRUE)

write.csv(bigDF, file.path(EXPTDIR, "OSCA", "data","2021","cims60sAvg.csv"),
          row.names=FALSE,quote=FALSE)
  

# winter

# summer
clno2DF <- read_csv(file.path(EXPTDIR, "OSCA", "data","2022","cims-clno2.csv")) %>% 
  rename(ts = t_start_Buf) %>% 
  mutate(ts = dmy_hm(ts))

cl2n2o5DF <-  read_csv(file.path(EXPTDIR, "OSCA", "data","2022","cims-n2o5-cl2.csv")) %>% 
  rename(ts = t_start_Buf)
  #mutate(ts = dmy_hm(ts))


bigDF <- clno2DF %>% 
  left_join(cl2n2o5DF) %>% 
  mutate(ts = floor_date(ts,"1 minute")) %>% 
  group_by(ts) %>% 
  summarize_all(mean, na.rm = TRUE)

write.csv(bigDF, file.path(EXPTDIR, "OSCA", "data","2022","cims60sAvg.csv"),
          row.names=FALSE,quote=FALSE)

