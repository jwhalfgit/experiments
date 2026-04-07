CIXDIR <- "G:/My Drive/Experiments/THECIX/data"


aimG <- read_csv(file.path(CIXDIR,"aim","cl-gas.csv")) %>% 
  mutate(ts = ymd_hms(ts))%>% 
  mutate_if(is.character,as.numeric)

aimP <- read_csv(file.path(CIXDIR,"aim","cl-aerosol.csv")) %>% 
  mutate(ts = ymd_hms(ts))%>% 
  mutate_if(is.character,as.numeric)

tildas <- read_csv("G:/My Drive/Experiments/THECIX/20230731-20230821.csv") %>% 
  drop_na() %>% 
  mutate(ts = floor_date(ts, unit = "1 min")) %>% 
  group_by(ts) %>% 
  summarize(hcl = mean(hcl))


dfAll <- tildas %>% 
  full_join(aimG) %>% 
  full_join(aimP) %>% 
  arrange(ts)


write.csv(dfAll,"G:/My Drive/Experiments/THECIX/tildas-aim.csv",
          row.names = FALSE,quote=FALSE)


TIME1 <- "2023-08-10 00:00:00"
TIME2 <- "2023-08-21 00:15:00"

P1 <- dfAll %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hcl)) +
  geom_line()+
  scale_y_continuous(limits = c(-0.2,2),name = "HCl")


P2 <- dfAll %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=clCD)) +
  geom_point()+
  scale_y_continuous(name = "Cl (conductivity)", limits = c(0,0.5))


P3 <- dfAll %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=pclMS)) +
  geom_point()+
  scale_y_continuous(name = "pCl (conductivity)")


grid.arrange(P1,P2,P3, nrow = 3,ncol=1)






