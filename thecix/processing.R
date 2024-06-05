load.tildas.data(START = "2023-07-31", STOP = "2023-08-21",
                 STC = TRUE, HCLONLY= FALSE)

dfAnal <- df.str %>% 
  left_join(df.stc)  %>% 
  filter(!between(ts,ymd_hm("2023-08-01 16:15"), ymd_hm("2023-08-01 19:39"))) %>% 
  filter(!between(ts,ymd_hm("2023-08-16 12:09"), ymd_hm("2023-08-16 12:15"))) %>% 
  filter(!between(ts,ymd_hm("2023-08-17 18:20"), ymd_hm("2023-08-17 22:00"))) %>% # PFBS experiments
  filter(!between(ts,ymd_hm("2023-08-19 14:10"), ymd_hm("2023-08-19 14:30"))) # opened inertial inlet for USB fiddling
  
  


# Let's first try to produce a blank subtracted dataset.
# We have three different bg flags -
# 0 - denuder is in line
# 8 - denuder is in line with overblow
# 24 - denuder is in line with dry air overblow

# While I have been alternating bg's between overblow and 
# denuder, I need to correct all data between 
# 2023-08-01 14:00 and 2023-08-02 16:30, as all blanks in
# this window were denuder (the pump had been shut off at 
# some point).

dfAnal <- dfAnal %>% 
  mutate(ValveW = replace(ValveW,
                        between(ts,
                                ymd_hm("2023-08-01 14:30"),
                                ymd_hm("2023-08-02 16:20"))&ValveW == 8,0)) %>% 
  mutate(ValveW = replace(ValveW,
                          between(ts,
                                  ymd_hm("2023-08-16 04:05"),
                                  ymd_hm("2023-08-16 12:00"))&ValveW == 8,0)) 



# This seems to have worked okay.  Now let's bg subtract
# I'm not so sure what I want to do with overblow data yet.  
# For this presentation, let's just remove it and blank based on denuders.
# I think that means we'll basically only have data from 31 Jul, 
# but that's oaky for now.

# 8 Aug 2023 - I now want to clean up this data a little.  
# Which means I want to remove 5s or after every valve switch.
# In the previous block of code, I have made a column
# "valveChange" that calculated the difference between
# successive valve points.  

# Next then, I need to identify which valveChange != 0, and
# coerce the following 5 points to !=0.  I think this most simply looks 
# like a flag column where != 0 ==> 1

valveChangeix <- which(c(0,diff(dfAnal$ValveW))!=0)
valveChangeixTail <- unique(unlist(lapply(valveChangeix,function(x){seq(x,x+5)})))
valveChangeixTail <- valveChangeixTail[!valveChangeixTail > nrow(dfAnal)]


# I now have all the 5 s periods after every valve change.  However,
# I don't actually care about the valve change from 2 (ambient) to
# 2 + 8 (ambient with overblow through denuder).
dfAnal$flag <- 0
dfAnal$flag[valveChangeixTail] <- 1
#dfAnal$flag[dfAnal$ValveW==10] <- 0  



# One issue we're having now is that, just by filtering out
# the valve states, we've not excluded the adjustment period
# after the valve switch, which is done in offset.correct()
# In offset.correct() it removes the 5 seconds after the valve
# switch.  How can I recreate that in TIDY


str.xts <- xts(x = dfAnal[2:6], order.by = dfAnal$ts)
stc.xts <- xts(x = dfAnal[c("ValveW","flag")], order.by = dfAnal$ts)
head(x)


offset.correct()
x<-offset.correct(just.zero = TRUE)
y <- split.groups(x)
z <- y[lengths(y) < 150]

sigmaVector <- sapply(z, sd, na.rm = TRUE)


str.tbl <- str.xts.BckCorr %>% fortify.zoo %>% as_tibble(.name_repair = "minimal")
stc.tbl <- stc.xts %>% fortify.zoo %>% as_tibble(.name_repair = "minimal")

dfAnalCorr <- str.tbl %>% 
  left_join(stc.tbl) %>% 
  rename(ts = "Index")

# Cal-nalysis -------------------------------------------------------------
# 1 - denuder with cal
# 8 - overblow through denuder
# 9 - overblow through denuder with cal
# 10 - ambient with overblow

# We probably want all 9s and 25 (separately)

cal9 <- dfAnalCorr %>% 
  filter(ValveW==9) %>% 
  mutate(id = floor_date(ts, unit = "10 minutes")) %>% 
  group_by(id) %>% 
  summarize_all(tail, n = 120) %>% 
  summarize_all(mean) %>% 
  ungroup() %>% 
  select(-id)

cal25 <- dfAnalCorr %>% 
  filter(ValveW == 25)
  mutate(id = floor_date(ts, unit = "10 minutes")) %>% 
  group_by(id) %>% 
  summarize_all(tail, n = 120) %>% 
  summarize_all(mean) %>% 
  ungroup() %>% 
  select(-id)


TIME1 <- "2023-08-01 15:00:00"
TIME2 <- "2023-08-01 15:10:00"
cal9 %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hcl)) +
  geom_point()+
  scale_y_continuous(limits = c(0.5,1.5),name = "HCl")


# Remove Cals -------------------------------------------------------------


dfAnalCorr <- dfAnalCorr%>% 
  filter(!ValveW %in% c(1, 8, 9, 10,25)) %>% 
  filter(flag == 0)
# 1 - denuder with cal
# 8 - overblow through denuder
# 9 - overblow through denuder with cal
# 10 - ambient with overblow


# 
dfAnalCorr <- dfAnalCorr %>%
  filter(!between(hour(ts)*60 + minute(ts) + (second(ts)/60), 5, 7)) %>%
  filter(!between(hour(ts)*60 + minute(ts) + (second(ts)/60), 185, 187)) %>%
  filter(!between(hour(ts)*60 + minute(ts) + (second(ts)/60), 365, 367)) %>%
  filter(!between(hour(ts)*60 + minute(ts) + (second(ts)/60), 545, 547)) %>%
  filter(!between(hour(ts)*60 + minute(ts) + (second(ts)/60), 725, 727)) %>%
  filter(!between(hour(ts)*60 + minute(ts) + (second(ts)/60), 905, 907)) %>%
  filter(!between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1085, 1087)) %>%
  filter(!between(hour(ts)*60 + minute(ts) + (second(ts)/60), 1265, 1267))




TIME1 <- "2023-08-17 00:00:00"
TIME2 <- "2023-08-17 00:15:00"
P1 <- dfAnalCorr %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hcl)) +
  geom_line()+
  scale_y_continuous(limits = c(-0.2,2),name = "HCl")


P2 <- dfAnalCorr %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=ValveW)) +
  geom_line()+
  scale_y_continuous(name = "HCl")
grid.arrange(P1,P2,nrow = 2,ncol=1)




# cims <- read_csv("G:/My Drive/Experiments/THECIX/Data/cims/28_3_24.csv",skip=1) %>% 
#   mutate(UTC = dmy_hm(UTC))
# 
# dummyTime <- data.frame("UTC"=seq(from= as.POSIXct("2023-07-28 04:12:00",tz="UTC"), 
#                                  to = as.POSIXct("2023-08-21 12:30:00",tz="UTC"), 
#                                  by = "1 min"))
# 
# outDF <- merge(dummyTime,cims, all= TRUE, by = "UTC")
# write.csv(outDF, "G:/My Drive/Experiments/THECIX/Data/cims/28_3_24_1min.csv",quote=FALSE,
#           row.names = FALSE)


dummyTime <- data.frame("ts"=seq(from= as.POSIXct("2023-07-31 00:00:00",tz="UTC"), 
                 to = as.POSIXct("2023-08-22 00:00:00",tz="UTC"), 
                 by = "1 sec"))

dfAnalOut <- dfAnalCorr %>% 
  select(!c("flag","ValveW")) %>% 
  mutate(ts = round_date(ts, unit = "second"))

dfAnalOut <- merge(dummyTime,dfAnalOut, all= TRUE, by = "ts")


write.csv(dfAnalOut,"G:/My Drive/Experiments/THECIX/20230731-20230821.csv",
          quote = FALSE,row.names = FALSE)



tildas <- read_csv("G:/My Drive/Experiments/THECIX/20230731-20230821.csv") %>% 
  drop_na() %>% 
  mutate(ts = floor_date(ts, unit = "1 min")) %>% 
  group_by(ts) %>% 
  summarize(hcl = mean(hcl))

dfAnalAvg <- dfAnalOut %>% 
  mutate(ts = floor_date(ts, unit = "10 min")) %>% 
  group_by(ts) %>% 
  summarize(hcl = mean(hcl))

write.csv(test,"G:/My Drive/Experiments/THECIX/hcl-foricartt.csv",
          quote = FALSE,row.names = FALSE)

metCix <- read_csv("G:/My Drive/Experiments/THECIX/Data/met/met.csv") %>% 
#  mutate("ts" = ymd_hm(date))
  rename("ts" = date)

hclMet <- dfAnalAvg %>% 
  left_join(metCix) %>% 
  filter(hcl > 0.05)

write.csv(hclMet,"G:/My Drive/Experiments/THECIX/hcl-met-10minavg-alt.csv",
          quote = FALSE,row.names = FALSE)

