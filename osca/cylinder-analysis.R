load.tildas.data(START = "2021-06-10", STOP = "2021-07-22",STC=TRUE, 
                 output.type="xts", MASK = FALSE)

offset.correct()

# So now I should have bg corrected data, with perm source and cylinder data 
# just being their ordinary magnitudes.  Let's convert the xts data back to
# tibbles and see what I can do...

df.str <- str.xts %>% 
  fortify.zoo %>% 
  as_tibble %>% 
  rename(ts="Index")

df.stc <- stc.xts %>% 
  fortify.zoo %>% 
  as_tibble %>% 
  rename(ts="Index")


dfAnal <- df.str %>% 
  left_join(df.stc) 


dry.cylinder.pts <- dfAnal %>% 
  filter(between(ts, ymd_hm("2021-06-29 11:00"),ymd_hm("2021-06-29 11:30"))| # test 4.7ppm hcl cylinder addition, dry air 5sccm 
        between(ts, ymd_hm("2021-07-05 12:15"), ymd_hm("2021-07-05 12:45"))|  # test 4.7ppm hcl cylinder addition, dry air 5sccm
        between(ts, ymd_hm("2021-07-08 13:45"), ymd_hm("2021-07-08 13:50"))| # test 4.7ppm hcl cylinder addition, dry air 5sccm
        between(ts, ymd_hm("2021-07-13 09:40"), ymd_hm("2021-07-13 09:45"))) %>% # test 4.7ppm hcl cylinder addition, dry air 5sccm
  mutate(theDate = as.Date(ts)) %>% 
  group_by(theDate) %>% 
  summarise(hclAvg = mean(hcl,na.rm=TRUE), hclSd = sd(hcl,na.rm = TRUE))



dry.cylinder.bg <- dfAnal %>% 
  filter(between(ts,ymd_hm("2021-06-29 11:33"),ymd_hm("2021-06-29 11:35"))|  # test 4.7ppm hcl cylinder addition, dry air 5sccm 
          between(ts,ymd_hm("2021-07-05 12:55"), ymd_hm("2021-07-05 13:00"))| # test 4.7ppm hcl cylinder addition, dry air 5sccm
          between(ts,ymd_hm("2021-07-08 14:10"),ymd_hm("2021-07-08 14:15"))|  # test 4.7ppm hcl cylinder addition, dry air 5sccm
          between(ts,ymd_hm("2021-07-13 09:20"),ymd_hm("2021-07-13 09:30"))) %>%  # test 4.7ppm hcl cylinder addition, dry air 5sccm
  mutate(theDate = as.Date(ts)) %>% 
  group_by(theDate) %>% 
  summarise(hclAvg = mean(hcl,na.rm=TRUE), hclSd = sd(hcl,na.rm = TRUE))



wet.cylinder.pts <- dfAnal %>% 
  filter(between(ts, ymd_hm("2021-07-05 14:10"), ymd_hm("2021-07-05 14:14"))| # test 4.7ppm hcl cylinder addition, humid air 5sccm
          between(ts, ymd_hm("2021-07-13 10:45"), ymd_hm("2021-07-13 10:55"))| # test 4.7ppm hcl cylinder addition, humid air 5sccm
          between(ts,ymd_hm("2021-07-13 14:45"), ymd_hm("2021-07-13 14:55"))| # test 4.7ppm hcl cylinder addition, humid air 2.5sccm
          between(ts, ymd_hm("2021-07-20 15:45"), ymd_hm("2021-07-20 15:59"))) %>% # test 4.7ppm hcl cylinder addition, humid air 2.5sccm
  mutate(theDate = floor_date(ts,unit = "hour")) %>% 
  group_by(theDate) %>% 
  summarise(hclAvg = mean(hcl,na.rm=TRUE), hclSd = sd(hcl,na.rm = TRUE))


wet.cylinder.bg <- dfAnal %>% 
  filter(between(ts, ymd_hm("2021-07-05 14:30"), ymd_hm("2021-07-05 14:32")) | # test 4.7ppm hcl cylinder addition, humid air 5sccm
          between(ts, ymd_hm("2021-07-13 11:20"), ymd_hm("2021-07-13 11:25"))| # test 4.7ppm hcl cylinder addition, humid air 5sccm
          between(ts, ymd_hm("2021-07-13 15:15"), ymd_hm("2021-07-13 15:25"))| # test 4.7ppm hcl cylinder addition, humid air 2.5sccm
          between(ts, ymd_hm("2021-07-20 16:10"), ymd_hm("2021-07-20 16:15"))) %>%  # test 4.7ppm hcl cylinder addition, humid air 2.5sccm
  mutate(theDate = floor_date(ts,unit = "hour")) %>% 
  group_by(theDate) %>% 
  summarise(hclAvg = mean(hcl,na.rm=TRUE), hclSd = sd(hcl,na.rm = TRUE))

# dryDF
# hcl      hclSd  pctError
# 1 6.188766 0.06177434 0.9744014
# 2 5.849278 0.04370436 0.9209502
# 3 6.195331 0.07918537 0.9754352
# 4 5.716541 0.03640522 0.9000512

dryDF <- data.frame(hcl = (dry.cylinder.pts$hclAvg - dry.cylinder.bg$hclAvg),
                    hclSd = sqrt(dry.cylinder.pts$hclSd^2 + dry.cylinder.bg$hclSd^2))
dryDF$pctError <- dryDF$hcl / (5*4.7/3700*1000)

wetDF <- data.frame(hcl = (wet.cylinder.pts$hclAvg - wet.cylinder.bg$hclAvg),
                    hclSd = sqrt(wet.cylinder.pts$hclSd^2 + wet.cylinder.bg$hclSd^2))
wetDF$pctError <- wetDF$hcl / c(5*4.7/3700*1000, 5*4.7/3700*1000, 2.5*4.7/3700*1000, 2.5*4.7/3700*1000)
# 
# hcl      hclSd  pctError
# 1 5.935944 0.06155121 0.9345954
# 2 5.899075 0.06254166 0.9287905
# 3 2.916200 0.05737629 0.9182928
# 4 3.001879 0.02716969 0.9452724

# For Plotting
TIME1 <- "2021-07-13 09:30:00"
TIME2 <- "2021-07-13 10:00:00"

P1 <- dfAnal %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  
  ggplot(aes(x=ts,y=hcl)) +
  geom_line()+
  scale_y_continuous(limits = c(-0.2,8),name = "HCl")



P2 <- dfAnal %>% 
  filter(between(ts, ymd_hms(TIME1), ymd_hms(TIME2))) %>% 
  ggplot(aes(x=ts,y=ValveW)) +
  geom_line()+
  scale_y_continuous(name = "Valve", limits = c(0,255))

grid.arrange(P1,P2,nrow = 2,ncol=1)

