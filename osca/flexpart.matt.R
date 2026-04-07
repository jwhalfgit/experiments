OSCADIR <- file.path(EXPTDIR,"OSCA")

FLEXPARTDIR <- file.path(OSCADIR, "FLEXPART")

amDIR <- file.path(FLEXPARTDIR,"am")
maqsDIR <- file.path(FLEXPARTDIR,"maqs")

maqs2021Quad <- read_csv(file.path(maqsDIR,"process_quadrant_2021.csv")) %>% 
  rename(nw = `North West`, ne = `North East`, 
         sw = `South West`, se= `South East`)
maqs2021Surface <- read_csv(file.path(maqsDIR,"process_land-ocean_2021.csv")) %>% 
  pivot_longer(cols = c(Land,Ocean),
               names_to = "variable",
               values_to = "value")

maqs2021SurfacePlot <- ggplot(data = maqs2021Surface, aes(fill = variable, x = ts, y = value))+
  geom_col(position = "stack")+
  scale_fill_manual(
    values =  c("#009e73", "#0072b2"),
    breaks = c("Land", "Ocean"))+
  theme_light(base_size = 16)
  


maqs2022Quad <- read_csv(file.path(maqsDIR,"process_quadrant_2022.csv"))%>% 
  rename(nw = `North West`, ne = `North East`, 
         sw = `South West`, se= `South East`)
maqs2022Surface <- read_csv(file.path(maqsDIR,"process_land-ocean_2022.csv"))%>% 
  pivot_longer(cols = c(Land,Ocean),
               names_to = "variable",
               values_to = "value")
maqs2022SurfacePlot <- ggplot(data = maqs2022Surface, aes(fill = variable, x = ts, y = value))+
  geom_col(position = "stack")+
  scale_fill_manual(
    values =  c("#009e73", "#0072b2"),
    breaks = c("Land", "Ocean"))+
  theme_light(base_size = 16)+
  labs(x = "Time (UTC)", y = "Percentage of trajectory (%)", fill ="Surface",
       title = "2022")





am2021Quad <- read_csv(file.path(amDIR,"process_quadrant_2021.csv"))%>% 
  rename(nw = `North West`, ne = `North East`, 
         sw = `South West`, se= `South East`)

am2021Surface <- read_csv(file.path(amDIR,"process_land-ocean_2021.csv"))%>% 
  pivot_longer(cols = c(Land,Ocean),
               names_to = "variable",
               values_to = "value")


am2021SurfacePlot <- ggplot(data = am2021Surface, aes(fill = variable, x = ts, y = value))+
  geom_col(position = "stack")+
  scale_fill_manual(
    values =  c("#009e73", "#0072b2"),
    breaks = c("Land", "Ocean"))+
  theme_light(base_size = 16)

am2022Quad <- read_csv(file.path(amDIR,"process_quadrant_2022.csv"))%>% 
  rename(nw = `North West`, ne = `North East`, 
         sw = `South West`, se= `South East`)

am2022Surface <- read_csv(file.path(amDIR,"process_land-ocean_2022.csv"))%>% 
  pivot_longer(cols = c(Land,Ocean),
               names_to = "variable",
               values_to = "value") %>% 
  filter(between(ts,ymd("2022-02-05"), ymd("2022-02-21")))
  
  
am2022SurfacePlot <- ggplot(data = am2022Surface, aes(fill = variable, x = ts, y = value))+
  geom_col(position = "stack")+
  scale_fill_manual(
    values =  c("#009e73", "#0072b2"),
    breaks = c("Land", "Ocean"))+
  theme_light(base_size = 16)+
  labs(x = "Time (UTC)", y = "Percentage of trajectory (%)", fill = "surface",
       title = "2022")


grid.arrange(am2022SurfacePlot,maqs2022SurfacePlot)
ggsave(file = paste0(FLEXPARTDIR,"/2022-surfaces.png"), width= 12.80, height = 7.68,
       units = "in",dpi = 900)

