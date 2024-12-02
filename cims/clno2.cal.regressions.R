library(tidyverse)
library(bfsl)

CLNO2DATADIR <- "C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2"

ff <- list.files(CLNO2DATADIR,pattern = "no2-cims-tildas",
                 full.names = TRUE)


datList <- lapply(ff,read_csv)
datList[[3]]$hclCorr <- datList[[3]]$hclCorr/0.9493
datList[[3]]$hclSd <- datList[[3]]$hclSd/0.9493


hclNo2 <-lapply(datList, function(xx){
  summary(bfsl(x = xx$no2Corr, y = xx$hclCorr, 
       sd_x = xx$no2Sd, sd_y = xx$hclSd))
})


cimsHcl <-lapply(datList, function(xx){
  summary(bfsl(x = xx$hclCorr, y = xx$cimsCorr, 
       sd_x = xx$hclSd, sd_y = xx$cimsSd))
})


cimsNo2 <- lapply(datList, function(xx){
    summary(bfsl(x = xx$no2Corr, y = xx$cimsCorr, 
         sd_x = xx$no2Sd, sd_y = xx$cimsSd))
  })



combined_data <- data.frame(
  y = c(datList[[1]]$hclCorr, datList[[2]]$hclCorr,datList[[3]]$hclCorr),
  x = c(datList[[1]]$no2Corr, datList[[2]]$no2Corr, datList[[3]]$no2Corr),
  group = factor(rep(c("Group1", "Group2", "Group 3"),
                     c(length(datList[[1]]$hclCorr),
                       length(datList[[2]]$hclCorr),
                       length(datList[[3]]$hclCorr))))
)
# 
# combined_data <- data.frame(
#   y = c(datList[[3]]$cimsCorr, datList[[3]]$cimsCorr),
#   x = c(datList[[3]]$hclCorr, datList[[3]]$no2Corr),
#   group = factor(rep(c("Group1", "Group2"),
#                      c(length(datList[[3]]$hclCorr),
#                        length(datList[[3]]$hclCorr))))
#   )


combined_model <- lm(y~x * group, data = combined_data)

anova(combined_model)


x <- c(11,44,66)
y <- c(658,1445,1690)

summary(lm(y~x))
