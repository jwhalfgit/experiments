library(tidyverse)
library(bfsl)

CLNO2DATADIR <- "C:/Users/jh2949/OneDrive - University of York/Desktop/CIMS_analysis/ClNO2"

ff <- list.files(CLNO2DATADIR,pattern = "no2-cims-tildas",
                 full.names = TRUE)


datList <- lapply(ff,read_csv)
datList[[3]]$hclCorr <- datList[[3]]$hclCorr/0.9493
datList[[3]]$hclSd <- datList[[3]]$hclSd/0.9493

# 0.9493 will have come from the loss experiments held end of June


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


# Pearson Coefficients:
hclNo220 <- cor(datList[[1]]$no2Corr, datList[[1]]$hclCorr, 
                method = "pearson")
hclNo244 <- cor(datList[[2]]$no2Corr, datList[[2]]$hclCorr, 
                method = "pearson")
hclNo266 <- cor(datList[[3]]$no2Corr, datList[[3]]$hclCorr, 
                method = "pearson")

cimsNo220 <- cor(datList[[1]]$no2Corr, datList[[1]]$cimsCorr, 
                method = "pearson")
cimsNo244 <- cor(datList[[2]]$no2Corr, datList[[2]]$cimsCorr, 
                method = "pearson")
cimsNo266 <- cor(datList[[3]]$no2Corr, datList[[3]]$cimsCorr, 
                method = "pearson")

cimsHcl20 <- cor(datList[[1]]$hclCorr, datList[[1]]$cimsCorr, 
                 method = "pearson")
cimsHcl44 <- cor(datList[[2]]$hclCorr, datList[[2]]$cimsCorr, 
                 method = "pearson")
cimsHcl66 <- cor(datList[[3]]$hclCorr, datList[[3]]$cimsCorr, 
                 method = "pearson")

data.frame(hclNo2 = c(hclNo220,hclNo244,hclNo266),
           cimsNo2 = c(cimsNo220, cimsNo244, cimsNo266),
           cimsHcl = c(cimsHcl20, cimsHcl44,cimsHcl66))

##################################
# HCl cal curve
x <- c(11,44,66)
y <- c(658,1445,1690)

summary(lm(y~x))
# 
# summary(lm(y~x))
# 
# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   1      2      3 
# -44.16 110.39 -66.24 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  491.342    160.737   3.057    0.201
# x             19.165      3.477   5.513    0.114
# 
# Residual standard error: 136.1 on 1 degrees of freedom
# Multiple R-squared:  0.9681,	Adjusted R-squared:  0.9363 
# F-statistic: 30.39 on 1 and 1 DF,  p-value: 0.1142

# For the waky NO2 plot, the RH is 28
# at 27% RH - 
# Sensitivity - 1008.46
# at 20% RH - 
# Sensitivity - 874.3

##################################
# NO2 cal curve
x <- c(11,44,66)
y <- c(631,1345,1499)
summary(lm(y~x))
# 
# > summary(lm(y~x))
# 
# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   1      2      3 
# -50.84 127.11 -76.26 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  503.158    185.068   2.719    0.224
# x             16.244      4.003   4.058    0.154
# 
# Residual standard error: 156.7 on 1 degrees of freedom
# Multiple R-squared:  0.9428,	Adjusted R-squared:  0.8855 
# F-statistic: 16.47 on 1 and 1 DF,  p-value: 0.1538

# at 27% RH - 
# Sensitivity - 941.48
# at 20% RH - 
# Sensitivity - 827.8


# September Cal Curve Regression ------------------------------------------
hcl <- c(0.266,1.267,1.718,1.005,0.657)
hclSd <- c(0.0194,0.0196,0.0206,0.0183,0.016)

cims <- c(198.4,912,1140,688,416)
cimsSd <- c(32.5,45.6,39.5,37.2,33)

summary(bfsl(x = hcl, y = cims, 
             sd_x = hclSd, sd_y = cimsSd))

summary(lm(cims~hcl))
