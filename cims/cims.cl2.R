# This is for looking at the lineloss as it relates to Cl2 and moisture across
# 15 cm of PFA tubing.  

CL2DIR <- "G:/My Drive/Experiments/CIMS/calibrations/cl2/"

cl2times <- read_csv(paste0(CL2DIR,"timestamps.csv")) %>% 
  mutate(st = dmy_hm(st), 
         et = dmy_hm(et))


cl2ff1 <- list.files(paste0(CL2DIR,"data"),pattern = "20250801", full.names = TRUE)
cl2ff1_ts <- basename(cl2ff1) %>% 
  strsplit(split = "_") %>% 
  sapply(function(x){paste(x[1],x[2])}) %>% 
  ymd_hms()

cl2List1 <- lapply(cl2ff1, read.csv,stringsAsFactors = FALSE)

for(ix in 1:length(cl2ff1_ts)){
  cl2List1[[ix]]$ts <- cl2List1[[ix]]$t_elapsed_Buf + cl2ff1_ts[ix]
}

cl2DF1 <- do.call(rbind,cl2List1)


cl2ff2 <- list.files(paste0(CL2DIR,"data"),
                     pattern = "20250804", 
                     full.names = TRUE)

cl2ff2_ts <- basename(cl2ff2) %>% 
  strsplit(split = "_") %>% 
  sapply(function(x){paste(x[1],x[2])}) %>% 
  ymd_hms()

cl2List2<- lapply(cl2ff2, read.csv,stringsAsFactors = FALSE)

for(ix in 1:length(cl2ff2_ts)){
  cl2List2[[ix]]$ts <- cl2List2[[ix]]$t_elapsed_Buf + cl2ff2_ts[ix]
}

cl2DF2 <- do.call(rbind,cl2List2)

common_cols <- intersect(names(cl2DF1),names(cl2DF2))


cl2DF <- rbind(cl2DF1[common_cols], cl2DF2[common_cols])

calList <- list()

for(ix in 1: nrow(cl2times)){
  calList[[ix]] <- cl2DF %>% 
    filter(between(ts,cl2times$st[ix],cl2times$et[ix])) %>% 
    select(ts,X.BrCl2..,X.Br..,X.BrH2O..) %>% 
    mutate(brratio = X.BrH2O../ (X.Br..) * 10^6,
           cl2Norm = X.BrCl2.. / (X.BrH2O.. + X.Br..) *10^6)
}


test2 <- test %>% 
  filter(between(ts, ymd_hms("2025-08-01 12:10:00"),ymd_hms("2025-08-01 13:00:00"))) 
  


cl2Means<- sapply(calList, function(x){mean(x$cl2Norm)})
cl2Sds <- sapply(calList, function(x){sd(x$cl2Norm)})

brratio <- sapply(calList, function(x){mean(x$brratio)})

cl2Out <- cbind(cl2times,cl2Means,cl2Sds,brratio)

write.csv(cl2Out,"G:/My Drive/Experiments/CIMS/calibrations/cl2/20250804-cl2.csv",
          quote = FALSE,
          row.names=  FALSE)

# 14 Aug ------------------------------------------------------------------


cl2times <- read_csv(paste0(CL2DIR,"20250812-times.csv")) %>% 
  mutate(st = dmy_hm(st), 
         et = dmy_hm(et))

cl2ff <- list.files(paste0(CL2DIR,"data"),pattern = "20250812", full.names = TRUE)
cl2List <- lapply(cl2ff, read.csv,stringsAsFactors = FALSE)
cl2DF <- do.call(rbind,cl2List) %>% 
  rename(ts = tWvDateTime) %>% 
  mutate(ts = dmy_hms(ts)+3600) %>% 
  mutate(brratio = X.BrH2O../ (X.Br..) * 10^6,
         cl2Norm = X.BrCl2.. / (X.BrH2O.. + X.Br..) *10^6) %>% 
  filter(ts > ymd_hm("2025-08-12 12:00"))


calList <- list()
for(ix in 1: nrow(cl2times)){
  calList[[ix]] <- cl2DF %>% 
    filter(between(ts,cl2times$st[ix],cl2times$et[ix])) %>% 
    summarize(ts = mean(ts), cl2NormMean = mean(cl2Norm), brratioMean = mean(brratio),
              cl2Norm_sd = sd(cl2Norm), brratio_sd = sd(brratio),)
}

test <- do.call(rbind,calList)[2:7,]
plot(test$ts,test$cl2NormMean)
write.csv(test,
          "G:/My Drive/Experiments/CIMS/calibrations/cl2/2025.08.12/calpts.csv",
          quote =FALSE, row.names = FALSE)



