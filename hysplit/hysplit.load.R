library(splitr)
library(openair)
library(gridExtra)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggallin)
library(parallel)
library(foreach)

library(foreach)
library(doParallel)
registerDoParallel(cores= 4)

# if the trajectories have been loaded and saved to a local object already,
# uncomment this line.
#load("G:/My Drive/Experiments/DEFRA/hysplit/trajDF.Rds")

# Makes a beep.  Useful in a loop or function that takes a while to process
beep <- function(n = 3){
  for(i in seq(n)){
    system("rundll32 user32.dll,MessageBeep -1")
    Sys.sleep(.5)
  }
}

TRAJDIR <- "G:/My Drive/Experiments/DEFRA/hysplit/trajectories"
PLOTDIR <- file.path(TRAJDIR,"plots")

YEARS2LOAD = c(2017:2024) # include all years you want loaded

sites <- as.data.frame(matrix(data = c(53.44,-2.21,"Manchester Air Quality Site",
                                       52.45, -1.93, "Birmingham Super Site",
                                       54.98, -1.61, "Newcastle Center",
                                       55.95, -3.20, "Edinburgh Center",
                                       54.60, -5.93, "Belfast Center",
                                       51.48, -3.18, "Cardiff Center",
                                       51.45, -0.04,  "London Super Site",
                                       51.08, -4.10, "Barnstaple Center",
                                       55.79, -3.24, "Auchencorth Moss",
                                       51.15, -1.44, "Chilbolton Observatory"),
                              ncol=3,byrow=TRUE,dimnames = list(c(),c("lat","lon","loc"))))

sites$lat <- as.numeric(sites$lat)
sites$lon <- as.numeric(sites$lon)

################################################################################
# Trajectory Load Functions
################################################################################
# this is an edited version of trajectory_read from the splitr package.
# It correctly adds receptor column information.  It is important
# the HYSPLIT output file have the prefix "tback"
trajectory_read_alt <- function (output_folder, YEARS=YEARS2LOAD){
  tfl <- list()
    
  for(ix in 1:length(YEARS)){
    tfl[[ix]]<- list.files(path = output_folder, 
                           pattern = paste0("tback-", YEARS[ix]))
  }
  
  trajectory_file_list <- unlist(tfl)
    
  traj_tbl <- dplyr::tibble(receptor = integer(0), year = integer(0), 
                            month = integer(0), day = integer(0), hour = integer(0), 
                            hour_along = integer(0), lat = numeric(0), lon = numeric(0), 
                            height = numeric(0), pressure = numeric(0), traj_dt = lubridate::as_datetime("2015-01-01")[-1], 
                            traj_dt_i = lubridate::as_datetime("2015-01-01")[-1])
  extended_col_names <- c("year", "month", "day", "hour", "hour_along", 
                          "lat", "lon", "height", "pressure", "theta", "air_temp", 
                          "rainfall", "mixdepth", "rh", "sp_humidity", "h2o_mixrate", 
                          "terr_msl", "sun_flux")
  standard_col_names <- c("receptor","year", "month", "day", "hour", "hour_along", 
                          "lat", "lon", "height", "pressure")
  
  # have replaced the default for loop with a parallel for loop.  Hopefully quickens the loading for
  # these 72 hour back trajectories.
  for (file_i in trajectory_file_list){
                        file_i_path <- file.path(output_folder, file_i)
                        file_lines <- readLines(file_i_path, encoding = "UTF-8", 
                                                skipNul = TRUE)
                        file_one_line <- readr::read_file(file_i_path)
                        header_line <- file_lines %>% vapply(FUN.VALUE = logical(1), 
                                                             USE.NAMES = FALSE, function(x) splitr:::tidy_grepl(x, "PRESSURE")) %>% 
                          which()
                        file_lines_data <- file_lines[(header_line + 1):(length(file_lines))] %>% 
                          splitr:::tidy_gsub("\\s\\s*", " ") %>% splitr:::tidy_gsub("^ ", "")
                        if (!file_one_line %>% splitr:::tidy_grepl("AIR_TEMP")) {
                          traj_tbl_i <- file_lines_data %>% strsplit("\\s+") %>% 
                            lapply(FUN = function(x) {
                              x[c(1,3:6, 9:13)] %>% as.numeric() %>% stats::setNames(standard_col_names) %>% 
                                as.list() %>% dplyr::as_tibble()
                            }) %>% dplyr::bind_rows() %>% dplyr::mutate_at(.vars = dplyr::vars(receptor,year, 
                                                                                               month, day, hour, hour_along), .funs = as.integer) %>% 
                            dplyr::mutate(run = which(trajectory_file_list == file_i)) %>% 
                            dplyr::mutate(year_full = ifelse(year < 50, year + 
                                                               2000, year + 1900)) %>% tidyr::unite(col = date_str, 
                                                                                                    year_full, month, day, sep = "-", remove = FALSE) %>% 
                            tidyr::unite(col = date_h_str, date_str, hour, 
                                         sep = " ", remove = FALSE) %>% dplyr::mutate(traj_dt = lubridate::ymd_h(date_h_str)) %>% 
                            dplyr::select(-c(date_h_str, date_str, year_full)) %>% 
                            dplyr::mutate(traj_dt_i = traj_dt[1])
                        
                          traj_tbl <- traj_tbl %>% dplyr::bind_rows(traj_tbl_i) 
                        }
                        if (file_one_line %>% splitr:::tidy_grepl("AIR_TEMP")) {
                          file_lines_data_20 <- file_lines_data %>% vapply(FUN.VALUE = logical(1), 
                                                                           USE.NAMES = FALSE, function(x) {
                                                                             tidy_grepl(x, paste0("^", rep("[0-9\\.-]*?", 
                                                                                                           20) %>% paste(collapse = " "), "$"))
                                                                           })
                          file_lines_data_02 <- file_lines_data %>% vapply(FUN.VALUE = logical(1), 
                                                                           USE.NAMES = FALSE, function(x) {
                                                                             tidy_grepl(x, paste0("^", rep("[0-9\\.-]*?", 
                                                                                                           2) %>% paste(collapse = " "), "$"))
                                                                           })
                          traj_tbl_i <- paste(file_lines_data[file_lines_data_20], 
                                              file_lines_data[file_lines_data_02]) %>% strsplit("\\s+") %>% 
                            lapply(FUN = function(x) {
                              x[c(3:6, 9:22)] %>% as.numeric() %>% stats::setNames(extended_col_names) %>% 
                                as.list() %>% dplyr::as_tibble()
                            }) %>% dplyr::bind_rows() %>% dplyr::mutate_at(.vars = dplyr::vars(year, 
                                                                                               month, day, hour, hour_along), .funs = as.integer) %>% 
                            dplyr::mutate(year_full = ifelse(year < 50, year + 
                                                               2000, year + 1900)) %>% tidyr::unite(col = date_str, 
                                                                                                    year_full, month, day, sep = "-", remove = FALSE) %>% 
                            tidyr::unite(col = date_h_str, date_str, hour, 
                                         sep = " ", remove = FALSE) %>% dplyr::mutate(traj_dt = lubridate::ymd_h(date_h_str)) %>% 
                            dplyr::select(-c(date_h_str, date_str, year_full)) %>% 
                            dplyr::mutate(traj_dt_i = traj_dt[1])

                           traj_tbl <- traj_tbl %>% dplyr::bind_rows(traj_tbl_i)
                        }
                      }
  traj_tbl
}



trajectory_read_alt_Par <- function (output_folder, YEARS=YEARS2LOAD){
  tfl <- list()
    
  for(ix in 1:length(YEARS)){
    tfl[[ix]]<- list.files(path = output_folder, 
                           pattern = paste0("tback-", YEARS[ix]))
  }
  
  trajectory_file_list <- unlist(tfl)
  
  traj_tbl <- dplyr::tibble(receptor = integer(0), year = integer(0), 
                            month = integer(0), day = integer(0), hour = integer(0), 
                            hour_along = integer(0), lat = numeric(0), lon = numeric(0), 
                            height = numeric(0), pressure = numeric(0), traj_dt = lubridate::as_datetime("2015-01-01")[-1], 
                            traj_dt_i = lubridate::as_datetime("2015-01-01")[-1])
  extended_col_names <- c("year", "month", "day", "hour", "hour_along", 
                          "lat", "lon", "height", "pressure", "theta", "air_temp", 
                          "rainfall", "mixdepth", "rh", "sp_humidity", "h2o_mixrate", 
                          "terr_msl", "sun_flux")
  standard_col_names <- c("receptor","year", "month", "day", "hour", "hour_along", 
                          "lat", "lon", "height", "pressure")
  
  # have replaced the default for loop with a parallel for loop.  Hopefully quickens the loading for
  # these 72 hour back trajectories.
  trajList<- foreach (file_i = trajectory_file_list, .combine = rbind, 
           .packages = c("tidyverse","splitr")) %dopar% {
  #for (file_i in trajectory_file_list[1:2]) {
    file_i_path <- file.path(output_folder, file_i)
    file_lines <- readLines(file_i_path, encoding = "UTF-8", 
                            skipNul = TRUE)
    file_one_line <- readr::read_file(file_i_path)
    header_line <- file_lines %>% vapply(FUN.VALUE = logical(1), 
                                         USE.NAMES = FALSE, function(x) splitr:::tidy_grepl(x, "PRESSURE")) %>% 
      which()
    file_lines_data <- file_lines[(header_line + 1):(length(file_lines))] %>% 
      splitr:::tidy_gsub("\\s\\s*", " ") %>% splitr:::tidy_gsub("^ ", "")
    if (!file_one_line %>% splitr:::tidy_grepl("AIR_TEMP")) {
      traj_tbl_i <- file_lines_data %>% strsplit("\\s+") %>% 
        lapply(FUN = function(x) {
          x[c(1,3:6, 9:13)] %>% as.numeric() %>% stats::setNames(standard_col_names) %>% 
            as.list() %>% dplyr::as_tibble()
        }) %>% dplyr::bind_rows() %>% dplyr::mutate_at(.vars = dplyr::vars(receptor,year, 
                                                                           month, day, hour, hour_along), .funs = as.integer) %>% 
        dplyr::mutate(run = which(trajectory_file_list == file_i)) %>% 
        dplyr::mutate(year_full = ifelse(year < 50, year + 
                                           2000, year + 1900)) %>% tidyr::unite(col = date_str, 
                                                                                year_full, month, day, sep = "-", remove = FALSE) %>% 
        tidyr::unite(col = date_h_str, date_str, hour, 
                     sep = " ", remove = FALSE) %>% dplyr::mutate(traj_dt = lubridate::ymd_h(date_h_str)) %>% 
        dplyr::select(-c(date_h_str, date_str, year_full)) %>% 
        dplyr::mutate(traj_dt_i = traj_dt[1])
      return(traj_tbl_i)
      #traj_tbl <- traj_tbl %>% dplyr::bind_rows(traj_tbl_i) 
    }
    if (file_one_line %>% splitr:::tidy_grepl("AIR_TEMP")) {
      file_lines_data_20 <- file_lines_data %>% vapply(FUN.VALUE = logical(1), 
                                                       USE.NAMES = FALSE, function(x) {
                                                         tidy_grepl(x, paste0("^", rep("[0-9\\.-]*?", 
                                                                                       20) %>% paste(collapse = " "), "$"))
                                                       })
      file_lines_data_02 <- file_lines_data %>% vapply(FUN.VALUE = logical(1), 
                                                       USE.NAMES = FALSE, function(x) {
                                                         tidy_grepl(x, paste0("^", rep("[0-9\\.-]*?", 
                                                                                       2) %>% paste(collapse = " "), "$"))
                                                       })
      traj_tbl_i <- paste(file_lines_data[file_lines_data_20], 
                          file_lines_data[file_lines_data_02]) %>% strsplit("\\s+") %>% 
        lapply(FUN = function(x) {
          x[c(3:6, 9:22)] %>% as.numeric() %>% stats::setNames(extended_col_names) %>% 
            as.list() %>% dplyr::as_tibble()
        }) %>% dplyr::bind_rows() %>% dplyr::mutate_at(.vars = dplyr::vars(year, 
                                                                           month, day, hour, hour_along), .funs = as.integer) %>% 
        dplyr::mutate(year_full = ifelse(year < 50, year + 
                                           2000, year + 1900)) %>% tidyr::unite(col = date_str, 
                                                                                year_full, month, day, sep = "-", remove = FALSE) %>% 
        tidyr::unite(col = date_h_str, date_str, hour, 
                     sep = " ", remove = FALSE) %>% dplyr::mutate(traj_dt = lubridate::ymd_h(date_h_str)) %>% 
         dplyr::select(-c(date_h_str, date_str, year_full)) %>% 
        dplyr::mutate(traj_dt_i = traj_dt[1])
      return(traj_tbl_i)
      
      #traj_tbl <- traj_tbl %>% dplyr::bind_rows(traj_tbl_i)
    }
           }
  beep()
  trajList
}


# as a reminder, here are the sites that are referenced in trajDF:
# 
# sites
#   lat   lon    loc
# 1 53.44 -2.21  Manchester Air Quality Site
# 2 52.45 -1.93  Birmingham Super Site
# 3 54.98 -1.61  Newcastle Center
# 4 55.95 -3.20  Edinburgh Center
# 5 54.60 -5.93  Belfast Center
# 6 51.48 -3.18  Cardiff Center
# 7 51.45 -0.04  London Super Site


# If the trajectories have already been loaded, formatted, and saved to an R
# object, comment out the next line.  
trajDF <- trajectory_read_alt_Par(TRAJDIR, YEARS = YEARS2LOAD) # this loads the data per splitr.
# I will reformat to work with openair.

trajDF_oa <- trajDF %>% 
  rename(hour.inc = hour_along,
         date2 = traj_dt,
         date = traj_dt_i) %>% 
  mutate(siteid = receptor) %>%
  # the scatterPlot function can easily make gridded plots, but needs
  # a categorical variable to do so.  So I will create site id
  mutate(siteid = recode(siteid, `1` = sites$loc[1], 
                         `2` =  sites$loc[2],
                         `3` = sites$loc[3], 
                         `4` = sites$loc[4], 
                         `5` = sites$loc[5], 
                         `6` = sites$loc[6],
                         `7` = sites$loc[7],
                         `8` = sites$loc[8],
                         `9` = sites$loc[9],
                         `10` = sites$loc[10]))


