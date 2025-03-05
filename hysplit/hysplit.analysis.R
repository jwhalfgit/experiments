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
load("G:/My Drive/Experiments/DEFRA/hysplit/trajDF.Rds")

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
# trajDF <- trajectory_read_alt_Par(TRAJDIR, YEARS = YEARS2LOAD) # this loads the data per splitr.
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
                         `8` = sites$loc[8]))



# Generate individual site data.frames
maqs <- trajDF_oa %>% filter(receptor == 1) 
bss <- trajDF_oa %>% filter(receptor == 2)
nc <- trajDF_oa %>% filter(receptor == 3)
ec <- trajDF_oa %>% filter(receptor == 4)
bc <- trajDF_oa %>% filter(receptor == 5)
cc <- trajDF_oa %>% filter(receptor == 6)
lc <- trajDF_oa %>% filter(receptor == 7)
barnc <- trajDF_oa %>% filter(receptor == 8)


################################################################################
# PLOTTING FUNCTIONS
################################################################################


DATE1 <- "01/01/2017"
DATE2 <- "31/12/2024"

plotTraj <- function(DF, PARTFILTER = 0.01, DATE1, DATE2, GRIDFILTER = 0){
  SITES = DF
  noTrajFilter <- length(seq(dmy(DATE1),dmy(DATE2),by =0.5)) * PARTFILTER
  
  df1 <- selectByDate(DF, start = DATE1,
                      end = DATE2)
  x <-trajLevel(df1,statistic = "frequency",plot="FALSE") %>% 
    `[[`("data") %>% 
    select(xgrid,ygrid,count) %>% 
    filter(count > noTrajFilter) %>% 
    mutate(ncount = count / (noTrajFilter/PARTFILTER)*100) %>% 
    filter(ncount >= quantile(ncount, GRIDFILTER))
  

  # Transform the coordinates to for plotting on a polar map
  dfTransform <- st_as_sf(x,coords = c("xgrid","ygrid"), crs = 4326)
  
  # Because we're plotting frequencies, let's make grids to fill
  grid_sf <- st_make_grid(dfTransform, cellsize = c(1, 1), square = TRUE) %>% 
    st_sf(geometry = .) %>% 
    st_join(dfTransform, join = st_intersects) %>% 
    st_transform(crs = st_crs("+proj=laea +lat_0=54 +lon_0=-2")) %>% 
    mutate(dcountPlot = ncount)

  # Need to define color scale cutoff.  When multiple years are
  # aggregated, (2021-2023 so far..) individual grid frequencies may
  # approach or exceed abs(10,000). , while for seasonal plots, it may
  #' be closer to 1000.
  # grid_sf$dcountPlot[grid_sf$dcountPlot > 5000] = 5000
  # grid_sf$dcountPlot[grid_sf$dcountPlot < -5000] = -5000
  # 
  #df_grid <- st_join(grid_sf, dfTransform, join = st_intersects)
  
  # Prep a map
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  #Project the map into a Lambert projection centered around the UK
  lambert_crs <- st_crs("+proj=laea +lat_0=54 +lon_0=-2")
  # world_lambert <- st_transform(world, lambert_crs)
  #df_grid_lambert <- st_transform(df_grid, lambert_crs)
  
  # Tell ggplot what plotting window you want, but this will create a square plot
  bbox <- st_bbox(c(xmin = -30, xmax = 20, ymin = 30, ymax = 70), crs = st_crs(4326))
  bbox_lambert <- st_transform(st_as_sfc(bbox), lambert_crs)
  
  xlim_4326 <- st_bbox(grid_sf)[c(1,3)]
  ylim_4326 <- st_bbox(grid_sf)[c(2,4)]
  
  # plot origin points
  points <- data.frame(
    lat=c(DF$lat[1]),
    lon=c(DF$lon[1])
  )
  
  site1Name <- sites$loc[which(sites$lat==points[1,1]&sites$lon == points[1,2])]
  
  points_sf <- st_as_sf(points, coords = c("lon", "lat"), crs = 4326)
  points_lambert <- st_transform(points_sf, lambert_crs)
  
  # Make the plot!
  ggplot() + 
    geom_sf(data = grid_sf, aes(fill = dcountPlot, color = NA),color = NA,alpha = 1)+
    # #scale_fill_gradientn(colors = rainbow(7), name = "Difference") +
    scale_fill_distiller(palette = "YlOrRd", 
                        direction = 1,
                                     limits = c(1,500),
                                     name = "particles per trajectory (%)", 
                                     trans = log_trans(base =10),
                                     breaks=c(0.1,100,200,300,400,500),
                                     na.value = "transparent")+#,
    #c2=10,l2=90, mid = 0.1, n_interp = 15) +
    # scale_fill_continuous_diverging(palette = "Blue-Red 3", limits = c(-100,100),name = "Difference", trans = pseudolog10_trans,
    #                                  breaks=c(-100,-10,0,10,100),na.value = "transparent") +
    # scale_fill_distiller(palette = "RdBu", limits = c(-100,100),name = "Difference", trans = pseudolog10_trans,
    #                       breaks=c(-100,-10,0,10,100),na.value = "transparent",type = "div") +
    # scale_fill_gradient2(low = "blue", mid = "white", high = "red",
    #                      midpoint = 0, #limits = c(-1*max(abs(grid_sf$dcountPlot)), max(abs(grid_sf$dcountPlot))),
    #                      name = "Difference", trans = pseudolog10_trans,
    #                      breaks = c(-100, -10, 0, 10, 100), na.value = "transparent",space = "Lab")+
    # scale_fill_distiller(palette = "RdBu",
    #                      #midpoint = 0, #limits = c(-1*max(abs(grid_sf$dcountPlot)), max(abs(grid_sf$dcountPlot))),
    #                      name = "Difference", trans = pseudolog10_trans,
    #                      breaks = c(-100, -10, 0, 10, 100), na.value = "transparent",space = "Lab")+
    theme_bw()+
    geom_point(data = points_lambert, aes(x = st_coordinates(points_lambert)[,1], y = st_coordinates(points_lambert)[,2]), color = "blue", size = 3, shape = 21, fill = "yellow")+
    geom_sf(data = world, fill =NA, color = "black")+
    coord_sf(xlim = st_bbox(bbox_lambert)[c("xmin", "xmax")], ylim = st_bbox(bbox_lambert)[c("ymin", "ymax")], expand = TRUE) +
    #coord_sf(crs = st_crs(3035), xlim = c(500000, 4500000), ylim = c(2000000, 7000000),expand=TRUE)+
    # #coord_sf(crs = st_crs(3035), xlim = c(500000, 4500000), ylim = c(2000000, 7000000),expand=TRUE)+
    labs(title = paste(site1Name, DATE1, "-", DATE2), x = "Longitude", y = "Latitude")+
    theme(axis.title = element_text(size = 24))
  
  
  ggsave(paste0(PLOTDIR,"/",gsub("-","",dmy(DATE1)),gsub("-","",dmy(DATE2)),
                "-",deparse(substitute(DF)),
                ".png"), width = 12.80, height = 7.68, units = "in")
  
}


plotTrajBin <- function(DF, DATE1, DATE2, BINS = c(0,0.5,5,50,500)){
  SITES = DF
  noTraj <- length(seq(dmy(DATE1),dmy(DATE2),by =0.5))
  
  df1 <- selectByDate(DF, start = DATE1,
                      end = DATE2)
  
  x <-trajLevel(df1,statistic = "frequency",plot="FALSE") %>% 
    `[[`("data") %>% 
    select(xgrid,ygrid,count) %>% 
    #filter(count > noTrajFilter) %>% 
    mutate(ncount = count / noTraj*100) %>%  # normalized count
    mutate(bins = cut(ncount, breaks = BINS))
    #filter(ncount >= quantile(ncount, GRIDFILTER))
  

  # Transform the coordinates to for plotting on a polar map
  dfTransform <- st_as_sf(x,coords = c("xgrid","ygrid"), crs = 4326)
  
  # Because we're plotting frequencies, let's make grids to fill
  grid_sf <- st_make_grid(dfTransform, cellsize = c(1, 1), square = TRUE) %>% 
    st_sf(geometry = .) %>% 
    st_join(dfTransform, join = st_intersects) %>% 
    st_transform(crs = st_crs("+proj=laea +lat_0=54 +lon_0=-2")) %>% 
    mutate(dcountPlot = bins)
  

  # Prep a map
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  #Project the map into a Lambert projection centered around the UK
  lambert_crs <- st_crs("+proj=laea +lat_0=54 +lon_0=-2")
  # world_lambert <- st_transform(world, lambert_crs)
  #df_grid_lambert <- st_transform(df_grid, lambert_crs)
  
  # Tell ggplot what plotting window you want, but this will create a square plot
  bbox <- st_bbox(c(xmin = -30, xmax = 20, ymin = 30, ymax = 70), crs = st_crs(4326))
  bbox_lambert <- st_transform(st_as_sfc(bbox), lambert_crs)
  
  xlim_4326 <- st_bbox(grid_sf)[c(1,3)]
  ylim_4326 <- st_bbox(grid_sf)[c(2,4)]
  
  # plot origin points
  points <- data.frame(
    lat=c(DF$lat[1]),
    lon=c(DF$lon[1])
  )
  
  site1Name <- sites$loc[which(sites$lat==points[1,1]&sites$lon == points[1,2])]
  
  points_sf <- st_as_sf(points, coords = c("lon", "lat"), crs = 4326)
  points_lambert <- st_transform(points_sf, lambert_crs)
  
  # Make the plot!
  ggplot() + 
    geom_sf(data = grid_sf, aes(fill = bins, color = NA),color = NA,alpha = 1)+
    # #scale_fill_gradientn(colors = rainbow(7), name = "Difference") +
    #scale_fill_manual(values = discr_colors,
    scale_fill_brewer(palette = "YlOrRd",
                         #direction = 1,
                         #limits = c(0.5,500),
                         name = "particles per trajectory (%)", 
                         #trans = log_trans(base =10),
                         #breaks=BREAKS,
                         na.value = "transparent")+#,
    
    theme_bw()+
    geom_point(data = points_lambert, aes(x = st_coordinates(points_lambert)[,1], y = st_coordinates(points_lambert)[,2]), color = "blue", size = 3, shape = 21, fill = "yellow")+
    geom_sf(data = world, fill =NA, color = "black")+
    coord_sf(xlim = st_bbox(bbox_lambert)[c("xmin", "xmax")], ylim = st_bbox(bbox_lambert)[c("ymin", "ymax")], expand = TRUE) +
    #coord_sf(crs = st_crs(3035), xlim = c(500000, 4500000), ylim = c(2000000, 7000000),expand=TRUE)+
    # #coord_sf(crs = st_crs(3035), xlim = c(500000, 4500000), ylim = c(2000000, 7000000),expand=TRUE)+
    labs(title = paste(site1Name, DATE1, "-", DATE2), x = "Longitude", y = "Latitude")+
    theme(axis.title = element_text(size = 24))
  
  
  ggsave(paste0(PLOTDIR,"/",gsub("-","",dmy(DATE1)),gsub("-","",dmy(DATE2)),
                "-",deparse(substitute(DF)),
                "-binned.png"), width = 12.80, height = 7.68, units = "in")
  
}


# Comparison of Manc and Newc with Birm -----------------------------------
DIFFPLOTDIR <- "G:/My Drive/Experiments/DEFRA/hysplit/differences/"


plotTrajDiff <- function(DF, FILTER, SITE1, SITE2,DATE1,DATE2){
  
  noTraj <- length(seq(dmy(DATE1),dmy(DATE2),by =0.5))
  
  dfFilter <- DF %>% 
    mutate(ncount = dcount / (noTraj) * 100) # delta points per trajectory
    #mutate(ncount = dcount / (noTraj*72) * 100) # delta points per total points
    #filter(between(dcount,-1*FILTER,FILTER))
  
  # Transform the coordinates to for plotting on a polar map
  dfTransform <- st_as_sf(dfFilter,coords = c("xgrid","ygrid"), crs = 4326)
  
  # Because we're plotting frequencies, let's make grids to fill
  grid_sf <- st_make_grid(dfTransform, cellsize = c(1, 1), square = TRUE) %>% 
    st_sf(geometry = .) %>% 
    st_join(dfTransform, join = st_intersects) %>% 
    st_transform(crs = st_crs("+proj=laea +lat_0=54 +lon_0=-2")) %>% 
    mutate(dcountPlot = ncount)
    #st_transform(crs = 4326)
  
  # Need to define color scale cutoff.  When multiple years are
  # aggregated, (2021-2023 so far..) individual grid frequencies may
  # approach or exceed abs(10,000). , while for seasonal plots, it may
  #' be closer to 1000.
  # grid_sf$dcountPlot[grid_sf$dcountPlot > 5000] = 5000
  # grid_sf$dcountPlot[grid_sf$dcountPlot < -5000] = -5000
  # 
  #df_grid <- st_join(grid_sf, dfTransform, join = st_intersects)
  
  # Prep a map
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  #Project the map into a Lambert projection centered around the UK
  lambert_crs <- st_crs("+proj=laea +lat_0=54 +lon_0=-2")
  # world_lambert <- st_transform(world, lambert_crs)
  #df_grid_lambert <- st_transform(df_grid, lambert_crs)
  
  # Tell ggplot what plotting window you want, but this will create a square plot
  bbox <- st_bbox(c(xmin = -30, xmax = 20, ymin = 30, ymax = 70), crs = st_crs(4326))
  bbox_lambert <- st_transform(st_as_sfc(bbox), lambert_crs)
  
  xlim_4326 <- st_bbox(grid_sf)[c(1,3)]
  ylim_4326 <- st_bbox(grid_sf)[c(2,4)]
  
  # plot origin points
  points <- data.frame(
    lat=c(SITE1$lat[1],SITE2$lat[1]),
    lon=c(SITE1$lon[1],SITE2$lon[1])
  )

  site1Name <- sites$loc[which(sites$lat==points[1,1]&sites$lon == points[1,2])]
  site2Name <- sites$loc[which(sites$lat==points[2,1]&sites$lon == points[2,2])]
  
  points_sf <- st_as_sf(points, coords = c("lon", "lat"), crs = 4326)
  points_lambert <- st_transform(points_sf, lambert_crs)

  # Make the plot!
  ggplot() + 
    geom_sf(data = grid_sf, aes(fill = dcountPlot, color = NA),color = NA,alpha = 1)+
    # #scale_fill_gradientn(colors = rainbow(7), name = "Difference") +
    scale_fill_continuous_divergingx(palette = "RdYlBu", 
                                     limits = c(-500,500),
                                     name = "% Difference", 
                                     trans = pseudo_log_trans(base =10),
                                     #breaks=c(-500,-50,-5,0,5,50,500),
                                     breaks = c(-500,-400,-300,-200,-100,-1,1,100,200,300,400,500),
                                     na.value = "transparent",
                                     rev=TRUE)+#,
                          #c2=10,l2=90, mid = 0.1, n_interp = 15) +
    # scale_fill_continuous_diverging(palette = "Blue-Red 3", limits = c(-100,100),name = "Difference", trans = pseudolog10_trans,
    #                                  breaks=c(-100,-10,0,10,100),na.value = "transparent") +
    # scale_fill_distiller(palette = "RdBu", limits = c(-100,100),name = "Difference", trans = pseudolog10_trans,
    #                       breaks=c(-100,-10,0,10,100),na.value = "transparent",type = "div") +
    # scale_fill_gradient2(low = "blue", mid = "white", high = "red",
    #                      midpoint = 0, #limits = c(-1*max(abs(grid_sf$dcountPlot)), max(abs(grid_sf$dcountPlot))),
    #                      name = "Difference", trans = pseudolog10_trans,
    #                      breaks = c(-100, -10, 0, 10, 100), na.value = "transparent",space = "Lab")+
    # scale_fill_distiller(palette = "RdBu",
    #                      #midpoint = 0, #limits = c(-1*max(abs(grid_sf$dcountPlot)), max(abs(grid_sf$dcountPlot))),
    #                      name = "Difference", trans = pseudolog10_trans,
    #                      breaks = c(-100, -10, 0, 10, 100), na.value = "transparent",space = "Lab")+
    theme_bw()+
    geom_point(data = points_lambert, aes(x = st_coordinates(points_lambert)[,1], y = st_coordinates(points_lambert)[,2]), color = "blue", size = 3, shape = 21, fill = "yellow")+
    geom_sf(data = world, fill =NA, color = "black")+
    coord_sf(xlim = st_bbox(bbox_lambert)[c("xmin", "xmax")], ylim = st_bbox(bbox_lambert)[c("ymin", "ymax")], expand = TRUE) +
    #coord_sf(crs = st_crs(3035), xlim = c(500000, 4500000), ylim = c(2000000, 7000000),expand=TRUE)+
    # #coord_sf(crs = st_crs(3035), xlim = c(500000, 4500000), ylim = c(2000000, 7000000),expand=TRUE)+
    labs(title = paste(site1Name,"vs", site2Name, DATE1, "-", DATE2), x = "Longitude", y = "Latitude")+
    theme(axis.title = element_text(size = 24))
  
}


plotTrajDiffBin <- function(DF, SITE1, SITE2,DATE1,DATE2,
                            #BINS = c(-Inf,-500,-50,-5,-0.5-0.05,0.05,0.5,5,50,500,Inf)){
                            BINS = c(-Inf,-50,-25,-10,-5,-1,1,5,10,25,50,Inf)){
  
  noTraj <- length(seq(dmy(DATE1),dmy(DATE2),by =0.5))
  
  dfFilter <- DF %>% 
    mutate(ncount = dcount / (noTraj) * 100) %>% # delta points per trajectory
    mutate(bins = cut(ncount, breaks = BINS))
  #mutate(ncount = dcount / (noTraj*72) * 100) # delta points per total points
  #filter(between(dcount,-1*FILTER,FILTER))
  
  # Transform the coordinates to for plotting on a polar map
  dfTransform <- st_as_sf(dfFilter,coords = c("xgrid","ygrid"), crs = 4326)
  
  # Because we're plotting frequencies, let's make grids to fill
  grid_sf <- st_make_grid(dfTransform, cellsize = c(1, 1), square = TRUE) %>% 
    st_sf(geometry = .) %>% 
    st_join(dfTransform, join = st_intersects) %>% 
    st_transform(crs = st_crs("+proj=laea +lat_0=54 +lon_0=-2")) %>% 
    mutate(dcountPlot = bins)
  #st_transform(crs = 4326)
  
  # Need to define color scale cutoff.  When multiple years are
  # aggregated, (2021-2023 so far..) individual grid frequencies may
  # approach or exceed abs(10,000). , while for seasonal plots, it may
  #' be closer to 1000.
  # grid_sf$dcountPlot[grid_sf$dcountPlot > 5000] = 5000
  # grid_sf$dcountPlot[grid_sf$dcountPlot < -5000] = -5000
  # 
  #df_grid <- st_join(grid_sf, dfTransform, join = st_intersects)
  
  # Prep a map
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  #Project the map into a Lambert projection centered around the UK
  lambert_crs <- st_crs("+proj=laea +lat_0=54 +lon_0=-2")
  # world_lambert <- st_transform(world, lambert_crs)
  #df_grid_lambert <- st_transform(df_grid, lambert_crs)
  
  # Tell ggplot what plotting window you want, but this will create a square plot
  bbox <- st_bbox(c(xmin = -30, xmax = 20, ymin = 30, ymax = 70), crs = st_crs(4326))
  bbox_lambert <- st_transform(st_as_sfc(bbox), lambert_crs)
  
  xlim_4326 <- st_bbox(grid_sf)[c(1,3)]
  ylim_4326 <- st_bbox(grid_sf)[c(2,4)]
  
  # plot origin points
  points <- data.frame(
    lat=c(SITE1$lat[1],SITE2$lat[1]),
    lon=c(SITE1$lon[1],SITE2$lon[1])
  )
  
  site1Name <- sites$loc[which(sites$lat==points[1,1]&sites$lon == points[1,2])]
  site2Name <- sites$loc[which(sites$lat==points[2,1]&sites$lon == points[2,2])]
  
  points_sf <- st_as_sf(points, coords = c("lon", "lat"), crs = 4326)
  points_lambert <- st_transform(points_sf, lambert_crs)
  
  # Make the plot!
  ggplot() + 
    geom_sf(data = grid_sf, aes(fill = bins, color = NA),color = NA,alpha = 1)+
    # #scale_fill_gradientn(colors = rainbow(7), name = "Difference") +
    scale_fill_brewer(palette = "RdYlBu", 
                                     #limits = c(-500,500),
                                     name = "% More", 
                                     #trans = pseudo_log_trans(base =10),
                                     #breaks=c(-500,-50,-5,0,5,50,500),
                                     na.value = "transparent")+
                                     #rev=TRUE)+#,
    #c2=10,l2=90, mid = 0.1, n_interp = 15) +
    # scale_fill_continuous_diverging(palette = "Blue-Red 3", limits = c(-100,100),name = "Difference", trans = pseudolog10_trans,
    #                                  breaks=c(-100,-10,0,10,100),na.value = "transparent") +
    # scale_fill_distiller(palette = "RdBu", limits = c(-100,100),name = "Difference", trans = pseudolog10_trans,
    #                       breaks=c(-100,-10,0,10,100),na.value = "transparent",type = "div") +
    # scale_fill_gradient2(low = "blue", mid = "white", high = "red",
    #                      midpoint = 0, #limits = c(-1*max(abs(grid_sf$dcountPlot)), max(abs(grid_sf$dcountPlot))),
    #                      name = "Difference", trans = pseudolog10_trans,
    #                      breaks = c(-100, -10, 0, 10, 100), na.value = "transparent",space = "Lab")+
    # scale_fill_distiller(palette = "RdBu",
    #                      #midpoint = 0, #limits = c(-1*max(abs(grid_sf$dcountPlot)), max(abs(grid_sf$dcountPlot))),
    #                      name = "Difference", trans = pseudolog10_trans,
    #                      breaks = c(-100, -10, 0, 10, 100), na.value = "transparent",space = "Lab")+
    theme_bw()+
    geom_point(data = points_lambert, aes(x = st_coordinates(points_lambert)[,1], y = st_coordinates(points_lambert)[,2]), color = "blue", size = 3, shape = 21, fill = "yellow")+
    geom_sf(data = world, fill =NA, color = "black")+
    coord_sf(xlim = st_bbox(bbox_lambert)[c("xmin", "xmax")], ylim = st_bbox(bbox_lambert)[c("ymin", "ymax")], expand = TRUE) +
    #coord_sf(crs = st_crs(3035), xlim = c(500000, 4500000), ylim = c(2000000, 7000000),expand=TRUE)+
    # #coord_sf(crs = st_crs(3035), xlim = c(500000, 4500000), ylim = c(2000000, 7000000),expand=TRUE)+
    labs(title = paste(site1Name,"vs", site2Name, DATE1, "-", DATE2), x = "Longitude", y = "Latitude")+
    theme(axis.title = element_text(size = 24))
  
}

# Filter is meant to be a percentage (as decimal) of the maximum number of trajectories
calcDiff <- function(SITE1,SITE2 = bss, 
                     DATE1, DATE2,
                     PLOT = TRUE, BIN= TRUE, FILTER = 0.01){
  
  noTrajFilter <- length(seq(dmy(DATE1),dmy(DATE2),by =0.5)) * FILTER
  
  
  df1 <- selectByDate(SITE1, start = DATE1,
                      end = DATE2)
  x <-trajLevel(df1,statistic = "frequency",plot="FALSE") %>% 
    `[[`("data") %>% 
    select(xgrid,ygrid,count) %>% 
    rename(df1_count = count) 

  df2 <- selectByDate(SITE2, start = DATE1,
                      end = DATE2)
  y <-trajLevel(df2,statistic = "frequency",plot="FALSE") %>% 
    `[[`("data") %>% 
    select(xgrid,ygrid,count) %>% 
    rename(df2_count = count)

  
  dfJoin <- x %>% 
    full_join(y) %>% 
    replace_na(list(df1_count = 0,df2_count = 0)) %>% 
    filter(df1_count > noTrajFilter | df2_count > noTrajFilter) %>% 
    mutate(dcount = df2_count - df1_count)
  
  if(PLOT == TRUE){
    if(BIN == TRUE){
      outPlot <- plotTrajDiffBin(dfJoin,SITE1,SITE2,DATE1=DATE1,DATE2=DATE2)
      print(outPlot)
      ggsave(paste0(DIFFPLOTDIR,gsub("-","",dmy(DATE1)),gsub("-","",dmy(DATE2)),
                    "-",deparse(substitute(SITE1)),deparse(substitute(SITE2)),
                    "-bin.png"), width = 12.80, height = 7.68, units = "in")
    }else{
      outPlot <- plotTrajDiff(dfJoin,FILTER,SITE1,SITE2,DATE1=DATE1,DATE2=DATE2)
      print(outPlot)
      ggsave(paste0(DIFFPLOTDIR,gsub("-","",dmy(DATE1)),gsub("-","",dmy(DATE2)),
                    "-",deparse(substitute(SITE1)),deparse(substitute(SITE2)),
                    ".png"), width = 12.80, height = 7.68, units = "in")
    }
  }
  

  
  return(dfJoin)
}



seasDates <- data.frame(START = c("01/01/2021","01/04/2021","01/07/2021", "01/10/2021"),
                        STOP = c("31/03/2021","30/06/2021","30/09/2021", "31/12/2021"))
 
# Seasonal dates
maqsBss <- list()
for(ix in 1:nrow(seasDates)){
  maqsBss[[ix]] <- calcDiff(SITE1 = maqs, SITE2 = bss,
                            DATE1 = seasDates[ix,1],DATE2 = seasDates[ix,2], 
                            PLOT = TRUE)
}


ncBss <- list()
for(ix in 1:nrow(seasDates)){
  ncBss[[ix]] <- calcDiff(SITE1 = nc, SITE2 = bss,
                          DATE1 = seasDates[ix,1],DATE2 = seasDates[ix,2], 
                          PLOT = TRUE)
}


DATE1 <- "01/01/2023"
DATE2 <- "31/12/2024"

# Aggregated Dates
maqsBss <- calcDiff(SITE1 = maqs, SITE2 = bss,
                          DATE1 = DATE1,DATE2 = DATE2, 
                          PLOT = TRUE)



ncBss <- calcDiff(SITE1 = nc, SITE2 = bss,
                    DATE1 = DATE1,DATE2 = DATE2, 
                    PLOT = TRUE)


ecBss <- calcDiff(SITE1 = ec, SITE2 = bss,
                  DATE1 = DATE1,DATE2 = DATE2, 
                  PLOT = TRUE)


bcBss <- calcDiff(SITE1 = bc, SITE2 = bss,
                  DATE1 = DATE1,DATE2 = DATE2, 
                  PLOT = TRUE)

ccBss <- calcDiff(SITE1 = cc, SITE2 = bss,
                  DATE1 = DATE1,DATE2 = DATE2, 
                  PLOT = TRUE)

ecLc <- calcDiff(SITE1 = ec, SITE2 = lc,
                 DATE1 = DATE1,DATE2 = DATE2, 
                 PLOT = TRUE)

bssLc <-  calcDiff(SITE1 = bss, SITE2 = lc,
                   DATE1 = DATE1,DATE2 = DATE2, 
                   PLOT = TRUE)

barncLc <-  calcDiff(SITE1 = barnc, SITE2 = lc,
                   DATE1 = DATE1,DATE2 = DATE2, 
                   PLOT = TRUE)

################################################################################
# SECTOR ANALYSIS
################################################################################
# From the openair book, Sect. 10.4.2
sectorAnal <- function(SITE){
  
  alloc <- SITE
  
  id <- which(alloc$hour.inc == 0)
  y0 <- alloc$lat[id[1]]
  x0 <- alloc$lon[id[1]]
  
  ## calculate angle and then assign sector
  alloc <- mutate(
    alloc,
    angle = atan2(lon - x0, lat - y0) * 360 / 2 / pi,
    angle = ifelse(angle < 0, angle + 360, angle),
    sector = cut(angle,
                 breaks = seq(22.5, 382.5, 45),
                 labels = c(
                   "NE", "E", "SE",
                   "S", "SW", "W",
                   "NW", "N"
                 )
    ),
    sector = as.character(sector),
    sector = ifelse(is.na(sector), "N", sector)
  )
  
  alloc <- group_by(alloc, date, sector) %>%
    mutate(n = n()) %>%
    group_by(date) %>%
    arrange(date, n) %>%
    slice_tail(n = 1) %>%
    mutate(sector = ifelse(n > 50, sector, "unallocated")) %>%
    select(date, sector, n)
  
  # combine with trajectories
  traj <- left_join(SITE, alloc, by = "date")
  
  
  group_by(traj, sector) %>%
    summarise(n = n()) %>%
    mutate(percent = 100 * n / nrow(traj))

}

# MAQS
# # A tibble: 9 × 3
# sector           n percent
# <chr>        <int>   <dbl>
#   1 E            30158   7.36 
# 2 N             3683   0.899
# 3 NE            6502   1.59 
# 4 NW           10881   2.66 
# 5 S             4693   1.15 
# 6 SE            5413   1.32 
# 7 SW           30522   7.45 
# 8 W           101586  24.8  
# 9 unallocated 216060  52.8  

# Birmingham Super Site
# A tibble: 9 × 3
# sector           n percent
# <chr>        <int>   <dbl>
#   1 E            27183   6.78 
# 2 N             3253   0.811
# 3 NE            8838   2.20 
# 4 NW           10512   2.62 
# 5 S             4909   1.22 
# 6 SE            3935   0.981
# 7 SW           28576   7.13 
# 8 W           100232  25.0  
# 9 unallocated 213589  53.3  

# Barnstaple Center
# A tibble: 9 × 3
# sector           n percent
# <chr>        <int>   <dbl>
#   1 E            23488   5.68 
# 2 N             3476   0.841
# 3 NE           11719   2.83 
# 4 NW           14735   3.56 
# 5 S             3167   0.766
# 6 SE            4096   0.991
# 7 SW           27786   6.72 
# 8 W           116881  28.3  
# 9 unallocated 208089  50.3  


# Belfast Center
# > sectorAnal(bc)
# # A tibble: 9 × 3
# sector           n percent
# <chr>        <int>   <dbl>
#   1 E            29816    7.19
# 2 N             5603    1.35
# 3 NE            6968    1.68
# 4 NW           12500    3.01
# 5 S             7617    1.84
# 6 SE            6000    1.45
# 7 SW           32656    7.87
# 8 W           103712   25.0 
# 9 unallocated 209962   50.6 

# Cardiff Center
# > sectorAnal(cc)
# # A tibble: 9 × 3
# sector           n percent
# <chr>        <int>   <dbl>
#   1 E            22830   5.52 
# 2 N             3945   0.953
# 3 NE           12795   3.09 
# 4 NW           14324   3.46 
# 5 S             3249   0.785
# 6 SE            3604   0.871
# 7 SW           29082   7.03 
# 8 W           111888  27.0  
# 9 unallocated 212230  51.3  

# Edinburgh Center
# > sectorAnal(ec)
# # A tibble: 9 × 3
# sector           n percent
# <chr>        <int>   <dbl>
#   1 E            32223   7.81 
# 2 N             3431   0.832
# 3 NE            8679   2.10 
# 4 NW            6505   1.58 
# 5 S             4818   1.17 
# 6 SE            7966   1.93 
# 7 SW           35699   8.65 
# 8 W            96768  23.5  
# 9 unallocated 216448  52.5  


# Newcastle Center
# > sectorAnal(nc)
# # A tibble: 9 × 3
# sector           n percent
# <chr>        <int>   <dbl>
#   1 E            32955    7.99
# 2 N             5325    1.29
# 3 NE            5602    1.36
# 4 NW            5948    1.44
# 5 S             4823    1.17
# 6 SE            6666    1.62
# 7 SW           32327    7.83
# 8 W           108213   26.2 
# 9 unallocated 210844   51.1 

# # London Center
# > sectorAnal(lc)
# # A tibble: 9 × 3
# sector           n percent
# <chr>        <int>   <dbl>
#   1 E            25675   6.20 
# 2 N             7582   1.83 
# 3 NE           10511   2.54 
# 4 NW            9980   2.41 
# 5 S             3648   0.882
# 6 SE            4144   1.00 
# 7 SW           29752   7.19 
# 8 W           106038  25.6  
# 9 unallocated 216481  52.3  






