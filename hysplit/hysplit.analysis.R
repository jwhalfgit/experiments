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


met.file.loc <- "C:/HYSPLIT/metfiles/" # HYSPLIT met file directory
wd.hysplit <- "C:/HYSPLIT/working" # HYSPLIT working directory
TRAJDIR <- "G:/My Drive/Experiments/DEFRA/hysplit/trajectories/"
PLOTDIR <- file.path(TRAJDIR,"plots/")

YEAR = 2021

sites <- as.data.frame(matrix(data = c(53.44,-2.21,"Manchester Air Quality Site",
                                       52.45, -1.93, "Birmingham Super Site",
                                       54.98, -1.61, "Newcastle Center",
                                       55.95, -3.20, "Edinburgh Center",
                                       54.60, -5.93, "Belfast Center",
                                       51.48, -3.18, "Cardiff Center",
                                       51.45, -0.04,  "London Super Site",
                                       51.08, -4.10, "Barnstaple Center"),
                              ncol=3,byrow=TRUE,dimnames = list(c(),c("lat","lon","loc"))))
sites$lat <- as.numeric(sites$lat)
sites$lon <- as.numeric(sites$lon)

# this is an edited version of trajectory_read from the splitr package.
# It correctly adds receptor column information.  It is important
# the HYSPLIT output file have the prefix "tback"
trajectory_read_alt <- function (output_folder, YEARS){
  if(is.na(YEARS)!=TRUE){
    trajectory_file_list <- list.files(path = output_folder, 
                                       pattern = "^tback-.*")
  }else{
    tfl <- list()
    
    for(ix in 1:length(YEARS)){
      tfl[[ix]]<- list.files(path = output_folder, 
                             pattern = paste0("tback-", YEARS[ix]))
    }
    trajectory_file_list <- unlist(tfl)
  }
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



trajectory_read_alt_Par <- function (output_folder, YEARS){
  if(is.na(YEARS)!=TRUE){
    trajectory_file_list <- list.files(path = output_folder, 
                                       pattern = "^tback-.*")
  }else{
    tfl <- list()
    
    for(ix in 1:length(YEARS)){
      tfl[[ix]]<- list.files(path = output_folder, 
                             pattern = paste0("tback-", YEARS[ix]))
    }
    trajectory_file_list <- unlist(tfl)
  }
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
  trajList
}





# as a reminder, here are the sites that are referenced in trajDF:
# 
# sites
# lat   lon                         loc
# 1 53.44 -2.21  Manchester Air Quality Site
# 2 52.45 -1.93  Birmingham Super Site
# 3 54.98 -1.61  Newcastle Center
# 4 55.95 -3.20  Edinburgh Center
# 5 54.60 -5.93  Belfast Center
# 6 51.48 -3.18  Cardiff Center
# 7 51.45 -0.04  London Super Site


trajDF <- trajectory_read_alt_Par(TRAJDIR, YEAR = YEAR) # this loads the data per splitr.
# I will reformat to work with openair
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
                         `7` = sites$loc[7]))



# Generate individual site data.frames
maqs <- trajDF_oa %>% filter(receptor == 1) 
bss <- trajDF_oa %>% filter(receptor == 2)
nc <- trajDF_oa %>% filter(receptor == 3)
ec <- trajDF_oa %>% filter(receptor == 4)
bc <- trajDF_oa %>% filter(receptor == 5)
cc <- trajDF_oa %>% filter(receptor == 6)
lc <- trajDF_oa %>% filter(receptor == 7)


# 
# dfJFM <- selectByDate(trajDF_oa, start = "01/01/2023",
#               end = "31/03/2023" )
#  
# png(filename = paste0(PLOTDIR,"JFM.png"), 
#     width = 1920, height = 1080, pointsize = 32,res=200)
# x <-trajLevel(dfJFM,type = "siteid",statistic = "frequency",plot="FALSE")
# dev.off()
# 
# 
# dfAMJ <- selectByDate(trajDF_oa, start = "01/04/2023",
#                       end = "30/06/2023" )
# 
# png(filename = paste0(PLOTDIR,"AMJ.png"), 
#     width = 1920, height = 1080, pointsize = 24)
# trajLevel(dfAMJ,type = "siteid")
# dev.off()
# 
# dfJAS <- selectByDate(trajDF_oa, start = "01/07/2023",
#                       end = "30/09/2023" )
# 
# png(filename = paste0(PLOTDIR,"JAS.png"), 
#     width = 1920, height = 1080, pointsize = 24)
# trajLevel(dfJAS,type = "siteid")
# dev.off()
# 
# dfOND <- selectByDate(trajDF_oa, start = "01/10/2023",
#                       end = "31/12/2023" )
# 
# png(filename = paste0(PLOTDIR,"OND.png"), 
#     width = 1920, height = 1080, pointsize = 24)
# trajLevel(dfOND,type = "siteid")
# dev.off()



# Comparison of Manc and Newc with Birm -----------------------------------
DIFFPLOTDIR <- "G:/My Drive/Experiments/DEFRA/hysplit/differences/"


plotTrajDiff <- function(DF, FILTER, SITE1, SITE2){
  dfFilter <- DF# %>% 
    #filter(between(dcount,-1*FILTER,FILTER))
  
  # Transform the coordinates to for plotting on a polar map
  dfTransform <- st_as_sf(dfFilter,coords = c("xgrid","ygrid"), crs = 4326)
  
  # Because we're plotting frequencies, let's make grids to fill
  grid_sf <- st_make_grid(dfTransform, cellsize = c(1, 1), square = TRUE) %>% 
    st_sf(geometry = .) %>% 
    st_join(dfTransform, join = st_intersects) %>% 
    st_transform(crs = st_crs("+proj=laea +lat_0=54 +lon_0=-2")) %>% 
    mutate(dcountPlot = dcount)
    #st_transform(crs = 4326)
  
  # Need to define color scale cutoff.  When multiple years are
  # aggregated, (2021-2023 so far..) individual grid frequencies may
  # approach or exceed abs(10,000). , while for seasonal plots, it may
  #' be closer to 1000.
  grid_sf$dcountPlot[grid_sf$dcountPlot > 5000] = 5000
  grid_sf$dcountPlot[grid_sf$dcountPlot < -5000] = -5000
  
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
    geom_sf(data = grid_sf, aes(fill = dcountPlot, color = NA),color = NA,alpha = 0.8)+
    # #scale_fill_gradientn(colors = rainbow(7), name = "Difference") +
    scale_fill_distiller(palette = "RdYlBu", limits = c(-5000,5000),name = "Difference", trans = pseudolog10_trans,
                          breaks=c(-5000,-100,-10,0,10,100,5000),na.value = "transparent") +
    theme_bw()+
    geom_point(data = points_lambert, aes(x = st_coordinates(points_lambert)[,1], y = st_coordinates(points_lambert)[,2]), color = "blue", size = 3, shape = 21, fill = "yellow")+
    geom_sf(data = world, fill =NA, color = "black")+
    coord_sf(xlim = st_bbox(bbox_lambert)[c("xmin", "xmax")], ylim = st_bbox(bbox_lambert)[c("ymin", "ymax")], expand = TRUE) +
    #coord_sf(crs = st_crs(3035), xlim = c(500000, 4500000), ylim = c(2000000, 7000000),expand=TRUE)+
    # #coord_sf(crs = st_crs(3035), xlim = c(500000, 4500000), ylim = c(2000000, 7000000),expand=TRUE)+
    labs(title = paste(site1Name,"vs", site2Name), x = "Longitude", y = "Latitude")+
    theme(axis.title = element_text(size = 24))
  
}


calcDiff <- function(SITE1,SITE2 = bss, 
                     DATE1, DATE2,
                     PLOT = TRUE, FILTER = 500){
  
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
    filter(df1_count > 5 | df2_count > 5) %>% 
    mutate(dcount = df2_count - df1_count)
  
  if(PLOT == TRUE){
    outPlot <- plotTrajDiff(dfJoin,FILTER,SITE1,SITE2)
    print(outPlot)
    ggsave(paste0(DIFFPLOTDIR,gsub("-","",dmy(DATE1)),gsub("-","",dmy(DATE2)),
                  "-",deparse(substitute(SITE1)),deparse(substitute(SITE2)),
                  ".png"), width = 12.80, height = 7.68, units = "in")
  }
  

  
  return(dfJoin)
}


seasDates <- data.frame(START = c("01/01/2021","01/04/2021","01/07/2021", "01/10/2021"),
                        STOP = c("31/03/2021","30/06/2021","30/09/2021", "31/12/2021"))
 DATE1 <- "01/01/2021"
 DATE2 <- "31/12/2023"

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

ecLc <- calcDiff(SITE1 = ec, SITE2 = lc,
                 DATE1 = DATE1,DATE2 = DATE2, 
                 PLOT = TRUE)

bssLc <-  calcDiff(SITE1 = bss, SITE2 = lc,
                   DATE1 = DATE1,DATE2 = DATE2, 
                   PLOT = TRUE)


# Testing code for taking differences -------------------------------------
# 
# maqsJFM <- selectByDate(maqs, start = DATE1,end = DATE2 )
# x <-trajLevel(maqsJFM,statistic = "frequency",plot="FALSE") %>% 
#   `[[`("data") %>% 
#   select(xgrid,ygrid,count) %>% 
#   rename(maqs_count = count)
# 
# bssJFM <- selectByDate(bss, start = DATE1,end = DATE2 )
# y <-trajLevel(bssJFM,statistic = "frequency",plot=FALSE)%>% 
#   `[[`("data") %>% 
#   select(xgrid,ygrid,count) %>% 
#   rename(bss_count = count)
# 
# ncJFM <- selectByDate(nc, start = DATE1,end = DATE2 )
# z <-trajLevel(ncJFM,statistic = "frequency",plot=FALSE)%>% 
#   `[[`("data") %>% 
#   select(xgrid,ygrid,count) %>% 
#   rename(nc_count = count)
# 
# 
# 
# maqsBss <- x %>% 
#   full_join(y) %>% 
#   replace_na(list(maqs_count = 0,bss_count = 0)) %>% 
#   mutate(dcount = bss_count - maqs_count)
# 
# ncBss <-  z %>% 
#   full_join(y) %>% 
#   replace_na(list(bss_count = 0, nc_count = 0)) %>% 
#   mutate(dcount = bss_count - nc_count)
# 



# Old ---------------------------------------------------------------------


# I have had to edit the original trajectory_plot_alt from splitr because the 
# trajecotires were not plotting.  This is now trajectory_plot_alt.
# It does work, but there's an issue with wrapping around the globe, so the plot 
# looks a bit funny.  Better to use openair functions from here I think.



# trajectory_plot_alt <- function (x, show_hourly = TRUE, color_scheme = "cycle_hues") 
# {
#   if (inherits(x, "trajectory_model")) {
#     if (!is.null(x$traj_df)) {
#       traj_df <- x$traj_df
#     }
#     else {
#       stop("There is no data available for plotting.")
#     }
#   }
#   if (inherits(x, "data.frame")) {
#     if (all(c("run", "receptor", "hour_along", "traj_dt", 
#               "lat", "lon", "height", "traj_dt_i") %in% colnames(x))) {
#       traj_df <- x
#     } else {
#       stop("This tibble does not contain plottable trajectory data.")
#     }
#   }
#   dt_runs <- traj_df$traj_dt_i %>% unique() %>% length()
#   if (color_scheme == "cycle_hues") {
#     colors <- (scales::hue_pal(c = 90, l = 70))(dt_runs)
#   }else if(color_scheme == "increasingly_gray") {
#     colors <- (scales::grey_pal(0.7, 0.1))(dt_runs)
#   }
#   traj_df$lon[which(traj_df$lon > 0)] <- traj_df$lon[which(traj_df$lon > 
#                                                              0)] - (180 * 2)
#   receptors <- traj_df %>% dplyr::pull(receptor) %>% unique()
#   dates <- traj_df %>% dplyr::pull(traj_dt_i) %>% unique()
#   traj_plot <- leaflet::leaflet() %>% 
#     leaflet::addProviderTiles(provider = "OpenStreetMap", group = "OpenStreetMap") %>% 
#     leaflet::addProviderTiles(provider = "CartoDB.DarkMatter",group = "CartoDB Dark Matter") %>% 
#     leaflet::addProviderTiles(provider = "CartoDB.Positron",group = "CartoDB Positron") %>% 
#     leaflet::addProviderTiles(provider = "Esri.WorldTerrain", group = "ESRI World Terrain") %>% 
#     #leaflet::addProviderTiles(provider = "Stamen.Toner", group = "Stamen Toner") %>% 
#     leaflet::fitBounds(lng1 = min(traj_df[["lon"]]), lat1 = min(traj_df[["lat"]]), lng2 = max(traj_df[["lon"]]), lat2 = max(traj_df[["lat"]])) %>% 
#     leaflet::addLayersControl(baseGroups = c("CartoDB Positron", "CartoDB Dark Matter", "Stamen Toner", "ESRI World Terrain"), overlayGroups = c("trajectory_points", "trajectory_paths"), position = "topright")
# 
#   
#   for (i in seq_along(receptors)) {
#     receptor_i <- receptors[i]
#     for (j in seq_along(dates)) {
#       date_i <- dates[j]
#       wind_traj_ij <- traj_df %>% dplyr::filter(receptor == 
#                                                   receptor_i, traj_dt_i == date_i)
#       popup <- paste0("<strong>trajectory</strong> ", wind_traj_ij[["traj_dt_i"]], 
#                       "<br><strong>at time</strong> ", wind_traj_ij[["traj_dt"]], 
#                       " (", wind_traj_ij[["hour_along"]], " h)<br><strong>height</strong> ", 
#                       wind_traj_ij[["height"]], " <font size=\"1\">m AGL</font> / ", 
#                       "<strong>P</strong> ", wind_traj_ij[["pressure"]], 
#                       " <font size=\"1\">hPa</font>")
#       traj_plot <- traj_plot %>% leaflet::addPolylines(lng = wind_traj_ij[["lon"]], 
#                                                        lat = wind_traj_ij[["lat"]], group = "trajectory_paths", 
#                                                        weight = 2, smoothFactor = 1, color = colors[j]) %>% 
#         leaflet::addCircles(lng = wind_traj_ij[["lon"]], 
#                             lat = wind_traj_ij[["lat"]], group = "trajectory_points", 
#                             radius = 250, fill = TRUE, color = colors[j], 
#                             fillColor = colors[j], popup = popup)
#     }
#   }
#   traj_plot
# }




