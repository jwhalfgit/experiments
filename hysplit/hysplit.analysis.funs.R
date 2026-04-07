# hysplit.analysis.funs.R are custom functions for making various plots and
# various analyses.  The functions are mostly based on openair functions,
# and modifications are detailed.

# Makes a beep.  Useful in a loop or function that takes a while to process
CODEDIR <- "C:/Users/jh2949/OneDrive - University of York/Documents/work/Code/R/experiments/hysplit"
source(file.path(CODEDIR,"hysplit.load.R"))

################################################################################
# PLOTTING FUNCTIONS
################################################################################

# trajLevelCustom modifies the openair Frequency calculation to only count
# individual trajectories once if they cross a given grid cell mulitple times
# (ie, its residence time in a grid cell can only be one time point).  The 
# default frequency calculation calculates relative to the grid cell with the
# highest number of points.
trajLevelCustom <- function (mydata, lon = "lon", lat = "lat", pollutant = "height", 
                             type = "default", smooth = FALSE, statistic = "frequency", plot = FALSE,
                             percentile = 90, map = TRUE, lon.inc = 1, lat.inc = 1, min.bin = 1, 
                             .combine = NA, sigma = 1.5, map.fill = TRUE, map.res = "default", 
                             map.cols = "grey40", map.alpha = 0.3, projection = "lambert", 
                             parameters = c(51, 51), orientation = c(90, 0, 0), grid.col = "deepskyblue", 
                             origin = TRUE, ...){
  plot = FALSE
  hour.inc <- N <- NULL
  vars <- c("date", "lat", "lon", "hour.inc", pollutant, "run")
  statistic <- tolower(statistic)
  if (!is.na(.combine)) {
    vars <- c(vars, .combine)
  }
  mydata <- openair:::checkPrep(mydata, vars, type, remove.calm = FALSE)
  if (statistic == "sqtba" && type != "default") {
    warning("'type' option not available yet with SQTBA", 
            call. = FALSE)
    type <- "default"
  }
  Args <- list()
  current.strip <- lattice:::trellis.par.get("strip.background")
  current.font <- lattice:::trellis.par.get("fontsize")
  on.exit(lattice:::trellis.par.set(fontsize = current.font))
  if (!"ylab" %in% names(Args)) {
    Args$ylab <- ""
  }
  if (!"xlab" %in% names(Args)) {
    Args$xlab <- ""
  }
  if (!"main" %in% names(Args)) {
    Args$main <- ""
  }
  if (!"border" %in% names(Args)) {
    Args$border <- NA
  }
  if ("fontsize" %in% names(Args)) {
    lattice:::trellis.par.set(fontsize = list(text = Args$fontsize))
  }
  if (!"key.header" %in% names(Args)) {
    if (statistic == "frequency") 
      Args$key.header <- "% trajectories"
    if (statistic == "pscf") 
      Args$key.header <- "PSCF \nprobability"
    if (statistic == "sqtba") 
      Args$key.header <- paste0("SQTBA \n", pollutant)
    if (statistic == "sqtba" && !is.na(.combine)) {
      Args$key.header <- paste0("SQTBA \n(Normalised)\n)", 
                                pollutant)
    }
    if (statistic == "difference") {
      Args$key.header <- quickText(paste("gridded differences", 
                                         "\n(", percentile, "th percentile)", sep = ""))
    }
  }
  if (!"key.footer" %in% names(Args)) {
    Args$key.footer <- ""
  }
  if (!"xlim" %in% names(Args)) {
    Args$xlim <- range(mydata$lon)
    Args$xlim <- c(round(quantile(mydata$lon, probs = 0.002)), 
                   round(quantile(mydata$lon, probs = 0.998)))
  }
  if (!"ylim" %in% names(Args)) {
    Args$ylim <- range(mydata$lat)
    Args$ylim <- c(round(quantile(mydata$lat, probs = 0.002)), 
                   round(quantile(mydata$lat, probs = 0.998)))
  }
  trajLims <- c(Args$xlim, Args$ylim)
  Args <- openair:::setTrajLims(mydata, Args, projection, parameters, 
                                orientation)
  Args$trajStat <- statistic
  if (!"method" %in% names(Args)) {
    method <- "traj"
  }else {
    method <- Args$method
    statistic <- "XX"
  }
  origin_xy <- mydata %>% filter(hour.inc == 0) %>% group_by(lat, 
                                                             lon) %>% slice_head(n = 1)
  tmp <- mapproj:::mapproject(x = origin_xy[["lon"]], y = origin_xy[["lat"]], 
                              projection = projection, parameters = parameters, orientation = orientation)
  receptor <- tibble(x = tmp$x, y = tmp$y)
  if (method == "hexbin") {
    tmp <- mapproj:::mapproject(x = mydata[["lon"]], y = mydata[["lat"]], 
                                projection = projection, parameters = parameters, 
                                orientation = orientation)
    mydata[["lon"]] <- tmp$x
    mydata[["lat"]] <- tmp$y
  }
  if (method == "density") 
    stop("Use trajPlot with method = 'density' instead")
  if (is.list(mydata)) 
    mydata <- bind_rows(mydata)
  mydata <- cutData(mydata, type)
  if (method == "traj") {
    mydata$ygrid <- plyr:::round_any(mydata[[lat]], lat.inc)
    mydata$xgrid <- plyr:::round_any(mydata[[lon]], lon.inc)
  }else {
    mydata$ygrid <- mydata[[lat]]
    mydata$xgrid <- mydata[[lon]]
  }
  rhs <- c("xgrid", "ygrid", type)
  rhs <- paste(rhs, collapse = "+")
  if (statistic == "sqtba") {
    mydata <- mydata %>% select(any_of(na.omit(c("date", 
                                                 "lon", "lat", "hour.inc", type, pollutant, .combine))))
  } else {
    mydata <- mydata[, c("date", "xgrid", "ygrid", "hour.inc", "run",
                         type, pollutant)]
    ids <- which(names(mydata) %in% c("xgrid", "ygrid", type))
  }
  vars <- c("xgrid", "ygrid", type)
  if (statistic %in% c("cwt", "median")) {
    mydata <- mydata %>% group_by(across(vars)) %>% summarise(N = length(date), 
                                                              date = head(date, 1), count = mean(.data[[pollutant]], 
                                                                                                 na.rm = TRUE))
    mydata[[pollutant]] <- mydata$count
    id <- which(mydata$N > 20 & mydata$N <= 80)
    mydata[id, pollutant] <- mydata[id, pollutant] * 0.7
    id <- which(mydata$N > 10 & mydata$N <= 20)
    mydata[id, pollutant] <- mydata[id, pollutant] * 0.42
    id <- which(mydata$N <= 10)
    mydata[id, pollutant] <- mydata[id, pollutant] * 0.05
    attr(mydata$date, "tzone") <- "GMT"
    out_data <- dplyr::ungroup(mydata) %>% dplyr::select(-dplyr::any_of(c("date", 
                                                                          "count"))) %>% dplyr::rename(count = "N") %>% dplyr::mutate(statistic = statistic, 
                                                                                                                                      .before = dplyr::everything())
  }
  if (statistic == "frequency" && method != "hexbin") {
    mydata <- mydata %>% distinct(ygrid,xgrid,run,.keep_all = TRUE)
    
    
    mydata <- mydata %>% group_by(across(vars)) %>% summarise(count = length(date), 
                                                              date = head(date, 1))
    mydata[[pollutant]] <- 100 * mydata$count/max(mydata$count) # data should all have same origin
    out_data <- dplyr::ungroup(mydata) %>% dplyr::select(-dplyr::any_of(c("date"))) %>% 
      dplyr::mutate(statistic = statistic, .before = dplyr::everything())
    
    
    
  }
  if (statistic == "pscf") {
    Q90 <- quantile(mydata[[pollutant]], probs = percentile/100, 
                    na.rm = TRUE)
    mydata <- mydata %>% group_by(across(vars)) %>% summarise(N = length(date), 
                                                              date = head(date, 1), count = length(which(.data[[pollutant]] > 
                                                                                                           Q90))/N)
    mydata[[pollutant]] <- mydata$count
    n <- mean(mydata$N)
    id <- which(mydata$N > n & mydata$N <= 2 * n)
    mydata[id, pollutant] <- mydata[id, pollutant] * 0.75
    id <- which(mydata$N > (n/2) & mydata$N <= n)
    mydata[id, pollutant] <- mydata[id, pollutant] * 0.5
    id <- which(mydata$N <= (n/2))
    mydata[id, pollutant] <- mydata[id, pollutant] * 0.15
    out_data <- dplyr::ungroup(mydata) %>% dplyr::select(-dplyr::any_of(c("date", 
                                                                          "count"))) %>% dplyr::rename(count = "N") %>% dplyr::mutate(statistic = statistic, 
                                                                                                                                      percentile = percentile, .before = dplyr::everything())
  }
  if (tolower(statistic) == "sqtba") {
    mydata <- mydata %>% mutate(sigma = sigma * abs(hour.inc)) %>% 
      drop_na({
        {
          pollutant
        }
      })
    r_grid <- expand_grid(lat = seq(round(quantile(mydata$lat, 
                                                   probs = 0.002)), round(quantile(mydata$lat, probs = 0.998)), 
                                    by = lat.inc), lon = seq(round(quantile(mydata$lon, 
                                                                            probs = 0.002)), round(quantile(mydata$lon, probs = 0.998)), 
                                                             by = lon.inc)) %>% as.matrix(.)
    if (is.na(.combine)) {
      mydata <- calc_SQTBA(mydata, r_grid, pollutant, min.bin) %>% 
        rename(`:=`({
          {
            pollutant
          }
        }, SQTBA))
    }
    else {
      mydata <- mydata %>% group_by(across(.combine)) %>% 
        group_modify(~calc_SQTBA(.x, r_grid, pollutant, 
                                 min.bin)) %>% mutate(SQTBA_norm = SQTBA/mean(SQTBA)) %>% 
        group_by(ygrid, xgrid) %>% summarise(SQTBA = mean(SQTBA), 
                                             SQTBA_norm = mean(SQTBA_norm)) %>% ungroup() %>% 
        mutate(SQTBA_norm = SQTBA_norm * mean(SQTBA)) %>% 
        rename(`:=`({
          {
            pollutant
          }
        }, SQTBA_norm))
    }
    names(mydata)[names(mydata) == "n"] <- "count"
    out_data <- dplyr::ungroup(mydata) %>% dplyr::select(-dplyr::any_of(c("lat_rnd", 
                                                                          "lon_rnd", "Q", "Q_c", "SQTBA"))) %>% dplyr::relocate(dplyr::any_of("count"), 
                                                                                                                                .before = pollutant) %>% dplyr::relocate("xgrid", 
                                                                                                                                                                         .before = "ygrid") %>% dplyr::mutate(statistic = statistic, 
                                                                                                                                                                                                              sigma = sigma, combine = .combine, .before = dplyr::everything())
  }
  if (statistic == "difference") {
    base <- mydata %>% group_by(across(vars)) %>% summarise(count = length(date), 
                                                            date = head(date, 1))
    base[[pollutant]] <- 100 * base$count/max(base$count)
    Q90 <- quantile(mydata[[pollutant]], probs = percentile/100, 
                    na.rm = TRUE)
    high <- mydata %>% group_by(across(vars)) %>% summarise(N = length(date), 
                                                            date = head(date, 1), 
                                                            count = length(which(.data[[pollutant]] > 
                                                                                   Q90)))
    high[[pollutant]] <- 100 * high$count/max(high$count)
    mydata <- base
    mydata[[pollutant]] <- high[[pollutant]] - mydata[[pollutant]]
    mydata <- subset(mydata, count >= min.bin)
    out_data <- dplyr::ungroup(mydata) %>% dplyr::select(-dplyr::any_of(c("date"))) %>% 
      dplyr::mutate(statistic = statistic, percentile = percentile, 
                    .before = dplyr::everything())
  }
  lon <- "xgrid"
  lat <- "ygrid"
  scatterPlot.args <- list(mydata, x = lon, y = lat, z = pollutant, 
                           type = type, method = method, smooth = smooth, map = map, 
                           x.inc = lon.inc, y.inc = lat.inc, map.fill = map.fill, 
                           map.res = map.res, map.cols = map.cols, map.alpha = map.alpha, 
                           traj = TRUE, projection = projection, parameters = parameters, 
                           orientation = orientation, grid.col = grid.col, trajLims = trajLims, 
                           receptor = receptor, origin = origin, dist = 0.05, k = 50)
  scatterPlot.args <- openair:::listUpdate(scatterPlot.args, Args)
  scatterPlot.args <- openair:::listUpdate(scatterPlot.args, list(plot = plot))
  plt <- do.call(scatterPlot, scatterPlot.args)
  output <- list(plot = plt$plot, data = if (exists("out_data")) {
    out_data
  } else {
    mydata
  }, call = match.call())
  class(output) <- "openair"
  invisible(output)
}


plotTraj <- function(DF, PARTFILTER = 0.01, DATE1, DATE2, GRIDFILTER = 0){
  SITES = DF
  noTrajFilter <- length(seq(dmy(DATE1),dmy(DATE2),by =0.5)) * PARTFILTER
  
  df1 <- selectByDate(DF, start = DATE1,
                      end = DATE2)
  x <-trajLevel(df1,statistic = "frequency",plot="FALSE") %>% 
    `[[`("data") %>% 
    select(xgrid,ygrid,count) %>% 
    filter(count > noTrajFilter) %>% 
    mutate(ncount = count / (noTrajFilter/PARTFILTER)*100) #%>% 
    #filter(ncount >= quantile(ncount, GRIDFILTER))
  

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



plotTrajBin <- function(DF, DATE1, DATE2, BINS = c(0,1,5,10,25,50,100)){
  SITES = DF
  noTraj <- length(seq(dmy(DATE1),dmy(DATE2),by =0.5))
  
  df1 <- selectByDate(DF, start = DATE1,
                      end = DATE2)
  
  x <-trajLevelCustom(df1,statistic = "frequency",plot="FALSE") %>% 
    `[[`("data") %>% 
    select(xgrid,ygrid,height) %>% 
    #filter(count > noTrajFilter) %>% 
    #mutate(ncount = count / noTraj*100) %>%  # normalized count
    mutate(bins = cut(height, breaks = BINS))
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
                         name = "% of Trajectories", 
                         #trans = log_trans(base =10),
                         #breaks=BREAKS,
                         na.value = "transparent")+#,
    
    theme_bw()+
    geom_point(data = points_lambert, aes(x = st_coordinates(points_lambert)[,1], y = st_coordinates(points_lambert)[,2]), color = "blue", size = 3, shape = 21, fill = "yellow")+
    geom_sf(data = world, fill =NA, color = "black")+
    coord_sf(xlim = st_bbox(bbox_lambert)[c("xmin", "xmax")], ylim = st_bbox(bbox_lambert)[c("ymin", "ymax")], expand = TRUE) +
    #coord_sf(crs = st_crs(3035), xlim = c(500000, 4500000), ylim = c(2000000, 7000000),expand=TRUE)+
    # #coord_sf(crs = st_crs(3035), xlim = c(500000, 4500000), ylim = c(2000000, 7000000),expand=TRUE)+
    labs(title = paste(site1Name),#, DATE1, "-", DATE2), 
         x = "Longitude", y = "Latitude")#+
   # theme(axis.title = element_text(size = 20), plot.title= element_text(size = 20))
  
  
  # ggsave(paste0(PLOTDIR,"/",gsub("-","",dmy(DATE1)),gsub("-","",dmy(DATE2)),
  #               "-",deparse(substitute(DF)),
  #               "-binned.png"), width = 12.80, height = 7.68, units = "in")
  
}



# Comparison of Manc and Newc with Birm -----------------------------------
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
                                     name = "Normalized % Difference", 
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



################################################################################
# SECTOR ANALYSIS
################################################################################
# From the openair book, Sect. 10.4.2
sectorAnal <- function(SITE, DATE1, DATE2){
  site <-  selectByDate(SITE, start = DATE1,
                        end = DATE2)
  
  alloc <- site
  
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
  
  alloc <- group_by(alloc, run,sector) %>%
    mutate(n = n()) %>%
    group_by(run) %>%
    arrange(run, n) %>%
    slice_tail(n = 1) %>%
    mutate(sector = ifelse(n >= 36, sector, "unallocated")) %>%
    select(run, sector, n)
  
  
  pctTraj <- alloc %>% 
    group_by(sector) %>%
    summarise(nTraj=n()) %>% 
    mutate(percentTraj = nTraj / sum(nTraj)  *100)
  
  
  # combine with trajectories
  traj <- left_join(site, alloc, by = "run")
  
  
  group_by(traj, sector) %>%
    summarise(n = n()) %>%
    mutate(percentEndPoiunts = 100 * n / nrow(traj)) %>% 
    left_join(pctTraj)

}




clusterPlots <-function(SITE, DATE1, DATE2){
  site <- selectByDate(SITE, start = DATE1,
                       end = DATE2)
  
  clust <- trajCluster(site,
                       method = "Euclid",
                       n.cluster = 6,
                       col = "Set2",
                       map.cols = openColours("Paired", 10))
  
  trajPlot(clust$data$traj, group = "cluster", col = "Set2")
  
  
  
  
}


# I want sourceBoxes to identify whether a lat/lon
# resides in a box that contains
# Northern Europe
# Mainland Europe
# Spain?
# North Atlantic
# Atlantic
# Iceland
# Greenland
# Africa

# The OUTPUT argument should be either "TS" or "BOXES"

sourceBoxes <- function(SITE, DATE1, DATE2, HOURS = 0:72, PCT= TRUE,
                        OUTPUT = "TS", PLOT = TRUE){
  site <- selectByDate(SITE, start = DATE1,
                       end = DATE2)
  
  site <- site %>% 
    filter(hour.inc %in% (-1*HOURS))
  
  
  boxes <- rep(0,12)
  lon <- site$lon
  lat <- site$lat
  loc <- vector()
  
  for(ix in 1:length(lon)){
    # Make sure we're in the right hemisphere...
    if(lon[ix] >= -100 & lon[ix] <= 51.5 & lat[ix] >= 0 & lat[ix] <= 80){
      # MARINE ATLANTIC
      if(lon[ix] >= -50 & lon[ix] < -10 & lat[ix] >= 35 & lat[ix] <= 59){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -75 & lon[ix] < -20 & lat[ix] >= 20 & lat[ix] <= 38){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -55 & lon[ix] < -54 & lat[ix] >= 55 & lat[ix] <= 71){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      # }else if(lon[ix] >= -75 & lon[ix] < -71 & lat[ix] >= 25 & lat[ix] <= 43){
      #   boxes[1] <- boxes[1] + 1
      #   loc[ix] <- 1
      }else if(lon[ix] >= -68 & lon[ix] < -30 & lat[ix] >= 13 & lat[ix] <= 43){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -53 & lon[ix] < -30 & lat[ix] >= 43 & lat[ix] <= 59){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -55 & lon[ix] < -17 & lat[ix] >= 55 & lat[ix] <= 59){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -50 & lon[ix] < -17 & lat[ix] >= 0 & lat[ix] <= 59){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -55 & lon[ix] < -17 & lat[ix] >= 5 & lat[ix] <= 20){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -60 & lon[ix] < -30 & lat[ix] >= 10 & lat[ix] <= 20){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -60 & lon[ix] < -55 & lat[ix] >= 55 & lat[ix] <= 71){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      # }else if(lon[ix] >= -65 & lon[ix] < -60 & lat[ix] >= 60 & lat[ix] <= 71){
      #   boxes[1] <- boxes[1] + 1
      #   loc[ix] <- 1
      }else if(lon[ix] >= -27 & lon[ix] < -10 & lat[ix] >= 29 & lat[ix] <= 41){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -20 & lon[ix] < -15 & lat[ix] >= 30 & lat[ix] <= 59){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -10 & lon[ix] < -5 & lat[ix] >= 45 & lat[ix] <= 51){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -23 & lon[ix] <  -7.5 & lat[ix] >= 55 & lat[ix] <= 59){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
        
        # GREENLAND  
      }else if(lon[ix] >= -73 & lon[ix] < -18 & lat[ix] >= 76 & lat[ix] <= 79){
        boxes[5] <- boxes[5] + 1
        loc[ix] <- 5
      }else if(lon[ix] >= -54 & lon[ix] < -40 & lat[ix] >= 59 & lat[ix] <= 66){
        boxes[5] <- boxes[5] + 1
        loc[ix] <- 5
      }else if(lon[ix] >= -56 & lon[ix] < -35 & lat[ix] >= 65 & lat[ix] <= 73){
        boxes[5] <- boxes[5] + 1
        loc[ix] <- 5
      }else if(lon[ix] >= -57 & lon[ix] < -20 & lat[ix] >= 68 & lat[ix] <= 74){
        boxes[5] <- boxes[5] + 1
        loc[ix] <- 5
      }else if(lon[ix] >= -40 & lon[ix] < -32 & lat[ix] >= 65 & lat[ix] <= 78){
        boxes[5] <- boxes[5] + 1
        loc[ix] <- 5
      }else if(lon[ix] >= -61 & lon[ix] < -17 & lat[ix] >= 74 & lat[ix] <= 76){
        boxes[5] <- boxes[5] + 1
        loc[ix] <- 5
      }else if(lon[ix] >= -68 & lon[ix] < -14 & lat[ix] >= 79 & lat[ix] <= 81){
        boxes[5] <- boxes[5] + 1
        loc[ix] <- 5
      }else if(lon[ix] >= -64 & lon[ix] < -10 & lat[ix] >= 81 & lat[ix] <= 84){
        boxes[5] <- boxes[5] + 1
        loc[ix] <- 5
        
      # AMERICAS
      }else if(lon[ix] >= -100 & lon[ix] < -80 & lat[ix] >= 0 & lat[ix] <= 80){
        boxes[2] <- boxes[2] + 1
        loc[ix] <- 2
      }else if(lon[ix] >= -80 & lon[ix] < -65 & lat[ix] >= 40 & lat[ix] <= 80){
        boxes[2] <- boxes[2] + 1
        loc[ix] <- 2
      }else if(lon[ix] >= -65 & lon[ix] < -60 & lat[ix] >= 40 & lat[ix] <= 80){
        boxes[2] <- boxes[2] + 1
        loc[ix] <- 2
      }else if(lon[ix] >= -60 & lon[ix] < -53 & lat[ix] >= 45 & lat[ix] <= 80){
        boxes[2] <- boxes[2] + 1
        loc[ix] <- 2
      }else if(lon[ix] >= -65 & lon[ix] < -60 & lat[ix] >= 65 & lat[ix] <= 80){
        boxes[2] <- boxes[2] + 1
        loc[ix] <- 2
      }else if(lon[ix] >= -80 & lon[ix] < -75 & lat[ix] >= 70 & lat[ix] <= 80){
        boxes[2] <- boxes[2] + 1
        loc[ix] <- 2
      }else if(lon[ix] >= -80 & lon[ix] < -75 & lat[ix] >= 30 & lat[ix] <= 80){
        boxes[2] <- boxes[2] + 1
        loc[ix] <- 2
      }else if(lon[ix] >= -80 & lon[ix] < -75 & lat[ix] >= 20 & lat[ix] <= 80){
        boxes[2] <- boxes[2] + 1
        loc[ix] <- 2
      }else if(lon[ix] >= -80 & lon[ix] < -70 & lat[ix] >= 15 & lat[ix] <= 80){
        boxes[2] <- boxes[2] + 1
        loc[ix] <- 2
      }else if(lon[ix] >= -80 & lon[ix] < -60 & lat[ix] >= 10 & lat[ix] <= 80){
        boxes[2] <- boxes[2] + 1
        loc[ix] <- 2
      }else if(lon[ix] >= -80 & lon[ix] < -53 & lat[ix] >= 40 & lat[ix] <= 80){
        boxes[2] <- boxes[2] + 1
        loc[ix] <- 2
      }else if(lon[ix] >= -80 & lon[ix] < -53 & lat[ix] >= 0 & lat[ix] <= 55){
        boxes[2] <- boxes[2] + 1
        loc[ix] <- 2
        
        # ICELAND  
      }else if(lon[ix] >= -25 & lon[ix] < -13 & lat[ix] >= 63 & lat[ix] <= 67){
        boxes[6] <- boxes[6] + 1
        loc[ix] <- 6
        
        
        # North Atlantic
      }else if(lon[ix] >= -20 & lon[ix] < 55 & lat[ix] >= 71 & lat[ix] <= 85){
        boxes[7] <- boxes[7] + 1
        loc[ix] <- 7
      }else if(lon[ix] >= -20 & lon[ix] < 10 & lat[ix] >= 64 & lat[ix] <= 71){
        boxes[7] <- boxes[7] + 1
        loc[ix] <- 7
      }else if(lon[ix] >= -13 & lon[ix] < 5 & lat[ix] >= 60 & lat[ix] <= 71){
        boxes[7] <- boxes[7] + 1
        loc[ix] <- 7
      }else if(lon[ix] >= -41 & lon[ix] < -25 & lat[ix] >= 59 & lat[ix] <= 65){
        boxes[7] <- boxes[7] + 1
        loc[ix] <- 7
      }else if(lon[ix] >= -41 & lon[ix] < 4 & lat[ix] >= 59 & lat[ix] <= 63){
        boxes[7] <- boxes[7] + 1
        loc[ix] <- 7
      }else if(lon[ix] >= -25 & lon[ix] < 1 & lat[ix] >= 67 & lat[ix] <= 69){
        boxes[7] <- boxes[7] + 1
        loc[ix] <- 7
        # }else if(lon[ix] >= -21 & lon[ix] < 1 & lat[ix] >= 67 & lat[ix] <= 69){
        #   boxes[7] <- boxes[7] + 1
        #   loc[ix] <- 7
        # }else if(lon[ix] >= -34 & lon[ix] < -25 & lat[ix] >= 67 & lat[ix] <= 75){
        #   boxes[7] <- boxes[7] + 1
        #   loc[ix] <- 7
      }else if(lon[ix] >= -34 & lon[ix] < -25 & lat[ix] >= 65 & lat[ix] <= 68){
        boxes[7] <- boxes[7] + 1
        loc[ix] <- 7

        # UK
      }else if(lon[ix] >= -10 & lon[ix] < -5 & lat[ix] >= 51 & lat[ix] <= 55){
        boxes[9] <- boxes[9] + 1
        loc[ix] <- 9
      }else if(lon[ix] >= -8 & lon[ix] < -1 & lat[ix] >= 50 & lat[ix] <= 59){
        boxes[9] <- boxes[9] + 1
        loc[ix] <- 9
      }else if(lon[ix] >= -3 & lon[ix] < 2 & lat[ix] >= 51 & lat[ix] <= 54){
        boxes[9] <- boxes[9] + 1
        loc[ix] <- 9
        
        
        # North Sea
      }else if(lon[ix] >= -1 & lon[ix] < 8 & lat[ix] >= 55 & lat[ix] <= 58){
        boxes[8] <- boxes[8] + 1
        loc[ix] <- 8
      }else if(lon[ix] >= -1 & lon[ix] < 8 & lat[ix] >= 54 & lat[ix] <= 55){
        boxes[8] <- boxes[8] + 1
        loc[ix] <- 8
      }else if(lon[ix] >= -1 & lon[ix] < 5 & lat[ix] >= 51 & lat[ix] <= 59){
        boxes[8] <- boxes[8] + 1
        loc[ix] <- 8
      

      # Northern Europe
      }else if(lon[ix] >= 4 & lon[ix] < 20 & lat[ix] >= 55 & lat[ix] <= 64){
        boxes[10] <- boxes[10] + 1
        loc[ix] <- 10
      }else  if(lon[ix] >= 5 & lon[ix] < 55 & lat[ix] >= 60 & lat[ix] <= 71){
        boxes[10] <- boxes[10] + 1
        loc[ix] <- 10
        
        
        # EUROPE
      }else if(lon[ix] >= 10 & lon[ix] < 55 & lat[ix] >= 45 & lat[ix] <= 55){
        boxes[4] <- boxes[4] + 1
        loc[ix] <- 4
      }else  if(lon[ix] >= 4 & lon[ix] < 55 & lat[ix] >= 50 & lat[ix] <= 55){
        boxes[4] <- boxes[4] + 1
        loc[ix] <- 4
      }else if(lon[ix] >= 4 & lon[ix] < 55 & lat[ix] >= 45 & lat[ix] <= 55){
        boxes[4] <- boxes[4] + 1
        loc[ix] <- 4
      }else  if(lon[ix] >= -10 & lon[ix] < 7 & lat[ix] >= 45 & lat[ix] <= 51){
        boxes[4] <- boxes[4] + 1
        loc[ix] <- 4
      }else  if(lon[ix] >= 20 & lon[ix] < 55 & lat[ix] >= 55 & lat[ix] <= 60){
        boxes[4] <- boxes[4] + 1
        loc[ix] <- 4
        
        
        # AFRICA
      }else if(lon[ix] >= -15 & lon[ix] < -5 & lat[ix] >= 30 & lat[ix] <= 37){
        boxes[3] <- boxes[3] + 1
        loc[ix] <- 3
      }else if(lon[ix] >= -20 & lon[ix] < -10 & lat[ix] >= 25 & lat[ix] <= 37){
        boxes[3] <- boxes[3] + 1
        loc[ix] <- 3
      }else if(lon[ix] >= -20 & lon[ix] < -15 & lat[ix] >= 10 & lat[ix] <= 37){
        boxes[3] <- boxes[3] + 1
        loc[ix] <- 3
      }else if(lon[ix] >= -15 & lon[ix] < -10 & lat[ix] >= 5 & lat[ix] <= 37){
        boxes[3] <- boxes[3] + 1
        loc[ix] <- 3
      }else if(lon[ix] >= -10 & lon[ix] < 10 & lat[ix] >= 0 & lat[ix] <= 37){
        boxes[3] <- boxes[3] + 1
        loc[ix] <- 3
      }else if(lon[ix] >= -5 & lon[ix] <= 55 & lat[ix] >= 5 & lat[ix] <= 37){
        boxes[3] <- boxes[3] + 1
        loc[ix] <- 3
      }else if(lon[ix] >= 10 & lon[ix] <= 55 & lat[ix] >= 0 & lat[ix] <= 37){
        boxes[3] <- boxes[3] + 1
        loc[ix] <- 3
      }else if(lon[ix] >= -10 & lon[ix] <= -5 & lat[ix] >= 5 & lat[ix] <= 37){
        boxes[3] <- boxes[3] + 1
        loc[ix] <- 3
      }else if(lon[ix] >= -15 & lon[ix] <= -10 & lat[ix] >= 10 & lat[ix] <= 37){
        boxes[3] <- boxes[3] + 1
        loc[ix] <- 3
        
        
      # Southern Euroope    
      }else if(lon[ix] >= -10 & lon[ix] < 55 & lat[ix] > 35 & lat[ix] <= 45){
        boxes[11] <- boxes[11] + 1
        loc[ix] <- 11
      }else{
        boxes[12] <- boxes[12]+1
        loc[ix] <- 12
      }
    }else{
      boxes[12] <- boxes[12]+1
      loc[ix] <- 12
    }
  }
  
  names(boxes) <- c("Atlantic", "Americas","Africa","Europe",
                    "Greenland", "Iceland","N Atlantic","North Sea",
                    "UK", "N Europe", "S Europe","uncategorized")

  # add locations to site data frame
  siteOut <- site %>% 
    mutate(region = loc) 
  
  if(OUTPUT == "BOXES" & PCT == TRUE){
    siteOut <- siteOut %>%
      mutate(region = factor(loc, levels = 1:12)) %>% 
      filter(hour.inc!=0) %>% 
      group_by(date) %>% 
      filter(n() >=70) %>% 
      select(date,region) %>% 
      count(region) %>% 
      pivot_wider(names_from=region, names_prefix = "loc_",values_from=n,
                  values_fill = 0,names_expand= TRUE) %>% 
      mutate(across(contains("loc"),function(x){x / 72}))
    return(siteOut)
    
  }else if(OUTPUT == "BOXES" & PCT == FALSE){
    siteOut <- siteOut %>%
      mutate(region = factor(loc, levels = 1:12)) %>% 
      filter(hour.inc!=0) %>% 
      group_by(date) %>% 
      filter(n() >=70) %>% 
      select(date,region) %>% 
      count(region) %>% 
      pivot_wider(names_from=region, names_prefix = "loc_",values_from=n,
                  values_fill = 0,names_expand= TRUE)
      return(siteOut)
  }
  
  
  siteName <- sites$loc[which(sites$lat==site$lat[1]&sites$lon == site$lon[1])]
  
  
  if(OUTPUT == "TS" & PLOT == FALSE){
    siteOut <- siteOut %>% 
      filter(hour.inc!=0) %>% 
      group_by(date) %>%
      select(date,run,region,hour.inc) %>% 
      group_by(run) %>% 
      filter(n() >=70) %>% 
      mutate(region = as.factor(region))
    
      
      # If you want percentages, uncomment this code:
      
      # mutate(count = length(region)) %>%
      # count(region) %>% 
      # mutate(pct = n / sum(n) * 100) %>% 

      # pivot_wider(id_cols = date, names_from = region,
      #             values_from = , names_prefix = "loc_",
      #             values_fill = 0) %>%

      
      # if_else("loc_2" %in% names(.),
      #         mutate(loc_2 = loc_2), 
      #         mutate(loc_2 =  0)) %>% 
      #ungroup() %>% 
      # mutate(loc_2 = ifelse("loc_2" %in% names(.),
      #                        loc_2,
      #                         0),
      #        loc_3 = ifelse("loc_3" %in% names(.),
      #                       loc_3,
      #                       0),
      #        loc_5 = ifelse("loc_5" %in% names(.),
      #                       loc_5,
      #                       0))


    
     
    
    
    return(siteOut)
    
  }else if(OUTPUT == "TS" & PLOT == TRUE){
    siteOut <- siteOut %>% 
      select(date,run,region,hour.inc) %>% 
        group_by(run) %>% 
        filter(n() >=70) %>% 
        mutate(region = as.factor(region))
    
    sitePlot <- ggplot(data = siteOut, aes(fill = region, x = date, y = hour.inc*1))+
      geom_raster()+
      scale_fill_manual(values = c("#72D9FF", # Marine Atlantic
                                   "#FF6F00", # N America
                                   "#D7B5A6", # Africa
                                   "#42B540",#Europe
                                   "azure2", #Greenland
                                   "azure4", #Iceland
                                   "#0072b2", #N Atlantic
                                   "#264DFF", # N Sea
                                   "#9D7660", # UK
                                   "springgreen4", #N Europe
                                   "#C5E1A5", # S Europe
                                   "grey20"),
                        breaks = 1:12,
                        labels = c("Atlantic", "Americas", "Africa", "Europe",
                                   "Greenland","Iceland","N Atlantic","North Sea","UK", 
                                   "N Europe","S Europe","uncategorized")) + #uncategorized
      labs(x = "Date (UTC)", y = "Hours Backward", fill = "Region",
           title = paste0(siteName," ", DATE1," thru ", DATE2, ", Hours ", HOURS[1],"-",
                          HOURS[length(HOURS)]))+
      theme_minimal()+
      theme(panel.grid.major.y = element_line(color = "darkgray",
                                                    size = 0.5,
                                                    linetype = 2)
            
            
      )
    return(sitePlot)
    
  # }else if(OUTPUT == "BOXES"){
  #     return(boxes)
  }else{
    return(print("OUTPUT not defined! Please define as either 'TS' or 'BOXES'"))
  }
  
  
  
}

region.names <- c("Atlantic", "Americas", "Africa", "Europe",
                  "Greenland","Iceland","N Atlantic","North Sea","UK", 
                  "N Europe","S Europe","uncategorized")
# plotEq will be a function to overlay the equivalence time series measurements
# with my stacked bar / raster plot.  Because ggplot2 is terrible, I'm sure this
# code will be a mess.

# EQUIVDF is referencing the data frames for which we have Reference / Candidate method
# PM data (notated as siteEq (e.g., maqsEq)) in hysplit.analysis.R
# SITE is referencing the hysplit dataframes (e.g., maqs)

# If you leave DATE1 and DATE2 as NULL, the function will automatically pull the
# date range that is present in EQUIVDF (ie, the timespan for which we have)
# PM2.5 measurements.
plotEq <- function(EQUIVDF, SITE,HOURS = 0:72,DATE1 = NULL,DATE2 = NULL, PLOT = TRUE){
  
  if(is.null(DATE1) & is.null(DATE2)){
    DATE1 = EQUIVDF$ts[1]
    DATE2 = EQUIVDF$ts[nrow(EQUIVDF)]
  }else{
    DATE1 = ymd(DATE1)
    DATE2 = ymd(DATE2)
  }
  
  site <- SITE %>% 
    filter(between(date, DATE1, DATE2))
  
  
  equivDF <- EQUIVDF %>% 
    filter(between(ts, DATE1, DATE2))
  
  # Generic eqn to rescale data:
  # ((x - old_min) * (new_max - new_min)) / (old_max - old_min) + new_min
  
  equivDFMorph <- equivDF %>% 
    #mutate(deltaMorph = (delta-min(delta))*(max(-1*HOURS)-min(-1*HOURS))/(max(delta)-min(delta))+ min(HOURS*-1))
    mutate(deltaMorph = (deltadelta-min(deltadelta))*(max(-1*HOURS)-min(-1*HOURS))/(max(deltadelta)-min(deltadelta))+ min(HOURS*-1))
  
  
  outPlot <- sourceBoxes(SITE,DATE1,DATE2,HOURS)+
    geom_line(data= equivDFMorph, aes(x = ts, y = deltaMorph),
              inherit.aes = FALSE, lwd = 1.25, color = "red")+
    scale_y_continuous(limits = c(max(HOURS)*-1,min(HOURS)*-1),
                       sec.axis = sec_axis(transform=(~(.-(max(HOURS)*-1))*(max(EQUIVDF$deltadelta)-min(EQUIVDF$deltadelta))/((min(HOURS)*-1)-(max(HOURS)*-1))+ min(EQUIVDF$deltadelta)),name = "Ref-Candidate (ug/m3)"))+
                       
    theme_minimal()+
    theme(panel.grid.major.y = element_line(color = "red",
                                            size = 0.5,
                                            linetype = 2))
  
  
  
  ggsave(paste0("G:/My Drive/Experiments/DEFRA/hysplit/sourceregions/",
                gsub("-","",DATE1),gsub("-","",DATE2),"-",HOURS[1],"-",
                HOURS[length(HOURS)],deparse(substitute(EQUIVDF)),"-ts.png"),
         width= 19.20, height = 10.8,
         units = "in", bg = "white")
  
  if(PLOT == TRUE){
    return(outPlot)
  }else{
    outDF <- sourceBoxes(SITE,DATE1,DATE2, HOURS, PLOT = FALSE) %>% 
      select(date,region) %>% 
      group_by(date,region) %>% 
      tally()
      
  }
  
}


# eventLocator will take a merged data frame containing equivalence and
# acsm data and identify the anomalous events above 1 SD above the mean.

eventLocator <- function(DFMERGE, SIGN = "POS", min.rows = 5){
  if(SIGN %in% c("POS", "POSITIVE", "pos", "positive", "+")){
    sign = "pos"
  }else if(SIGN %in% c("NEG","NEGATIVE", "neg", "negative", "-")){
    sign = "neg"
  }else{
    return(print("Error - SIGN is not recognized. Check acceptable arguments"))
  }
  
  mean_anomaly <- mean(DFMERGE$deltadelta)
  sd_anomaly <- sd(DFMERGE$deltadelta)
  
  if(sign == "pos"){
    threshold <- mean_anomaly + 1 * sd_anomaly
    outside_threshold <- DFMERGE$deltadelta > threshold
  }else if(sign == "neg"){
    threshold <- mean_anomaly + -1 * sd_anomaly
    outside_threshold <- DFMERGE$deltadelta < threshold
  }
  
  # finds number of consistent values above/below threshold.
  rle_info <- rle(outside_threshold)
  
  
  
  
  event_details <- list()
  current_index <- 1
  event <- list()
  
  # this section will isolate the events
  for (ix in seq_along(rle_info$lengths)) {
    if (rle_info$values[ix] == TRUE) {
      # This is a core event segment
      core_start_ix <- current_index
      core_end_ix <- current_index + rle_info$lengths[ix] - 1
      
      # Expand backwards (preceding rise)
      # Go back as long as the data is above the overall mean, or until the start
      event_start_ix <- core_start_ix
      if(sign == "pos"){
        while (event_start_ix > 1 && DFMERGE$deltadelta[event_start_ix - 1] > mean_anomaly) {
          event_start_ix <- event_start_ix - 1
        }
      }else if(sign =="neg"){
        while (event_start_ix > 1 && DFMERGE$deltadelta[event_start_ix - 1] < mean_anomaly) {
          event_start_ix <- event_start_ix - 1
        }
      }
      # Expand forwards (following fall)
      # Go forward as long as the data is above the overall mean, or until the end
      event_end_ix <- core_end_ix
      if(sign == "pos"){
        while (event_end_ix < length(DFMERGE$deltadelta) && DFMERGE$deltadelta[event_end_ix + 1] > mean_anomaly) {
          event_end_ix <- event_end_ix + 1
        }
      }else if(sign == "neg"){
        while (event_end_ix < length(DFMERGE$deltadelta) && DFMERGE$deltadelta[event_end_ix + 1] < mean_anomaly) {
          event_end_ix <- event_end_ix + 1
        }
      }
    event[[length(event_details) + 1]] <- DFMERGE[event_start_ix:event_end_ix,]  
      
      
      # Store the event details
      event_details[[length(event_details) + 1]] <- list(
        event_number = length(event_details) + 1,
        startix = event_start_ix,
        endix = event_end_ix,
        #core_start_index = core_start_ix, # For visualization/debugging
        #core_end_index = core_end_ix,     # For visualization/debugging
        tsStart = DFMERGE$datetime[event_start_ix],
        tsEnd = DFMERGE$datetime[event_end_ix],
        event_duration = event_end_ix - event_start_ix + 1
        #peak_value = max(anomaly_data[event_start_ix:event_end_ix]),
        #peak_time_ix = which.max(anomaly_data[event_start_ix:event_end_ix]) + event_start_ix - 1
      )
    }
    current_index <- current_index + rle_info$lengths[ix]
  }
  
  event[which(sapply(event, nrow) < min.rows)] = NULL
  
  outDF <- do.call(rbind,event)
  
  return(outDF)
  
}


comp.regression <- function(ACSM, EQDATA,DATES = dateMat){
  plotList <- list() # for output plot
  statsList <- list() # for R2
  outList <- list() # for model statistics
  outVifList <- list() # for VIF calculations
  
  
  
  for(ix in 1:nrow(DATES)){
    
    ACSMfiltered <- ACSM %>% 
      filter(between(datetime, ymd(DATES[ix,1]), ymd(DATES[ix,2])))
    
    
    EQDATAfiltered <- EQDATA %>% 
      filter(between(date, ymd(DATES[ix,1]), ymd(DATES[ix,2])))
    
    #VARS = names(ACSMfiltered)[names(ACSMfiltered)%in%c("cl","nh4","no3","so4","organic")]
    VARS = names(ACSMfiltered)[names(ACSMfiltered)%in%c("cl","no3","so4","organic")]
    dfMerge <- ACSMfiltered %>% 
      left_join(EQDATAfiltered, by = join_by(datetime == date)) %>% 
      #select(!"nh4") %>% 
      na.omit() %>% 
      mutate(massSum = rowSums(pick(all_of(VARS)))) %>% 
      mutate(across(all_of(VARS),
                    ~.x / massSum,
                    .names = "{.col}Pct"
                    ))
    
    nzvStats <- nearZeroVar(dfMerge, saveMetrics = TRUE)
    print(nzvStats)
    nonZeroVars <- rownames(nzvStats[nzvStats$nzv == FALSE, ])
    print(nonZeroVars)
    
    nonZeroVars <- nonZeroVars[which(nonZeroVars %in% c("cl", 
                                                        #"nh4",
                                                        "no3",
                                                        "so4",
                                                        "organic"))] 
    
    attach(dfMerge)
    regEq <- paste0("deltadelta~",paste(nonZeroVars,collapse = "+"))
    regMod <- lm(regEq)
    detach(dfMerge)
    
    tidy_out <- broom::tidy(regMod, conf.int = TRUE) %>% 
      filter(term != "(Intercept)")
    
    # 1. Vertical Logic: Check for headroom
    # Find the highest point (estimate + error)
    peak_val <- max(tidy_out$estimate + tidy_out$std.error, na.rm = TRUE)
    min_val <- min(tidy_out$estimate - tidy_out$std.error, na.rm = TRUE)
    
    biggest_val <- ifelse(peak_val > abs(min_val), peak_val, min_val)
    # If bars are tall (e.g., > 0.08 on your scale), move box to bottom
    if(peak_val > 0.5) {
      y_val <- -Inf
      v_val <- -0.5 # Hangs it just above the x-axis labels
    } else {
      y_val <- Inf
      v_val <- 1.5  # Hangs it just below the top ceiling
    }
    
    # 2. Horizontal Logic: Find the "shorter" side of the group
    num_bars <- nrow(tidy_out)
    mid <- ceiling(num_bars / 2)
    max_left  <- max(tidy_out$estimate[1:mid] + tidy_out$std.error[1:mid], na.rm = TRUE)
    max_right <- max(tidy_out$estimate[(mid+1):num_bars] + tidy_out$std.error[(mid+1):num_bars], na.rm = TRUE)
    
    # If left is shorter, nudge left (-0.3) and right-align (1). Vice versa.
    nx_val <- ifelse(max_left < max_right, 0.01, -0.01)   
    hj_val <- ifelse(max_left < max_right, 1, 0)
    
    
    modSummary <- summary(regMod)
    outVif <- car::vif(regMod)
    outVifList[[ix]] <- unlist(outVif)
    
    print(outVif)
  
    outList[[ix]] <- broom::tidy(regMod,conf.int = TRUE)
    
    
    statsList[[ix]] <- broom::glance(regMod) %>%
      select(r.squared, p.value) %>%
      mutate(p_label = ifelse(p.value < 0.001,
                              "p < 0.001",
                              paste0("p = ", round(p.value, 3))),
             combined_label = paste0("R² = ", round(r.squared, 2), "\n", p_label),
             model = as.character(ix),
             y_pos = y_val,
             v_just = v_val,
             nudge_x_val = nx_val,
             h_just = hj_val ) %>%  # Match the ID used in bind_rows
      select(model, combined_label, 
             y_pos, 
             v_just, 
             nudge_x_val,
             h_just)
  }
  
  outputCombined <- bind_rows(outList, .id = "model") %>%
    filter(term != "(Intercept)")
  
  statsCombined <- bind_rows(statsList)
  
  outVifCombined <- bind_rows(outVifList, .id = "model")
  # define some useful plotting constants:
  num_terms <- length(unique(outputCombined$model))
  if(num_terms > 1){#
    sep_lines <- seq(1.5, num_terms - 0.5, by = 1)
  }else{
    sep_lines <- 1.5
  }
  
  compCols <- c(
    "cl"    = "#ff80b3", 
    "nh4"   = "#E69F00", 
    "no3"       = "#0072B2", 
    "organic"   = "#228b22", 
    "so4" = "#aa0000" 
  )
  
  master_labels <- c("cl" = "Chloride", "nh4" = "Ammonia", 
                     "no3" = "Nitrate", "organic" = "Organic",
                     "so4" = "Sulfate")
  
  
  
  plotOut <- ggplot(data = outputCombined,
                    aes(x =as.factor(model),
                         y = estimate,
                        fill = term, color = term))+
    
      geom_bar(stat = "identity", 
               position = position_dodge(width = 0.9),
               alpha = 0.5)+
      
      #ylim(c(-0.2,0.2))+
      #coord_cartesian(ylim = c(-1, 1.2)) + # prevents removal of info if offscale
      #coord_cartesian(ylim = c(-0.15, 0.2)) + # prevents removal of info if offscale
      geom_text(aes(y = estimate + ifelse(estimate >= 0, std.error + 0.5, -std.error - 0.5),
                    label = paste0("p=", round(p.value, 3))), 
                size = 3.5, 
                position = position_dodge(width = 0.9),
                show.legend = FALSE) +
      geom_vline(xintercept = sep_lines, 
                 color = "gray50", 
                 linetype = "dashed") +
      scale_fill_manual(name = "Species",
                        values = compCols,
                        labels = master_labels
      ) +
      scale_color_manual(name = "Species",
                         values = compCols,
                         labels = master_labels
      ) +
      geom_errorbar(aes(ymin = estimate - std.error, 
                        ymax = estimate + std.error), 
                    width = 0.2,
                    position = position_dodge(width = 0.9),
                    show.legend = FALSE)+
      # Label for R2
    geom_label(data = statsCombined, 
               aes(x = as.numeric(as.factor(model)) + nudge_x_val, 
                   y = y_pos, 
                   label = combined_label,
                   vjust = v_just,
                   #hjust = h_just),
                   hjust = 0.5),
               fill = "white", 
               color = "black", 
               fontface = "bold",
               lineheight = 0.9, 
               size = 4, 
               alpha = 0.9,       # Slight transparency in case of close calls
               label.size = 0.2,   # Cleaner border
               show.legend = FALSE) +
    
    # Use coord_cartesian to ensure the Inf/-Inf labels don't get cut off
    coord_cartesian(clip = "off") +
      
      theme_minimal()+
      labs(x = "Region", y = expression("Predictor (ug m" ^-3* ")"))+
      # fill = "Region",color= "Region")+
      # labs(x = "Region", y = expression("Predictor (ug m" ^-3* ")"),
      #      title = paste0(DATES[ix,1],"/", DATES[ix,2]), fill = "Region")+
      theme(legend.position = "bottom", text = element_text(size = 22))+
      #guides(fill = guide_legend(override.aes = list(alpha = 1, color = NA)))+
      scale_x_discrete(#guide = guide_axis(n.dodge = 3),
      labels = c("JJA", "SON", "DJF", "MAM", "All"),
                        breaks = c("1", "2", "3", "4","5"))+
      #  labels = c("All"))+
      scale_y_continuous(n.breaks = 10,limits = c(-12.5,5))
  
  print(plotOut)
  
  ggsave(paste0("G:/My Drive/Experiments/DEFRA/hysplit/seasonal_mvr/composition",
                deparse(substitute(EQDATA)),"-nh4.png"),plot = plotOut,
         width = 12.8, height = 12.8, units = "in")
  
  return(list(outputCombined, statsCombined, outVifCombined))
  #return(plotOut)
}

# 
grubbs.rowwise <- function(DF){
  
  # Use apply(MARGIN=1, ...) to apply a function over the rows (MARGIN = 1)
  # The function is an anonymous function that runs grubbs.test on the row data.
  # We use simplify = FALSE to keep the output as a list for easier access to results.
  results_list <- apply(X = DF, 
                        MARGIN = 1, 
                        FUN = function(row_vector) {
                          # Convert row_vector to numeric if it's not already
                          row_vector <- as.numeric(row_vector) 
                          
                          # You might need a check here if the number of values is too small
                          # The test requires at least 3 observations.
                          if (length(row_vector) < 3) {
                            return(NA) 
                          }
                          
                          tryCatch({
                            test_result_high <- grubbs.test(row_vector, 
                                                            type = 10)
                            test_result_low <- grubbs.test(row_vector, 
                                                           type = 10, 
                                                           opposite = TRUE)
                            
                            # Return just the p-value and outlier value for simplicity
                            return(list(
                              p_high = test_result_high$p.value,
                              outlier_high = test_result_high$statistic,
                              
                              p_low = test_result_low$p.value,
                              outlier_low = test_result_low$statistic
                              
                              
                            ))
                          }, error = function(e) {
                            return(NA) # Return NA if the test fails for any reason
                          })
                        },
                        simplify = FALSE)
  
  # Bind the results into a single data frame
  return(
    data.frame(
      p_high = sapply(results_list, function(x) ifelse(is.list(x), x$p_high, NA)),
      grubbs_high = sapply(results_list, function(x) ifelse(is.list(x), x$outlier_high, NA)),
      p_low = sapply(results_list, function(x) ifelse(is.list(x), x$p_low, NA)),
      grubbs_low = sapply(results_list, function(x) ifelse(is.list(x), x$outlier_low, NA))
    )
  )
}


#eqMVRLoc

# DATES needs to be a 2 column matrix
eqMVRLoc <- function(locEq, locBox, DATES = dateMat,TAG){
  plotList <- list() # for output plot
  statsList <- list() # for R2
  outList <- list() # for model statistics
  outVifList <- list() # for VIF calculations
  
  for(ix in 1:nrow(DATES)){
    locSurf <- sourceBoxes(locBox,DATE1 = DATES[ix,1], DATE2 = DATES[ix,2],
                           PLOT = FALSE, PCT = FALSE, OUTPUT = "BOXES") %>% 
      filter(hour(date) == 12) %>%  # I think noon will be more appropriate than midnight, since 
      # there's only one eq point each day.  So now we've selected noon,
      # let's now round down to the start of the day:
      mutate(date = floor_date(date, "1 day"))
    
    
    locDf <- locEq %>% 
      left_join(locSurf) %>% 
      filter(complete.cases(.)) %>% 
      mutate(water = loc_1 + loc_7 + loc_8,
             europe = loc_4 + loc_10 + loc_11,
             uk = loc_9,
             africa = loc_3,
             americas = loc_2 + loc_5) # includes Greeland
    
    nzvStats <- nearZeroVar(locDf, saveMetrics = TRUE)
    
    
    # View the results
    print(nzvStats)
    nonZeroVars <- rownames(nzvStats[nzvStats$nzv == FALSE, ])
    nonZeroVars <- nonZeroVars[which(nonZeroVars %in% c("water", 
                                                        "europe",
                                                        "uk",
                                                        "africa",
                                                        "americas"))]    
    attach(locDf)
    regEq <- paste0("deltadelta~",paste(nonZeroVars,collapse = "+"))
    regMod <- lm(regEq)
    #test <- lm(deltadelta ~ water + europe + uk + africa + americas)
    
    detach(locDf)
    out <- summary(regMod)
    outVif <- car::vif(regMod) 
    outVif$`(Intercept)` = NA
    outVifList[[ix]] <- unlist(outVif)
    
    tidy_out <- broom::tidy(regMod, conf.int = TRUE) %>% 
      filter(term != "(Intercept)")
    
    
    
    # 1. Vertical Logic: Check for headroom
    # Find the highest point (estimate + error)
    peak_val <- max(tidy_out$estimate + tidy_out$std.error, na.rm = TRUE)
    min_val <- min(tidy_out$estimate - tidy_out$std.error, na.rm = TRUE)
    
    biggest_val <- ifelse(peak_val > abs(min_val), peak_val, min_val)
    # If bars are tall (e.g., > 0.08 on your scale), move box to bottom
    if(peak_val > 0.5) {
      y_val <- -Inf
      v_val <- -0.5 # Hangs it just above the x-axis labels
    } else {
      y_val <- Inf
      v_val <- 1.5  # Hangs it just below the top ceiling
    }
    
    # 2. Horizontal 
    num_bars <- nrow(tidy_out)
    mid <- ceiling(num_bars / 2)
    max_left  <- max(tidy_out$estimate[1:mid] + tidy_out$std.error[1:mid], na.rm = TRUE)
    max_right <- max(tidy_out$estimate[(mid+1):num_bars] + tidy_out$std.error[(mid+1):num_bars], na.rm = TRUE)
    
    # If left is shorter, nudge left (-0.3) and right-align (1). Vice versa.
    nx_val <- ifelse(max_left < max_right, 0.01, -0.01)   
    hj_val <- ifelse(max_left < max_right, 1, 0)
    
    
    modSummary <- summary(regMod)
    outVif <- car::vif(regMod)
    outVifList[[ix]] <- unlist(outVif)
    
    print(outVif)
    
    outList[[ix]] <- broom::tidy(regMod,conf.int = TRUE)
    
    
    
    outList[[ix]] <- broom::tidy(out,conf.int = TRUE)
    
    statsList[[ix]] <- broom::glance(regMod) %>%
      select(r.squared, p.value) %>%
      mutate(p_label = ifelse(p.value < 0.001,
                              "p < 0.001",
                              paste0("p = ", round(p.value, 3))),
             combined_label = paste0("R² = ", round(r.squared, 2), "\n", p_label),
             model = as.character(ix),
             y_pos = y_val,
             v_just = v_val,
             nudge_x_val = nx_val,
             h_just = hj_val ) %>%  # Match the ID used in bind_rows
      select(model, combined_label, 
             y_pos, 
             v_just, 
             nudge_x_val,
             h_just)
  }
  
  # get R2
  
  
  
  outputCombined <- bind_rows(outList, .id = "model") %>%
    filter(term != "(Intercept)")
  
  statsCombined <- bind_rows(statsList)
  
  outVifCombined <- bind_rows(outVifList, .id = "model")
  
  # define some useful plotting constants:
  num_terms <- length(unique(outputCombined$model))
  if(num_terms > 1){#
    sep_lines <- seq(1.5, num_terms - 0.5, by = 1)
  }else{
    sep_lines <- 1.5
  }
  
  
  
  
  regionCols <- c(
    "water"    = "#0072b2", # Blue
    "europe"   = "#42B540", # Green
    "uk"       = "#9D7660", # Brown
    "africa"   = "#D7B5A6", # Tan/Peach
    "americas" = "#FF6F00"  # Orange
  )
  
  master_labels <- c("africa" = "Africa", "americas" = "Americas", 
                     "europe" = "Europe", "water" = "Water", "uk" = "UK")
  
  plotOut <- ggplot(data = outputCombined,aes(x =as.factor(model), 
                                              y = estimate,
                                              fill = term, 
                                              color = term))+
    geom_bar(stat = "identity", 
             position = position_dodge(width = 0.9),
             alpha = 0.5)+
    
    
    #ylim(c(-0.2,0.2))+
    #coord_cartesian(ylim = c(-1, 1.2)) + # prevents removal of info if offscale
    coord_cartesian(ylim = c(-0.15, 0.2)) + # prevents removal of info if offscale
    geom_text(aes(y = estimate + ifelse(estimate >= 0, std.error + 0.025, -std.error - 0.025),
                  label = paste0("p=", round(p.value, 3))), 
              size = 3.5, 
              position = position_dodge(width = 0.9),
              show.legend = FALSE) +
    
    geom_vline(xintercept = sep_lines, color = "gray80", linetype = "dashed") +
    scale_fill_manual(name = "Region",
                      values = regionCols,
                      labels = master_labels
    ) +
    scale_color_manual(name = "Region",
                       values = regionCols,
                       labels = master_labels
    ) +
    geom_errorbar(aes(ymin = estimate - std.error, 
                      ymax = estimate + std.error), 
                  width = 0.2,
                  position = position_dodge(width = 0.9),
                  show.legend = FALSE)+
    # Label for R2
    geom_label(data = statsCombined, 
               aes(x = as.numeric(as.factor(model)) + nudge_x_val, 
                   y = y_pos, 
                   label = combined_label,
                   vjust = v_just,
                   #hjust = h_just),
                   hjust = 0.5),
               fill = "white", 
               color = "black", 
               fontface = "bold",
               lineheight = 0.9, 
               size = 4, 
               alpha = 0.9,       # Slight transparency in case of close calls
               label.size = 0.2,   # Cleaner border
               show.legend = FALSE) +
    coord_cartesian(clip = "off") +
    
    
    theme_minimal()+
    labs(x = "Region", y = expression("Predictor Estimate"))+
    theme(legend.position = "bottom", text = element_text(size = 18))+
    #guides(fill = guide_legend(override.aes = list(alpha = 1, color = NA)))+
    scale_x_discrete(#guide = guide_axis(n.dodge = 3),
      labels = c("JJA", "SON", "DJF", "MAM", "All"),
      breaks = c("1", "2", "3", "4","5"))+
    scale_y_continuous(n.breaks = 10)
  #expand = expansion(mult = c(0.15,0.1)))#,
  #limits = c(-0.2,0.2))
  
  print(plotOut)
  
  # outPlot <- grid.arrange(grobs = plotList, ncol = 3)
  ggsave(paste0("G:/My Drive/Experiments/DEFRA/hysplit/seasonal_mvr/",
                deparse(substitute(locEq)),TAG,".png"),plot = plotOut,
         width = 12.8, height = 10.24, units = "in")
  
  return(list(outputCombined, statsCombined, outVifCombined))
  #return(print(plotOut))
}






# I think eqMVRComp has been made redundant by comp.regression
# eqMVRComp will plot bars for the multivariable regression
# between the equivalence methods and the grouped HYSPLIT
# output

# DATES needs to be a 2 column matrix
eqMVRComp <- function(locEq, locAcsm, DATES = dateMat){
  plotList <- list() # for output plot
  statsList <- list() # for R2
  outList <- list() # for model statistics
  outVifList <- list() # for VIF calculations
  
  
  for(ix in 1:nrow(DATES)){
    locSurf <- sourceBoxes(locBox,DATE1 = DATES[ix,1], DATE2 = DATES[ix,2],
                           PLOT = FALSE, PCT = FALSE, OUTPUT = "BOXES") %>% 
      filter(hour(date) == 12) %>%  # I think noon will be more appropriate than midnight, since 
      # there's only one eq point each day.  So now we've selected noon,
      # let's now round down to the start of the day:
      mutate(date = floor_date(date, "1 day"))
    
    
    locDf <- locEq %>% 
      left_join(locSurf) %>% 
      filter(complete.cases(.)) %>% 
      mutate(water = loc_1 + loc_7 + loc_8,
             europe = loc_4 + loc_10 + loc_11,
             uk = loc_9,
             africa = loc_3,
             americas = loc_2 + loc_5) # includes Greeland
    
    nzvStats <- nearZeroVar(locDf, saveMetrics = TRUE)
    
    
    # View the results
    print(nzvStats)
    nonZeroVars <- rownames(nzvStats[nzvStats$nzv == FALSE, ])
    nonZeroVars <- nonZeroVars[which(nonZeroVars %in% c("water", 
                                                        "europe",
                                                        "uk",
                                                        "africa",
                                                        "americas"))]    
    attach(locDf)
    regEq <- paste0("deltadelta~",paste(nonZeroVars,collapse = "+"))
    test <- lm(regEq)
    #test <- lm(deltadelta ~ water + europe + uk + africa + americas)
    
    detach(locDf)
    out <- summary(test)
    outVif <- car::vif(test) 
    outVif$`(Intercept)` = NA
    outVifList[[ix]] <- unlist(outVif)
    
    
    toPlot <- out %>% 
      broom::tidy(conf.int = TRUE) %>% 
      filter(term != "(Intercept)") # not sure if meaningful?
    
    
    #   
    sink(file = paste0("G:/My Drive/Experiments/DEFRA/hysplit/seasonal_mvr/",
                       deparse(substitute(locEq)), "_surf-lessnzv",
                       gsub("-","",DATES[ix,1]),
                       gsub("-","",DATES[ix,2])))
    print(out)
    print(outVif)
    sink()
    
    
    outList[[ix]] <- broom::tidy(out,conf.int = TRUE)
    
    statsList[[ix]] <- broom::glance(test) %>%
      select(r.squared, p.value) %>%
      mutate(p_label = ifelse(p.value < 0.001,
                              "p < 0.001",
                              paste0("p = ", round(p.value, 3))),
             combined_label = paste0("R² = ", round(r.squared, 2), "\n", p_label),
             model = as.character(ix)) %>%  # Match the ID used in bind_rows
      select(model, combined_label)
  }
  
  # get R2
  
  
  outputCombined <- bind_rows(outList, .id = "model") %>%
    filter(term != "(Intercept)")
  
  statsCombined <- bind_rows(statsList)
  
  vifCombined <- bind_rows(outVifList) %>% 
    write_csv(paste0("G:/My Drive/Experiments/DEFRA/hysplit/seasonal_mvr/",
                     deparse(substitute(locEq)), "_surf-lessnzv-VIF",
                     gsub("-","",DATES[ix,1]),
                     gsub("-","",DATES[ix,2]),".csv"))
  
  
  
  # define some useful plotting constants:
  num_terms <- length(unique(outputCombined$model))
  if(num_terms > 1){#
    sep_lines <- seq(1.5, num_terms - 0.5, by = 1)
  }else{
    sep_lines <- 1.5
  }
  
  regionCols <- c(
    "water"    = "#0072b2", # Blue
    "europe"   = "#42B540", # Green
    "uk"       = "#9D7660", # Brown
    "africa"   = "#D7B5A6", # Tan/Peach
    "americas" = "#FF6F00"  # Orange
  )
  
  master_labels <- c("africa" = "Africa", "americas" = "Americas", 
                     "europe" = "Europe", "water" = "Water", "uk" = "UK")
  
  plotOut <- ggplot(data = outputCombined,aes(x =as.factor(model), 
                                              y = estimate,
                                              fill = term, color = term))+
    geom_bar(stat = "identity", 
             position = position_dodge(width = 0.9),
             alpha = 0.5)+
    
    
    #ylim(c(-0.2,0.2))+
    #coord_cartesian(ylim = c(-1, 1.2)) + # prevents removal of info if offscale
    coord_cartesian(ylim = c(-0.15, 0.2)) + # prevents removal of info if offscale
    geom_text(aes(y = estimate + ifelse(estimate >= 0, std.error + 0.025, -std.error - 0.025),
                  label = paste0("p=", round(p.value, 3))), 
              size = 3.5, 
              position = position_dodge(width = 0.9),
              show.legend = FALSE) +
    
    geom_vline(xintercept = sep_lines, color = "gray80", linetype = "dashed") +
    scale_fill_manual(name = "Region",
                      values = regionCols,
                      labels = master_labels
    ) +
    scale_color_manual(name = "Region",
                       values = regionCols,
                       labels = master_labels
    ) +
    geom_errorbar(aes(ymin = estimate - std.error, 
                      ymax = estimate + std.error), 
                  width = 0.2,
                  position = position_dodge(width = 0.9),
                  show.legend = FALSE)+
    # Label for R2
    geom_label(data = statsCombined, 
               aes(x = as.factor(model), 
                   y = 1.3,
                   label = combined_label),
               fill = "white", 
               vjust = 1.2, # adjusts the vertical position of the annotation
               color = "black", 
               fontface = "bold",
               lineheight = 0.9, 
               size = 4.5) +
    
    theme_minimal()+
    labs(x = "Region", y = expression("Predictor (ug m" ^-3* ")"))+
    # fill = "Region",color= "Region")+
    # labs(x = "Region", y = expression("Predictor (ug m" ^-3* ")"),
    #      title = paste0(DATES[ix,1],"/", DATES[ix,2]), fill = "Region")+
    theme(legend.position = "bottom", text = element_text(size = 18))+
    #guides(fill = guide_legend(override.aes = list(alpha = 1, color = NA)))+
    scale_x_discrete(#guide = guide_axis(n.dodge = 3),
      #labels = c("JJA", "SON", "DJF", "MAM"),
      #                  breaks = c("1", "2", "3", "4"))+
      labels = c("All"))+
    scale_y_continuous(n.breaks = 10)
  #expand = expansion(mult = c(0.15,0.1)))#,
  #limits = c(-0.2,0.2))
  
  # outPlot <- grid.arrange(grobs = plotList, ncol = 3)
  ggsave(paste0("G:/My Drive/Experiments/DEFRA/hysplit/seasonal_mvr/",
                deparse(substitute(locEq)),"-nvrRemoved-allseasons.png"),plot = plotOut,
         width = 12.8, height = 10.24, units = "in")
  
  return(print(plotOut))
}




  