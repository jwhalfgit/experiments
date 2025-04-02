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
    labs(title = paste(site1Name, DATE1, "-", DATE2), x = "Longitude", y = "Latitude")+
    theme(axis.title = element_text(size = 20), plot.title= element_text(size = 20))
  
  
  ggsave(paste0(PLOTDIR,"/",gsub("-","",dmy(DATE1)),gsub("-","",dmy(DATE2)),
                "-",deparse(substitute(DF)),
                "-binned.png"), width = 12.80, height = 7.68, units = "in")
  
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

sourceBoxes <- function(SITE, DATE1, DATE2, HOURS, PCT= TRUE,
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
      if(lon[ix] >= -55 & lon[ix] < -10 & lat[ix] >= 35 & lat[ix] <= 59){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -75 & lon[ix] < -20 & lat[ix] >= 20 & lat[ix] <= 43){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -60 & lon[ix] < -55 & lat[ix] >= 40 & lat[ix] <= 71){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -75 & lon[ix] < -71 & lat[ix] >= 25 & lat[ix] <= 43){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -70 & lon[ix] < -30 & lat[ix] >= 15 & lat[ix] <= 43){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -60 & lon[ix] < -30 & lat[ix] >= 10 & lat[ix] <= 59){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -55 & lon[ix] < -15 & lat[ix] >= 5 & lat[ix] <= 59){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -50 & lon[ix] < -17 & lat[ix] >= 0 & lat[ix] <= 59){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -60 & lon[ix] < -55 & lat[ix] >= 55 & lat[ix] <= 71){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -65 & lon[ix] < -60 & lat[ix] >= 60 & lat[ix] <= 71){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -27 & lon[ix] < -10 & lat[ix] >= 29 & lat[ix] <= 41){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -20 & lon[ix] < -15 & lat[ix] >= 30 & lat[ix] <= 59){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
      }else if(lon[ix] >= -10 & lon[ix] < 5 & lat[ix] >= 45 & lat[ix] <= 51){
        boxes[1] <- boxes[1] + 1
        loc[ix] <- 1
        
      # AMERICAN
      }else if(lon[ix] >= -100 & lon[ix] < -80 & lat[ix] >= 0 & lat[ix] <= 80){
        boxes[2] <- boxes[2] + 1
        loc[ix] <- 2
      }else if(lon[ix] >= -80 & lon[ix] < -65 & lat[ix] >= 40 & lat[ix] <= 80){
        boxes[2] <- boxes[2] + 1
        loc[ix] <- 2
      }else if(lon[ix] >= -65 & lon[ix] < -60 & lat[ix] >= 40 & lat[ix] <= 80){
        boxes[2] <- boxes[2] + 1
        loc[ix] <- 2
      }else if(lon[ix] >= -60 & lon[ix] < -55 & lat[ix] >= 45 & lat[ix] <= 80){
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
      }else if(lon[ix] >= -80 & lon[ix] < -55 & lat[ix] >= 5 & lat[ix] <= 80){
        boxes[2] <- boxes[2] + 1
        loc[ix] <- 2
      }else if(lon[ix] >= -80 & lon[ix] < -50 & lat[ix] >= 0 & lat[ix] <= 80){
        boxes[2] <- boxes[2] + 1
        loc[ix] <- 2
        
        # AFRICAN COAST
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
      # AFRICAN
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
      }else  if(lon[ix] >= -10 & lon[ix] < 7 & lat[ix] >= 45 & lat[ix] <= 50){
        boxes[4] <- boxes[4] + 1
        loc[ix] <- 4
      }else  if(lon[ix] >= 20 & lon[ix] < 55 & lat[ix] >= 55 & lat[ix] <= 60){
        boxes[4] <- boxes[4] + 1
        loc[ix] <- 4
        
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
        
      # ICELAND  
      }else if(lon[ix] >= -25 & lon[ix] < -13 & lat[ix] >= 63 & lat[ix] <= 67){
        boxes[6] <- boxes[6] + 1
        loc[ix] <- 6
      
        # North Atlantic
      }else if(lon[ix] >= -17 & lon[ix] < 55 & lat[ix] >= 70 & lat[ix] <= 80){
        boxes[7] <- boxes[7] + 1
        loc[ix] <- 7
      }else if(lon[ix] >= -13 & lon[ix] < 10 & lat[ix] >= 64 & lat[ix] <= 71){
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
      }else if(lon[ix] >= -25 & lon[ix] < -11 & lat[ix] >= 67 & lat[ix] <= 69){
        boxes[7] <- boxes[7] + 1
        loc[ix] <- 7
      }else if(lon[ix] >= -21 & lon[ix] < 1 & lat[ix] >= 67 & lat[ix] <= 69){
        boxes[7] <- boxes[7] + 1
        loc[ix] <- 7
      }else if(lon[ix] >= -34 & lon[ix] < -25 & lat[ix] >= 67 & lat[ix] <= 75){
        boxes[7] <- boxes[7] + 1
        loc[ix] <- 7

        # North Sea
      }else if(lon[ix] >= -1 & lon[ix] < 8 & lat[ix] >= 55 & lat[ix] <= 58){
        boxes[8] <- boxes[8] + 1
        loc[ix] <- 8
      }else if(lon[ix] >= 0 & lon[ix] < 8 & lat[ix] >= 54 & lat[ix] <= 55){
        boxes[8] <- boxes[8] + 1
        loc[ix] <- 8
      }else if(lon[ix] >= 0 & lon[ix] < 5 & lat[ix] >= 53 & lat[ix] <= 59){
        boxes[8] <- boxes[8] + 1
        loc[ix] <- 8
      
        # UK
      }else if(lon[ix] >= -10 & lon[ix] < -5 & lat[ix] >= 51 & lat[ix] <= 55){
        boxes[9] <- boxes[9] + 1
        loc[ix] <- 9
      }else if(lon[ix] >= -7 & lon[ix] < -1 & lat[ix] >= 50 & lat[ix] <= 59){
        boxes[9] <- boxes[9] + 1
        loc[ix] <- 9
      }else if(lon[ix] >= -3 & lon[ix] < 2 & lat[ix] >= 51 & lat[ix] <= 54){
        boxes[9] <- boxes[9] + 1
        loc[ix] <- 9
        
      # Northern Europe
      }else if(lon[ix] >= 4 & lon[ix] < 20 & lat[ix] >= 55 & lat[ix] <= 64){
        boxes[10] <- boxes[10] + 1
        loc[ix] <- 10
      }else  if(lon[ix] >= 5 & lon[ix] < 55 & lat[ix] >= 60 & lat[ix] <= 71){
        boxes[10] <- boxes[10] + 1
        loc[ix] <- 10
      
      # Southern Euroope    
      }else if(lon[ix] >= -10 & lon[ix] < 55 & lat[ix] > 35 & lat[ix] <= 45){
        boxes[11] <- boxes[11] + 1
        loc[ix] <- 11
      }else{
        boxes[12] <- boxes[12]+1
        loc[ix] <- 12
      }
    }
  }
  names(boxes) <- c("Atlantic", "N America", "Africa", "Europe",
                    "Greenland","Iceland","N Atlantic","North Sea","UK", 
                    "N Europe","S Europe","uncategorized")
  
  if(PCT == TRUE){
    boxes <- boxes/sum(boxes)*100
  }
  
  # add locations to site data frame
  siteOut <- site %>% 
    mutate(region = loc) 
  
  siteName <- sites$loc[which(sites$lat==site$lat[1]&sites$lon == site$lon[1])]
  
  
  if(OUTPUT == "TS" & PLOT == FALSE){
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
                        labels = c("Atlantic", "N America", "Africa", "Europe",
                                   "Greenland","Iceland","N Atlantic","North Sea","UK", 
                                   "N Europe","S Europe","uncategorized")) + #uncategorized
      labs(x = "Date (UTC)", y = "Hours Backward", fill = "Region",
           title = paste0(siteName," ", DATE1," thru ", DATE2, ", Hours ", HOURS[1],"-",
                          HOURS[length(HOURS)]))+
      theme_minimal()
    return(sitePlot)
    
  }else if(OUTPUT == "BOXES"){
      return(boxes)
  }else{
    return(print("OUTPUT not defined! Please define as either 'TS' or 'BOXES'"))
  }
  
  
  
}

  