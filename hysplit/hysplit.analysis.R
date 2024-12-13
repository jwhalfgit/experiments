library(splitr)
library(openair)
library(gridExtra)

met.file.loc <- "C:/HYSPLIT/metfiles/" # HYSPLIT met file directory
wd.hysplit <- "C:/HYSPLIT/working" # HYSPLIT working directory
TRAJDIR <- file.path(wd.hysplit,"trajectories","bin")


library(tidyverse)

trajectory_read_alt <- function (output_folder){
  trajectory_file_list <- list.files(path = output_folder, 
                                     pattern = "^tback-.*")
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
  
  for (file_i in trajectory_file_list) {
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




# I have had to edit the original trajectory_plot_alt from splitr because the 
# trajecotires were not plotting.  This is now trajectory_plot_alt.
# It does work, but there's an issue with wrapping around the globe, so the plot 
# looks a bit funny.  Better to use openair functions from here I think
trajectory_plot_alt <- function (x, show_hourly = TRUE, color_scheme = "cycle_hues") 
{
  if (inherits(x, "trajectory_model")) {
    if (!is.null(x$traj_df)) {
      traj_df <- x$traj_df
    }
    else {
      stop("There is no data available for plotting.")
    }
  }
  if (inherits(x, "data.frame")) {
    if (all(c("run", "receptor", "hour_along", "traj_dt", 
              "lat", "lon", "height", "traj_dt_i") %in% colnames(x))) {
      traj_df <- x
    } else {
      stop("This tibble does not contain plottable trajectory data.")
    }
  }
  dt_runs <- traj_df$traj_dt_i %>% unique() %>% length()
  if (color_scheme == "cycle_hues") {
    colors <- (scales::hue_pal(c = 90, l = 70))(dt_runs)
  }else if(color_scheme == "increasingly_gray") {
    colors <- (scales::grey_pal(0.7, 0.1))(dt_runs)
  }
  traj_df$lon[which(traj_df$lon > 0)] <- traj_df$lon[which(traj_df$lon > 
                                                             0)] - (180 * 2)
  receptors <- traj_df %>% dplyr::pull(receptor) %>% unique()
  dates <- traj_df %>% dplyr::pull(traj_dt_i) %>% unique()
  traj_plot <- leaflet::leaflet() %>% 
    leaflet::addProviderTiles(provider = "OpenStreetMap", group = "OpenStreetMap") %>% 
    leaflet::addProviderTiles(provider = "CartoDB.DarkMatter",group = "CartoDB Dark Matter") %>% 
    leaflet::addProviderTiles(provider = "CartoDB.Positron",group = "CartoDB Positron") %>% 
    leaflet::addProviderTiles(provider = "Esri.WorldTerrain", group = "ESRI World Terrain") %>% 
    #leaflet::addProviderTiles(provider = "Stamen.Toner", group = "Stamen Toner") %>% 
    leaflet::fitBounds(lng1 = min(traj_df[["lon"]]), lat1 = min(traj_df[["lat"]]), lng2 = max(traj_df[["lon"]]), lat2 = max(traj_df[["lat"]])) %>% 
    leaflet::addLayersControl(baseGroups = c("CartoDB Positron", "CartoDB Dark Matter", "Stamen Toner", "ESRI World Terrain"), overlayGroups = c("trajectory_points", "trajectory_paths"), position = "topright")

  
  for (i in seq_along(receptors)) {
    receptor_i <- receptors[i]
    for (j in seq_along(dates)) {
      date_i <- dates[j]
      wind_traj_ij <- traj_df %>% dplyr::filter(receptor == 
                                                  receptor_i, traj_dt_i == date_i)
      popup <- paste0("<strong>trajectory</strong> ", wind_traj_ij[["traj_dt_i"]], 
                      "<br><strong>at time</strong> ", wind_traj_ij[["traj_dt"]], 
                      " (", wind_traj_ij[["hour_along"]], " h)<br><strong>height</strong> ", 
                      wind_traj_ij[["height"]], " <font size=\"1\">m AGL</font> / ", 
                      "<strong>P</strong> ", wind_traj_ij[["pressure"]], 
                      " <font size=\"1\">hPa</font>")
      traj_plot <- traj_plot %>% leaflet::addPolylines(lng = wind_traj_ij[["lon"]], 
                                                       lat = wind_traj_ij[["lat"]], group = "trajectory_paths", 
                                                       weight = 2, smoothFactor = 1, color = colors[j]) %>% 
        leaflet::addCircles(lng = wind_traj_ij[["lon"]], 
                            lat = wind_traj_ij[["lat"]], group = "trajectory_points", 
                            radius = 250, fill = TRUE, color = colors[j], 
                            fillColor = colors[j], popup = popup)
    }
  }
  traj_plot
}
# as a reminder, here are the sites that are referenced in trajDF:
# 
# sites
# lat   lon                         loc
# 1 53.44 -2.21 Manchester Air Quality Site
# 2 52.45 -1.93       Birmingham Super Site
# 3 54.98 -1.61            Newcastle Center
# 4 55.95 -3.20            Edinburgh Center
# 5 54.60 -5.93              Belfast Center
# 6 51.48 -3.18              Cardiff Center
# 7 51.45 -0.04 London Super Site


trajDF <- trajectory_read_alt(TRAJDIR) # this loads the data per splitr.
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
                         `6` = sites$loc[6]))



# Generate individual site data.frames
maqs <- trajDF_oa %>% filter(receptor == 1) 
bss <- trajDF_oa %>% filter(receptor == 2)
nc <- trajDF_oa %>% filter(receptor == 3)
ec <- trajDF_oa %>% filter(receptor == 4)
bc <- trajDF_oa %>% filter(receptor == 5)
cc <- trajDF_oa %>% filter(receptor == 6)


# 
seasonal.plots <- function(DF){
  
  dfFiltered <- selectByDate(trajDF_oa, start = "01/1/2023",
               end = "31/3/2023" )
 
   

  
 
}





