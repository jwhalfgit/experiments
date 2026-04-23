# Plotting functions for SMPS data.
# Source sourceMeFirst_ufp.R and load_ufp.R before this file.


# plot_smps_banana() ------------------------------------------------------
# Creates a time series "banana plot" of SMPS size distributions.
# x-axis: time; y-axis: diameter (nm, log scale); fill: concentration.
#
# The y-axis is worked in log10(diameter) space so that geom_tile heights
# fill the axis evenly regardless of bin spacing. Zeros are treated as NA
# for the colour scale (log transform cannot handle zero).
#
# Arguments:
#   smps_data — output of read_smps_files()
#   title     — optional plot title string
#   na_colour — colour for NA/zero cells (default "grey20")
#   start     — optional start date string for time filtering (default NULL)
#   end       — optional end date string for time filtering (default NULL)
#   clim      — length-2 numeric vector c(min, max) for the colour scale.
#               Values outside this range are squished to the nearest limit
#               colour (not removed). Default NULL uses data range.

plot_smps_banana <- function(smps_data,
                             title     = NULL,
                             na_colour = "grey20",
                             start     = NULL,
                             end       = NULL,
                             clim      = NULL) {

  # Optional time filtering
  if (!is.null(start))
    smps_data <- smps_data %>% filter(date >= as.POSIXct(start, tz = "UTC"))
  if (!is.null(end))
    smps_data <- smps_data %>% filter(date <= as.POSIXct(end,   tz = "UTC"))

  # Pivot to long, compute log10 diameter
  long <- smps_data %>%
    pivot_longer(cols      = -date,
                 names_to  = "diameter",
                 values_to = "conc") %>%
    mutate(diameter = as.numeric(diameter),
           log_diam = log10(diameter),
           conc     = if_else(conc == 0, NA_real_, conc))

  # Tile heights in log10 space: each bin spans half the gap to its neighbours
  diams     <- sort(unique(long$diameter))
  log_diams <- log10(diams)
  n         <- length(diams)
  tile_h    <- numeric(n)
  tile_h[1] <- log_diams[2] - log_diams[1]
  tile_h[n] <- log_diams[n] - log_diams[n - 1]
  if (n > 2) tile_h[2:(n - 1)] <- (log_diams[3:n] - log_diams[1:(n - 2)]) / 2

  long <- long %>%
    left_join(tibble(diameter = diams, tile_h = tile_h), by = "diameter")

  # Tile width: median time step in seconds (POSIXct axis unit)
  times  <- sort(unique(long$date))
  dt_sec <- median(as.numeric(diff(times), units = "secs"))

  # y-axis breaks at round diameters within the data range
  candidate_breaks <- c(1, 3, 10, 30, 100, 300, 1000)
  y_breaks <- candidate_breaks[candidate_breaks >= min(diams) &
                                  candidate_breaks <= max(diams)]

  ggplot(long, aes(x = date, y = log_diam, fill = conc)) +
    geom_tile(aes(width = dt_sec, height = tile_h)) +
    scale_y_continuous(breaks = log10(y_breaks), labels = y_breaks) +
    scale_fill_viridis_c(
      option   = "plasma",
      trans    = "log10",
      limits   = clim,
      oob      = scales::squish,
      na.value = na_colour,
      name     = "dN/dlogDp"
    ) +
    labs(title = title, x = NULL, y = "Diameter (nm)") +
    theme_bw()
}


# plot_smps_conversion() --------------------------------------------------
# Plots original vs. interpolated size distributions for a single row,
# to visually verify the spline conversion quality.
#
# Arguments:
#   file_path  — path to a single SMPS CSV file
#   row_index  — which observation row to plot (default 1, skips NA rows)
#   new_scale  — target scale passed to read_smps_files (default SMPS_SCALE)
#   spar       — smoothing parameter (default 0.1)

plot_smps_conversion <- function(file_path,
                                 row_index = 1,
                                 new_scale = SMPS_SCALE,
                                 spar = 0.1) {

  df <- read_csv(file_path, show_col_types = FALSE)

  if (any(grepl("^PN_", names(df)))) {
    bin_cols  <- grep("^PN_", names(df), value = TRUE)
    diameters <- as.numeric(sub("^PN_", "", bin_cols))
    data_mat  <- as.matrix(df[, bin_cols])
  } else {
    bin_cols  <- names(df)[!is.na(suppressWarnings(as.numeric(names(df))))]
    diameters <- as.numeric(bin_cols)
    data_mat  <- as.matrix(df[, bin_cols])
  }

  # Find first non-NA row at or after row_index
  for (r in seq(row_index, nrow(data_mat))) {
    row_vals <- as.numeric(data_mat[r, ])
    valid    <- !is.na(row_vals)
    if (sum(valid) >= 4) { row_index <- r; break }
  }

  row_vals <- as.numeric(data_mat[row_index, ])
  valid    <- !is.na(row_vals)
  fit      <- smooth.spline(diameters[valid], row_vals[valid],
                            spar = spar)

  in_range  <- new_scale >= min(diameters) & new_scale <= max(diameters)
  pred_vals <- rep(NA_real_, length(new_scale))
  pred_vals[in_range] <- pmax(predict(fit, new_scale[in_range])$y, 0)

  orig   <- tibble(diameter = diameters[valid], value = row_vals[valid],
                   source = "original")
  interp <- tibble(diameter = new_scale[in_range], value = pred_vals[in_range],
                   source = "interpolated")

  ggplot() +
    geom_point(data = orig,   aes(x = diameter, y = value, colour = source), 
               size = 2) +
    geom_line( data = interp, aes(x = diameter, y = value, colour = source)) +
    scale_x_log10() +
    scale_colour_manual(values = c("original" = "black", 
                                   "interpolated" = "steelblue")) +
    labs(
      title    = paste0("SMPS spline conversion — row ", row_index),
      subtitle = basename(file_path),
      x        = "Diameter (nm)",
      y        = "Concentration",
      colour   = NULL
    ) +
    theme_bw()
}




plot_cpc_ts <- function(cpc_data, start = NULL, end = NULL){
  # perform time filtering
  if(!is.null(start)){
    cpc_data <- cpc_data %>% 
      filter(date >= as.POSIXct(start, tz = "UTC"))
  }
  
  if(!is.null(end)){
    cpc_data <- cpc_data %>% 
      filter(date <= as.POSIXct(end,   tz = "UTC"))
  }

  
  ggplot(cpc_data, aes(x = date, y = conc)) +
    geom_line() +
    labs(x = NULL, y = "Particle concentration (#/cm³)") +
    theme_bw()
}
