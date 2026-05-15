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
# When show_NSD = TRUE, total particle number size distribution (NSD, integrated
# across all size bins) is overlaid as a line on a secondary y-axis. NSD is
# linearly scaled to span the diameter axis range; the 99th percentile of NSD caps the
# upper scale limit to prevent outlier spikes from compressing the line.
#
# Arguments:
#   smps_data — output of read_smps_files()
#   title     — optional plot title string
#   na_colour — colour for NA/zero cells (default "grey20")
#   start     — optional start date string for time filtering (default NULL)
#   end       — optional end date string for time filtering (default NULL)
#   clim      — length-2 numeric vector c(min, max) for the colour scale.
#               Values outside this range are squished to the nearest limit
#               colour (not removed). Default c(1, 1e6).
#   show_NSD    — overlay total NSD concentration line (default FALSE)
#   NSD_colour  — colour of the NSD line (default "white"; use "black" for light themes)
#   NSD_avg     — time-averaging unit for the NSD line, passed to lubridate::floor_date
#               (e.g. "month", "week", "day", "hour"). Default "month". NULL disables
#               averaging and plots every data point.

plot_smps_banana <- function(smps_data,
                             title     = NULL,
                             na_colour = "grey20",
                             start     = NULL,
                             end       = NULL,
                             clim      = c(1, 1e6),
                             show_NSD    = FALSE,
                             NSD_colour  = "white",
                             NSD_avg     = "month") {

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
  long <- long %>% left_join(tibble(diameter = diams, tile_h = tile_h), by = "diameter")

  times  <- sort(unique(long$date))
  dt_sec <- median(as.numeric(diff(times), units = "secs"))

  candidate_breaks <- c(1, 3, 10, 30, 100, 300, 1000)
  y_breaks <- candidate_breaks[candidate_breaks >= min(diams) &
                                  candidate_breaks <= max(diams)]

  # Build y scale — optionally with a secondary NSD axis
  if (show_NSD) {
    diam_cols  <- sort(as.numeric(names(smps_data)[-1]))
    logD       <- log10(diam_cols)
    n_b        <- length(logD)
    edges      <- c(logD[1]     - (logD[2]      - logD[1])     / 2,
                    (logD[-n_b] +  logD[-1])                    / 2,
                    logD[n_b]   + (logD[n_b]    - logD[n_b-1]) / 2)
    data_mat   <- as.matrix(smps_data[, as.character(diam_cols)])
    all_na     <- apply(data_mat, 1, function(r) all(is.na(r)))
    NSD_vals     <- rowSums(sweep(data_mat, 2, diff(edges), "*"), na.rm = TRUE)
    NSD_vals[all_na] <- NA_real_
    NSD_ts <- tibble(date = smps_data$date, N = NSD_vals)

    if (!is.null(NSD_avg))
      NSD_ts <- NSD_ts %>%
        mutate(date = floor_date(date, NSD_avg)) %>%
        group_by(date) %>%
        summarise(N = mean(N, na.rm = TRUE), .groups = "drop") %>%
        mutate(N = if_else(is.nan(N), NA_real_, N))

    y_min   <- log10(min(diams))
    y_max   <- log10(max(diams))
    NSD_valid <- NSD_ts$N[!is.na(NSD_ts$N)]
    NSD_lo    <- 0
    NSD_hi    <- if (length(NSD_valid) > 0) quantile(NSD_valid, 0.99) else 1

    scale_fac <- (y_max - y_min) / (NSD_hi - NSD_lo)
    intercept <- y_min - NSD_lo * scale_fac

    NSD_ts <- NSD_ts %>%
      mutate(NSD_scaled = pmin(pmax(N * scale_fac + intercept, y_min), y_max))

    NSD_axis_breaks <- pretty(c(NSD_lo, NSD_hi), n = 5)
    NSD_axis_breaks <- NSD_axis_breaks[NSD_axis_breaks >= NSD_lo & NSD_axis_breaks <= NSD_hi * 1.05]

    y_scale <- scale_y_continuous(
      breaks   = log10(y_breaks),
      labels   = y_breaks,
      sec.axis = sec_axis(
        transform = ~ (. - intercept) / scale_fac,
        name      = "NSD  (#/cm³)",
        breaks    = NSD_axis_breaks
      )
    )
  } else {
    y_scale <- scale_y_continuous(breaks = log10(y_breaks), labels = y_breaks)
  }

  p <- ggplot(long, aes(x = date, y = log_diam, fill = conc)) +
    geom_tile(aes(width = dt_sec, height = tile_h)) +
    y_scale +
    scale_fill_viridis_c(
      trans    = "log10",
      limits   = clim,
      oob      = scales::squish,
      na.value = na_colour,
      name     = "dN/dlogDp"
    ) +
    labs(title = title, x = NULL, y = "Diameter (nm)") +
    theme_bw()

  if (show_NSD)
    p <- p + geom_line(data        = NSD_ts,
                       mapping     = aes(x = date, y = NSD_scaled),
                       colour      = NSD_colour,
                       linewidth   = 0.6,
                       na.rm       = TRUE,
                       inherit.aes = FALSE)
  p
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




# plot_lognormals() -------------------------------------------------------
# Visualises a single SMPS size distribution together with its deconvolved
# log-normal modes.
#
# The measured (or mean) spectrum is drawn as a solid black line. Each fitted
# mode from fit_lognormals() is drawn as a dotted coloured line. When
# show_sum = TRUE (the default) and more than one mode is present, the sum
# of all fitted components is added as a dashed grey line so you can judge
# goodness-of-fit by eye.
#
# To use with the mean spectrum across a dataset:
#   diameters <- as.numeric(names(smps_data)[-1])
#   mean_conc <- colMeans(smps_data[, -1], na.rm = TRUE)
#   fit       <- fit_lognormals(diameters, mean_conc)
#   plot_lognormals(diameters, mean_conc, fit)
#
# Arguments:
#   diameters      — numeric vector of bin midpoints (nm)
#   concentrations — numeric vector of dN/d log10(Dp) values (same length)
#   fit            — tibble returned by fit_lognormals()
#   title          — optional plot title string (default NULL)
#   show_sum       — whether to overlay the total reconstructed spectrum
#                    (default TRUE)

plot_lognormals <- function(diameters, concentrations, fit,
                            title    = NULL,
                            show_sum = TRUE) {

  # Dense log-spaced grid for smooth reconstructed curves
  logD_grid <- seq(log10(min(diameters)), log10(max(diameters)), length.out = 400)
  d_grid    <- 10^logD_grid

  measured_df <- tibble(diameter = diameters, conc = concentrations)

  # Reconstruct each mode on the dense grid
  mode_df <- map_dfr(seq_len(nrow(fit)), function(i) {
    lsg <- log10(fit$sigma_g[i])
    lpg <- log10(fit$Dpg_nm[i])
    y   <- (fit$N_percm3[i] / (sqrt(2 * pi) * lsg)) *
           exp(-0.5 * ((logD_grid - lpg) / lsg)^2)
    tibble(
      diameter = d_grid,
      conc     = y,
      label    = paste0("Mode ", fit$mode[i], " — ",
                        round(fit$Dpg_nm[i]), " nm  (σₒ=",
                        round(fit$sigma_g[i], 2), ")")
    )
  })

  p <- ggplot() +
    geom_line(data    = measured_df,
              mapping = aes(x = diameter, y = conc),
              colour = "black", linewidth = 0.9) +
    geom_line(data    = mode_df,
              mapping = aes(x = diameter, y = conc, colour = label),
              linetype = "dotted", linewidth = 1.0) +
    scale_x_log10(
      breaks = c(10, 20, 50, 100, 200, 500, 1000),
      labels = c("10", "20", "50", "100", "200", "500", "1000")
    ) +
    labs(
      x      = "Particle diameter (nm)",
      y      = expression(dN / d * log[10](D[p]) ~ "(#/cm"^3 * ")"),
      colour = NULL,
      title  = title
    ) +
    theme_bw() +
    theme(legend.position = "bottom")

  if (show_sum && nrow(fit) > 1L) {
    sum_conc <- Reduce("+", lapply(seq_len(nrow(fit)), function(i) {
      lsg <- log10(fit$sigma_g[i])
      lpg <- log10(fit$Dpg_nm[i])
      (fit$N_percm3[i] / (sqrt(2 * pi) * lsg)) *
        exp(-0.5 * ((logD_grid - lpg) / lsg)^2)
    }))
    total_df <- tibble(diameter = d_grid, conc = sum_conc)
    p <- p +
      geom_line(data    = total_df,
                mapping = aes(x = diameter, y = conc),
                colour = "grey40", linetype = "dashed", linewidth = 0.7)
  }

  p
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
