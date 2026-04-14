# Each site has produced their data differently, so I will wrangle into
# a unified data frame.


# SMPS bin scale from 2025/2026 baqs files (96 bins, 1.04–964.66 nm) -----
SMPS_SCALE <- c(
  1.04, 1.11, 1.2, 1.29, 1.38, 1.49, 1.6, 1.72, 1.84, 1.98, 2.13, 2.29,
  2.46, 2.64, 2.84, 3.05, 3.28, 3.52, 3.79, 4.07, 4.37, 4.7, 5.05, 5.42,
  5.83, 6.26, 6.73, 7.23, 7.77, 8.35, 8.98, 9.65, 10.37, 11.14, 11.97,
  12.86, 13.82, 14.86, 15.96, 17.15, 18.43, 19.81, 21.29, 22.88, 24.58,
  26.42, 28.39, 30.51, 32.78, 35.23, 37.86, 40.68, 43.71, 46.98, 50.48,
  54.25, 58.29, 62.64, 67.32, 72.34, 77.74, 83.54, 89.77, 96.47, 103.66,
  111.4, 119.71, 128.64, 138.24, 148.55, 159.63, 171.54, 184.34, 198.1,
  212.88, 228.76, 245.82, 264.16, 283.87, 305.05, 327.81, 352.27, 378.55,
  406.79, 437.14, 469.76, 504.81, 542.47, 582.94, 626.43, 673.17, 723.39,
  777.37, 835.36, 897.69, 964.66
)


# read_smps_files() -------------------------------------------------------
# Reads a vector of SMPS CSV file paths and interpolates each row onto a
# common diameter scale using smooth.spline 
#
# Handles two formats automatically:
#   baqs  — columns named PN_<diam_nm>; first column is datetime (unnamed).
#            Values are particle number per bin (NOT dN/d(log Dp)).
#   maqs  — columns named as plain numeric diameters (nm); column "datetime"
#            is the timestamp; trailing summary-statistic columns are ignored.
#            Values are dN/d(log Dp).
#
# NOTE: unit difference between sites is preserved — no conversion is applied.
#       Rows with fewer than 4 valid bins are left as NA. Predictions outside
#       a file's measured size range are left as NA.
#
# Arguments:
#   file_paths  — character vector of CSV paths
#   new_scale   — numeric vector of target diameter midpoints (nm);
#                 defaults to SMPS_SCALE (2025/2026 baqs bin structure)
#   spar        — smoothing parameter passed to smooth.spline (default 0.1)

read_smps_files <- function(FF,
                            new_scale = SMPS_SCALE,
                            spar = 0.1) {

  result <- vector("list", length(FF))

  for (ix in seq_along(FF)) {
    df <- read_csv(FF[ix], show_col_types = FALSE)

    if (any(grepl("^PN_", names(df)))) {
      # baqs format
      bin_cols  <- grep("^PN_", names(df), value = TRUE)
      diameters <- as.numeric(sub("^PN_", "", bin_cols))
      date  <- as.POSIXct(df[[1]], tz = "UTC")
      data_mat  <- as.matrix(df[, bin_cols]) # matrix of aerosol data
    } else {
      # maqs format: size-bin columns have purely numeric names
      bin_cols  <- names(df)[!is.na(suppressWarnings(as.numeric(names(df))))]
      diameters <- as.numeric(bin_cols)
      date  <- as.POSIXct(df$datetime, tz = "UTC")
      data_mat  <- as.matrix(df[, bin_cols])
    }

    file_min <- min(diameters)
    file_max <- max(diameters)
    in_range <- between(new_scale,file_min,file_max) # checks that new scale
    # is appropriately within limits of the existing file.

    interp_mat <- matrix(NA_real_, nrow = nrow(data_mat), ncol = length(new_scale))
    colnames(interp_mat) <- new_scale

    for (r in seq_len(nrow(data_mat))) {
      row_vals <- as.numeric(data_mat[r, ])
      valid    <- !is.na(row_vals)
      n_valid  <- sum(valid)
      if (n_valid < 4) next

      degFree <- min(40, n_valid - 1) # note - this is degrees of freedom
      fit    <- smooth.spline(diameters[valid], row_vals[valid],
                              spar = spar, df = degFree) # 
      interp_mat[r, in_range] <- pmax(predict(fit, new_scale[in_range])$y, 0)
      #predict pulls the value from the spline function, but may be negative on
      # tails.  pmax will pick the max between 0 and the spline function
    }

    result[[ix]] <- bind_cols(
      tibble(date = date),
      as_tibble(interp_mat)
    )
  }

  bind_rows(result)
}


# plot_smps_banana() ------------------------------------------------------
# Creates a time series "banana plot" of SMPS size distributions.
# x-axis: time; y-axis: diameter (nm, log scale); fill: dN/dlogDp.
#
# The y-axis is worked in log10(diameter) space so that geom_tile heights
# fill the axis evenly regardless of bin spacing. Zeros are treated as NA
# for the colour scale (log transform cannot handle zero).
#
# Arguments:
#   smps_data   — output of read_smps_files()
#   title       — optional plot title string
#   na_colour   — colour for NA/zero cells (default "grey20")
#   clim        — length-2 numeric vector c(min, max) for the colour scale.
#                 Values outside this range are squished to the nearest limit
#                 colour (not removed). Default NULL uses data range.

plot_smps_banana <- function(smps_data,
                             title     = NULL,
                             na_colour = "grey20",
                             start     = "2024-01-01",
                             end       = "2024-12-31",
                             clim      = c(1e-5,500)) {

  # Optional time filtering
  if (!is.null(start)){ 
    smps_data <- smps_data %>% 
      filter(datetime >= as.POSIXct(start, tz = "UTC"))
  }
  if (!is.null(end)){
    smps_data <- smps_data %>% 
      filter(datetime <= as.POSIXct(end,   tz = "UTC"))
  }

  # Pivot to long, compute log10 diameter
  long <- smps_data %>%
    pivot_longer(cols      = -datetime,
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
  times  <- sort(unique(long$datetime))
  dt_sec <- median(as.numeric(diff(times), units = "secs"))

  # y-axis breaks at round diameters within the data range
  candidate_breaks <- c(1, 3, 10, 30, 100, 300, 1000)
  y_breaks <- candidate_breaks[candidate_breaks >= min(diams) &
                                  candidate_breaks <= max(diams)]

  ggplot(long, aes(x = datetime, y = log_diam, fill = conc)) +
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
# to visually verify the spline conversion.
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
  df_use   <- min(40, sum(valid) - 1)
  fit      <- smooth.spline(diameters[valid], row_vals[valid],
                            spar = spar, df = df_use)

  in_range   <- new_scale >= min(diameters) & new_scale <= max(diameters)
  pred_vals  <- rep(NA_real_, length(new_scale))
  pred_vals[in_range] <- pmax(predict(fit, new_scale[in_range])$y, 0)

  orig <- tibble(diameter = diameters[valid], value = row_vals[valid],
                 source = "original")
  interp <- tibble(diameter = new_scale[in_range], value = pred_vals[in_range],
                   source = "interpolated")

  ggplot() +
    geom_point(data = orig,   aes(x = diameter, y = value, colour = source), size = 2) +
    geom_line( data = interp, aes(x = diameter, y = value, colour = source)) +
    scale_x_log10() +
    scale_colour_manual(values = c("original" = "black", "interpolated" = "steelblue")) +
    labs(
      title = paste0("SMPS spline conversion — row ", row_index),
      subtitle = basename(file_path),
      x = "Diameter (nm)",
      y = "Concentration",
      colour = NULL
    ) +
    theme_bw()
}


# NPF EVENT DETECTION -----------------------------------------------------
# Implements the four criteria of Dal Masso et al. (2005):
#   1. A new particle mode appears in the size distribution
#   2. The mode starts within the nucleation-mode size range
#   3. The mode persists over a span of hours
#   4. The new mode shows signs of growth (diameter increases with time)
#
# Typical workflow:
#   modes_ts <- find_modes_timeseries(baqsSMPS)
#   tracks   <- link_mode_tracks(modes_ts)
#   npf      <- detect_npf_events(tracks)


# find_modes_spectrum() ---------------------------------------------------
# Identifies local maxima (modes) in a single SMPS size distribution.
# A candidate peak must be the maximum within ± half_window bins AND exceed
# min_conc. Bins are processed in diameter-sorted order.
#
# Prominence is defined topographically: peak height minus the highest
# valley floor connecting it to the nearest taller neighbour (or edge).
# This filters weak shoulders from dominant modes.
#
# Arguments:
#   diameters      — numeric vector of particle diameters (nm)
#   concentrations — numeric vector of concentrations (same length)
#   half_window    — half-width of peak-detection window in bins (default 3)
#   min_conc       — minimum peak concentration to report (default 1)
#   min_prominence — minimum topographic prominence to keep a mode (default 0)
#
# Returns: tibble(mode_diam, mode_conc, prominence); zero-row if no modes.

find_modes_spectrum <- function(diameters, concentrations,
                                half_window    = 3,
                                min_conc       = 1,
                                min_prominence = 0) {

  empty <- tibble(mode_diam = numeric(), mode_conc = numeric(),
                  prominence = numeric())

  n <- length(diameters)
  if (n != length(concentrations) || n < 2 * half_window + 1L) return(empty)

  ord <- order(diameters)
  d   <- diameters[ord]
  cc  <- concentrations[ord]
  cc[is.na(cc) | cc < 0] <- 0

  # Local maximum: strictly highest value in the symmetric window
  is_peak <- logical(n)
  for (i in seq(half_window + 1L, n - half_window)) {
    win <- cc[seq(i - half_window, i + half_window)]
    if (cc[i] == max(win) && cc[i] >= min_conc) is_peak[i] <- TRUE
  }

  peak_idx <- which(is_peak)
  if (length(peak_idx) == 0L) return(empty)

  # Topographic prominence for each peak
  prom <- vapply(peak_idx, function(i) {
    # Scan left to the nearest higher peak (or edge) to find valley floor
    left_floor <- cc[i]
    for (j in seq(i - 1L, 1L)) {
      if (cc[j] > cc[i]) break
      left_floor <- min(left_floor, cc[j])
    }
    # Same scan rightwards
    right_floor <- cc[i]
    for (j in seq(i + 1L, n)) {
      if (cc[j] > cc[i]) break
      right_floor <- min(right_floor, cc[j])
    }
    # Key col is the higher of the two valley floors
    cc[i] - max(left_floor, right_floor)
  }, numeric(1L))

  keep <- prom >= min_prominence
  tibble(mode_diam  = d[peak_idx[keep]],
         mode_conc  = cc[peak_idx[keep]],
         prominence = prom[keep])
}


# find_modes_timeseries() -------------------------------------------------
# Applies find_modes_spectrum() row-by-row to an smps_data tibble and
# returns a tidy long tibble of all mode observations.
#
# Arguments:
#   smps_data      — tibble from read_smps_files() (col 1: date; remaining
#                    cols: concentrations named by diameter in nm)
#   half_window, min_conc, min_prominence — forwarded to find_modes_spectrum()
#
# Returns: tibble(date, mode_diam, mode_conc, prominence)

find_modes_timeseries <- function(smps_data,
                                  half_window    = 3,
                                  min_conc       = 1,
                                  min_prominence = 0) {

  if (nrow(smps_data) == 0L)
    return(tibble(date = as.POSIXct(character()),
                  mode_diam = numeric(), mode_conc = numeric(),
                  prominence = numeric()))

  diameters <- as.numeric(names(smps_data)[-1])

  map_dfr(seq_len(nrow(smps_data)), function(r) {
    conc  <- as.numeric(unlist(smps_data[r, -1]))
    modes <- find_modes_spectrum(diameters, conc,
                                 half_window    = half_window,
                                 min_conc       = min_conc,
                                 min_prominence = min_prominence)
    if (nrow(modes) > 0L) modes$date <- smps_data$date[[r]]
    modes
  }) %>%
    select(date, mode_diam, mode_conc, prominence)
}


# link_mode_tracks() ------------------------------------------------------
# Groups mode observations into continuous tracks across timesteps using
# nearest-neighbour matching in log10(diameter) space.
#
# Pairs are assigned greedily in ascending cost order (closest diameter
# matches claimed first). A track is kept alive across gaps of up to
# max_gap_steps missing timesteps.
#
# Arguments:
#   modes_ts       — output of find_modes_timeseries()
#   max_log10_jump — max |Δlog10(diameter)| allowed between consecutive
#                    timesteps to continue the same track (default 0.15,
#                    corresponding to ~±40% diameter change per step)
#   max_gap_steps  — consecutive missing timesteps a track may bridge
#                    before being terminated (default 2)
#
# Returns: modes_ts with an added integer column track_id.

link_mode_tracks <- function(modes_ts,
                             max_log10_jump = 0.15,
                             max_gap_steps  = 2) {

  if (nrow(modes_ts) == 0L) return(mutate(modes_ts, track_id = integer()))

  modes_ts <- modes_ts %>%
    arrange(date, mode_diam) %>%
    mutate(track_id = NA_integer_, .ld = log10(mode_diam))

  times   <- sort(unique(modes_ts$date))
  next_id <- 1L
  # active: currently live tracks — track_id, last log10 diameter, last time index
  active  <- tibble(track_id = integer(), ld = numeric(), last_ti = integer())

  for (ti in seq_along(times)) {
    cur <- which(modes_ts$date == times[ti])
    n_c  <- length(cur)

    # Expire tracks that have been inactive beyond the gap tolerance
    active <- filter(active, ti - last_ti <= max_gap_steps)

    if (nrow(active) > 0L && n_c > 0L) {
      cur_ld <- modes_ts$.ld[cur]
      cost   <- outer(cur_ld, active$ld, function(a, b) abs(a - b))
      pairs  <- which(cost <= max_log10_jump, arr.ind = TRUE)

      if (nrow(pairs) > 0L) {
        # Process pairs in ascending cost order (greedy optimal assignment)
        pairs  <- pairs[order(cost[pairs]), , drop = FALSE]
        used_c <- rep(FALSE, n_c)
        used_a <- rep(FALSE, nrow(active))
        for (p in seq_len(nrow(pairs))) {
          ci <- pairs[p, 1L]; ai <- pairs[p, 2L]
          if (used_c[ci] || used_a[ai]) next
          modes_ts$track_id[cur[ci]] <- active$track_id[ai]
          active$ld[ai]              <- cur_ld[ci]  # update position
          active$last_ti[ai]         <- ti
          used_c[ci] <- used_a[ai]   <- TRUE
        }
      }
    }

    # Start new tracks for any unmatched modes at this timestep
    new_cur <- cur[is.na(modes_ts$track_id[cur])]
    n_new   <- length(new_cur)
    if (n_new > 0L) {
      new_ids  <- seq(next_id, next_id + n_new - 1L)
      next_id  <- next_id + n_new
      modes_ts$track_id[new_cur] <- new_ids
      active <- bind_rows(
        active,
        tibble(track_id = new_ids,
               ld       = modes_ts$.ld[new_cur],
               last_ti  = ti)
      )
    }
  }

  select(modes_ts, -.ld)
}


# detect_npf_events() -----------------------------------------------------
# Classifies each calendar day as an NPF event, undefined, or non-event
# using the four criteria of Dal Masso et al. (2005).
#
# For each day the "best candidate" track is the nucleation-mode track with
# the longest duration. It is then assessed against all four criteria:
#   Crit 1 — a mode appears in the nucleation range (diam ≤ nucl_max_diam)
#   Crit 2 — that mode's first observation is within the nucleation range
#   Crit 3 — the mode persists for ≥ min_hours
#   Crit 4 — the mode grows at ≥ min_growth_nm_hr (OLS slope, nm h⁻¹)
#
# Classification:
#   "event"     — all four criteria met
#   "undefined" — criterion 1 met but at least one of 2–4 not satisfied
#   "non-event" — no nucleation-range mode found
#
# Arguments:
#   tracks           — output of link_mode_tracks()
#   nucl_max_diam    — upper diameter (nm) of the nucleation mode (default 25)
#   min_hours        — minimum track duration in hours (default 2)
#   min_growth_nm_hr — minimum growth rate in nm h⁻¹ for criterion 4
#                      (default 0.5)
#
# Returns: tibble(date, npf_class, growth_rate_nm_hr,
#                 event_start, event_end, start_diam, end_diam)

detect_npf_events <- function(tracks,
                              nucl_max_diam    = 25,
                              min_hours        = 2,
                              min_growth_nm_hr = 0.5) {

  if (nrow(tracks) == 0L) return(tibble())

  tracks <- mutate(tracks, .day = as.Date(date))

  # Per-day per-track summary
  track_stats <- tracks %>%
    group_by(.day, track_id) %>%
    arrange(date, .by_group = TRUE) %>%
    summarise(
      t_start     = first(date),
      t_end       = last(date),
      diam_start  = first(mode_diam),
      diam_end    = last(mode_diam),
      min_diam    = min(mode_diam),
      n_steps     = n(),
      duration_hr = as.numeric(difftime(last(date), first(date), units = "hours")),
      .groups     = "drop"
    ) %>%
    mutate(
      starts_in_nucl = diam_start <= nucl_max_diam,
      touches_nucl   = min_diam   <= nucl_max_diam
    )

  # Growth rate: OLS slope of mode_diam ~ elapsed hours (tracks with ≥ 3 points
  # that touch the nucleation range)
  gr <- tracks %>%
    mutate(.day = as.Date(date)) %>%
    semi_join(filter(track_stats, touches_nucl, n_steps >= 3),
              by = c(".day", "track_id")) %>%
    group_by(.day, track_id) %>%
    arrange(date, .by_group = TRUE) %>%
    summarise(
      growth_rate_nm_hr = {
        t_hr <- as.numeric(difftime(date, first(date), units = "hours"))
        if (var(t_hr) > 0) coef(lm(mode_diam ~ t_hr))[["t_hr"]] else NA_real_
      },
      .groups = "drop"
    )

  track_stats <- left_join(track_stats, gr, by = c(".day", "track_id"))

  # Best candidate per day: nucleation-touching track with the longest duration
  best <- track_stats %>%
    filter(touches_nucl) %>%
    group_by(.day) %>%
    slice_max(duration_hr, n = 1L, with_ties = FALSE) %>%
    ungroup()

  # Classify every calendar day present in the dataset
  tibble(.day = sort(unique(tracks$.day))) %>%
    left_join(best, by = ".day") %>%
    mutate(
      crit1 = !is.na(track_id),
      crit2 = !is.na(starts_in_nucl) & starts_in_nucl,
      crit3 = !is.na(duration_hr)    & duration_hr    >= min_hours,
      crit4 = !is.na(growth_rate_nm_hr) & growth_rate_nm_hr >= min_growth_nm_hr,
      npf_class = case_when(
        crit1 & crit2 & crit3 & crit4 ~ "event",
        crit1                          ~ "undefined",
        TRUE                           ~ "non-event"
      )
    ) %>%
    select(
      date             = .day,
      npf_class,
      growth_rate_nm_hr,
      event_start      = t_start,
      event_end        = t_end,
      start_diam       = diam_start,
      end_diam         = diam_end
    )
}


# BAQS --------------------------------------------------------------------
ff_baqsCPC <- list.files(file.path(DATADIR, "baqs", "cpc"),
                         pattern = "CPC",
                         full.names = TRUE)

baqsCPC <- lapply(ff_baqsCPC, read_csv) %>%
  bind_rows()
summary(baqsCPC)


ff_baqsSMPS <- list.files(file.path(DATADIR, "baqs", "smps"),
                          pattern = "SMPS",
                          full.names = TRUE)

baqsSMPS <- read_smps_files(ff_baqsSMPS)

plot_smps_conversion(ff_baqsSMPS[4])

###########################################################################
#
#
#
#
#
#
#
#
#
#
###########################################################################
# MAQS --------------------------------------------------------------------
ff_maqsCPC <- list.files(file.path(DATADIR,"maqs", "cpc"),
                         pattern = "maqs-CPC",
                         full.names = TRUE)


maqsCPC <-lapply(ff_maqsCPC, read_csv) %>% 
  bind_rows()
summary(maqsCPC)


# I note that the MAQS SMPS data are in units of d(N) / d(log(Dp))
ff_maqsSMPS <- list.files(file.path(DATADIR, "maqs", "smps"),
                          pattern = "maqs-SMPS",
                          full.names = TRUE)

maqsSMPS <- read_smps_files(ff_maqsSMPS)



###############################################################
###
#
#
#
##

# London Honor Oak Park ---------------------------------------------------









