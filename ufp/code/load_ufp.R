# Loads SMPS and CPC data for all sites into named data frames.
# Source sourceMeFirst_ufp.R before this file.
#
# Outputs: baqsCPC, baqsSMPS, maqsCPC, maqsSMPS
#
# NOTE: both baqs and maqs SMPS values are in dN/d(log Dp).
# The PN_ column prefix in baqs files is misleading — confirmed by comparing
# row sums against CPC concentrations (naive sum ~40x too high, consistent
# with summing dN/d(log Dp) without multiplying by Δ(log Dp) ≈ 0.031).


# CACHE_DIR ---------------------------------------------------------------
# Shared location for all cached Rds files (raw per-site SMPS/CPC caches,
# polar met cache, lognormal mode-fit caches). Previously scattered between
# ROOT and code/; consolidated here so every script references one place.
CACHE_DIR <- file.path(DATADIR, "cache")
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)


# SMPS bin scale from 2025/2026 baqs files, truncated to >= 10 nm (64 bins, 10.37–964.66 nm) -----
SMPS_SCALE <- c(
  10.37, 11.14, 11.97,
  12.86, 13.82, 14.86, 15.96, 17.15, 18.43, 19.81, 21.29, 22.88, 24.58,
  26.42, 28.39, 30.51, 32.78, 35.23, 37.86, 40.68, 43.71, 46.98, 50.48,
  54.25, 58.29, 62.64, 67.32, 72.34, 77.74, 83.54, 89.77, 96.47, 103.66,
  111.4, 119.71, 128.64, 138.24, 148.55, 159.63, 171.54, 184.34, 198.1,
  212.88, 228.76, 245.82, 264.16, 283.87, 305.05, 327.81, 352.27, 378.55,
  406.79, 437.14, 469.76, 504.81, 542.47, 582.94, 626.43, 673.17, 723.39,
  777.37, 835.36, 897.69, 964.66
)


# read_smps_files() -------------------------------------------------------
# Reads a vector of SMPS CSV file paths and returns a list of tibbles, one
# per file. Each tibble has a `date` (POSIXct UTC) column followed by columns
# named by diameter (nm) containing dN/d(log Dp) values at the original
# instrument bin centres.
#
# Handles three formats automatically:
#   baqs         — columns named PN_<diam_nm>; first column is the timestamp.
#   maqs ratified — columns named as plain numeric diameters; "datetime" or
#                   "date" is the timestamp.
#   maqs raw AIM  — 52 metadata header rows; "DateTime Sample Start" column;
#                   datetime as DD/MM/YYYY HH:MM:SS.
#
# All formats are in dN/d(log Dp) — no unit conversion needed between sites.
#
# Arguments:
#   FF — character vector of CSV paths

read_smps_files <- function(FF, TIME_AVG = NULL) {

  result <- vector("list", length(FF))

  for (ix in seq_along(FF)) {
    first_line <- readLines(FF[ix], n = 1)

    needs_conversion <- FALSE

    if (grepl("AIM Version", first_line, ignore.case = TRUE)) {
      # raw maqs AIM format: 52 metadata rows before column header — dN/d(log Dp)
      df        <- read_csv(FF[ix], skip = 52, show_col_types = FALSE)
      df        <- filter(df, `Detector Status` == "Normal Scan")
      bin_cols  <- names(df)[!is.na(suppressWarnings(as.numeric(names(df))))]
      diameters <- as.numeric(bin_cols)
      date      <- dmy_hms(df$`DateTime Sample Start`, tz = "UTC")
      data_mat  <- as.matrix(df[, bin_cols])
    } else {
      df <- read_csv(FF[ix], show_col_types = FALSE)

      if (any(grepl("^PN_", names(df)))) {
        # baqs format — dN/d(log Dp)
        bin_cols  <- grep("^PN_", names(df), value = TRUE)
        diameters <- as.numeric(sub("^PN_", "", bin_cols))
        date      <- as.POSIXct(df[[1]], tz = "UTC")
        data_mat  <- as.matrix(df[, bin_cols])
      } else {
        # Numeric bin columns, no AIM header, no PN_ prefix.
        # Sub-cases distinguished by timestamp col and extra columns:
        #   "datetime"              → maqs ratified               — dN/d(log Dp)
        #   "date", Total or TNC   → new-site (NPL/PMP/Beddows)  — dN
        #   "date", _pmp_ yr>=2005 → PMP 2005+ (no Total col)    — dN
        #   "date", no Total/TNC   → maqs with date timestamp     — dN/d(log Dp)
        bin_cols  <- names(df)[!is.na(suppressWarnings(as.numeric(names(df))))]
        diameters <- as.numeric(bin_cols)
        data_mat  <- as.matrix(df[, bin_cols])

        if ("datetime" %in% names(df)) {
          date <- as.POSIXct(df$datetime, tz = "UTC")
        } else {
          date <- as.POSIXct(df$date, tz = "UTC")
          if (any(c("Total", "TNC-SMPS") %in% names(df))) {
            needs_conversion <- TRUE
          } else if (grepl("_pmp_", basename(FF[ix]), ignore.case = TRUE)) {
            # PMP files at harwell/marylebone/kensington from 2005 onwards
            # switched from dN/d(log Dp) to raw dN counts without adding a
            # Total column. Kensington was confirmed to need the same fix as
            # harwell/marylebone by cross-checking SMPS-integrated N against
            # Kensington's independent CPC record: kensington_smps_pmp_2007/
            # 2008.csv treated as already-dN/dlogDp gave an implausible ~1% of
            # CPC total N; converting them brings the ratio to ~35-53%,
            # consistent with the rest of the record (2009+).
            yr_pmp <- as.integer(str_extract(basename(FF[ix]), "[0-9]{4}"))
            if (!is.na(yr_pmp) && yr_pmp >= 2005 &&
                grepl("^(harwell|marylebone|kensington)_", basename(FF[ix])))
              needs_conversion <- TRUE
          }
        }
      }
    }

    keep      <- diameters >= 10
    diameters <- diameters[keep]
    data_mat  <- data_mat[, keep, drop = FALSE]

    # Convert dN → dN/d(log Dp) for new-site files by dividing each bin by
    # its Δ(log Dp), derived from the bin midpoints after the >= 10 nm filter.
    if (needs_conversion) {
      logD_f         <- log10(diameters)
      n_f            <- length(logD_f)
      edges_f        <- numeric(n_f + 1)
      edges_f[1]     <- logD_f[1] - (logD_f[2] - logD_f[1]) / 2
      edges_f[2:n_f] <- (logD_f[seq_len(n_f - 1)] + logD_f[2:n_f]) / 2
      edges_f[n_f+1] <- logD_f[n_f] + (logD_f[n_f] - logD_f[n_f - 1]) / 2
      data_mat       <- sweep(data_mat, 2, diff(edges_f), "/")
    }

    colnames(data_mat) <- as.character(diameters)
    if(is.null(TIME_AVG)){
      result[[ix]] <- bind_cols(tibble(date = date), as_tibble(data_mat))
    }else{
      result[[ix]] <- bind_cols(tibble(date = date), as_tibble(data_mat)) %>% 
        mutate(date = floor_date(date, TIME_AVG)) %>% 
        group_by(date) %>% 
        summarize_all(mean,na.rm = TRUE)
    }
  }

  result
}


# smps_spline() ------------------------------------------------------
# Interpolates a list of SMPS tibbles (from read_smps_files()) onto a common
# diameter scale using smooth.spline, then row-binds into a single tibble.
#
# Rows with fewer than 4 valid bins are left as NA. Predictions outside a
# file's measured size range are left as NA.
#
# Arguments:
#   smps_list — list of tibbles from read_smps_files()
#   new_scale — numeric vector of target diameter midpoints (nm);
#               defaults to SMPS_SCALE (2025/2026 baqs bin structure)
#   spar      — smoothing parameter passed to smooth.spline (default 0.1)

smps_spline <- function(smps_list,
                             new_scale = SMPS_SCALE,
                             spar = 0.1) {

  result <- vector("list", length(smps_list))

  for (ix in seq_along(smps_list)) {
    df        <- smps_list[[ix]]
    diameters <- as.numeric(names(df)[-1])
    date      <- df$date
    data_mat  <- as.matrix(df[, -1])

    file_min <- min(diameters)
    file_max <- max(diameters)
    in_range <- between(new_scale, file_min, file_max)

    interp_mat <- matrix(NA_real_,
                         nrow = nrow(data_mat),
                         ncol = length(new_scale))
    colnames(interp_mat) <- new_scale

    for (r in seq_len(nrow(data_mat))) {
      row_vals <- as.numeric(data_mat[r, ])
      valid    <- !is.na(row_vals)
      if (sum(valid) < 4) next

      fit <- smooth.spline(diameters[valid], row_vals[valid], spar = spar)
      interp_mat[r, in_range] <- pmax(predict(fit, new_scale[in_range])$y, 0)
      # pmax(..., 0): spline predictions can be slightly negative on tails
    }

    result[[ix]] <- bind_cols(tibble(date = date), as_tibble(interp_mat))
  }

  bind_rows(result) %>%
    filter(if_any(-date, ~!is.na(.x))) %>%
    group_by(date) %>%
    summarise(across(everything(),
                     ~{ 
                       v <- .x[!is.na(.x)]; 
                       if (length(v) == 0L){ 
                         NA_real_
                       }else{ 
                         mean(v)
                       }
                       
                    }),
              .groups = "drop")
}


# read_cpc_files() --------------------------------------------------------
# Reads a vector of CPC CSV file paths and returns a standardised tibble
# with columns: date (POSIXct UTC), conc (#/cm³).
#
# Handles four formats automatically:
#   baqs          — columns: date (DD/MM/YYYY HH:MM), conc, counts.
#                   Hourly resolution.
#   maqs ratified — columns: datetime (ISO), Conc (#/cc), qc_flags.
#                   1-minute resolution. Rows with qc_flags != 1 are dropped.
#   maqs raw      — "CPC 3750 Data" first line; columns: Date (DD/MM/YYYY),
#                   Time (HH:MM:SS), 3750_Conc_(#/cc). 1-second resolution,
#                   averaged to 1-minute.
#   new sites     — columns: date (ISO UTC), conc. No QC flags.
#
# Instrument priority: when CPC-3750 and CPC-3772 share a timestamp,
# the CPC-3750 is kept.
#
# Arguments:
#   FF — character vector of CSV file paths

read_cpc_files <- function(FF) {

  result <- vector("list", length(FF))

  for(ix in seq_along(FF)){
    fname      <- basename(FF[ix])
    first_line <- readLines(FF[ix], n = 1)

    if (grepl("CPC.*Data", first_line, ignore.case = TRUE)) {
      # maqs raw format: skip the instrument-name header line
      result[[ix]] <- read_csv(FF[ix], skip = 1, show_col_types = FALSE) %>%
        transmute(
          date       = dmy_hms(paste(Date, Time), tz = "UTC"),
          conc       = `3750_Conc_(#/cc)`,
          instrument = "CPC-3750"
        ) %>%
        mutate(date = floor_date(date, "1 minute")) %>%
        group_by(date, instrument) %>%
        summarise(conc = mean(conc, na.rm = TRUE), .groups = "drop")
    } else {
      df <- read_csv(FF[ix], show_col_types = FALSE)

      if ("qc_flags" %in% names(df)) {
        # maqs ratified format
        instrument <- if (grepl("3750", fname)) "CPC-3750" else "CPC-3772"
        result[[ix]] <- df %>%
          filter(qc_flags == 1) %>%
          transmute(
            date       = as.POSIXct(datetime, tz = "UTC"),
            conc       = `Conc (#/cc)`,
            instrument = instrument
          )
      } else if ("counts" %in% names(df)) {
        # baqs format: date is DD/MM/YYYY HH:MM string
        result[[ix]] <- df %>%
          transmute(
            date       = dmy_hm(date),
            conc       = conc,
            instrument = "baqs-CPC"
          )
      } else {
        # new-site format: date is ISO UTC string.
        # ukair files take highest priority when timestamps overlap.
        instrument <- if (grepl("ukair", fname, ignore.case = TRUE)) "ukair-CPC" else "site-CPC"
        result[[ix]] <- df %>%
          transmute(
            date       = as.POSIXct(date, tz = "UTC"),
            conc       = as.numeric(conc),
            instrument = instrument
          )
      }
    }
  }

  out <- bind_rows(result) %>%
          mutate(priority = case_when(
            instrument == "ukair-CPC" ~ 1L,
            instrument == "CPC-3750"  ~ 2L,
            TRUE                      ~ 3L
          )) %>%
          group_by(date) %>%
          slice_min(priority, n = 1, with_ties = FALSE) %>%
          ungroup() %>%
          select(date, conc)
        
  
  return(out)
}


# smps_number_conc() ------------------------------------------------------
# Integrates dN/d(log Dp) across all size bins to give total particle number
# concentration, comparable to a CPC measurement.
#
# Bin widths (Δlog Dp) are derived from the diameter midpoints by treating
# bin edges as the geometric mean between adjacent midpoints, with the
# outermost edges extrapolated symmetrically.
#
# Rows where all bins are NA return NA. Rows with some valid bins are
# integrated over the available range only (partial coverage).
#
# Arguments:
#   smps_data — tibble from read_smps_files() or a hourly-averaged equivalent
#
# Returns: tibble(date, N) where N is total number concentration (#/cm³)

smps_number_conc <- function(smps_data) {

  diameters <- as.numeric(names(smps_data)[-1])

  # Bin edges in log10 space: geometric means between adjacent midpoints;
  # outermost edges extrapolated by the same half-step as the nearest pair
  logD         <- log10(diameters)
  n          <- length(logD)
  edges      <- numeric(n + 1)
  edges[1]   <- logD[1] - (logD[2]   - logD[1])   / 2
  edges[2:n] <- (logD[seq_len(n - 1)] + logD[2:n]) / 2
  edges[n + 1] <- logD[n] + (logD[n] - logD[n - 1]) / 2
  delta_logD   <- diff(edges)

  data_mat <- as.matrix(smps_data[, -1])
  all_na   <- apply(data_mat, 1, function(r){ all(is.na(r))})

  # calculate the integral by
  # 1:sweeping - by column, multiply the data by the delta_logD (change in diameter)
  # 2: summing across the rows to 
  N          <- rowSums(sweep(data_mat, 2, delta_logD, "*"), na.rm = TRUE)
  N[all_na]  <- NA_real_

  tibble(date = smps_data$date, N = N)
}


# smps_metrics() --------------------------------------------------------------
# Derives per-timestep size-mode metrics from a splined SMPS tibble.
# Bin integration follows the same delta_logD approach as smps_number_conc().
#
# Arguments:
#   smps_splined — tibble from smps_spline(): date col + numeric bin cols
#   range_nm     — optional c(min_nm, max_nm); when provided, adds a
#                  smps_ranged column integrating only bins within that range,
#                  enabling like-for-like comparison across sites with
#                  different native size ranges
#
# Returns tibble: date, smps_total (#/cm³), nuc (<30nm), acc (30–100nm),
#                 large (>100nm), modal_diam (nm), and smps_ranged if range_nm set
smps_metrics <- function(smps_splined, range_nm = NULL) {

  diameters <- as.numeric(names(smps_splined)[-1])

  logD       <- log10(diameters)
  n          <- length(logD)
  edges      <- numeric(n + 1)
  edges[1]   <- logD[1] - (logD[2] - logD[1]) / 2
  edges[2:n] <- (logD[seq_len(n - 1)] + logD[2:n]) / 2
  edges[n+1] <- logD[n] + (logD[n] - logD[n - 1]) / 2
  delta_logD <- diff(edges)

  nuc_mask   <- diameters < 30
  acc_mask   <- diameters >= 30 & diameters <= 100
  large_mask <- diameters > 100

  data_mat <- as.matrix(smps_splined[, -1])
  all_na   <- apply(data_mat, 1, function(r) all(is.na(r)))

  integrate_range <- function(mask) {
    out <- rowSums(sweep(data_mat[, mask, drop = FALSE], 2, delta_logD[mask], "*"),
                   na.rm = TRUE)
    out[all_na] <- NA_real_
    out
  }

  modal_idx  <- apply(data_mat, 1, function(r) if (all(is.na(r))) NA_integer_ else which.max(r))
  modal_diam <- ifelse(is.na(modal_idx), NA_real_, diameters[modal_idx])

  result <- tibble(
    date       = smps_splined$date,
    smps_total = integrate_range(!logical(length(diameters))),
    nuc        = integrate_range(nuc_mask),
    acc        = integrate_range(acc_mask),
    large      = integrate_range(large_mask),
    modal_diam = modal_diam
  )

  if (!is.null(range_nm)) {
    common_mask       <- diameters >= range_nm[1] & diameters <= range_nm[2]
    result$smps_ranged <- integrate_range(common_mask)
  }

  result
}


# fit_lognormal_modes() -------------------------------------------------------
# Ports PyNSD's multi-lognormal mode deconvolution (SummaryPanel) to R.
# Fits a sum of three Gaussians in log10(Dp) space to one dN/d(log Dp) spectrum,
# seeded at nucleation (15 nm), Aitken (50 nm) and accumulation (150 nm) modes.
# A Gaussian in log10(Dp) is a lognormal mode in Dp.
#
#   model:  dN/dlogDp(x) = Σ H_i · exp(-(x - mu_i)^2 / (2 sigma_i^2)),  x = log10(Dp)
#   bounds: H_i ∈ [0, 1.5·max], mu_i ∈ [log10 range], sigma_i ∈ [0.05, 0.45]
#   keep:   a fitted mode is retained only if H_i > keep_frac · max(spectrum)
#
# Solved with optim(L-BFGS-B) minimising the sum of squared residuals — the
# dependency-free analogue of scipy.optimize.curve_fit with box bounds, which is
# what PyNSD uses.
#
# Surviving modes are assigned to nucleation (<30 nm), Aitken (30–100 nm) and
# accumulation (>100 nm) bands by their fitted peak diameter; if two modes land
# in one band the taller is kept. Bands with no surviving mode return NA.
#
# Arguments:
#   dndlogdp  — numeric vector of dN/d(log Dp) for one spectrum
#   diameters — numeric vector of bin midpoint diameters (nm), same length
#   seed_dp   — initial mode diameters (nm); default c(15, 50, 150)
#   sigma_lim — c(min, max) log-width bounds; default c(0.05, 0.45)
#   keep_frac — min mode height as a fraction of the spectrum peak; default 0.02
#
# Returns a named numeric vector:
#   nuc_dp, nuc_H, ait_dp, ait_H, acc_dp, acc_H, n_modes, fit_r2

fit_lognormal_modes <- function(dndlogdp, diameters,
                                seed_dp   = c(15, 50, 150),
                                sigma_lim = c(0.05, 0.45),
                                keep_frac = 0.02) {

  empty <- c(nuc_dp = NA_real_, nuc_H = NA_real_,
             ait_dp = NA_real_, ait_H = NA_real_,
             acc_dp = NA_real_, acc_H = NA_real_,
             n_modes = NA_real_, fit_r2 = NA_real_)

  ok <- is.finite(dndlogdp) & is.finite(diameters) & dndlogdp >= 0
  if (sum(ok) < 6) return(empty)

  x     <- log10(diameters[ok])
  y     <- dndlogdp[ok]
  max_y <- max(y)
  if (max_y <= 0) return(empty)

  xmin  <- min(x); xmax <- max(x)
  dp_lo <- min(diameters[ok]) * 1.1     # keep seeds inside the measured range
  dp_hi <- max(diameters[ok]) * 0.9

  p0 <- lo <- hi <- numeric(0)
  for (dp in seed_dp) {
    dp_s <- min(max(dp, dp_lo), dp_hi)
    idx  <- which.min(abs(diameters[ok] - dp_s))
    p0 <- c(p0, y[idx] + max_y * 0.1, log10(dp_s), 0.25)
    lo <- c(lo, 0,           xmin, sigma_lim[1])
    hi <- c(hi, max_y * 1.5, xmax, sigma_lim[2])
  }

  model <- function(p, xv) {
    out <- numeric(length(xv))
    for (i in seq(1, length(p), by = 3))
      out <- out + p[i] * exp(-((xv - p[i + 1])^2) / (2 * p[i + 2]^2))
    out
  }
  sse <- function(p) sum((model(p, x) - y)^2)

  fit  <- tryCatch(
    optim(p0, sse, method = "L-BFGS-B", lower = lo, upper = hi,
          control = list(maxit = 500)),
    error = function(e) NULL
  )
  popt <- if (is.null(fit)) p0 else fit$par

  # Collect surviving modes (height above keep_frac of the spectrum peak)
  modes <- list()
  for (i in seq(1, length(popt), by = 3)) {
    H <- popt[i]; mu <- popt[i + 1]
    if (H > keep_frac * max_y)
      modes[[length(modes) + 1]] <- c(dp = 10^mu, H = H)
  }
  if (length(modes) == 0) return(empty)

  mm  <- do.call(rbind, modes)
  res <- empty
  res["n_modes"] <- nrow(mm)

  yhat   <- model(popt, x)                                   # goodness of fit
  ss_res <- sum((y - yhat)^2); ss_tot <- sum((y - mean(y))^2)
  res["fit_r2"] <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_

  band <- function(dp) if (dp < 30) "nuc" else if (dp <= 100) "ait" else "acc"
  for (r in seq_len(nrow(mm))) {
    b <- band(mm[r, "dp"])
    if (is.na(res[[paste0(b, "_H")]]) || mm[r, "H"] > res[[paste0(b, "_H")]]) {
      res[paste0(b, "_dp")] <- mm[r, "dp"]
      res[paste0(b, "_H")]  <- mm[r, "H"]
    }
  }
  res
}


# smps_mode_grouped() ---------------------------------------------------------
# Aggregates a splined SMPS spectrum tibble to one mean dN/d(log Dp) spectrum
# per grouping key, then fits lognormal modes (fit_lognormal_modes) to each mean
# spectrum. This mirrors PyNSD, which fits modes to a time-averaged distribution
# rather than to individual (noisy) scans. The grouping is arbitrary, so the same
# routine drives time-series (group by month), seasonal (group by month-of-year)
# and diurnal (group by hour-of-day) mode climatologies.
#
# Arguments:
#   smps_splined — tibble from smps_spline(): date col + numeric bin cols
#   keys         — tibble of grouping columns, one row per row of smps_splined
#                  (e.g. tibble(month = month(date)) or
#                  tibble(season = ..., hour = hour(date)))
#
# Returns tibble: the key column(s), then nuc_dp, nuc_H, ait_dp, ait_H, acc_dp,
#                 acc_H, n_modes, fit_r2, n_obs (rows averaged into the group)
smps_mode_grouped <- function(smps_splined, keys) {

  diameters <- as.numeric(names(smps_splined)[-1])
  key_names <- names(keys)

  agg <- bind_cols(keys, select(smps_splined, -date)) %>%
    group_by(across(all_of(key_names)))

  n_obs <- summarise(agg, n_obs = n(), .groups = "drop")

  spectra  <- summarise(agg, across(everything(), ~mean(.x, na.rm = TRUE)),
                        .groups = "drop")
  bin_cols <- setdiff(names(spectra), key_names)

  fits <- lapply(seq_len(nrow(spectra)), function(r) {
    vals <- as.numeric(unlist(spectra[r, bin_cols], use.names = FALSE))
    as_tibble_row(fit_lognormal_modes(vals, diameters))
  })

  bind_cols(select(spectra, all_of(key_names)), bind_rows(fits)) %>%
    left_join(n_obs, by = key_names)
}


# smps_mode_timeseries() ------------------------------------------------------
# Time-series wrapper around smps_mode_grouped(): aggregates to period-mean
# spectra (e.g. monthly) and fits modes per period.
#
# Arguments:
#   smps_splined — tibble from smps_spline(): date col + numeric bin cols
#   period       — floor_date unit for aggregation (default "month")
#
# Returns tibble: date (period start), nuc_dp, nuc_H, ait_dp, ait_H, acc_dp,
#                 acc_H, n_modes, fit_r2, n_obs (rows averaged into the period)
smps_mode_timeseries <- function(smps_splined, period = "month") {
  smps_mode_grouped(smps_splined,
                    tibble(date = floor_date(smps_splined$date, period)))
}


# find_site_files() -----------------------------------------------------------
# Locates CSV files for a site/instrument directory, preferring the main
# folder and falling back to a raw/ subfolder if the main folder has no CSVs.
#
# Arguments:
#   dir     — path to the site instrument directory (e.g. data/baqs/cpc)
#   pattern — optional regex passed to list.files (default: all CSVs)

find_site_files <- function(dir, pattern = NULL) {
  csv_pattern <- if (is.null(pattern)) "\\.csv$" else pattern
  ff <- list.files(dir, pattern = csv_pattern, full.names = TRUE)
  ff <- ff[!grepl("forPyNSD", ff, ignore.case = TRUE)]
  if (length(ff) == 0)
    ff <- c(
      list.files(file.path(dir, "ratified"), pattern = csv_pattern,
                 full.names = TRUE),
      list.files(file.path(dir, "raw"),      pattern = csv_pattern,
                 full.names = TRUE)
    )
  ff[!grepl("forPyNSD", ff, ignore.case = TRUE)]
}


# smps_bin_signature() ---------------------------------------------------------
# Reads just the header row of each SMPS CSV (cheap — no data rows parsed) and
# returns each file's diameter-bin structure. Used to decide whether a set of
# files can be combined natively (identical bins) or need harmonising onto a
# common scale via smps_spline() first — see npf_load_site() in
# code/npf-analysis.R, which calls this before combining any year's files.
# Same lightweight-read approach as smps_dataset_summary() in prep_external.R
# (AIM 52-row header skip, PN_ prefix stripping); also applies read_smps_files()'s
# own >= 10 nm filter so the comparison matches what it will actually produce.
#
# Arguments:
#   files — character vector of SMPS CSV paths
#
# Returns: tibble(file, n_bins, bin_min_nm, bin_max_nm, bin_sig) — bin_sig is a
#          literal comma-joined string of the sorted bin midpoints, so two
#          files harmonise natively iff their bin_sig is identical.

smps_bin_signature <- function(files) {
  map_dfr(files, function(f) {
    first_line <- readLines(f, n = 1)
    skip_n   <- if (grepl("AIM Version", first_line, ignore.case = TRUE)) 52L else 0L
    cols     <- names(read_csv(f, n_max = 0, skip = skip_n, show_col_types = FALSE))
    stripped <- sub("^PN_", "", cols)
    num_vals <- suppressWarnings(as.numeric(stripped))
    diams    <- sort(num_vals[!is.na(num_vals) & num_vals >= 10])

    tibble(
      file       = f,
      n_bins     = length(diams),
      bin_min_nm = if (length(diams) > 0) min(diams) else NA_real_,
      bin_max_nm = if (length(diams) > 0) max(diams) else NA_real_,
      bin_sig    = paste(diams, collapse = ",")
    )
  })
}


# tukey_filter() --------------------------------------------------------------
# Replaces values above the upper Tukey fence (Q3 + k * IQR) with NA.
# Use on integrated hourly concentrations (per site) after smps_metrics() /
# CPC averaging to remove instrument spikes that survive the per-bin cap.
# k = 3 ("far out" fence) is conservative enough to preserve genuine pollution
# events while catching artifact spikes orders of magnitude above the norm.
#
# Arguments:
#   x — numeric vector
#   k — fence multiplier (default 3)

tukey_filter <- function(x, k = 3) {
  q   <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  hi  <- q[2] + k * (q[2] - q[1])
  ifelse(x > hi, NA_real_, x)
}


# theilsen_stats() ------------------------------------------------------------
# Sen's slope + seasonal Mann-Kendall test on a monthly series, after
# subtracting the calendar-month climatological mean (de-seasonalising).
# Moved here from trends-analysis.R (2026-08-05) so other scripts (e.g.
# readme_figures.R) can reuse it without sourcing that 1300-line driver.
#
# BUG FIX (2026-08-06): trend::sens.slope() takes a plain ts() and computes
# each pairwise slope as (x[j]-x[i])/(j-i) -- i.e. it assumes every RETAINED
# sample is one regular time step from the next. Feeding it a series with
# months missing (e.g. Marylebone's ~6-year 2009-2015 SMPS outage) silently
# compacts the gap out: index positions run 1,2,3... with no record that a
# 6-year jump sits between two of them, so the fitted slope is calibrated
# against a badly wrong elapsed time. Confirmed empirically on a synthetic
# gapped series with true slope -2/unit: the compacted-index approach
# returned -4.5, more than double. This version computes each pairwise slope
# against the REAL elapsed time (in years) between the two dates instead.
# The significance test and CI rank-selection are unaffected by this bug and
# so are unchanged: trend::mk.test()'s S statistic and its variance
# (trend:::.mkScore / trend:::.varmk) depend only on the sign/order of the
# y-values and on n, never on how far apart the x's actually are. Only the
# slope values entering the median (and the CI's order statistics) needed to
# change from index-based to time-based; the CI rank-selection formula below
# is otherwise identical to trend::sens.slope()'s internal implementation
# (Sen 1968 / Gilbert 1987).
#
# Arguments:
#   df        — tibble with a `date` column (monthly) and the value column
#   value_col — character, name of the column to test
#
# Returns: tibble(slope, ci_lo, ci_hi, pval, signif) — slope and CI already
# in units per year. NA slope/CI if fewer than 12 valid months.

theilsen_stats <- function(df, value_col) {
  d <- df %>%
    mutate(mon = month(date)) %>%
    group_by(mon) %>%
    mutate(anom = .data[[value_col]] - mean(.data[[value_col]], na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(date) %>%
    filter(!is.na(anom))

  n <- nrow(d)
  if (n < 12)
    return(tibble(slope = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                  pval  = NA_real_, signif = ""))

  x       <- d$anom
  # units="days" is required, not optional -- as.numeric() on a POSIXct
  # difftime silently auto-selects a unit (secs/mins/hours/days/weeks) based
  # on the vector's smallest gap rather than its overall range, and for a
  # monthly series (smallest gap ~1 month) it picks "secs", not "days". Found
  # empirically this session: without an explicit unit, every slope here
  # came back rounding to zero (off by the ~86400 seconds/day factor).
  t_years <- as.numeric(d$date - d$date[1], units = "days") / 365.25

  # Significance: rank/order-based, so computing it on the compacted (gap-
  # free) series is valid -- it never depends on real elapsed time.
  p <- trend::mk.test(ts(x, frequency = 12))$p.value

  # Sen's slope + CI: pairwise slopes against real elapsed time, not ts()
  # index position (see BUG FIX note above).
  pairs   <- combn(n, 2)
  dy      <- x[pairs[2, ]] - x[pairs[1, ]]
  dt      <- t_years[pairs[2, ]] - t_years[pairs[1, ]]
  d_slope <- dy / dt
  b_sen   <- median(d_slope)

  k    <- length(d_slope)
  tt   <- table(x); names(tt) <- NULL
  varS <- (n * (n - 1) * (2 * n + 5) - sum(tt * (tt - 1) * (2 * tt + 5))) / 18
  Zc   <- qnorm(0.975) * sqrt(varS)
  sorted_d <- sort(d_slope)
  ci_lo <- sorted_d[max(1, round((k - Zc) / 2))]
  ci_hi <- sorted_d[min(k, round((k + Zc) / 2 + 1))]

  tibble(
    slope  = b_sen,
    ci_lo  = ci_lo,
    ci_hi  = ci_hi,
    pval   = p,
    signif = case_when(p < 0.001 ~ "***", p < 0.01 ~ "**",
                       p < 0.05  ~ "*",   TRUE      ~ "")
  )
}


# monthly_summary() ------------------------------------------------------------
# Monthly median + Q25/Q75 IQR per (site, label) group for one value column.
# Moved here from trends-analysis.R (2026-08-06) so it can be reused outside
# that driver script (e.g. code/readme_figures.R). Coverage filter: >= 50% of
# hours in the month must be present. Ribbon (IQR) is used rather than
# discrete boxplots for readability over long (~20 year) records.
#
# Arguments:
#   df               — tibble with date, site, label, and value_col columns
#   value_col        — character, name of the column to summarise
#   instrument_label — character, stamped onto every row as `instrument`
#                       (lets multiple calls be bind_rows()'d and faceted/
#                       coloured by instrument)
#
# Returns: tibble(site, label, month_date, N_median, N_q25, N_q75, n_hrs, instrument)

monthly_summary <- function(df, value_col, instrument_label) {
  df %>%
    filter(!is.na(.data[[value_col]])) %>%
    mutate(month_date = floor_date(date, "month")) %>%
    group_by(site, label, month_date) %>%
    summarise(
      N_median = median(.data[[value_col]], na.rm = TRUE),
      N_q25    = quantile(.data[[value_col]], 0.25, na.rm = TRUE),
      N_q75    = quantile(.data[[value_col]], 0.75, na.rm = TRUE),
      n_hrs    = n(),
      .groups  = "drop"
    ) %>%
    filter(n_hrs >= 0.5 * 24 * days_in_month(month_date)) %>%
    mutate(instrument = instrument_label)
}


# smps_filter_outliers() -----------------------------------------------------
# Replaces per-bin dN/d(log Dp) values that exceed a physical plausibility cap
# with NA. Apply to the raw list from read_smps_files() before spline
# interpolation so instrument spikes do not propagate into the smoothed spectrum.
#
# Arguments:
#   smps_list    — list of tibbles from read_smps_files()
#   max_dndlogdp — per-bin cap (#/cm³/log(nm)); default 1e5

smps_filter_outliers <- function(smps_list, max_dndlogdp = 1e5) {
  lapply(smps_list, function(df) {
    bin_cols <- names(df)[!is.na(suppressWarnings(as.numeric(names(df))))]
    df[bin_cols] <- lapply(df[bin_cols], function(x) ifelse(x > max_dndlogdp, NA_real_, x))
    df
  })
}


# load_raw_smps() / load_raw_cpc() --------------------------------------------
# Thin wrappers that read CSV files for one site and return unprocessed data.
# Cache the results as Rds; run processing (outlier filter, spline, metrics,
# hourly averaging) separately so filter settings can be tuned without
# re-reading all CSVs.
#
# Arguments:
#   site_info — list element from TREND_SITES (uses *_dir, *_pattern, label)
#   site_name — character site key (passed automatically by purrr::imap)
#
# Returns:
#   load_raw_smps — list of hourly-averaged SMPS tibbles (from read_smps_files)
#   load_raw_cpc  — tibble with columns date, conc (from read_cpc_files)

load_raw_smps <- function(site_info, site_name) {
  smps_dir   <- file.path(DATADIR, site_info$smps_dir)
  smps_files <- find_site_files(smps_dir, site_info$smps_pattern)
  if (length(smps_files) == 0) { message("No SMPS files: ", site_name); return(NULL) }
  message("Reading SMPS CSVs: ", site_name, " (", length(smps_files), " files)...")
  read_smps_files(smps_files, TIME_AVG = "1 hour")
}

load_raw_cpc <- function(site_info, site_name) {
  cpc_dir   <- file.path(DATADIR, site_info$cpc_dir)
  cpc_files <- find_site_files(cpc_dir, site_info$cpc_pattern)
  if (length(cpc_files) == 0) { message("No CPC files: ", site_name); return(NULL) }
  message("Reading CPC CSVs: ", site_name, " (", length(cpc_files), " files)...")
  read_cpc_files(cpc_files)
}


# write_working_csv() ---------------------------------------------------------
# Writes a processed data frame (from read_smps_files or read_cpc_files) as
# a CSV into its site folder, making the result available to external tools.
#
# Arguments:
#   data — tibble to write
#   path — destination file path (parent directory is created if absent)

write_working_csv <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_csv(data, path)
  message("wrote ", nrow(data), " rows  →  ", path)
  invisible(data)
}


# load_site_cpc() -------------------------------------------------------------
# Loads the full CPC record for one site, independent of SMPS availability.
# Use this for CPC-only trend analysis; use load_site_data() when paired
# CPC + SMPS metrics are needed.
#
# Arguments:
#   site_info — list element from TREND_SITES (uses cpc_dir, cpc_pattern, label)
#   site_name — character site key (passed automatically by purrr::imap)
#
# Returns tibble: date, cpc (#/cm³), site, label.
#         Returns NULL (with a message) if no CPC files are found.
load_site_cpc <- function(site_info, site_name) {

  cpc_dir   <- file.path(DATADIR, site_info$cpc_dir)
  cpc_files <- find_site_files(cpc_dir, site_info$cpc_pattern)

  if (length(cpc_files) == 0) { message("No CPC files: ", site_name); return(NULL) }

  message("Loading CPC ", site_name, " (", length(cpc_files), " files)...")

  read_cpc_files(cpc_files) %>%
    mutate(date = floor_date(date, "1 hour")) %>%
    group_by(date) %>%
    summarise(cpc = mean(conc, na.rm = TRUE), .groups = "drop") %>%
    mutate(site  = site_name,
           label = site_info$label)
}


# load_site_smps() ------------------------------------------------------------
# Loads the full SMPS record for one site, independent of CPC availability.
# Use this for SMPS-only trend analysis; use load_site_data() when paired
# CPC + SMPS metrics are needed.
#
# Arguments:
#   site_info    — list element from TREND_SITES (uses smps_dir, smps_pattern, label)
#   site_name    — character site key (passed automatically by purrr::imap)
#   common_range — optional c(min_nm, max_nm) passed to smps_metrics();
#                  when set, adds smps_ranged column
#
# Returns tibble: date, smps_total, smps_ranged (if common_range set),
#         nuc, acc, large, modal_diam, site, label.
#         Returns NULL (with a message) if no SMPS files are found.
load_site_smps <- function(site_info, site_name, common_range = NULL) {

  smps_dir   <- file.path(DATADIR, site_info$smps_dir)
  smps_files <- find_site_files(smps_dir, site_info$smps_pattern)

  if (length(smps_files) == 0) { message("No SMPS files: ", site_name); return(NULL) }

  message("Loading SMPS ", site_name, " (", length(smps_files), " files)...")

  read_smps_files(smps_files, TIME_AVG = "1 hour") %>%
    smps_spline() %>%
    smps_metrics(range_nm = common_range) %>%
    mutate(site  = site_name,
           label = site_info$label)
}


# load_site_data() ------------------------------------------------------------
# Loads CPC and SMPS files for one site, hourly-averages both, computes SMPS
# metrics via smps_metrics(), and inner-joins on date.
#
# Intended as an orchestrator for paired CPC+SMPS trend analysis; the
# constituent functions (read_cpc_files, read_smps_files, etc.) remain
# available for single-instrument use.
#
# Arguments:
#   site_info    — list with fields: label, cpc_dir, smps_dir,
#                  cpc_pattern (regex or NULL), smps_pattern (regex or NULL).
#                  Paths in cpc_dir / smps_dir are relative to DATADIR.
#   site_name    — character site key (passed automatically by purrr::imap)
#   common_range — optional c(min_nm, max_nm) passed to smps_metrics();
#                  when set, adds smps_ranged column for like-for-like
#                  cross-site comparison
#
# Returns tibble with columns: date, cpc, smps_total, nuc, acc, large,
#         modal_diam, cpc_smps_ratio, site, label, and smps_ranged if
#         common_range is set.
#         Returns NULL (with a message) if either CPC or SMPS files are absent.
load_site_data <- function(site_info, site_name, common_range = NULL) {

  cpc_dir  <- file.path(DATADIR, site_info$cpc_dir)
  smps_dir <- file.path(DATADIR, site_info$smps_dir)

  cpc_files  <- find_site_files(cpc_dir,  site_info$cpc_pattern)
  smps_files <- find_site_files(smps_dir, site_info$smps_pattern)

  if (length(cpc_files)  == 0) { message("No CPC files:  ", site_name); return(NULL) }
  if (length(smps_files) == 0) { message("No SMPS files: ", site_name); return(NULL) }

  message("Loading ", site_name, " (", length(cpc_files), " CPC, ",
          length(smps_files), " SMPS files)...")

  cpc <- read_cpc_files(cpc_files) %>%
    mutate(date = floor_date(date, "1 hour")) %>%
    group_by(date) %>%
    summarise(cpc = mean(conc, na.rm = TRUE), .groups = "drop")

  metrics <- read_smps_files(smps_files, TIME_AVG = "1 hour") %>%
    smps_spline() %>%
    smps_metrics(range_nm = common_range)

  inner_join(cpc, metrics, by = "date") %>%
    mutate(
      site           = site_name,
      label          = site_info$label,
      cpc_smps_ratio = cpc / smps_total
    )
}


# check_col_names() -----------------------------------------------------------
# Checks whether all CSV files in a directory share identical column names.
#
# Returns TRUE invisibly if all files match; otherwise prints a per-file
# summary of differences and returns a named list of the differing files'
# column name vectors.
#
# Arguments:
#   dir — path to the directory containing CSV files

check_col_names <- function(dir) {
  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) stop("No CSV files found in: ", dir)

  col_list <- lapply(files, function(f) {
    names(read.csv(f, nrows = 0, check.names = FALSE))
  })
  names(col_list) <- basename(files)

  ref <- col_list[[1]]
  identical_to_ref <- vapply(col_list, function(x) identical(x, ref), logical(1))

  if (all(identical_to_ref)) {
    message("All ", length(files), " files have identical column names (", length(ref), " columns).")
    return(invisible(TRUE))
  }

  n_differ <- sum(!identical_to_ref)
  message(n_differ, " of ", length(files), " files differ from the first file's column names:")
  message("  Reference: ", basename(files[1]))

  differing <- col_list[!identical_to_ref]
  for (nm in names(differing)) {
    only_in_ref  <- setdiff(ref,            differing[[nm]])
    only_in_this <- setdiff(differing[[nm]], ref)
    col_delta    <- length(differing[[nm]]) - length(ref)
    message("  ", nm,
            "  [", length(differing[[nm]]), " cols, delta=", col_delta, "]",
            if (length(only_in_ref))  paste0("  missing: ", paste(only_in_ref,  collapse = ", ")),
            if (length(only_in_this)) paste0("  extra: ",   paste(only_in_this, collapse = ", ")))
  }

  invisible(differing)
}
