# Loads SMPS and CPC data for all sites into named data frames.
# Source sourceMeFirst_ufp.R before this file.
#
# Outputs: baqsCPC, baqsSMPS, maqsCPC, maqsSMPS
#
# NOTE: both baqs and maqs SMPS values are in dN/d(log Dp).
# The PN_ column prefix in baqs files is misleading — confirmed by comparing
# row sums against CPC concentrations (naive sum ~40x too high, consistent
# with summing dN/d(log Dp) without multiplying by Δ(log Dp) ≈ 0.031).


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

read_smps_files <- function(FF) {

  result <- vector("list", length(FF))

  for (ix in seq_along(FF)) {
    first_line <- readLines(FF[ix], n = 1)

    if (grepl("AIM Version", first_line, ignore.case = TRUE)) {
      # raw maqs AIM format: 52 metadata rows before column header
      df        <- read_csv(FF[ix], skip = 52, show_col_types = FALSE)
      bin_cols  <- names(df)[!is.na(suppressWarnings(as.numeric(names(df))))]
      diameters <- as.numeric(bin_cols)
      date      <- dmy_hms(df$`DateTime Sample Start`, tz = "UTC")
      data_mat  <- as.matrix(df[, bin_cols])
    } else {
      df <- read_csv(FF[ix], show_col_types = FALSE)

      if (any(grepl("^PN_", names(df)))) {
        # baqs format
        bin_cols  <- grep("^PN_", names(df), value = TRUE)
        diameters <- as.numeric(sub("^PN_", "", bin_cols))
        date      <- as.POSIXct(df[[1]], tz = "UTC")
        data_mat  <- as.matrix(df[, bin_cols])
      } else {
        # maqs ratified format: size-bin columns have purely numeric names
        bin_cols  <- names(df)[!is.na(suppressWarnings(as.numeric(names(df))))]
        diameters <- as.numeric(bin_cols)
        date_col  <- if ("datetime" %in% names(df)) df$datetime else df$date
        date      <- as.POSIXct(date_col, tz = "UTC")
        data_mat  <- as.matrix(df[, bin_cols])
      }
    }

    keep      <- diameters >= 10
    diameters <- diameters[keep]
    data_mat  <- data_mat[, keep, drop = FALSE]

    colnames(data_mat) <- as.character(diameters)
    result[[ix]] <- bind_cols(tibble(date = date), as_tibble(data_mat))
  }

  result
}


# smps_interpolate() ------------------------------------------------------
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

smps_interpolate <- function(smps_list,
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
        # new-site format (chilbolton, hop, marylebone): date is ISO UTC string
        result[[ix]] <- df %>%
          transmute(
            date       = as.POSIXct(date, tz = "UTC"),
            conc       = conc,
            instrument = "site-CPC"
          )
      }
    }
  }

  out <- bind_rows(result) %>%
          mutate(priority = if_else(instrument == "CPC-3750", 1L, 2L)) %>%
          group_by(date) %>%
          slice_min(priority, n = 1, with_ties = FALSE) %>% # selects the CPC-3750 in case there are dual observations
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
  if (length(ff) == 0)
    ff <- c(
      list.files(file.path(dir, "ratified"), pattern = csv_pattern,
                 full.names = TRUE),
      list.files(file.path(dir, "raw"),      pattern = csv_pattern,
                 full.names = TRUE)
    )
  ff
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
