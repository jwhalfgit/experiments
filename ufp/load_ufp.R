# Loads SMPS and CPC data for all sites into named data frames.
# Source sourceMeFirst_ufp.R before this file.
#
# Outputs: baqsCPC, baqsSMPS, maqsCPC, maqsSMPS
#
# NOTE: unit difference between sites is preserved throughout —
#   baqs SMPS values are particle number per bin (NOT dN/d(log Dp))
#   maqs SMPS values are dN/d(log Dp)
# Conversion is required before combining sites.


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
# common diameter scale using smooth.spline.
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
#   FF        — character vector of CSV paths
#   new_scale — numeric vector of target diameter midpoints (nm);
#               defaults to SMPS_SCALE (2025/2026 baqs bin structure)
#   spar      — smoothing parameter passed to smooth.spline (default 0.1)

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
      date      <- as.POSIXct(df[[1]], tz = "UTC")
      data_mat  <- as.matrix(df[, bin_cols])
    } else {
      # maqs format: size-bin columns have purely numeric names
      bin_cols  <- names(df)[!is.na(suppressWarnings(as.numeric(names(df))))]
      diameters <- as.numeric(bin_cols)
      date      <- as.POSIXct(df$datetime, tz = "UTC")
      data_mat  <- as.matrix(df[, bin_cols])
    }

    file_min <- min(diameters)
    file_max <- max(diameters)
    in_range <- between(new_scale, file_min, file_max)

    interp_mat <- matrix(NA_real_, nrow = nrow(data_mat), ncol = length(new_scale))
    colnames(interp_mat) <- new_scale

    for (r in seq_len(nrow(data_mat))) {
      row_vals <- as.numeric(data_mat[r, ])
      valid    <- !is.na(row_vals)
      n_valid  <- sum(valid)
      if (n_valid < 4) next

      degFree <- min(40, n_valid - 1)
      fit     <- smooth.spline(diameters[valid], row_vals[valid],
                               spar = spar, df = degFree)
      interp_mat[r, in_range] <- pmax(predict(fit, new_scale[in_range])$y, 0)
      # pmax(..., 0): spline predictions can be slightly negative on tails
    }

    result[[ix]] <- bind_cols(
      tibble(date = date),
      as_tibble(interp_mat)
    )
  }

  bind_rows(result)
}


# BAQS (Birmingham) -------------------------------------------------------
ff_baqsCPC <- list.files(file.path(DATADIR, "baqs", "cpc"),
                         pattern = "CPC",
                         full.names = TRUE)

baqsCPC <- lapply(ff_baqsCPC, read_csv) %>%
  bind_rows()


ff_baqsSMPS <- list.files(file.path(DATADIR, "baqs", "smps"),
                          pattern = "SMPS",
                          full.names = TRUE)

baqsSMPS <- read_smps_files(ff_baqsSMPS)


# MAQS (Manchester) -------------------------------------------------------
# CPC: 1-minute resolution; two instrument models (CPC-3750, CPC-3772) with
# overlapping periods — deduplication/instrument selection not yet applied.
ff_maqsCPC <- list.files(file.path(DATADIR, "maqs", "cpc"),
                         pattern = "maqs-CPC",
                         full.names = TRUE)

maqsCPC <- lapply(ff_maqsCPC, read_csv) %>%
  bind_rows()


# SMPS: 5-minute resolution; values are dN/d(log Dp)
ff_maqsSMPS <- list.files(file.path(DATADIR, "maqs", "smps"),
                          pattern = "maqs-SMPS",
                          full.names = TRUE)

maqsSMPS <- read_smps_files(ff_maqsSMPS)


# London Honor Oak Park ---------------------------------------------------
