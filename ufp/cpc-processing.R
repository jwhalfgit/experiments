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

plot_smps_conversion(ff_baqsSMPS)

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









