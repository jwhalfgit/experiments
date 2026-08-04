# NPF (New Particle Formation) supporting physics: condensation sink,
# coagulation sink, and particle formation rate (J), ported from PyNSD
# (PyNSD/utils/calculations.py, PyNSD/physics/condensation.py).
# Source sourceMeFirst_ufp.R before this file.
#
# All functions take the standard smps_data tibble shape used throughout this
# project: a `date` (POSIXct UTC) column followed by columns named by
# diameter (nm), holding dN/d(log10 Dp) in #/cm3.
#
# Two deliberate departures from the PyNSD reference:
#   1. PyNSD carries two disagreeing condensation-sink implementations —
#      physics/condensation.py divides by dlogdp, utils/calculations.py
#      multiplies. The NPF panel itself calls the "multiply" version
#      (calc_condensation_sink), so that is the one ported here.
#   2. PyNSD integrates every bin using one scalar mean(dlogdp) for the whole
#      spectrum. Bin widths in this project's data are not perfectly uniform,
#      so bin_dlogdp() below computes a per-bin delta_logD instead (the same
#      edge-extrapolation approach used by smps_number_conc() / smps_metrics()
#      in load_ufp.R). Expect small (few %) numeric differences from PyNSD.
#
# Typical workflow for one classified day:
#   coags <- coagulation_sink(day_data)
#   cs    <- condensation_sink(day_data)
#   j     <- formation_rate(day_data, coags, gr_nm_hr = 3.4, j_min_nm = 17, j_max_nm = 25)
#   j15   <- formation_rate_j15(j$J, d_x_nm = 17, gr_nm_hr = 3.4, day_data)


# bin_dlogdp() --------------------------------------------------------------
# Per-bin Delta log10(Dp) from bin midpoints, treating bin edges as the
# geometric mean between adjacent midpoints and extrapolating the outermost
# edges symmetrically. Same approach used (inline) by smps_number_conc() and
# smps_metrics() in load_ufp.R; factored out here for reuse by the physics
# functions below.
#
# Arguments:
#   diameters — numeric vector of bin midpoint diameters (nm), in the same
#               order as the data columns they describe
#
# Returns: numeric vector, same length as diameters

bin_dlogdp <- function(diameters) {
  n <- length(diameters)
  if (n < 2) return(rep(NA_real_, n))

  logD         <- log10(diameters)
  edges        <- numeric(n + 1)
  edges[1]     <- logD[1] - (logD[2] - logD[1]) / 2
  edges[2:n]   <- (logD[seq_len(n - 1)] + logD[2:n]) / 2
  edges[n + 1] <- logD[n] + (logD[n] - logD[n - 1]) / 2
  diff(edges)
}


# coagulation_coef() ---------------------------------------------------------
# Brownian coagulation coefficient matrix K (m3/s) between every pair of
# particle diameters, following the Fuchs interpolation formula. Port of
# PyNSD's get_coagulation_coef(). Assumes spherical particles of density
# 1.83 g/cm3 (PyNSD's sulfate-proxy value, used for the mean free path /
# thermal-speed terms only — the coagulation coefficient is not sensitive to
# this choice) and air mean free path 65 nm.
#
# Arguments:
#   d_nm    — numeric vector of particle diameters (nm)
#   temp_K  — temperature (K), default 293.15
#
# Returns: n x n numeric matrix, dimnames set to as.character(d_nm)

coagulation_coef <- function(d_nm, temp_K = 293.15) {
  d   <- d_nm * 1e-9
  dij <- outer(d, d, "+")

  mfp_air <- 65e-9
  Kn      <- (2 * mfp_air) / d
  mu_air  <- 1.7e-5
  C       <- 1 + Kn * (1.257 + 0.4 * exp(-(1.10 / Kn)))
  D       <- (1.3806e-23 * temp_K * C) / (3 * pi * mu_air * d)

  particle_density_kg_m3 <- 1.83e3
  m <- ((4 / 3) * pi * (d / 2)^3) * particle_density_kg_m3
  c <- sqrt((8 * 1.3806e-23 * temp_K) / (pi * m))

  yi    <- (8 * D) / (pi * c)
  omega <- (((d + yi)^3 - (d^2 + yi^2)^(3 / 2)) / (3 * d * yi)) - d

  Dij <- outer(D, D, "+")
  cij <- sqrt(outer(c^2, c^2, "+"))
  oij <- sqrt(outer(omega^2, omega^2, "+"))

  Kc <- 4 * pi * dij * Dij
  K  <- Kc / ((dij / (dij + oij)) + (4 * Dij / (cij * dij)))

  dimnames(K) <- list(as.character(d_nm), as.character(d_nm))
  K
}


# coagulation_sink() ----------------------------------------------------------
# Coagulation sink (s-1) for every bin at every timestep: the rate at which
# particles in a given bin are scavenged by coagulation with equal-or-larger
# particles in the measured distribution. Port of PyNSD's
# calc_coagulation_sink(). Diameters are taken from column order, so
# smps_data's diameter columns must already be sorted ascending (true of
# every read_smps_files()/smps_spline() output in this project).
#
# Arguments:
#   smps_data — tibble: date + diameter columns, dN/d(log10 Dp)
#   temp_K    — temperature (K), default 293.15
#
# Returns: tibble — date + one column per diameter bin, CoagS (s-1). Rows
#          where every bin is NA return NA throughout.

coagulation_sink <- function(smps_data, temp_K = 293.15) {

  diameters <- as.numeric(names(smps_data)[-1])
  K         <- coagulation_coef(diameters, temp_K)

  # Zero out the strict upper triangle so a bin is only scavenged by bins of
  # equal or larger diameter (columns are ascending, so row >= col == larger).
  K_lower <- K
  K_lower[upper.tri(K_lower)] <- 0

  delta_logD <- bin_dlogdp(diameters)
  data_mat   <- as.matrix(smps_data[, -1])
  all_na     <- apply(data_mat, 1, function(r) all(is.na(r)))

  data_mat[is.na(data_mat)] <- 0
  N_m3      <- sweep(data_mat, 2, delta_logD, "*") * 1e6   # dN/dlogDp -> N (m-3)
  coags_mat <- N_m3 %*% K_lower

  coags_mat[all_na, ] <- NA_real_
  colnames(coags_mat) <- as.character(diameters)

  bind_cols(tibble(date = smps_data$date), as_tibble(coags_mat))
}


# condensation_sink() ---------------------------------------------------------
# Condensation sink CS (s-1): the rate at which condensable vapour is lost to
# the existing aerosol surface, via the Fuchs-Sutugin transition-regime
# correction. Vapour properties are fixed at those of sulfuric acid (H2SO4),
# following PyNSD's calc_condensation_sink(). Port of that function.
#
# Arguments:
#   smps_data — tibble: date + diameter columns, dN/d(log10 Dp)
#   temp_K    — temperature (K), default 293.15
#   press_kPa — pressure (kPa), default 101.325
#
# Returns: tibble(date, CS) — CS in s-1

condensation_sink <- function(smps_data, temp_K = 293.15, press_kPa = 101.325) {

  diameters <- as.numeric(names(smps_data)[-1])
  d_m       <- diameters * 1e-9

  mfp_air <- 65e-9
  Kn      <- (2 * mfp_air) / d_m
  betaM   <- (Kn + 1) / (1 + 1.677 * Kn + 1.333 * Kn^2)

  M_air   <- 28.965
  d_air   <- 19.7
  d_h2so4 <- 22.9 + 6.11 * 4 + 2.31 * 2
  D_vap   <- ((0.00143 * temp_K)^1.75) /
             (press_kPa * sqrt(M_air) * (d_air^(1 / 3) + d_h2so4^(1 / 3))^2)

  delta_logD <- bin_dlogdp(diameters)
  data_mat   <- as.matrix(smps_data[, -1])
  all_na     <- apply(data_mat, 1, function(r) all(is.na(r)))

  data_mat[is.na(data_mat)] <- 0
  N_m3 <- sweep(data_mat, 2, delta_logD, "*") * 1e6         # dN/dlogDp -> N (m-3)
  CS   <- 2 * pi * D_vap * rowSums(sweep(N_m3, 2, betaM * d_m, "*"))
  CS[all_na] <- NA_real_

  tibble(date = smps_data$date, CS = CS)
}


# formation_rate() ------------------------------------------------------------
# Particle formation rate J (cm-3 s-1) in a diameter window [j_min_nm,
# j_max_nm], decomposed into its three PyNSD terms: accumulation (dN/dt),
# growth out of the window (J_GR), and coagulation loss from within the
# window (J_coag). Port of PyNSD's calc_formation_rate().
#
# Unlike PyNSD (which assumes a fixed 3600 s timestep), dN/dt is computed
# from the actual elapsed time between consecutive observations, so this
# also works on sub-hourly or irregularly-spaced data.
#
# Arguments:
#   smps_data — tibble: date + diameter columns, dN/d(log10 Dp)
#   coags     — output of coagulation_sink(smps_data) — MUST be built from
#               this same smps_data object (same column order is assumed;
#               columns are matched by position, not by name, to avoid
#               floating-point diameter-label mismatches)
#   gr_nm_hr  — assumed/fitted growth rate (nm/h) used for the J_GR term
#   j_min_nm, j_max_nm — diameter bounds (nm) of the formation-rate window
#
# Returns: tibble(date, J, J_dNdt, J_GR, J_coag)

formation_rate <- function(smps_data, coags, gr_nm_hr, j_min_nm, j_max_nm) {

  diameters <- as.numeric(names(smps_data)[-1])
  j_mask    <- diameters >= j_min_nm & diameters <= j_max_nm
  if (!any(j_mask))
    stop("No SMPS bins fall within [", j_min_nm, ", ", j_max_nm, "] nm")

  delta_logD <- bin_dlogdp(diameters)
  data_mat   <- as.matrix(smps_data[, -1])

  N_j       <- sweep(data_mat[, j_mask, drop = FALSE], 2, delta_logD[j_mask], "*")
  bin_total <- rowSums(N_j, na.rm = TRUE)
  bin_total[apply(is.na(N_j), 1, all)] <- NA_real_

  # dN/dt using actual elapsed seconds between observations (not a fixed 3600 s)
  dt_sec <- c(NA_real_, as.numeric(diff(smps_data$date), units = "secs"))
  dN_dt  <- c(NA_real_, diff(bin_total)) / dt_sec

  # Weighted-mean CoagS across the window bins, weighted by each bin's share
  # of the window total (rows with bin_total <= 0 or NA get weight 0 throughout)
  N_j0    <- N_j
  N_j0[is.na(N_j0)] <- 0
  valid   <- !is.na(bin_total) & bin_total > 0
  weights <- matrix(0, nrow = nrow(N_j0), ncol = ncol(N_j0))
  weights[valid, ] <- N_j0[valid, , drop = FALSE] / bin_total[valid]

  coags_mat  <- as.matrix(coags[, -1, drop = FALSE])[, j_mask, drop = FALSE]
  mean_coags <- rowSums(weights * coags_mat, na.rm = TRUE)
  coag_term  <- mean_coags * bin_total

  gr_term <- (gr_nm_hr / (3600 * (j_max_nm - j_min_nm))) * bin_total

  j_total <- dN_dt + coag_term + gr_term

  tibble(date = smps_data$date, J = j_total, J_dNdt = dN_dt,
         J_GR = gr_term, J_coag = coag_term)
}


# coag_sink_at() --------------------------------------------------------------
# Coagulation sink (s-1) experienced by a hypothetical particle of diameter
# d_target_nm, scavenged by ambient particles of equal or larger diameter in
# the measured distribution. Used to evaluate CoagS at diameters not
# necessarily present in the SMPS bin set (e.g. 1.5 nm for the Kerminen-
# Kulmala survival equation), unlike PyNSD which snaps to the nearest
# existing bin.
#
# Arguments:
#   d_target_nm — target particle diameter (nm), scalar
#   smps_data   — tibble: date + diameter columns, dN/d(log10 Dp)
#   temp_K      — temperature (K), default 293.15
#
# Returns: tibble(date, CoagS) — CoagS in s-1

coag_sink_at <- function(d_target_nm, smps_data, temp_K = 293.15) {

  diameters <- as.numeric(names(smps_data)[-1])
  K_full    <- coagulation_coef(c(d_target_nm, diameters), temp_K)
  K_row     <- K_full[1, -1]                     # target vs. each ambient bin
  larger    <- diameters >= d_target_nm

  delta_logD <- bin_dlogdp(diameters)
  data_mat   <- as.matrix(smps_data[, -1])
  all_na     <- apply(data_mat, 1, function(r) all(is.na(r)))

  data_mat[is.na(data_mat)] <- 0
  N_m3 <- sweep(data_mat, 2, delta_logD, "*") * 1e6
  CoagS <- as.numeric(N_m3[, larger, drop = FALSE] %*% K_row[larger])
  CoagS[all_na] <- NA_real_

  tibble(date = smps_data$date, CoagS = CoagS)
}


# formation_rate_j15() --------------------------------------------------------
# Back-calculates the formation rate at 1.5 nm (J1.5) from a measured
# formation rate Jx at diameter d_x_nm, using the Kerminen-Kulmala (2002)
# survival equation. Port of PyNSD's calculate_m() / calculate_j1_5().
#
# If d_x_nm == d1_nm, J is returned unchanged (no extrapolation needed).
#
# Caveat: this is a long extrapolation whenever d_x_nm is well above 1.5 nm
# (e.g. Kensington's 16.55 nm bin floor) — treat the result with scepticism;
# see the caveats in code/npf-analysis.R.
#
# Arguments:
#   J         — numeric vector, formation rate at d_x_nm (cm-3 s-1)
#   d_x_nm    — diameter (nm) at which J was measured
#   gr_nm_hr  — assumed/fitted growth rate (nm/h)
#   smps_data — tibble: date + diameter columns, dN/d(log10 Dp) (used to
#               evaluate CoagS at d1_nm via coag_sink_at())
#   temp_K    — temperature (K), default 293.15
#   d1_nm     — target back-calculation diameter (nm), default 1.5
#
# Returns: numeric vector, J1.5 (cm-3 s-1), same length as J

formation_rate_j15 <- function(J, d_x_nm, gr_nm_hr, smps_data,
                               temp_K = 293.15, d1_nm = 1.5) {

  if (isTRUE(all.equal(d_x_nm, d1_nm))) return(J)

  coags_d1 <- coag_sink_at(d1_nm,  smps_data, temp_K)$CoagS
  coags_dx <- coag_sink_at(d_x_nm, smps_data, temp_K)$CoagS

  m <- log(coags_dx / coags_d1) / log(d_x_nm / d1_nm)
  m[m == -1] <- -0.999   # avoid a division-by-zero singularity in xi below

  xi <- (1 / (m + 1)) * ((d_x_nm / d1_nm)^(m + 1) - 1)
  J * exp(xi * d_x_nm * (coags_d1 * 3600) / gr_nm_hr)   # CoagS: s-1 -> h-1 to match GR
}


# smps_mass_conc() -------------------------------------------------------------
# Total particle number (N, #/cm3) and estimated mass concentration (assuming
# spherical particles of a fixed density) from an SMPS spectrum. Port of
# PyNSD's NPFPanel._calc_mass().
#
# Arguments:
#   smps_data       — tibble: date + diameter columns, dN/d(log10 Dp)
#   density_g_cm3   — assumed particle density (g/cm3), default 1.5 (matches
#                     DENSITY_G_CM3 in smps-pnsd.R; PyNSD itself hard-codes 1.5
#                     here even though it uses 1.83 for the coagulation kernel)
#
# Returns: tibble(date, N, mass_ug_m3)

smps_mass_conc <- function(smps_data, density_g_cm3 = 1.5) {

  diameters <- as.numeric(names(smps_data)[-1])
  delta_logD <- bin_dlogdp(diameters)
  data_mat   <- as.matrix(smps_data[, -1])
  all_na     <- apply(data_mat, 1, function(r) all(is.na(r)))

  data_mat[is.na(data_mat)] <- 0
  N_cm3 <- sweep(data_mat, 2, delta_logD, "*")     # dN/dlogDp -> N (#/cm3) per bin

  d_m           <- diameters * 1e-9
  vol_m3        <- (pi / 6) * d_m^3
  density_kg_m3 <- density_g_cm3 * 1e3

  # N_cm3 * 1e6 -> #/m3; * vol_m3 (m3/particle) * density (kg/m3) -> kg/m3; * 1e9 -> ug/m3
  mass_kg_m3 <- rowSums(sweep(N_cm3, 2, vol_m3, "*")) * 1e6 * density_kg_m3
  mass_ug_m3 <- mass_kg_m3 * 1e9

  N <- rowSums(N_cm3)
  N[all_na]          <- NA_real_
  mass_ug_m3[all_na] <- NA_real_

  tibble(date = smps_data$date, N = N, mass_ug_m3 = mass_ug_m3)
}
