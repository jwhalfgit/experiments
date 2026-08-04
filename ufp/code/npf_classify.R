# NPF day-classification: a crash-safe replacement for PyNSD's manual NPF
# panel. Source sourceMeFirst_ufp.R before this file (needs smps_number_conc()
# from load_ufp.R and find_modes_spectrum() from npf_ufp.R).
#
# Design: the classification "logbook" (one CSV row per calendar day) is the
# only durable state. Every keypress in npf_classify() writes the logbook
# immediately, so a crash costs at most the day in progress — restart and
# npf_classify() resumes at the first unclassified day.
#
# Typical workflow:
#   npf_logbook_init(kensingtonSMPS, "kensington")   # seed the logbook once
#   npf_classify(kensingtonSMPS, "kensington")        # interactive loop
#   npf_refit_logbook(kensingtonSMPS, "kensington")   # batch re-trace all saved GR windows
#
# Growth-rate fitting mirrors PyNSD's drag-a-box: press "g" inside
# npf_classify(), click two opposite corners of the growth window on the
# heatmap, and the nucleation-mode peak is traced hour-by-hour within that
# box (npf_trace_mode(), a port of PyNSD's fit_modes_to_pnsd) and regressed
# against time (npf_fit_growth()). The clicked window bounds are saved to the
# logbook, so npf_refit_logbook() can redo every fit later with no clicking.
#
# plot_npf_day() uses base graphics (not ggplot) because locator() — used for
# growth-window picking — needs a base graphics device with data coordinates
# on both axes. It still writes PNGs (via the `file` argument) for
# non-interactive use, e.g. contact sheets in code/npf-analysis.R.


# Logbook schema and I/O ------------------------------------------------------

npf_logbook_path <- function(site) {
  file.path(DATADIR, site, "npf", paste0(site, "_npf_log.csv"))
}

NPF_LOG_EMPTY <- tibble(
  date          = as.Date(character()),
  site          = character(),
  n_obs         = integer(),
  auto_class    = character(),
  class         = character(),
  gr_t_start    = as.POSIXct(character(), tz = "UTC"),
  gr_t_end      = as.POSIXct(character(), tz = "UTC"),
  gr_dp_min     = numeric(),
  gr_dp_max     = numeric(),
  gr_nm_hr      = numeric(),
  gr_r2         = numeric(),
  gr_n_points   = integer(),
  notes         = character(),
  classified_at = as.POSIXct(character(), tz = "UTC")
)


# npf_logbook_read() ----------------------------------------------------------
# Reads the classification logbook for one site. Returns an empty
# zero-row tibble (NPF_LOG_EMPTY schema) if no logbook exists yet.
#
# Arguments:
#   site — site key, e.g. "kensington"

npf_logbook_read <- function(site) {
  path <- npf_logbook_path(site)
  if (!file.exists(path)) return(NPF_LOG_EMPTY)

  read_csv(path, show_col_types = FALSE, col_types = cols(
    date          = col_date(),
    site          = col_character(),
    n_obs         = col_integer(),
    auto_class    = col_character(),
    class         = col_character(),
    gr_t_start    = col_datetime(),
    gr_t_end      = col_datetime(),
    gr_dp_min     = col_double(),
    gr_dp_max     = col_double(),
    gr_nm_hr      = col_double(),
    gr_r2         = col_double(),
    gr_n_points   = col_integer(),
    notes         = col_character(),
    classified_at = col_datetime()
  ))
}


# npf_logbook_write() ---------------------------------------------------------
# Writes the logbook for one site. Writes to a temp file then renames it into
# place, so an interrupt mid-write can never leave a truncated logbook.
#
# Arguments:
#   log  — logbook tibble (NPF_LOG_EMPTY schema)
#   site — site key

npf_logbook_write <- function(log, site) {
  path <- npf_logbook_path(site)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  write_csv(arrange(log, date), tmp)
  file.rename(tmp, path)
  invisible(log)
}


# npf_logbook_init() -----------------------------------------------------------
# Seeds the logbook with every calendar day that has at least one non-NA SMPS
# row. Never overwrites an existing logbook — only appends days not yet
# present, so re-running after adding more data is safe.
#
# Arguments:
#   smps_data — tibble: date + diameter columns
#   site      — site key
#
# Returns (invisibly): the full logbook tibble after seeding

npf_logbook_init <- function(smps_data, site) {

  data_mat <- as.matrix(smps_data[, -1])
  has_data <- !apply(data_mat, 1, function(r) all(is.na(r)))

  day_counts <- tibble(date = as.Date(smps_data$date), has_data = has_data) %>%
    filter(has_data) %>%
    count(date, name = "n_obs")

  existing <- npf_logbook_read(site)
  new_days <- anti_join(day_counts, existing, by = "date")

  if (nrow(new_days) == 0) {
    message("npf_logbook_init: logbook already covers all ", nrow(day_counts),
            " day(s) with data (", site, ").")
    return(invisible(existing))
  }

  new_rows <- new_days %>%
    transmute(
      date, site = site, n_obs,
      auto_class    = NA_character_,
      class         = NA_character_,
      gr_t_start    = as.POSIXct(NA, tz = "UTC"),
      gr_t_end      = as.POSIXct(NA, tz = "UTC"),
      gr_dp_min     = NA_real_,
      gr_dp_max     = NA_real_,
      gr_nm_hr      = NA_real_,
      gr_r2         = NA_real_,
      gr_n_points   = NA_integer_,
      notes         = NA_character_,
      classified_at = as.POSIXct(NA, tz = "UTC")
    )

  log <- bind_rows(existing, new_rows)
  npf_logbook_write(log, site)
  message("npf_logbook_init: added ", nrow(new_rows), " new day(s) to ", site,
          " logbook (", nrow(log), " total).")
  invisible(log)
}


# Daily plot -------------------------------------------------------------------

# plot_npf_day() ---------------------------------------------------------------
# Base-graphics contour ("banana") plot of one day's SMPS spectrum, with a
# total-N overlay on a secondary right-hand axis. Optionally overlays a traced
# nucleation-mode (magenta points) and/or a fitted growth-rate line (red
# dashed). x/y axes are drawn in raw data coordinates (POSIXct-as-numeric,
# log10(diameter)) so that locator() clicks made on this plot can be converted
# straight back to time/diameter by npf_pick_growth_window().
#
# Arguments:
#   day_data — tibble: date + diameter columns, one calendar day
#   trace    — optional tibble(date, mode_dp) from npf_trace_mode()
#   gr_fit   — optional list(gr_nm_hr, intercept_dp) from npf_fit_growth()
#   clim     — c(min, max) colour-scale limits (#/cm3), default c(1, 1e5)
#   file     — optional PNG output path; if NULL, plots to the active device
#   title    — optional plot title
#   width, height — PNG dimensions in inches (only used when file is set)

plot_npf_day <- function(day_data, trace = NULL, gr_fit = NULL,
                         clim = c(1, 1e5), file = NULL, title = NULL,
                         width = 10, height = 5) {

  diameters <- as.numeric(names(day_data)[-1])
  ord       <- order(diameters)
  diameters <- diameters[ord]
  log_diam  <- log10(diameters)

  z <- as.matrix(day_data[, -1])[, ord, drop = FALSE]
  z[z <= 0] <- NA
  z <- pmin(pmax(z, clim[1]), clim[2])
  log_z <- log10(z)

  x <- as.numeric(day_data$date)

  draw <- function() {
    par(mar = c(4, 4.5, 3, 4.5))
    image(x, log_diam, log_z,
          col  = viridisLite::turbo(256),
          xlab = NA, ylab = "Diameter (nm)",
          axes = FALSE, zlim = log10(clim), main = title)
    box()

    axis(2, at = pretty(log_diam), labels = round(10^pretty(log_diam)))
    time_at <- pretty(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
    axis(1, at = as.numeric(time_at), labels = format(time_at, "%H:%M"))

    N <- smps_number_conc(day_data)$N
    if (any(!is.na(N))) {
      rng  <- range(log_diam)
      N99  <- quantile(N, 0.99, na.rm = TRUE)
      if (is.finite(N99) && N99 > 0) {
        Nscl <- rng[1] + pmin(pmax(N / N99, 0), 1) * diff(rng)
        lines(x, Nscl, col = "white", lwd = 2)
        axis(4, at = seq(rng[1], rng[2], length.out = 5),
             labels = round(seq(0, N99, length.out = 5)))
        mtext("N (#/cm3)", side = 4, line = 3)
      }
    }

    if (!is.null(trace) && nrow(trace) > 0)
      points(as.numeric(trace$date), log10(trace$mode_dp),
             col = "magenta", pch = 4, lwd = 2)

    if (!is.null(gr_fit) && !is.null(trace) && nrow(trace) >= 2 &&
        !is.na(gr_fit$gr_nm_hr)) {
      t0 <- as.numeric(trace$date[1])
      xr <- range(as.numeric(trace$date))
      y  <- log10(gr_fit$intercept_dp + gr_fit$gr_nm_hr * (xr - t0) / 3600)
      lines(xr, y, col = "red", lwd = 2, lty = 2)
    }
  }

  if (!is.null(file)) {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    png(file, width = width, height = height, units = "in", res = 150)
    on.exit(dev.off())
    draw()
  } else {
    draw()
  }
  invisible(NULL)
}


# Growth-rate window picking and tracing ---------------------------------------

# npf_pick_growth_window() ------------------------------------------------------
# Prompts the user to click two opposite corners of the growth window on the
# currently displayed plot_npf_day() plot (must be a live, interactive
# graphics device — locator() cannot work on a saved PNG).
#
# Returns: list(t_start, t_end, dp_min, dp_max), or NULL if the pick is
# cancelled (fewer than 2 points returned, e.g. Esc on some devices).

npf_pick_growth_window <- function(day_data) {
  message("Click two corners of the growth window on the heatmap...")
  pts <- locator(2)
  if (is.null(pts) || length(pts$x) < 2) {
    message("Growth-window pick cancelled.")
    return(NULL)
  }
  t_range  <- range(as.POSIXct(pts$x, origin = "1970-01-01", tz = "UTC"))
  dp_range <- range(10^pts$y)
  list(t_start = t_range[1], t_end = t_range[2],
       dp_min = dp_range[1], dp_max = dp_range[2])
}


# npf_trace_mode() ---------------------------------------------------------------
# Traces the dominant particle mode within a diameter/time window, hour by
# hour (or at whatever the data's native resolution is). Port of PyNSD's
# fit_modes_to_pnsd(): at each timestep the candidate peaks are found with
# find_modes_spectrum() (from npf_ufp.R); the peak closest to the previous
# timestep's peak diameter is kept, subject to a maximum diameter jump. This
# reuses the existing peak-finding logic rather than re-implementing it, at
# the cost of one difference from PyNSD: find_modes_spectrum() does not treat
# the diameter-range edge bins as candidate peaks (PyNSD's fit_modes_to_pnsd
# does), so a mode sitting exactly on the window boundary can be missed.
#
# Arguments:
#   day_data    — tibble: date + diameter columns, one calendar day
#   window      — list(t_start, t_end, dp_min, dp_max), e.g. from
#                 npf_pick_growth_window()
#   max_jump_nm — maximum allowed |Delta diameter| between consecutive
#                 timesteps to keep the same mode (default 15 nm)
#
# Returns: tibble(date, mode_dp)

npf_trace_mode <- function(day_data, window, max_jump_nm = 15) {

  diameters <- as.numeric(names(day_data)[-1])
  ord       <- order(diameters)
  diameters <- diameters[ord]

  in_time <- day_data$date >= window$t_start & day_data$date <= window$t_end
  sub     <- day_data[in_time, , drop = FALSE]
  if (nrow(sub) == 0)
    return(tibble(date = as.POSIXct(character(), tz = "UTC"), mode_dp = numeric()))

  dp_mask   <- diameters >= window$dp_min & diameters <= window$dp_max
  active_dp <- diameters[dp_mask]
  n_dp      <- length(active_dp)
  data_mat  <- as.matrix(sub[, -1])[, ord, drop = FALSE][, dp_mask, drop = FALSE]

  # find_modes_spectrum() needs >= 2*half_window+1 bins to define a local
  # maximum; shrink half_window for narrow windows, and fall back to a plain
  # argmax when there are too few bins (< 3) for any windowed search at all.
  half_window <- if (n_dp >= 5) 2L else max(1L, (n_dp - 1L) %/% 2L)

  last_dp  <- NULL
  out_date <- as.POSIXct(character(), tz = "UTC")
  out_dp   <- numeric()

  for (i in seq_len(nrow(data_mat))) {
    row <- data_mat[i, ]
    if (all(is.na(row))) next
    row[is.na(row)] <- 0
    if (max(row) <= 0) next

    if (n_dp < 3) {
      idx <- which.max(row)
      if (!is.null(last_dp) && abs(active_dp[idx] - last_dp) > max_jump_nm) next
      out_date <- c(out_date, sub$date[i])
      out_dp   <- c(out_dp, active_dp[idx])
      last_dp  <- active_dp[idx]
      next
    }

    modes <- find_modes_spectrum(active_dp, row, half_window = half_window,
                                 min_conc = 0, min_prominence = 0.05 * max(row))
    if (nrow(modes) == 0) next

    if (!is.null(last_dp)) {
      candidates <- modes[abs(modes$mode_diam - last_dp) <= max_jump_nm, ]
      if (nrow(candidates) == 0) next
      best <- candidates[which.max(candidates$mode_conc), ]
    } else {
      best <- modes[which.max(modes$mode_conc), ]
    }

    out_date <- c(out_date, sub$date[i])
    out_dp   <- c(out_dp, best$mode_diam)
    last_dp  <- best$mode_diam
  }

  tibble(date = out_date, mode_dp = out_dp)
}


# npf_fit_growth() ---------------------------------------------------------------
# Ordinary least-squares growth rate from a traced mode: mode_dp ~ elapsed
# hours since the trace's first point.
#
# Arguments:
#   trace — tibble(date, mode_dp) from npf_trace_mode()
#
# Returns: list(gr_nm_hr, r2, n_points, intercept_dp). gr_nm_hr and r2 are
#          NA if trace has fewer than 2 points.

npf_fit_growth <- function(trace) {
  n <- nrow(trace)
  if (n < 2)
    return(list(gr_nm_hr = NA_real_, r2 = NA_real_, n_points = n,
                intercept_dp = NA_real_))

  t_hr <- as.numeric(difftime(trace$date, trace$date[1], units = "hours"))
  fit  <- lm(mode_dp ~ t_hr, data = data.frame(mode_dp = trace$mode_dp, t_hr = t_hr))

  list(
    gr_nm_hr     = unname(coef(fit)[["t_hr"]]),
    r2           = summary(fit)$r.squared,
    n_points     = n,
    intercept_dp = unname(coef(fit)[["(Intercept)"]])
  )
}


# Interactive classification loop -----------------------------------------------

NPF_CLASS_KEYS <- c(n = "NPF", o = "Non-NPF", b = "Burst", u = "Undefined")

.npf_fmt_na <- function(x) if (length(x) == 0 || is.na(x)) "-" else as.character(x)

# npf_classify() -------------------------------------------------------------
# Console day-by-day classification loop. Draws each day's contour with
# plot_npf_day(), prompts for a single keypress, and writes the logbook
# immediately after every change — a crash mid-session loses at most the day
# in progress.
#
# Keys:
#   n / o / b / u — classify NPF / Non-NPF / Burst / Undefined, advance a day
#   g             — pick a growth-rate window (locator, 2 clicks), fit GR,
#                   save the window + fit to the logbook, redraw this day
#   c             — add/edit a free-text note for this day
#   p             — go back one day
#   s             — skip (leave unclassified), advance a day
#   q             — quit; logbook is already saved (every action writes it)
#
# Arguments:
#   smps_data    — tibble: date + diameter columns (full site record)
#   site         — site key (must match npf_logbook_init()'s site)
#   start, end   — optional Date/character bounds on which days to visit
#   class_filter — which days to visit, in date order:
#                    "unclassified" (default) — class == NA only; this is what
#                                    makes restart-after-crash resume automatically
#                    "classified"   — any day already classified (any class)
#                    "all"          — every day, classified or not
#                    "NPF" / "Non-NPF" / "Burst" / "Undefined" — only days
#                                    already carrying that exact class, e.g. to
#                                    review/relabel a single class
#   max_jump_nm  — forwarded to npf_trace_mode()
#   clim         — forwarded to plot_npf_day()
#
# Returns (invisibly): the logbook tibble as it stands after the session

npf_classify <- function(smps_data, site, start = NULL, end = NULL,
                         class_filter = "unclassified", max_jump_nm = 15,
                         clim = c(1, 1e5)) {

  log <- npf_logbook_read(site)
  if (nrow(log) == 0)
    stop("No logbook found for '", site, "' — run npf_logbook_init() first.")

  known_filters <- c("unclassified", "classified", "all", unname(NPF_CLASS_KEYS))
  if (!class_filter %in% known_filters)
    warning("class_filter '", class_filter, "' is not one of: ",
            paste(known_filters, collapse = ", "), " -- treating it as an ",
            "exact class match anyway.")

  days <- log$date
  if (!is.null(start)) days <- days[days >= as.Date(start)]
  if (!is.null(end))   days <- days[days <= as.Date(end)]

  class_by_day <- log$class[match(days, log$date)]
  days <- switch(class_filter,
    unclassified = days[is.na(class_by_day)],
    classified   = days[!is.na(class_by_day)],
    all          = days,
    days[!is.na(class_by_day) & class_by_day == class_filter]
  )
  days <- sort(days)

  if (length(days) == 0) {
    message("Nothing to classify (", site, ").")
    return(invisible(log))
  }

  smps_date_days <- as.Date(smps_data$date)
  i <- 1L

  repeat {
    if (i > length(days)) { message("All selected days classified."); break }
    d <- days[i]

    day_data <- smps_data[smps_date_days == d, , drop = FALSE]
    if (nrow(day_data) == 0) {
      message("No SMPS rows for ", d, " — skipping.")
      i <- i + 1L
      next
    }

    row_idx <- match(d, log$date)
    row     <- log[row_idx, ]

    trace  <- NULL
    gr_fit <- NULL
    if (!is.na(row$gr_t_start)) {
      window <- list(t_start = row$gr_t_start, t_end = row$gr_t_end,
                     dp_min = row$gr_dp_min,   dp_max = row$gr_dp_max)
      trace  <- npf_trace_mode(day_data, window, max_jump_nm = max_jump_nm)
      fit    <- npf_fit_growth(trace)
      if (!is.na(fit$gr_nm_hr)) gr_fit <- fit
    }

    n_classified <- sum(!is.na(log$class))
    plot_npf_day(day_data, trace = trace, gr_fit = gr_fit, clim = clim,
                title = sprintf("%s -- %s   [day %d/%d, classified %d/%d]",
                                 site, d, i, length(days), n_classified, nrow(log)))

    cat(sprintf("\n%s  (auto_class: %s, current class: %s)\n",
                d, .npf_fmt_na(row$auto_class), .npf_fmt_na(row$class)))
    cat("  n=NPF  o=non-NPF  b=burst  u=undefined  g=fit growth rate\n")
    cat("  c=note  p=previous day  s=skip  q=quit (already saved)\n")
    key <- tolower(trimws(readline("Key: ")))

    if (identical(key, "q")) { message("Quitting -- logbook saved."); break }
    if (identical(key, "p")) { i <- max(1L, i - 1L); next }
    if (identical(key, "s")) { i <- i + 1L; next }

    if (identical(key, "g")) {
      window <- npf_pick_growth_window(day_data)
      if (!is.null(window)) {
        trace <- npf_trace_mode(day_data, window, max_jump_nm = max_jump_nm)
        fit   <- npf_fit_growth(trace)

        log$gr_t_start[row_idx]  <- window$t_start
        log$gr_t_end[row_idx]    <- window$t_end
        log$gr_dp_min[row_idx]   <- window$dp_min
        log$gr_dp_max[row_idx]   <- window$dp_max
        log$gr_nm_hr[row_idx]    <- fit$gr_nm_hr
        log$gr_r2[row_idx]       <- fit$r2
        log$gr_n_points[row_idx] <- fit$n_points
        npf_logbook_write(log, site)
        message(sprintf("  GR = %.2f nm/h  (r2 = %.2f, n = %d)",
                        fit$gr_nm_hr, fit$r2, fit$n_points))
      }
      next   # redraw the same day with the new fit overlaid
    }

    if (identical(key, "c")) {
      note <- readline("Note: ")
      log$notes[row_idx] <- note
      npf_logbook_write(log, site)
      next
    }

    if (key %in% names(NPF_CLASS_KEYS)) {
      log$class[row_idx]         <- NPF_CLASS_KEYS[[key]]
      log$classified_at[row_idx] <- Sys.time()
      npf_logbook_write(log, site)
      i <- i + 1L
      next
    }

    message("Unrecognised key '", key, "' -- try again.")
  }

  invisible(npf_logbook_read(site))
}


# npf_refit_logbook() -------------------------------------------------------------
# Batch re-traces and re-fits every logbook row that has a saved growth
# window, without any interaction. Makes the whole set of growth-rate fits
# reproducible from the logbook alone (e.g. after tweaking max_jump_nm).
#
# Arguments:
#   smps_data   — tibble: date + diameter columns (full site record)
#   site        — site key
#   max_jump_nm — forwarded to npf_trace_mode()
#
# Returns (invisibly): the updated logbook tibble (also written to disk)

npf_refit_logbook <- function(smps_data, site, max_jump_nm = 15) {

  log <- npf_logbook_read(site)
  has_window <- !is.na(log$gr_t_start) & !is.na(log$gr_t_end) &
                !is.na(log$gr_dp_min)  & !is.na(log$gr_dp_max)
  idx <- which(has_window)

  if (length(idx) == 0) {
    message("No saved growth windows to refit (", site, ").")
    return(invisible(log))
  }

  smps_date_days <- as.Date(smps_data$date)

  for (r in idx) {
    d        <- log$date[r]
    day_data <- smps_data[smps_date_days == d, , drop = FALSE]
    if (nrow(day_data) == 0) { message("No SMPS rows for ", d, " -- skipped."); next }

    window <- list(t_start = log$gr_t_start[r], t_end = log$gr_t_end[r],
                   dp_min = log$gr_dp_min[r],   dp_max = log$gr_dp_max[r])
    trace <- npf_trace_mode(day_data, window, max_jump_nm = max_jump_nm)
    fit   <- npf_fit_growth(trace)

    log$gr_nm_hr[r]    <- fit$gr_nm_hr
    log$gr_r2[r]       <- fit$r2
    log$gr_n_points[r] <- fit$n_points
  }

  npf_logbook_write(log, site)
  message("Refit ", length(idx), " day(s) (", site, ").")
  invisible(log)
}
