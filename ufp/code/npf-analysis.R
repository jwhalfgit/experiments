# npf-analysis.R
# Driver for the R-based NPF (New Particle Formation) classification and
# physics workflow — a crash-safe replacement for PyNSD's manual NPF panel.
# See code/npf_classify.R (logbook + classify loop), code/npf_physics.R
# (CS/CoagS/J), code/npf_ufp.R (automatic pre-screen, npf_prescreen()).
#
# Scripts in this project are designed to be run interactively (see
# code/CLAUDE.md) — this one especially so, since npf_classify() reads
# keypresses from the console. Lines marked "interactive" below should be run
# one at a time in an R console, not via Rscript.
source("sourceMeFirst_ufp.R")


# ============================================================================
# Site registry
# ============================================================================
# Each entry describes how to find one site's SMPS files for NPF work.
# Kensington is first. Its own files share one bin structure throughout
# (51 bins, 16.55-604.30 nm), so it never actually needs the harmonisation
# path in npf_load_site() below -- but that path exists because other trend
# sites (harwell, marylebone, hop) genuinely do change bin structure
# mid-record (see smps_coverage_summary.csv), and this registry is meant to
# grow to include them.
SITES_NPF <- list(
  kensington = list(
    label    = "Kensington",
    smps_dir = "kensington/smps",
    # Full-year processed files (2009 onward) supersede the raw defra_pmp
    # files for the same years: kensington_smps_pmp_2009.csv covers almost
    # exactly the same period as kensington_smps_2009.csv (both span all of
    # 2009), so it is deliberately excluded here — only kensington_smps_
    # pmp_2007.csv and _pmp_2008.csv are kept, since there is no full-year
    # equivalent for those two years.
    smps_pattern = "^kensington_smps_(pmp_(2007|2008)|[0-9]{4})\\.csv$"
  )
  # Add further sites here as they're brought into the manual NPF workflow,
  # e.g.:
  # baqs       = list(label = "BAQS",          smps_dir = "baqs/smps/brean", smps_pattern = "conjoined_32cpd\\.csv"),
  # maqs       = list(label = "MAQS",          smps_dir = "maqs/smps",       smps_pattern = NULL),
  # marylebone = list(label = "Marylebone Rd", smps_dir = "marylebone/smps", smps_pattern = NULL),
  # hop        = list(label = "Honor Oak Park", smps_dir = "hop/smps",       smps_pattern = NULL)
)

# Kensington's smallest bin is 16.55 nm, so the classic 3-25 nm formation-rate
# window used by PyNSD is unreachable here; nucleation is only ever seen
# after it has already grown past ~17 nm. J1.5 (formation_rate_j15()) is
# therefore always a substantial extrapolation for this site — compute it if
# you like, but treat it with scepticism (see code/npf_physics.R).
J_MIN_NM <- 17
J_MAX_NM <- 25


# ============================================================================
# Load + cache — one calendar year at a time
# ============================================================================

# .npf_file_years() -----------------------------------------------------------
# Internal helper. Cheap per-file scan (date column only, no bin data) of
# which calendar year(s) each file contains data for. Shared by
# npf_years_available() and npf_load_site() so that loading "one year" never
# requires reading every file in a site's whole record to work out which
# ones are relevant. Handles the same three date-column formats as
# read_smps_files() (AIM header, "datetime", "date").
#
# Returns: tibble(file, year) — one row per file per year it touches

.npf_file_years <- function(files) {
  map_dfr(files, function(f) {
    first_line <- readLines(f, n = 1)
    if (grepl("AIM Version", first_line, ignore.case = TRUE)) {
      df  <- read_csv(f, skip = 52, col_select = c("DateTime Sample Start"),
                      show_col_types = FALSE)
      dts <- dmy_hms(df[[1]], tz = "UTC")
    } else {
      df  <- read_csv(f, col_select = any_of(c("date", "datetime")),
                      show_col_types = FALSE)
      col <- if ("datetime" %in% names(df)) df$datetime else df$date
      dts <- as.POSIXct(col, tz = "UTC")
    }
    tibble(file = f, year = sort(unique(year(dts))))
  })
}


# npf_years_available() -------------------------------------------------------
# Which calendar years have SMPS data for a site — the starting point for
# working through a multi-year record one year at a time.
#
# Arguments:
#   site_name — key into SITES_NPF
#
# Returns: sorted integer vector of calendar years

npf_years_available <- function(site_name) {
  site_info <- SITES_NPF[[site_name]]
  files     <- find_site_files(file.path(DATADIR, site_info$smps_dir), site_info$smps_pattern)
  if (length(files) == 0) stop("No SMPS files found for: ", site_name)
  sort(unique(.npf_file_years(files)$year))
}


# npf_load_site() ---------------------------------------------------------------
# Loads exactly one calendar year of one site's SMPS record, hourly-averaged,
# cached as Rds under CACHE_DIR. Loading one year at a time (rather than a
# site's whole multi-year record in one go) keeps peak memory bounded to a
# single year's data, which matters as more/larger sites are added.
#
# Bin harmonisation: a year's data can come from more than one source file
# (e.g. hop's 2023_jan-feb.csv + 2023_mar.csv, which switch from 51 to 122
# bins mid-year). smps_bin_signature() checks whether the files actually
# being combined agree; if they do (true for every one of Kensington's own
# files), they're combined natively with no interpolation, preserving
# nucleation-tail fidelity. If they don't, smps_spline() harmonises them onto
# the shared SMPS_SCALE and a message says so -- this never happens silently.
#
# Any exact-timestamp duplicates within the combined year (e.g. a shared
# file boundary) are resolved by averaging.
#
# Arguments:
#   site_name    — key into SITES_NPF
#   year         — single calendar year to load (integer)
#   force_reload — if TRUE, ignore any cached Rds and re-read from CSV
#
# Returns: tibble — date + diameter columns, dN/d(log10 Dp), hourly

npf_load_site <- function(site_name, year, force_reload = FALSE) {

  site_info  <- SITES_NPF[[site_name]]
  cache_path <- file.path(CACHE_DIR, paste0(site_name, "_smps_npf_", year, ".Rds"))
  obj_name   <- paste0(site_name, "_smps_npf_", year)

  if (!force_reload && file.exists(cache_path)) {
    message("Loading cached ", site_name, " ", year, " NPF SMPS data...")
    env <- new.env()
    load(cache_path, envir = env)
    return(env[[obj_name]])
  }

  all_files <- find_site_files(file.path(DATADIR, site_info$smps_dir), site_info$smps_pattern)
  if (length(all_files) == 0) stop("No SMPS files found for: ", site_name)

  file_years <- .npf_file_years(all_files)
  files      <- unique(file_years$file[file_years$year == year])
  if (length(files) == 0) stop("No SMPS data for ", site_name, " in ", year)

  message("Reading ", length(files), " SMPS file(s) for ", site_name, " ", year, "...")
  smps_list <- read_smps_files(files, TIME_AVG = "1 hour")

  sig <- smps_bin_signature(files)
  if (n_distinct(sig$bin_sig) > 1) {
    message("  bin structure differs across ", length(files), " file(s) for ",
            year, " -- harmonising onto SMPS_SCALE via smps_spline().")
    smps_data <- smps_spline(smps_list)
  } else {
    # Collapse any exact-timestamp duplicates across files (e.g. a shared
    # boundary) by averaging, rather than double-counting them.
    smps_data <- bind_rows(smps_list) %>%
      group_by(date) %>%
      summarise(across(everything(), ~{
        v <- .x[!is.na(.x)]
        if (length(v) == 0L) NA_real_ else mean(v)
      }), .groups = "drop") %>%
      arrange(date)
  }

  assign(obj_name, smps_data)
  save(list = obj_name, file = cache_path)
  message("Cached ", nrow(smps_data), " hourly rows -> ", cache_path)
  smps_data
}


# ============================================================================
# npf_summarise() — per-timestep physics for every classified day
# ============================================================================

# npf_summarise() ---------------------------------------------------------------
# Computes condensation sink, coagulation sink, formation rate (J), and mass/
# number concentration for every timestamp of every classified day in a
# site's NPF logbook, and writes the result to
# data/<site>/npf/<site>_npf_results.csv. Core columns match PyNSD's manual-
# panel export (Class, NPF_start_date, In_GR_Window, J_Window, Mode_Dp, GR, J,
# J[dNdt], J[GR], J[coag], CS, m, J1.5) so existing downstream reads of that
# format keep working; N and mass_ug_m3 are added.
#
# Processes one calendar year at a time (via npf_load_site()): classified
# days are grouped by year, and each year's SMPS tibble is loaded, used, and
# discarded before the next year is loaded, so peak memory is one year's
# data, not the whole site record.
#
# GR policy per day:
#   - a logged gr_nm_hr (from npf_classify()'s "g" fit) is used if present
#   - otherwise, "Burst" days assume 1.0 nm/h (matching PyNSD's default)
#   - otherwise (Non-NPF / Undefined with no fit), J/J[...]/m/J1.5 are left
#     NA and only CS/N/mass are reported — mirroring PyNSD's
#     auto_calculate_non_npf()
#
# Unclassified days (class == NA) are skipped entirely.
#
# Arguments:
#   site           — site key
#   j_min_nm, j_max_nm — formation-rate window (nm); defaults J_MIN_NM/J_MAX_NM
#   calc_j15       — if TRUE, also back-calculate J1.5 and the survival-
#                    equation exponent m (default FALSE — see the J1.5
#                    extrapolation caveat above)
#   burst_gr_nm_hr — assumed GR (nm/h) for "Burst" days with no logged fit
#
# Returns (invisibly): the results tibble (also written to CSV)

npf_summarise <- function(site, j_min_nm = J_MIN_NM, j_max_nm = J_MAX_NM,
                          calc_j15 = FALSE, burst_gr_nm_hr = 1.0) {

  log        <- npf_logbook_read(site)
  classified <- filter(log, !is.na(class))
  if (nrow(classified) == 0) {
    message("npf_summarise: no classified days yet (", site, ").")
    return(invisible(tibble()))
  }

  j_window_str <- sprintf("%g-%g", j_min_nm, j_max_nm)
  years        <- sort(unique(year(classified$date)))
  message("npf_summarise: ", nrow(classified), " classified day(s) across ",
          length(years), " year(s) (", site, ")...")

  results <- map_dfr(years, function(yr) {
    message("  processing ", site, " ", yr, "...")
    smps_data      <- npf_load_site(site, yr)
    smps_date_days <- as.Date(smps_data$date)
    yr_rows        <- filter(classified, year(date) == yr)

    map_dfr(seq_len(nrow(yr_rows)), function(r) {
      row      <- yr_rows[r, ]
      d        <- row$date
      day_data <- smps_data[smps_date_days == d, , drop = FALSE]
      if (nrow(day_data) == 0) return(tibble())

      cs    <- condensation_sink(day_data)
      coags <- coagulation_sink(day_data)
      mc    <- smps_mass_conc(day_data)

      has_window <- !is.na(row$gr_t_start)
      gr_to_use  <- if (!is.na(row$gr_nm_hr))              row$gr_nm_hr
                   else if (identical(row$class, "Burst")) burst_gr_nm_hr
                   else                                    NA_real_

      mode_dp   <- rep(NA_real_, nrow(day_data))
      in_window <- rep(0L, nrow(day_data))
      npf_start_date <- d

      if (has_window) {
        window <- list(t_start = row$gr_t_start, t_end = row$gr_t_end,
                       dp_min = row$gr_dp_min,   dp_max = row$gr_dp_max)
        trace  <- npf_trace_mode(day_data, window)

        in_window <- as.integer(day_data$date >= row$gr_t_start &
                                day_data$date <= row$gr_t_end)
        idx <- match(trace$date, day_data$date)
        mode_dp[idx[!is.na(idx)]] <- trace$mode_dp[!is.na(idx)]
        npf_start_date <- as.Date(row$gr_t_start)
      }

      if (!is.na(gr_to_use)) {
        jr           <- formation_rate(day_data, coags, gr_nm_hr = gr_to_use,
                                       j_min_nm = j_min_nm, j_max_nm = j_max_nm)
        j_window_out <- j_window_str
      } else {
        jr <- tibble(date = day_data$date, J = NA_real_, J_dNdt = NA_real_,
                     J_GR = NA_real_, J_coag = NA_real_)
        j_window_out <- "N/A"
      }

      if (calc_j15 && !is.na(gr_to_use)) {
        j15      <- formation_rate_j15(jr$J, j_min_nm, gr_to_use, day_data)
        coags_d1 <- coag_sink_at(1.5,      day_data)$CoagS
        coags_dx <- coag_sink_at(j_min_nm, day_data)$CoagS
        m_val    <- log(coags_dx / coags_d1) / log(j_min_nm / 1.5)
      } else {
        j15   <- rep(NA_real_, nrow(day_data))
        m_val <- rep(NA_real_, nrow(day_data))
      }

      tibble(
        date           = day_data$date,
        Class          = row$class,
        NPF_start_date = npf_start_date,
        In_GR_Window   = in_window,
        J_Window       = j_window_out,
        Mode_Dp        = mode_dp,
        GR             = gr_to_use,
        J              = jr$J,
        `J[dNdt]`      = jr$J_dNdt,
        `J[GR]`        = jr$J_GR,
        `J[coag]`      = jr$J_coag,
        CS             = cs$CS,
        m              = m_val,
        J1.5           = j15,
        N              = mc$N,
        mass_ug_m3     = mc$mass_ug_m3
      )
    })
  })

  out_path <- file.path(DATADIR, site, "npf", paste0(site, "_npf_results.csv"))
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  write_csv(results, out_path)
  message("npf_summarise: wrote ", nrow(results), " rows -> ", out_path)
  invisible(results)
}


# ============================================================================
# Summary plots -> plots/npf/<site>/
# ============================================================================

.npf_season <- function(dt) {
  case_when(
    month(dt) %in% 3:5  ~ "Spring",
    month(dt) %in% 6:8  ~ "Summer",
    month(dt) %in% 9:11 ~ "Autumn",
    TRUE                ~ "Winter"
  )
}
.NPF_SEASON_LEVELS <- c("Spring", "Summer", "Autumn", "Winter")


# npf_plot_classification_frequency() ---------------------------------------
# Stacked bar chart of classified-day counts per month or season, coloured by
# class. Split-by-period convention: one file, faceted/stacked, not one file
# per period.
#
# Arguments:
#   log  — logbook tibble, e.g. npf_logbook_read(site)
#   site — site key (used for the title and output filename)
#   by   — "month" (default) or "season"

npf_plot_classification_frequency <- function(log, site, by = c("month", "season")) {
  by      <- match.arg(by)
  classed <- filter(log, !is.na(class))
  if (nrow(classed) == 0) {
    message("npf_plot_classification_frequency: no classified days (", site, ").")
    return(invisible(NULL))
  }

  if (by == "month") {
    classed <- mutate(classed, period = floor_date(date, "month"))
  } else {
    classed <- mutate(classed,
                      period = factor(.npf_season(date), levels = .NPF_SEASON_LEVELS))
  }

  p <- classed %>%
    count(period, class) %>%
    ggplot(aes(x = period, y = n, fill = class)) +
    geom_col() +
    labs(x = NULL, y = "Days", fill = "Class",
        title = sprintf("%s -- NPF classification frequency (%d days classified)",
                        site, nrow(classed))) +
    theme_bw()

  if (by == "month")
    p <- p + scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

  out <- file.path(PLOTDIR, "npf", site, paste0(site, "_npf_frequency_", by, ".png"))
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  ggsave(out, p, width = 12, height = 5, dpi = 150, bg = "white")
  message("Saved ", out)
  invisible(p)
}


# npf_plot_diurnal_by_class() -------------------------------------------------
# Mean diurnal size-distribution contour per NPF class, one panel per class
# (facet_wrap — split-by-class convention). Each panel is annotated with the
# number of days averaged into it.
#
# Processes one calendar year at a time: each year's SMPS tibble is loaded via
# npf_load_site(), reduced immediately to a small per-(class, hour, diameter)
# running sum + count, then discarded before the next year loads. The running
# totals (not the raw per-timestamp data) are what's combined across years —
# a streaming mean, so no more than one year's SMPS tibble is ever resident
# in memory at once.
#
# Arguments:
#   site — site key
#   log  — logbook tibble, e.g. npf_logbook_read(site)

npf_plot_diurnal_by_class <- function(site, log) {
  classed <- log %>% filter(!is.na(class)) %>% select(date, class)
  if (nrow(classed) == 0) {
    message("npf_plot_diurnal_by_class: no classified days (", site, ").")
    return(invisible(NULL))
  }

  n_days <- count(classed, class, name = "n_days")
  years  <- sort(unique(year(classed$date)))

  running <- NULL
  for (yr in years) {
    yr_days <- filter(classed, year(date) == yr)
    if (nrow(yr_days) == 0) next

    message("  accumulating ", site, " ", yr, "...")
    smps_data <- npf_load_site(site, yr)

    yr_summary <- smps_data %>%
      mutate(day = as.Date(date), hour = hour(date)) %>%
      inner_join(yr_days, by = c("day" = "date")) %>%
      pivot_longer(cols = -c(date, day, hour, class),
                  names_to = "diameter", values_to = "conc") %>%
      filter(!is.na(conc)) %>%
      mutate(diameter = as.numeric(diameter)) %>%
      group_by(class, hour, diameter) %>%
      summarise(sum_conc = sum(conc), n_conc = n(), .groups = "drop")

    running <- if (is.null(running)) yr_summary else
      bind_rows(running, yr_summary) %>%
      group_by(class, hour, diameter) %>%
      summarise(sum_conc = sum(sum_conc), n_conc = sum(n_conc), .groups = "drop")
  }

  if (is.null(running)) {
    message("npf_plot_diurnal_by_class: no matching SMPS rows (", site, ").")
    return(invisible(NULL))
  }

  diurnal <- running %>%
    mutate(conc = sum_conc / n_conc) %>%
    left_join(n_days, by = "class") %>%
    mutate(class_lab = sprintf("%s (n=%d days)", class, n_days))

  p <- ggplot(diurnal, aes(x = hour, y = diameter, fill = conc)) +
    geom_tile() +
    scale_y_log10() +
    scale_fill_viridis_c(trans = "log10", option = "turbo", oob = scales::squish,
                        na.value = "grey20", name = "dN/dlogDp") +
    facet_wrap(~ class_lab) +
    labs(x = "Hour of day", y = "Diameter (nm)",
        title = paste0(site, " -- mean diurnal size distribution by NPF class")) +
    theme_bw()

  out <- file.path(PLOTDIR, "npf", site, paste0(site, "_npf_diurnal_by_class.png"))
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  ggsave(out, p, width = 12, height = 5, dpi = 150, bg = "white")
  message("Saved ", out)
  invisible(p)
}


# npf_plot_kinetics_diurnal() --------------------------------------------------
# Mean diurnal J, CS, N and mass concentration panels, split by NPF class
# (PyNSD's DiurnalSummaryWindow, as a static grid instead of a live dialog).
# Each panel column is annotated with the number of days averaged into it.
# Operates on npf_summarise()'s (already small, one-row-per-timestamp-per-
# classified-day) results tibble, so no per-year chunking is needed here.
#
# Arguments:
#   results — tibble from npf_summarise()
#   site    — site key

npf_plot_kinetics_diurnal <- function(results, site) {
  if (nrow(results) == 0) {
    message("npf_plot_kinetics_diurnal: no NPF results yet (", site, ").")
    return(invisible(NULL))
  }

  diurnal <- results %>%
    mutate(hour = hour(date)) %>%
    group_by(Class, hour) %>%
    summarise(
      n_days     = n_distinct(as.Date(date)),
      J          = mean(J, na.rm = TRUE),
      CS         = mean(CS, na.rm = TRUE),
      N          = mean(N, na.rm = TRUE),
      mass_ug_m3 = mean(mass_ug_m3, na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    mutate(class_lab = sprintf("%s (n=%d days)", Class, n_days)) %>%
    pivot_longer(cols = c(J, CS, N, mass_ug_m3), names_to = "metric", values_to = "value") %>%
    mutate(metric = factor(metric, levels = c("J", "CS", "N", "mass_ug_m3"),
                           labels = c("J (cm-3 s-1)", "CS (s-1)", "N (cm-3)", "Mass (ug/m3)")))

  p <- ggplot(diurnal, aes(x = hour, y = value)) +
    geom_line(linewidth = 0.8, colour = "steelblue") +
    facet_grid(metric ~ class_lab, scales = "free_y", switch = "y") +
    labs(x = "Hour of day", y = NULL,
        title = paste0(site, " -- mean diurnal kinetics by NPF class")) +
    theme_bw() +
    theme(strip.placement = "outside")

  out <- file.path(PLOTDIR, "npf", site, paste0(site, "_npf_kinetics_diurnal.png"))
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  ggsave(out, p, width = 12, height = 9, dpi = 150, bg = "white")
  message("Saved ", out)
  invisible(p)
}


# npf_plot_gr_j_boxplots() -----------------------------------------------------
# Seasonal boxplots of fitted growth rate (from the logbook) and formation
# rate J (from npf_summarise() results), annotated with the number of values
# behind each box.
#
# Arguments:
#   log     — logbook tibble, e.g. npf_logbook_read(site)
#   results — tibble from npf_summarise()
#   site    — site key

npf_plot_gr_j_boxplots <- function(log, results, site) {

  gr_df <- log %>%
    filter(!is.na(gr_nm_hr)) %>%
    transmute(season = factor(.npf_season(date), levels = .NPF_SEASON_LEVELS),
             value = gr_nm_hr, metric = "Growth rate (nm/h)")

  j_df <- results %>%
    filter(!is.na(J)) %>%
    transmute(season = factor(.npf_season(date), levels = .NPF_SEASON_LEVELS),
             value = J, metric = "J (cm-3 s-1)")

  df <- bind_rows(gr_df, j_df)
  if (nrow(df) == 0) {
    message("npf_plot_gr_j_boxplots: no GR/J values to plot (", site, ").")
    return(invisible(NULL))
  }

  n_lab <- df %>% count(metric, season, name = "n")

  p <- ggplot(df, aes(x = season, y = value)) +
    geom_boxplot(outlier.shape = NA, fill = "steelblue", alpha = 0.4) +
    geom_jitter(width = 0.15, alpha = 0.3, size = 0.8) +
    geom_text(data = n_lab, aes(x = season, y = -Inf, label = paste0("n=", n)),
             vjust = -0.5, size = 3, inherit.aes = FALSE) +
    facet_wrap(~ metric, scales = "free_y") +
    labs(x = NULL, y = NULL,
        title = paste0(site, " -- growth rate and formation rate by season")) +
    theme_bw()

  out <- file.path(PLOTDIR, "npf", site, paste0(site, "_npf_gr_j_boxplots.png"))
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  ggsave(out, p, width = 10, height = 5, dpi = 150, bg = "white")
  message("Saved ", out)
  invisible(p)
}


# npf_plot_coags_heatmap() -----------------------------------------------------
# Two-panel coagulation-sink / coagulation-lifetime heatmap for a single day
# (PyNSD's CoagSWindow, as a static plot).
#
# Arguments:
#   smps_data   — tibble: date + diameter columns — pass in the result of
#                 npf_load_site(site, year(target_date)), i.e. just the one
#                 year that contains target_date
#   site        — site key (used for the title and output filename)
#   target_date — Date (or coercible) — the day to plot
#   temp_K      — forwarded to coagulation_sink()

npf_plot_coags_heatmap <- function(smps_data, site, target_date, temp_K = 293.15) {

  day_data <- filter(smps_data, as.Date(date) == as.Date(target_date))
  if (nrow(day_data) == 0) {
    message("npf_plot_coags_heatmap: no SMPS rows for ", target_date, " (", site, ").")
    return(invisible(NULL))
  }

  coags <- coagulation_sink(day_data, temp_K)
  long <- coags %>%
    pivot_longer(cols = -date, names_to = "diameter", values_to = "CoagS") %>%
    mutate(diameter    = as.numeric(diameter),
          lifetime_hr = 1 / (pmax(CoagS, 1e-7) * 3600))

  p_coags <- ggplot(long, aes(x = date, y = diameter, fill = CoagS)) +
    geom_tile() + scale_y_log10() +
    scale_fill_viridis_c(trans = "log10", option = "turbo",
                        oob = scales::squish, name = "CoagS (s-1)") +
    labs(x = NULL, y = "Diameter (nm)",
        title = paste0(site, " -- ", target_date, "  Coagulation sink")) +
    theme_bw()

  p_life <- ggplot(long, aes(x = date, y = diameter, fill = lifetime_hr)) +
    geom_tile() + scale_y_log10() +
    scale_fill_viridis_c(trans = "log10", option = "turbo",
                        oob = scales::squish, name = "Lifetime (h)") +
    labs(x = NULL, y = "Diameter (nm)", title = "Coagulation lifetime") +
    theme_bw()

  p <- ggarrange(p_coags, p_life, ncol = 2)
  out <- file.path(PLOTDIR, "npf", site, paste0(site, "_coags_", target_date, ".png"))
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  ggsave(out, p, width = 14, height = 5, dpi = 150, bg = "white")
  message("Saved ", out)
  invisible(p)
}


# ============================================================================
# Kensington: end-to-end example
# ============================================================================

kensington_years <- npf_years_available("kensington")

# One year at a time: load, seed the logbook for that year's days, prescreen.
# The logbook itself is a single small file covering the whole site (one row
# per day) -- only the raw SMPS tibble is loaded and discarded year by year.
for (yr in kensington_years) {
  message("=== kensington ", yr, " ===")
  smps_yr <- npf_load_site("kensington", yr)
  npf_logbook_init(smps_yr, "kensington")
  npf_prescreen(smps_yr, "kensington")   # seeds auto_class as a triage aid
}

# --- Interactive: run one at a time in an R console, not via Rscript -------
# Classify one year at a time (loads just that year's SMPS data):
  smps_2010 <- npf_load_site("kensington", 2010)
  npf_classify(smps_2010, "kensington")
  npf_refit_logbook(smps_2010, "kensington")   # after tweaking max_jump_nm etc.

# --- Once some days are classified ------------------------------------------
# npf_summarise() and npf_plot_diurnal_by_class() load each classified year's
# data internally, one at a time -- no need to hold the whole record in memory.
kensington_npf_results <- npf_summarise("kensington", j_min_nm = J_MIN_NM, j_max_nm = J_MAX_NM)

kensington_log <- npf_logbook_read("kensington")
npf_plot_classification_frequency(kensington_log, "kensington", by = "month")
npf_plot_diurnal_by_class("kensington", kensington_log)
npf_plot_kinetics_diurnal(kensington_npf_results, "kensington")
npf_plot_gr_j_boxplots(kensington_log, kensington_npf_results, "kensington")

# Pick any classified NPF day to inspect its coagulation sink in detail
# (load just the year that contains it):
# smps_2020 <- npf_load_site("kensington", 2020)
# npf_plot_coags_heatmap(smps_2020, "kensington", as.Date("2020-04-12"))


# ============================================================================
# BAQS
# ============================================================================

kensington_years <- npf_years_available("kensington")

# One year at a time: load, seed the logbook for that year's days, prescreen.
# The logbook itself is a single small file covering the whole site (one row
# per day) -- only the raw SMPS tibble is loaded and discarded year by year.
for (yr in kensington_years) {
  message("=== kensington ", yr, " ===")
  smps_yr <- npf_load_site("kensington", yr)
  npf_logbook_init(smps_yr, "kensington")
  npf_prescreen(smps_yr, "kensington")   # seeds auto_class as a triage aid
}

# --- Interactive: run one at a time in an R console, not via Rscript -------
# Classify one year at a time (loads just that year's SMPS data):
smps_2010 <- npf_load_site("kensington", 2010)
npf_classify(smps_2010, "kensington")
npf_refit_logbook(smps_2010, "kensington")   # after tweaking max_jump_nm etc.

# --- Once some days are classified ------------------------------------------
# npf_summarise() and npf_plot_diurnal_by_class() load each classified year's
# data internally, one at a time -- no need to hold the whole record in memory.
kensington_npf_results <- npf_summarise("kensington", j_min_nm = J_MIN_NM, j_max_nm = J_MAX_NM)

kensington_log <- npf_logbook_read("kensington")
npf_plot_classification_frequency(kensington_log, "kensington", by = "month")
npf_plot_diurnal_by_class("kensington", kensington_log)
npf_plot_kinetics_diurnal(kensington_npf_results, "kensington")
npf_plot_gr_j_boxplots(kensington_log, kensington_npf_results, "kensington")

# Pick any classified NPF day to inspect its coagulation sink in detail
# (load just the year that contains it):
# smps_2020 <- npf_load_site("kensington", 2020)
# npf_plot_coags_heatmap(smps_2020, "kensington", as.Date("2020-04-12"))

