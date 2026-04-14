# NPF (New Particle Formation) event detection functions.
# Source sourceMeFirst_ufp.R before this file.
#
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
