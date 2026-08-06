# readme_figures.R
# Regenerates the three figures embedded in README.md into ROOT/figures/.
# Standalone and side-effect-free: reuses existing library functions
# (load_ufp.R, npf_classify.R) rather than sourcing the full analysis
# drivers (trends-analysis.R, npf-analysis.R), which run a complete
# multi-hour analysis as a side effect of being sourced.
#
# Source sourceMeFirst_ufp.R before running this script.

source("sourceMeFirst_ufp.R")

FIGDIR <- file.path(ROOT, "figures")
dir.create(FIGDIR, recursive = TRUE, showWarnings = FALSE)


# =============================================================================
# Figure 1 — measurement inventory (CPC + SMPS coverage timeline)
# =============================================================================
# Sourced entirely from the two coverage-audit CSVs already written by
# cpc-analysis.R / trends-analysis.R to ROOT. Segment-building logic (run
# collapsing across gaps > 90 days, instrument-generation colour bands)
# mirrors trends-analysis.R's data-availability section, with two fixes:
# per-segment geom_label bin annotations are dropped (they overplot into an
# unreadable smear on the MAQS row) and sites are ordered by record start
# rather than reverse-alphabetically.

message("Figure 1: data coverage timeline...")

cpc_cov <- read_csv(file.path(ROOT, "cpc_coverage_summary.csv"), show_col_types = FALSE) %>%
  mutate(instrument = "CPC")

smps_cov <- read_csv(file.path(ROOT, "smps_coverage_summary.csv"), show_col_types = FALSE) %>%
  mutate(
    source = case_when(
      n_bins <= 55  ~ "~51 bins (PMP)",
      n_bins <= 70  ~ "~64 bins (BAQS legacy)",
      n_bins <= 115 ~ "~100 bins (MAQS)",
      TRUE          ~ "120+ bins (modern)"
    ),
    instrument = "SMPS"
  )

cov_all <- bind_rows(
  cpc_cov  %>% select(site, source, date_start, date_end, instrument),
  smps_cov %>% select(site, source, date_start, date_end, instrument)
)

# Site display order: earliest record first, within each instrument panel.
site_order <- cov_all %>%
  group_by(site) %>%
  summarise(first_date = min(date_start), .groups = "drop") %>%
  arrange(first_date) %>%
  pull(site)

# Collapse into runs: same source, gap <= 90 days, per site x instrument.
cov_runs <- cov_all %>%
  arrange(instrument, site, source, date_start) %>%
  group_by(instrument, site, source) %>%
  mutate(
    gap_days = as.numeric(difftime(date_start, lag(date_end, default = date_start[1]),
                                   units = "days")),
    period   = cumsum(gap_days > 90)
  ) %>%
  group_by(instrument, site, source, period) %>%
  summarise(date_start = min(date_start), date_end = max(date_end), .groups = "drop") %>%
  mutate(site = factor(site, levels = site_order))

SOURCE_LEVELS <- c("PMP", "Beddows (AURN)", "UK Air", "NPL",
                   "Site (BAQS)", "Site (MAQS)",
                   "~51 bins (PMP)", "~64 bins (BAQS legacy)",
                   "~100 bins (MAQS)", "120+ bins (modern)")
cov_runs <- cov_runs %>% mutate(source = factor(source, levels = SOURCE_LEVELS))

p_coverage <- ggplot(cov_runs, aes(y = site, colour = source, fill = source)) +
  geom_segment(aes(x = date_start, xend = date_end, yend = site),
               linewidth = 5, lineend = "butt", alpha = 0.85) +
  facet_grid(rows = vars(instrument), scales = "free_y", space = "free_y") +
  scale_colour_brewer(palette = "Paired", drop = FALSE) +
  scale_fill_brewer(palette = "Paired", drop = FALSE) +
  labs(x = NULL, y = NULL, colour = "Source / instrument generation",
       fill = "Source / instrument generation",
       title = "UK ultrafine particle monitoring: data coverage, 1998–2025") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave(file.path(FIGDIR, "fig1_data_coverage.png"), p_coverage,
       width = 12, height = 8, dpi = 150)
message("  saved fig1_data_coverage.png")


# =============================================================================
# Figure 2 — long-term CPC trend with Theil-Sen slopes
# =============================================================================
# Six sites with SMPS+CPC records (TREND_SITES registry, copied from
# trends-analysis.R). Loads each site's cached raw CPC pull
# (data/cache/<site>_cpc_raw.Rds, written by load_raw_cpc()), applies the
# same QC as trends-analysis.R (Tukey far-out fence, >=50%-of-month coverage),
# and fits theilsen_stats() (load_ufp.R) per site on monthly medians.

message("Figure 2: long-term CPC trend...")

TREND_SITES_LABELS <- c(
  baqs       = "Birmingham (BAQS)",
  maqs       = "Manchester (MAQS)",
  harwell    = "Harwell",
  hop        = "London Honor Oak Park",
  marylebone = "London Marylebone Rd",
  chilbolton = "Chilbolton"
)

cpc_sites <- imap(TREND_SITES_LABELS, function(label, site_name) {
  cache_path <- file.path(CACHE_DIR, paste0(site_name, "_cpc_raw.Rds"))
  if (!file.exists(cache_path)) {
    message("  no cached CPC data for ", site_name, " -- skipping (run trends-analysis.R Stage 1 first)")
    return(NULL)
  }
  readRDS(cache_path) %>%
    mutate(date = floor_date(date, "1 hour")) %>%
    group_by(date) %>%
    summarise(cpc = mean(conc, na.rm = TRUE), .groups = "drop") %>%
    mutate(site = site_name, label = label)
}) %>% compact() %>% bind_rows()

cpc_sites <- cpc_sites %>%
  group_by(site) %>%
  mutate(cpc = tukey_filter(cpc, k = 3)) %>%
  ungroup()

cpc_monthly <- cpc_sites %>%
  mutate(month_date = floor_date(date, "month")) %>%
  group_by(site, label, month_date) %>%
  summarise(cpc_median = median(cpc, na.rm = TRUE), n_hrs = n(), .groups = "drop") %>%
  filter(n_hrs >= 0.5 * 24 * days_in_month(month_date)) %>%
  group_by(site, label) %>%
  complete(month_date = seq(min(month_date), max(month_date), by = "month")) %>%
  ungroup()

ts_stats <- cpc_monthly %>%
  filter(!is.na(cpc_median)) %>%
  rename(date = month_date) %>%
  group_by(site, label) %>%
  group_modify(~theilsen_stats(.x, "cpc_median")) %>%
  ungroup() %>%
  filter(!is.na(slope)) %>%
  mutate(stat_text = sprintf("%.0f [%.0f, %.0f] #/cm³/yr %s", slope, ci_lo, ci_hi, signif))

trend_segs <- cpc_monthly %>%
  filter(!is.na(cpc_median)) %>%
  group_by(site, label) %>%
  summarise(t_min = min(month_date), t_max = max(month_date),
            y_med = median(cpc_median), t_med = median(month_date), .groups = "drop") %>%
  left_join(ts_stats %>% select(site, label, slope), by = c("site", "label")) %>%
  filter(!is.na(slope)) %>%
  mutate(
    yr_min  = as.numeric(t_min - t_med, units = "days") / 365.25,
    yr_max  = as.numeric(t_max - t_med, units = "days") / 365.25,
    y_start = y_med + slope * yr_min,
    y_end   = y_med + slope * yr_max
  )

label_pos <- ts_stats %>% select(site, label, stat_text)

p_trend <- ggplot(cpc_monthly %>% filter(!is.na(cpc_median)),
                  aes(x = month_date, y = cpc_median)) +
  geom_point(size = 0.7, colour = "steelblue3", alpha = 0.6) +
  geom_segment(data = trend_segs,
               aes(x = t_min, xend = t_max, y = y_start, yend = y_end),
               colour = "red", linewidth = 1, inherit.aes = FALSE) +
  geom_text(data = label_pos, aes(x = -Inf, y = Inf, label = stat_text),
            inherit.aes = FALSE, hjust = -0.05, vjust = 1.5, size = 3, colour = "black") +
  facet_wrap(~label, scales = "free_y") +
  labs(x = NULL, y = "Monthly median CPC (#/cm³)",
       title = "Long-term ultrafine particle number trend (de-seasonalised Theil-Sen)",
       caption = "Points: monthly median (≥50% hourly coverage). Red: Sen's slope. Seasonal Mann-Kendall significance: * p<0.05, ** p<0.01, *** p<0.001.") +
  theme_bw(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))

ggsave(file.path(FIGDIR, "fig2_longterm_trend.png"), p_trend,
       width = 12, height = 8, dpi = 150)
message("  saved fig2_longterm_trend.png")


# =============================================================================
# Figure 3 — new-particle-formation event day
# =============================================================================
# Kensington, 2010-05-23: classified NPF in the logbook, growth rate 5.6 nm/hr
# fitted over 8 traced points (r^2 = 0.90), 21-83 nm. Reconstructs the fit
# from the saved logbook window exactly as npf_refit_logbook() does, without
# sourcing npf-analysis.R (which runs a full multi-year analysis on load).
#
# Fallback candidates if this day doesn't render well: Kensington 2010-06-20
# (3.06 nm/hr, r^2 = 0.92) or BAQS 2023-04-25 (1.94 nm/hr, r^2 = 0.96).

message("Figure 3: NPF event day...")

NPF_SITE <- "kensington"
NPF_DATE <- as.Date("2010-05-23")
NPF_YEAR <- year(NPF_DATE)

npf_cache <- file.path(CACHE_DIR, paste0(NPF_SITE, "_smps_npf_", NPF_YEAR, ".Rds"))
if (!file.exists(npf_cache)) {
  stop("Missing NPF SMPS cache: ", npf_cache,
       "\nGenerate it first via npf_load_site('", NPF_SITE, "', ", NPF_YEAR,
       ") in npf-analysis.R.")
}

# Cache was written with save(), not saveRDS() (see npf_load_site() in
# npf-analysis.R) -- must be loaded with load(), not readRDS().
.npf_env <- new.env()
load(npf_cache, envir = .npf_env)
smps_data <- .npf_env[[paste0(NPF_SITE, "_smps_npf_", NPF_YEAR)]]

npf_log <- npf_logbook_read(NPF_SITE)
log_row <- npf_log %>% filter(date == NPF_DATE)
if (nrow(log_row) != 1 || is.na(log_row$gr_t_start[1]))
  stop("No saved growth-rate window for ", NPF_SITE, " ", NPF_DATE)

day_data <- smps_data[as.Date(smps_data$date) == NPF_DATE, , drop = FALSE]

window <- list(t_start = log_row$gr_t_start[1], t_end = log_row$gr_t_end[1],
              dp_min  = log_row$gr_dp_min[1],  dp_max  = log_row$gr_dp_max[1])
trace <- npf_trace_mode(day_data, window)
fit   <- npf_fit_growth(trace)

message(sprintf("  refit check: %.2f nm/hr, r2=%.2f (logbook: %.2f nm/hr, r2=%.2f)",
                fit$gr_nm_hr, fit$r2, log_row$gr_nm_hr[1], log_row$gr_r2[1]))

plot_npf_day(
  day_data, trace = trace, gr_fit = fit,
  title = paste0("New-particle-formation event — Kensington, ", format(NPF_DATE, "%d %b %Y"),
                 sprintf(" (GR = %.1f nm/hr, r² = %.2f)", fit$gr_nm_hr, fit$r2)),
  file = file.path(FIGDIR, "fig3_npf_event.png"),
  width = 10, height = 5
)
message("  saved fig3_npf_event.png")

message("Done. Figures written to ", FIGDIR)
