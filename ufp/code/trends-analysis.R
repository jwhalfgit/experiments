# trends-analysis.R
# Temporal trend analysis for UFP sites with concurrent CPC and SMPS records.
# Source sourceMeFirst_ufp.R before running this script.

source("sourceMeFirst_ufp.R")


# Site registry ---------------------------------------------------------------
# Sites known to have concurrent CPC and SMPS records.
# cpc_pattern / smps_pattern: regex passed to find_site_files(); NULL = all CSVs.
TREND_SITES <- list(
  baqs = list(
    label        = "Birmingham (BAQS)",
    cpc_dir      = "baqs/cpc",
    smps_dir     = "baqs/smps/brean",
    cpc_pattern  = NULL,
    smps_pattern = "conjoined_32cpd\\.csv"
  ),
  maqs = list(
    label        = "Manchester (MAQS)",
    cpc_dir      = "maqs/cpc",
    smps_dir     = "maqs/smps",
    cpc_pattern  = NULL,
    smps_pattern = NULL
  ),
  harwell = list(
    label        = "Harwell",
    cpc_dir      = "harwell/cpc",
    smps_dir     = "harwell/smps",
    cpc_pattern  = NULL,
    smps_pattern = NULL
  ),
  hop = list(
    label        = "London Honor Oak Park",
    cpc_dir      = "hop/cpc",
    smps_dir     = "hop/smps",
    cpc_pattern  = NULL,
    smps_pattern = NULL
  ),
  marylebone = list(
    label        = "London Marylebone Rd",
    cpc_dir      = "marylebone/cpc",
    smps_dir     = "marylebone/smps",
    cpc_pattern  = "cpc_pmp|cpc_20|ukair",
    smps_pattern = NULL
  ),
  chilbolton = list(
    label        = "Chilbolton",
    cpc_dir      = "chilbolton/cpc",
    smps_dir     = "chilbolton/smps",
    cpc_pattern  = NULL,
    smps_pattern = NULL
  )
)


# Priority sites for the modal/size-range analysis (best-instrumented urban &
# urban-background records). Used to subset the mode-trend plots.
PRIORITY_SITES <- c("marylebone", "maqs", "baqs", "hop")

# Output directory ------------------------------------------------------------
LTDIR <- file.path(PLOTDIR, "longterm_trends")
dir.create(LTDIR, showWarnings = FALSE, recursive = TRUE)

# Common SMPS size range ------------------------------------------------------
# Determines the intersection of size ranges covered across all TREND_SITES,
# using smps_site_summary.csv produced by smps-overview.R. The intersection
# enables like-for-like cross-site comparison (smps_ranged column).
#
# Per-site: most restrictive range across all instrument periods
#   (max of lower bounds, min of upper bounds).
# Common: intersection across all sites (max of per-site lowers, min of uppers).
smps_summary <- read_csv(file.path(ROOT, "smps_site_summary.csv"),
                         show_col_types = FALSE) %>%
  filter(site %in% names(TREND_SITES))

site_ranges <- smps_summary %>%
  pivot_longer(starts_with("size_range_"), values_to = "range_str") %>%
  filter(!is.na(range_str)) %>%
  mutate(
    rmin = as.numeric(str_extract(range_str, "^[0-9.]+")),
    rmax = as.numeric(str_extract(range_str, "[0-9.]+(?= nm)"))
  ) %>%
  group_by(site) %>%
  summarise(site_min = max(rmin), site_max = min(rmax), .groups = "drop")

SMPS_COMMON_RANGE <- c(max(site_ranges$site_min), min(site_ranges$site_max))
message("Common SMPS range: ", SMPS_COMMON_RANGE[1], "–", SMPS_COMMON_RANGE[2], " nm")


# --- SMPS data availability overview -----------------------------------------
# One horizontal segment per instrument period (continuous run with the same
# bin structure). Coloured and annotated by size range + bin count.
smps_coverage <- read_csv(file.path(ROOT, "smps_coverage_summary.csv"),
                          show_col_types = FALSE) %>%
  mutate(
    bin_label = paste0(round(bin_min_nm, 2), "–", round(bin_max_nm, 2),
                       " nm (", n_bins, " bins)"),
    # Colour group: broad instrument generation by bin count.
    # MAQS monthly files vary by a few bins but all cluster near ~100.
    bin_gen = case_when(
      n_bins <= 55  ~ "~51 bins (PMP)",
      n_bins <= 70  ~ "~64 bins (BAQS legacy)",
      n_bins <= 115 ~ "~100 bins (MAQS)",
      TRUE          ~ "120+ bins (modern)"
    ) %>% factor(levels = c("~51 bins (PMP)", "~64 bins (BAQS legacy)",
                             "~100 bins (MAQS)", "120+ bins (modern)"))
  ) %>%
  arrange(site, date_start) %>%
  group_by(site) %>%
  mutate(
    gap_days = as.numeric(difftime(date_start, lag(date_end, default = date_start[1]),
                                   units = "days")),
    period   = cumsum(bin_label != lag(bin_label, default = "") | gap_days > 90)
  ) %>%
  group_by(site, period, bin_label, bin_gen) %>%
  summarise(date_start = min(date_start),
            date_end   = max(date_end),
            .groups    = "drop") %>%
  mutate(
    mid_date = date_start + (date_end - date_start) / 2,
    site     = factor(site, levels = rev(sort(unique(site))))
  )

p_smps_availability <- ggplot(smps_coverage,
                              aes(y = site, colour = bin_gen, fill = bin_gen)) +
  geom_segment(aes(x = date_start, xend = date_end, yend = site),
               linewidth = 6, lineend = "butt") +
  geom_label(aes(x = mid_date, label = bin_label),
             colour = "black", size = 2.4,
             label.padding = unit(0.2, "lines"),
             show.legend = FALSE) +
  scale_colour_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = NULL, y = NULL,
       colour = "Instrument generation",
       fill   = "Instrument generation") +
  theme_bw() +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank())

ggsave(file.path(LTDIR, "smps_data_availability.png"),
       p_smps_availability, width = 14, height = 5, dpi = 150)


# --- CPC data availability overview ------------------------------------------
# Reads cpc_coverage_summary.csv generated by cpc-analysis.R.
# One segment per (site, source); overlapping segments for the same site reflect
# genuinely overlapping data providers (e.g. Beddows + UK Air for AURN sites).

SOURCE_LEVELS_CPC <- c("PMP", "Beddows (AURN)", "UK Air", "NPL",
                        "Site (BAQS)", "Site (MAQS)")

cpc_avail <- read_csv(file.path(ROOT, "cpc_coverage_summary.csv"),
                      show_col_types = FALSE) %>%
  mutate(source = factor(source, levels = SOURCE_LEVELS_CPC)) %>%
  arrange(site, source, date_start) %>%
  group_by(site, source) %>%
  mutate(
    gap_days = as.numeric(difftime(date_start, lag(date_end, default = date_start[1]),
                                   units = "days")),
    period   = cumsum(gap_days > 90)
  ) %>%
  group_by(site, source, period) %>%
  summarise(date_start = min(date_start), date_end = max(date_end), .groups = "drop") %>%
  mutate(
    mid_date = date_start + (date_end - date_start) / 2,
    site     = factor(site, levels = rev(sort(unique(site))))
  )

p_cpc_availability <- ggplot(cpc_avail, aes(y = site, colour = source, fill = source)) +
  geom_segment(aes(x = date_start, xend = date_end, yend = site),
               linewidth = 6, lineend = "butt", alpha = 0.75) +
  scale_colour_manual(
    values = setNames(RColorBrewer::brewer.pal(6, "Set2"), SOURCE_LEVELS_CPC),
    drop = FALSE
  ) +
  scale_fill_manual(
    values = setNames(RColorBrewer::brewer.pal(6, "Set2"), SOURCE_LEVELS_CPC),
    drop = FALSE
  ) +
  labs(x = NULL, y = NULL, colour = "Data source", fill = "Data source") +
  theme_bw() +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank())

ggsave(file.path(LTDIR, "cpc_data_availability.png"),
       p_cpc_availability, width = 14, height = 7, dpi = 150)


# =============================================================================
# STAGE 1: Load raw CSV data per site (cached individually as Rds)
# =============================================================================
# Delete <site>_smps_raw.Rds or <site>_cpc_raw.Rds from CACHE_DIR to force a reload.

smps_raw_cache <- imap(TREND_SITES, function(site_info, site_name) {
  ff <- file.path(CACHE_DIR, paste0(site_name, "_smps_raw.Rds"))
  if (file.exists(ff)) { message("Cache: ", basename(ff)); return(readRDS(ff)) }
  raw <- load_raw_smps(site_info, site_name)
  if (!is.null(raw)) saveRDS(raw, ff)
  raw
})

cpc_raw_cache <- imap(TREND_SITES, function(site_info, site_name) {
  ff <- file.path(CACHE_DIR, paste0(site_name, "_cpc_raw.Rds"))
  if (file.exists(ff)) { message("Cache: ", basename(ff)); return(readRDS(ff)) }
  raw <- load_raw_cpc(site_info, site_name)
  if (!is.null(raw)) saveRDS(raw, ff)
  raw
})


# =============================================================================
# STAGE 2: Process — outlier filter → spline → metrics / hourly average
# =============================================================================
# Per-bin dN/d(log Dp) values above SMPS_OUTLIER_CAP are set to NA before
# spline interpolation so instrument spikes don't inflate annual means.
# Adjust and re-run this block without re-reading the CSVs.
SMPS_OUTLIER_CAP <- 1e5  # #/cm³/log(nm)

# Spline once per site (the expensive step), then reuse the splined spectra for
# both integrated metrics and lognormal mode fitting.
smps_splined <- imap(compact(smps_raw_cache), function(raw, site_name) {
  message("Splining: ", site_name)
  smps_filter_outliers(raw, SMPS_OUTLIER_CAP) %>% smps_spline()
})

smps_sites <- imap(smps_splined, function(sp, site_name) {
  sp %>%
    smps_metrics(range_nm = SMPS_COMMON_RANGE) %>%
    mutate(site  = site_name,
           label = TREND_SITES[[site_name]]$label,
           year  = year(date))
}) %>% bind_rows()

# Monthly lognormal modes (PyNSD method 1): fit nuc/Aitken/accumulation modes to
# each site's monthly-mean spectrum. Cached because the fits are non-trivial;
# delete smps_modes_monthly.Rds to force a refit.
modes_cache <- file.path(CACHE_DIR, "smps_modes_monthly.Rds")
if (file.exists(modes_cache)) {
  message("Cache: ", basename(modes_cache))
  smps_modes <- readRDS(modes_cache)
} else {
  smps_modes <- imap(smps_splined, function(sp, site_name) {
    message("Fitting modes: ", site_name)
    smps_mode_timeseries(sp, period = "month") %>%
      mutate(site  = site_name,
             label = TREND_SITES[[site_name]]$label,
             year  = year(date))
  }) %>% bind_rows()
  saveRDS(smps_modes, modes_cache)
}

# Seasonal & diurnal mode climatologies. The same multi-lognormal fit is applied
# to mean spectra grouped by month-of-year, hour-of-day, and season × hour — so
# we can see how the modes shift seasonally and over the day. Restricted to the
# priority sites; cached (delete smps_modes_climatology.Rds to refit).
#
# NOTE: each climatology pools the full multi-year record, so it mixes instrument
# generations at sites that changed setup (e.g. Marylebone PMP→AURN). Treat as a
# first look at the within-year/within-day structure, not an instrument-clean one.
season_of <- function(m) factor(
  case_when(m %in% c(12, 1, 2) ~ "Winter",
            m %in% c(3, 4, 5)  ~ "Spring",
            m %in% c(6, 7, 8)  ~ "Summer",
            TRUE               ~ "Autumn"),
  levels = c("Spring", "Summer", "Autumn", "Winter"))

priority_splined <- smps_splined[intersect(PRIORITY_SITES, names(smps_splined))]

clim_cache <- file.path(CACHE_DIR, "smps_modes_climatology.Rds")
if (file.exists(clim_cache)) {
  message("Cache: ", basename(clim_cache))
  modes_clim <- readRDS(clim_cache)
} else {
  add_meta <- function(df, s) df %>%
    mutate(site = s, label = TREND_SITES[[s]]$label)

  smps_modes_seasonal <- imap(priority_splined, function(sp, s) {
    message("Seasonal modes: ", s)
    smps_mode_grouped(sp, tibble(month = month(sp$date))) %>% add_meta(s)
  }) %>% bind_rows()

  smps_modes_diurnal <- imap(priority_splined, function(sp, s) {
    message("Diurnal modes: ", s)
    smps_mode_grouped(sp, tibble(hour = hour(sp$date))) %>% add_meta(s)
  }) %>% bind_rows()

  smps_modes_seasonal_diurnal <- imap(priority_splined, function(sp, s) {
    message("Seasonal-diurnal modes: ", s)
    smps_mode_grouped(sp, tibble(season = season_of(month(sp$date)),
                                 hour   = hour(sp$date))) %>% add_meta(s)
  }) %>% bind_rows()

  modes_clim <- list(seasonal         = smps_modes_seasonal,
                     diurnal          = smps_modes_diurnal,
                     seasonal_diurnal = smps_modes_seasonal_diurnal)
  saveRDS(modes_clim, clim_cache)
}
smps_modes_seasonal         <- modes_clim$seasonal
smps_modes_diurnal          <- modes_clim$diurnal
smps_modes_seasonal_diurnal <- modes_clim$seasonal_diurnal

cpc_sites <- imap(compact(cpc_raw_cache), function(raw, site_name) {
  raw %>%
    mutate(date = floor_date(date, "1 hour")) %>%
    group_by(date) %>%
    summarise(cpc = mean(conc, na.rm = TRUE), .groups = "drop") %>%
    mutate(site  = site_name,
           label = TREND_SITES[[site_name]]$label,
           year  = year(date))
}) %>% bind_rows()

# Paired CPC + SMPS (inner join — only timesteps where both instruments have data)
trends_all <- inner_join(
  smps_sites %>% select(-year),
  cpc_sites  %>% select(date, site, label, cpc),
  by = c("date", "site", "label")
) %>%
  mutate(
    year           = year(date),
    month          = month(date),
    hour           = hour(date),
    cpc_smps_ratio = cpc / smps_total,
    season = case_when(
      month %in% c(12, 1, 2)  ~ "Winter",
      month %in% c(3, 4, 5)   ~ "Spring",
      month %in% c(6, 7, 8)   ~ "Summer",
      month %in% c(9, 10, 11) ~ "Autumn"
    ) %>% factor(levels = c("Spring", "Summer", "Autumn", "Winter"))
  )

# Metrics available in trends_all: cpc, smps_total, smps_ranged, cpc_smps_ratio,
#                                  modal_diam, nuc, acc, large


# =============================================================================
# 1. LONG-TERM TRENDS
# =============================================================================

# Tukey outlier filter on hourly integrated concentrations (per site).
# Applied after Stage 2 so the raw cache and splined data are unmodified.
# k = 3 (far-out fence) removes artifact spikes while preserving real events.
smps_sites <- smps_sites %>%
  group_by(site) %>%
  mutate(across(c(smps_total, smps_ranged, nuc, acc, large), tukey_filter)) %>%
  ungroup()

cpc_sites <- cpc_sites %>%
  group_by(site) %>%
  mutate(cpc = tukey_filter(cpc)) %>%
  ungroup()

# Helper used in 1d, 1h, 1j: Sen's slope + seasonal Mann-Kendall on a monthly
# series after subtracting the calendar-month climatological mean.
# theilsen_stats() moved to load_ufp.R (2026-08-05) so it can be reused
# outside this driver script (e.g. code/readme_figures.R).

# --- 1a. CPC annual means ----------------------------------------------------
cpc_annual <- cpc_sites %>%
  filter(!is.na(cpc)) %>%
  group_by(site, label, year) %>%
  summarise(
    cpc_mean     = mean(cpc,              na.rm = TRUE),
    cpc_median   = median(cpc,            na.rm = TRUE),
    cpc_q10      = quantile(cpc, 0.10,    na.rm = TRUE),
    cpc_q25      = quantile(cpc, 0.25,    na.rm = TRUE),
    cpc_q75      = quantile(cpc, 0.75,    na.rm = TRUE),
    cpc_q90      = quantile(cpc, 0.90,    na.rm = TRUE),
    pct_coverage = n() / (365.25 * 24) * 100,
    .groups = "drop"
  ) %>%
  filter(pct_coverage >= 25)

p_cpc_annual <- ggplot(cpc_annual, aes(x = year)) +
  geom_linerange(aes(ymin = cpc_q10, ymax = cpc_q90),
                 colour = "steelblue", alpha = 0.45) +
  geom_crossbar(aes(y = cpc_median, ymin = cpc_q25, ymax = cpc_q75),
                width = 0.5, colour = "steelblue", fill = "steelblue",
                alpha = 0.15, fatten = 2) +
  geom_line( aes(y = cpc_mean, colour = "Mean"), linewidth = 0.7) +
  geom_point(aes(y = cpc_mean, colour = "Mean")) +
  scale_colour_manual(values = c(Mean = "coral3")) +
  facet_wrap(~label, scales = "free_y") +
  labs(x = NULL, y = "Annual CPC (#/cm³)", colour = NULL,
       caption = "Box: IQR (Q25–Q75); whiskers: 10th–90th percentile; line/point: annual mean") +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(LTDIR, "trends_cpc_annual_means.png"),
       p_cpc_annual, width = 11, height = 8.5, dpi = 150)

p_cpc_annual_allsites <- ggplot(cpc_annual, aes(x = year, colour = label)) +
  geom_line(aes(y = cpc_mean)) +
  geom_point(aes(y = cpc_mean)) +
  geom_line(aes(y = cpc_median), linetype = "dashed") +
  labs(x = NULL, y = "Annual CPC (#/cm³)", colour = NULL,
       caption = "Solid = mean, dashed = median") +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(LTDIR, "trends_cpc_annual_means_allsites.png"),
       p_cpc_annual_allsites, width = 11, height = 8.5, dpi = 150)

# --- 1b. CPC Theil-Sen trend (seasonal Mann-Kendall) -------------------------
# deseason = TRUE removes the monthly mean cycle before the trend test,
# preventing the seasonal signal from inflating or masking the long-term slope.
png(file.path(LTDIR, "thiel_sen_trends_cpc.png"),
    width = 11, height = 8.5, res = 300, units = "in")
TheilSen(cpc_sites, pollutant = "cpc", type = "label",
         deseason = TRUE, ylab = "CPC (#/cm³)",
         main = "Long-term CPC trend (Theil-Sen)", y.relation = "free",
         x.relation = "free")
dev.off()



# --- 1c. SMPS annual means ---------------------------------------------------
range_label <- paste0("Common range (", SMPS_COMMON_RANGE[1], "–", SMPS_COMMON_RANGE[2], " nm)")

smps_annual <- smps_sites %>%
  filter(!is.na(smps_total)) %>%
  group_by(site, label, year) %>%
  summarise(
    total_mean    = mean(smps_total,           na.rm = TRUE),
    total_median  = median(smps_total,         na.rm = TRUE),
    total_q10     = quantile(smps_total, 0.10, na.rm = TRUE),
    total_q25     = quantile(smps_total, 0.25, na.rm = TRUE),
    total_q75     = quantile(smps_total, 0.75, na.rm = TRUE),
    total_q90     = quantile(smps_total, 0.90, na.rm = TRUE),
    ranged_mean   = mean(smps_ranged,           na.rm = TRUE),
    ranged_median = median(smps_ranged,         na.rm = TRUE),
    ranged_q10    = quantile(smps_ranged, 0.10, na.rm = TRUE),
    ranged_q25    = quantile(smps_ranged, 0.25, na.rm = TRUE),
    ranged_q75    = quantile(smps_ranged, 0.75, na.rm = TRUE),
    ranged_q90    = quantile(smps_ranged, 0.90, na.rm = TRUE),
    pct_coverage  = n() / (365.25 * 24) * 100,
    .groups = "drop"
  ) %>%
  filter(pct_coverage >= 25) %>%
  pivot_longer(
    cols      = c(total_mean, total_median, total_q10, total_q25, total_q75, total_q90,
                  ranged_mean, ranged_median, ranged_q10, ranged_q25, ranged_q75, ranged_q90),
    names_to  = c("measure", ".value"),
    names_sep = "_"
  ) %>%
  mutate(measure = factor(measure,
                          levels = c("total",      "ranged"),
                          labels = c("Full range", range_label)))

# --- annual banana: annual mean dN/dlogDp per bin × site ---------------------
smps_annual_long <- imap(smps_splined, function(sp, site_name) {
  sp %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarise(across(-date, ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(-year, names_to = "diameter", values_to = "dndlogdp") %>%
    mutate(
      diameter = as.numeric(diameter),
      log_diam = log10(diameter),
      site     = site_name,
      label    = TREND_SITES[[site_name]]$label
    )
}) %>% bind_rows()

# tile heights: each bin spans to the midpoint with its neighbours in log10 space
diams_ann <- sort(unique(smps_annual_long$diameter))
log_da    <- log10(diams_ann)
n_da      <- length(log_da)
th_ann    <- c(
  log_da[2]        - log_da[1],
  (log_da[3:n_da]  - log_da[1:(n_da - 2)]) / 2,
  log_da[n_da]     - log_da[n_da - 1]
)
smps_annual_long <- smps_annual_long %>%
  left_join(tibble(diameter = diams_ann, tile_h = th_ann), by = "diameter") %>%
  mutate(dndlogdp = if_else(dndlogdp == 0, NA_real_, dndlogdp))

# annual mean N (Tukey-filtered smps_sites), scaled per site onto the diameter axis
y_lo_ann <- log10(min(diams_ann))
y_hi_ann <- log10(max(diams_ann))

annual_N_overlay <- smps_sites %>%
  filter(!is.na(smps_total)) %>%
  mutate(year = year(date)) %>%
  group_by(site, label, year) %>%
  summarise(
    N            = mean(smps_total, na.rm = TRUE),
    pct_coverage = n() / (365.25 * 24) * 100,
    .groups = "drop"
  ) %>%
  filter(pct_coverage >= 25) %>%
  group_by(site) %>%
  mutate(
    N_hi     = quantile(N, 0.99, na.rm = TRUE),
    N_scaled = y_lo_ann + (pmin(N, N_hi) / N_hi) * (y_hi_ann - y_lo_ann)
  ) %>%
  ungroup()

p_smps_annual <- ggplot(smps_annual_long,
                        aes(x = year, y = log_diam, fill = dndlogdp)) +
  geom_tile(aes(width = 1, height = tile_h)) +
  geom_line(data        = annual_N_overlay,
            aes(x = year, y = N_scaled, group = site),
            colour      = "white",
            linewidth   = 0.8,
            inherit.aes = FALSE) +
  scale_fill_viridis_c(
    trans    = "log10",
    limits   = c(10, 5e4),
    oob      = scales::squish,
    na.value = "grey20",
    name     = "dN/dlogDp\n(#/cm³)"
  ) +
  scale_y_continuous(
    breaks = log10(c(10, 30, 100, 300, 1000)),
    labels = c(10, 30, 100, 300, 1000)
  ) +
  scale_x_continuous(breaks = seq(1998, 2024, by = 4)) +
  facet_wrap(~label, ncol = 2) +
  labs(x = NULL, y = "Diameter (nm)",
       caption = "White line: annual mean N (#/cm³), scaled independently per site to the diameter axis") +
  theme_bw() +
  theme(
    legend.position  = "right",
    panel.grid.major = element_line(colour = "grey50", linetype = "dotted"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(LTDIR, "trends_smps_annual_means.png"),
       p_smps_annual, width = 12, height = 10, dpi = 150)

# All-sites: common range only (like-for-like)
p_smps_annual_allsites <- smps_annual %>%
  filter(measure == range_label) %>%
  ggplot(aes(x = year, colour = label)) +
  geom_line(aes(y = mean)) +
  geom_point(aes(y = mean)) +
  geom_line(aes(y = median), linetype = "dashed") +
  labs(x = NULL, y = paste0("Annual SMPS (#/cm³)\n", range_label),
       colour = NULL, caption = "Solid = mean, dashed = median") +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(LTDIR, "trends_smps_annual_means_allsites.png"),
       p_smps_annual_allsites, width = 10, height = 6, dpi = 150)

# --- 1c-seasonal. SMPS seasonal banana (season × year × site) ----------------
# Same approach as the annual banana but averaged by season (DJF/MAM/JJA/SON)
# × year, so each column is one season-year rather than one calendar year.
# December is assigned to the following year so DJF winters are coherent.

smps_seasonal_long <- imap(smps_splined, function(sp, site_name) {
  sp %>%
    mutate(
      month    = month(date),
      year     = year(date),
      season   = season_of(month),
      year_adj = if_else(month == 12L, year + 1L, year)
    ) %>%
    group_by(year_adj, season) %>%
    filter(n() >= 0.25 * 90 * 24) %>%
    summarise(across(-c(date, month, year), ~mean(.x, na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(-c(year_adj, season),
                 names_to = "diameter", values_to = "dndlogdp") %>%
    mutate(
      diameter = as.numeric(diameter),
      log_diam = log10(diameter),
      site     = site_name,
      label    = TREND_SITES[[site_name]]$label
    ) %>%
    rename(year = year_adj)
}) %>% bind_rows() %>%
  left_join(tibble(diameter = diams_ann, tile_h = th_ann), by = "diameter") %>%
  mutate(dndlogdp = if_else(dndlogdp == 0, NA_real_, dndlogdp))

# Annual mean N per site × season × year for overlay; scale per site so all
# four seasons for a given site share the same concentration axis
seasonal_N_overlay <- smps_sites %>%
  filter(!is.na(smps_total)) %>%
  mutate(
    month    = month(date),
    year     = year(date),
    season   = season_of(month),
    year_adj = if_else(month == 12L, year + 1L, year)
  ) %>%
  group_by(site, label, year_adj, season) %>%
  filter(n() >= 0.25 * 90 * 24) %>%
  summarise(N = mean(smps_total, na.rm = TRUE), .groups = "drop") %>%
  rename(year = year_adj) %>%
  group_by(site) %>%
  mutate(
    N_hi     = quantile(N, 0.99, na.rm = TRUE),
    N_scaled = y_lo_ann + (pmin(N, N_hi) / N_hi) * (y_hi_ann - y_lo_ann)
  ) %>%
  ungroup()

p_smps_seasonal_banana <- ggplot(
    smps_seasonal_long,
    aes(x = year, y = log_diam, fill = dndlogdp)
  ) +
  geom_tile(aes(width = 1, height = tile_h)) +
  geom_line(data        = seasonal_N_overlay,
            aes(x = year, y = N_scaled, group = label),
            colour      = "white",
            linewidth   = 0.7,
            inherit.aes = FALSE) +
  scale_fill_viridis_c(
    trans    = "log10",
    limits   = c(10, 5e4),
    oob      = scales::squish,
    na.value = "grey20",
    name     = "dN/dlogDp\n(#/cm³)"
  ) +
  scale_y_continuous(
    breaks = log10(c(10, 30, 100, 300, 1000)),
    labels = c(10, 30, 100, 300, 1000)
  ) +
  scale_x_continuous(breaks = seq(1998, 2024, by = 4)) +
  facet_grid(label ~ season) +
  labs(x = NULL, y = "Diameter (nm)",
       caption = paste0("White line: seasonal mean N (#/cm³), scaled per site",
                        " (all seasons share same scale).\n",
                        "Winter (DJF): December assigned to the following year.")) +
  theme_bw() +
  theme(
    legend.position  = "right",
    panel.grid.major = element_line(colour = "grey50", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(size = 8)
  )

ggsave(file.path(LTDIR, "trends_smps_seasonal_banana.png"),
       p_smps_seasonal_banana, width = 16, height = 18, dpi = 150)

# --- 1d. SMPS Theil-Sen trend ------------------------------------------------
# Custom ggplot2 version: monthly medians + geom_point only (no connecting
# line), so data gaps cannot be bridged. complete() inserts NA rows for missing
# months so lines break at gaps (same approach as 1h / 1j).

smps_monthly_ts <- smps_sites %>%
  mutate(month_date = floor_date(date, "month")) %>%
  group_by(site, label, month_date) %>%
  summarise(
    smps_total  = median(smps_total,  na.rm = TRUE),
    smps_ranged = median(smps_ranged, na.rm = TRUE),
    n_hrs       = n(),
    .groups     = "drop"
  ) %>%
  filter(n_hrs >= 0.5 * 24 * days_in_month(month_date)) %>%
  group_by(site, label) %>%
  complete(month_date = seq(min(month_date), max(month_date), by = "month")) %>%
  ungroup()

smps_ts_specs <- list(
  list(col = "smps_total",  file = "theil_sen_trends_smps_total.png",
       ylab  = "SMPS total (#/cm³)",
       title = "Long-term SMPS trend – full range (Theil-Sen)"),
  list(col = "smps_ranged", file = "theil_sen_trends_smps_ranged.png",
       ylab  = paste0("SMPS (#/cm³)\n", range_label),
       title = paste0("Long-term SMPS trend – ", range_label, " (Theil-Sen)"))
)

for (spec in smps_ts_specs) {
  plot_data <- smps_monthly_ts %>%
    filter(!is.na(.data[[spec$col]])) %>%
    rename(date = month_date, value = all_of(spec$col))

  if (nrow(plot_data) < 24) next

  ts_stats <- plot_data %>%
    group_by(site, label) %>%
    group_modify(~theilsen_stats(.x, "value")) %>%
    ungroup() %>%
    filter(!is.na(slope)) %>%
    mutate(stat_text = sprintf("%.0f [%.0f, %.0f] #/cm³/yr %s",
                               slope, ci_lo, ci_hi, signif))

  trend_segs <- plot_data %>%
    group_by(site, label) %>%
    summarise(t_min = min(date), t_max = max(date),
              y_med = median(value), t_med = median(date),
              .groups = "drop") %>%
    left_join(ts_stats %>% select(site, label, slope), by = c("site", "label")) %>%
    filter(!is.na(slope)) %>%
    mutate(
      yr_min  = as.numeric(t_min - t_med, units = "days") / 365.25,
      yr_max  = as.numeric(t_max - t_med, units = "days") / 365.25,
      y_start = y_med + slope * yr_min,
      y_end   = y_med + slope * yr_max
    )

  p_smps_ts <- ggplot(plot_data, aes(x = date, y = value)) +
    geom_point(size = 0.7, colour = "steelblue3", alpha = 0.6) +
    geom_segment(data = trend_segs,
                 aes(x = t_min, xend = t_max, y = y_start, yend = y_end),
                 colour = "red", linewidth = 1, inherit.aes = FALSE) +
    geom_label(data = ts_stats,
               aes(x = -Inf, y = Inf, label = stat_text),
               hjust = -0.05, vjust = 1.4, size = 3.0,
               colour = "darkgreen", fill = "white", label.size = 0,
               inherit.aes = FALSE) +
    facet_wrap(~label, scales = "free", ncol = 2) +
    labs(x = NULL, y = spec$ylab, title = spec$title) +
    theme_bw()

  ggsave(file.path(LTDIR, spec$file), p_smps_ts, width = 11, height = 8.5, dpi = 150)
}


# --- 1e. SMPS N vs CPC N monthly comparison ----------------------------------
# Monthly medians with Q25-Q75 ribbon per instrument per site.
# Draws from cpc_sites and smps_sites independently so each shows its full
# record. Coverage filter: >= 50% of hours in the month must be present.
# Ribbon (IQR) is used rather than discrete boxplots for readability over
# long (~20 year) records.

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

comparison_monthly <- bind_rows(
  monthly_summary(cpc_sites,  "cpc",         "CPC"),
  monthly_summary(smps_sites, "smps_total",  "SMPS (full range)"),
  monthly_summary(smps_sites, "smps_ranged",
                  paste0("SMPS (", SMPS_COMMON_RANGE[1], "–",
                         SMPS_COMMON_RANGE[2], " nm)"))
) %>%
  mutate(instrument = factor(instrument,
                             levels = c("CPC", "SMPS (full range)",
                                        paste0("SMPS (", SMPS_COMMON_RANGE[1],
                                               "–", SMPS_COMMON_RANGE[2], " nm)")))) %>%
  # Insert NA rows for missing months so geom_line breaks across data gaps
  # (affects Chilbolton, Harwell, and Marylebone where coverage is not continuous)
  group_by(site, label, instrument) %>%
  complete(month_date = seq(min(month_date), max(month_date), by = "month")) %>%
  ungroup()

# Monthly banana: monthly mean dN/dlogDp per bin × site
smps_monthly_long <- imap(smps_splined, function(sp, site_name) {
  sp %>%
    mutate(month_date = floor_date(date, "month"),
           days_in_m  = days_in_month(date)) %>%
    group_by(month_date) %>%
    filter(n() >= 0.5 * 24 * days_in_m[1]) %>%
    summarise(across(-c(date, days_in_m), ~mean(.x, na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(-month_date, names_to = "diameter", values_to = "dndlogdp") %>%
    mutate(
      diameter = as.numeric(diameter),
      log_diam = log10(diameter),
      site     = site_name,
      label    = TREND_SITES[[site_name]]$label
    )
}) %>% bind_rows() %>%
  left_join(tibble(diameter = diams_ann, tile_h = th_ann), by = "diameter") %>%
  mutate(dndlogdp = if_else(dndlogdp == 0, NA_real_, dndlogdp))

dt_month_sec <- median(as.numeric(
  diff(sort(unique(smps_monthly_long$month_date))), units = "secs"))

# SMPS + CPC N overlay: shared per-site scale (combined 99th percentile)
monthly_N_scale <- comparison_monthly %>%
  filter(instrument %in% c("CPC", "SMPS (full range)"), !is.na(N_median)) %>%
  group_by(site) %>%
  summarise(N_hi = quantile(N_median, 0.99, na.rm = TRUE), .groups = "drop")

monthly_N_overlay <- comparison_monthly %>%
  filter(instrument %in% c("CPC", "SMPS (full range)")) %>%
  left_join(monthly_N_scale, by = "site") %>%
  mutate(
    N_scaled = y_lo_ann + (pmin(N_median, N_hi) / N_hi) * (y_hi_ann - y_lo_ann),
    overlay  = if_else(instrument == "CPC", "CPC", "SMPS")
  )

p_cpc_smps_monthly <- ggplot(smps_monthly_long,
                             aes(x = month_date, y = log_diam, fill = dndlogdp)) +
  geom_tile(aes(width = dt_month_sec, height = tile_h)) +
  geom_line(data        = monthly_N_overlay %>% filter(overlay == "SMPS"),
            aes(x = month_date, y = N_scaled, group = label),
            colour      = "white",
            linewidth   = 0.6,
            inherit.aes = FALSE) +
  geom_line(data        = monthly_N_overlay %>% filter(overlay == "CPC"),
            aes(x = month_date, y = N_scaled, group = label),
            colour      = "red",
            linewidth   = 0.5,
            inherit.aes = FALSE) +
  scale_fill_viridis_c(
    trans    = "log10",
    limits   = c(10, 5e4),
    oob      = scales::squish,
    na.value = "grey20",
    name     = "dN/dlogDp\n(#/cm³)"
  ) +
  scale_y_continuous(
    breaks = log10(c(10, 30, 100, 300, 1000)),
    labels = c(10, 30, 100, 300, 1000)
  ) +
  facet_wrap(~label, ncol = 2) +
  labs(x = NULL, y = "Diameter (nm)",
       caption = "White: monthly mean SMPS N; red: CPC N — both scaled per site to the diameter axis") +
  theme_bw() +
  theme(
    legend.position  = "right",
    panel.grid.major = element_line(colour = "grey50", linetype = "dotted"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(LTDIR, "trends_cpc_vs_smps_monthly.png"),
       p_cpc_smps_monthly, width = 14, height = 12, dpi = 150)


# --- 1f. De-seasoned monthly CPC vs SMPS ------------------------------------
# Removes the monthly climatological median (computed per site × instrument ×
# calendar month across the full record) to expose the long-term trend without
# the seasonal cycle. The IQR ribbon is shifted by the same climatology so
# spread is preserved as an anomaly too.
comparison_monthly_deseasoned <- comparison_monthly %>%
  mutate(mon = month(month_date)) %>%
  group_by(site, label, instrument, mon) %>%
  mutate(clim = median(N_median, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    N_anom     = N_median - clim,
    N_q25_anom = N_q25   - clim,
    N_q75_anom = N_q75   - clim
  ) %>%
  select(-mon, -clim)

p_cpc_smps_deseasoned <- ggplot(comparison_monthly_deseasoned,
                                aes(x = month_date, colour = instrument,
                                    fill = instrument)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_ribbon(aes(ymin = N_q25_anom, ymax = N_q75_anom), alpha = 0.15, colour = NA) +
  geom_line(aes(y = N_anom), linewidth = 0.5) +
  facet_wrap(~label, scales = "free_y", ncol = 2) +
  labs(x = NULL, y = "Monthly anomaly N (#/cm³)", colour = NULL, fill = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(LTDIR, "trends_cpc_vs_smps_monthly_deseasoned.png"),
       p_cpc_smps_deseasoned, width = 12, height = 10, dpi = 150)


# --- 1g. STL decomposition ---------------------------------------------------
# Decomposes each site × instrument monthly series into trend, seasonal, and
# remainder components via Loess (Cleveland et al. 1990).
#
# s.window = 13: seasonal Loess window (months); allows the seasonal pattern to
#   evolve slowly over the record. Use "periodic" to fix it (equivalent to 1f).
# robust = TRUE: downweights large remainder values so outlier months don't
#   distort the trend or seasonal fit.
#
# Gaps in the monthly series (e.g. Harwell/Marylebone 2010–2014) are linearly
# interpolated before decomposition; trend and seasonal values in those regions
# should be treated as modelled, not observed.
# Sites/instruments with fewer than 24 valid months are skipped.

run_stl <- function(df, s_window = 13) {
  all_dates <- seq(min(df$month_date), max(df$month_date), by = "month")
  x <- tibble(month_date = all_dates) %>%
    left_join(select(df, month_date, N_median), by = "month_date") %>%
    pull(N_median)

  if (sum(!is.na(x)) < 24) return(tibble())

  was_na <- is.na(x)

  if (any(was_na)) {
    idx <- seq_along(x)
    x   <- approx(idx[!is.na(x)], x[!is.na(x)], idx, rule = 2)$y
  }

  x_ts <- ts(x,
             start     = c(year(min(all_dates)), month(min(all_dates))),
             frequency = 12)
  fit  <- stl(x_ts, s.window = s_window, robust = TRUE)

  out <- as_tibble(fit$time.series) %>%
    mutate(observed   = x,
           month_date = all_dates)

  # Mask interpolated positions so plots break at data gaps rather than
  # drawing modelled lines through them
  out[was_na, c("trend", "seasonal", "remainder", "observed")] <- NA
  out
}

stl_all <- comparison_monthly %>%
  group_by(site, label, instrument) %>%
  group_modify(~run_stl(.x)) %>%
  ungroup() %>%
  pivot_longer(c(observed, trend, seasonal, remainder),
               names_to  = "component",
               values_to = "value") %>%
  mutate(component = factor(component,
                            levels = c("observed", "trend", "seasonal", "remainder"),
                            labels = c("Observed", "Trend", "Seasonal", "Remainder")))

# Trend component only — primary comparison across instruments
p_stl_trend <- stl_all %>%
  filter(component == "Trend") %>%
  ggplot(aes(x = month_date, y = value, colour = instrument)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~label, scales = "free_y", ncol = 2) +
  labs(x = NULL, y = "Trend component N (#/cm³)", colour = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(LTDIR, "stl_trend.png"),
       p_stl_trend, width = 12, height = 10, dpi = 150)

# Full decomposition — all four components, faceted by site × component
p_stl_decomp <- stl_all %>%
  ggplot(aes(x = month_date, y = value, colour = instrument)) +
  geom_line(linewidth = 0.4) +
  facet_grid(component ~ label, scales = "free_y") +
  labs(x = NULL, y = NULL, colour = NULL) +
  theme_bw() +
  theme(legend.position  = "bottom",
        strip.text       = element_text(size = 7),
        axis.text.x      = element_text(size = 6, angle = 30, hjust = 1))

ggsave(file.path(LTDIR, "stl_decomposition.png"),
       p_stl_decomp, width = 18, height = 10, dpi = 150)


# --- 1h. Lognormal modal-diameter trends (PyNSD method 1) --------------------
# smps_modes holds, per site per month, the fitted nucleation / Aitken /
# accumulation mode peak diameters (nuc_dp, ait_dp, acc_dp) and heights
# (nuc_H, ait_H, acc_H) from the multi-lognormal deconvolution of the
# monthly-mean spectrum. Focus on the four priority sites.
#
# Coverage filter: a month must average >= 50% of its hours, otherwise the
# monthly-mean spectrum (and any mode fit from it) is unreliable. This drops,
# e.g., the 2009 Marylebone months that average a single hour of data.
# No fit is ever produced for months absent from the record (e.g. the genuine
# 2010–2014 Marylebone/Harwell gap) — those simply have no rows here.
modes_ok <- smps_modes %>%
  filter(site %in% PRIORITY_SITES,
         n_obs >= 0.5 * 24 * days_in_month(date)) %>%
  arrange(site, date) %>%
  group_by(site) %>%
  # Break the series into contiguous runs: a new run starts whenever the gap to
  # the previous retained month exceeds ~6 weeks (i.e. a month is missing). Lines
  # are drawn within runs only, so nothing is connected across data gaps.
  mutate(run = cumsum(replace_na(as.numeric(date - lag(date)) > 45, FALSE))) %>%
  ungroup()

modes_long <- modes_ok %>%
  select(site, label, date, run, nuc_dp, ait_dp, acc_dp) %>%
  pivot_longer(c(nuc_dp, ait_dp, acc_dp),
               names_to = "mode", values_to = "dp") %>%
  filter(!is.na(dp)) %>%
  mutate(mode = factor(mode,
                       levels = c("nuc_dp", "ait_dp", "acc_dp"),
                       labels = c("Nucleation (<30 nm)",
                                  "Aitken (30–100 nm)",
                                  "Accumulation (>100 nm)")))

# Monthly mode-diameter series with a 13-month rolling median to show drift,
# computed within each contiguous run (>= 13 months) so it never spans a gap.
modes_roll <- modes_long %>%
  arrange(site, mode, date) %>%
  group_by(site, mode, run) %>%
  mutate(dp_roll = if (n() >= 13)
                     zoo::rollmedian(dp, 13, fill = NA, align = "center")
                   else NA_real_) %>%
  ungroup()

p_modes_monthly <- ggplot(modes_roll, aes(x = date, colour = mode, fill = mode)) +
  geom_point(aes(y = dp), size = 0.5, alpha = 0.3) +
  geom_line(aes(y = dp_roll, group = interaction(site, mode, run)),
            linewidth = 0.8) +
  facet_wrap(~label, scales = "free_x", ncol = 2) +
  scale_y_log10() +
  labs(x = NULL, y = "Modal diameter (nm)", colour = NULL, fill = NULL,
       caption = "Points: monthly fitted mode. Lines: 13-month rolling median (within data runs).") +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(LTDIR, "modes_monthly_diameter.png"),
       p_modes_monthly, width = 12, height = 8, dpi = 150)

# Theil-Sen trend per mode per site (nm/year), seasonal Mann-Kendall.
modes_ts <- modes_ok %>%
  transmute(date, site, label,
            nucleation = nuc_dp, aitken = ait_dp, accumulation = acc_dp)

# -- openair version (kept for reference; bridges data gaps with connecting line)
# modes_ts_oa <- modes_ts %>%
#   group_by(site, label) %>%
#   complete(date = seq(min(date), max(date), by = "month")) %>%
#   ungroup()
# for (md in c("nucleation", "aitken", "accumulation")) {
#   if (sum(!is.na(modes_ts_oa[[md]])) < 24) next
#   png(file.path(LTDIR, paste0("theil_sen_mode_", md, "_openair.png")),
#       width = 11, height = 8.5, res = 300, units = "in")
#   TheilSen(as.data.frame(modes_ts_oa), pollutant = md, type = "label",
#            deseason = TRUE, ylab = paste0(md, " mode diameter (nm)"),
#            main = paste0("Modal diameter trend – ", md, " (Theil-Sen)"),
#            y.relation = "free", x.relation = "free")
#   dev.off()
# }

# theilsen_stats() is defined in the LONG-TERM TRENDS preamble above.

for (md in c("nucleation", "aitken", "accumulation")) {
  plot_data <- modes_ts %>%
    select(site, label, date, value = all_of(md)) %>%
    filter(!is.na(value))
  if (nrow(plot_data) < 24) next

  ts_stats <- plot_data %>%
    group_by(site, label) %>%
    group_modify(~theilsen_stats(.x, "value")) %>%
    ungroup() %>%
    filter(!is.na(slope)) %>%
    mutate(stat_text = sprintf("%.2f [%.2f, %.2f] nm/yr %s",
                               slope, ci_lo, ci_hi, signif))

  trend_segs <- plot_data %>%
    group_by(site, label) %>%
    summarise(t_min = min(date), t_max = max(date),
              y_med = median(value), t_med = median(date),
              .groups = "drop") %>%
    left_join(ts_stats %>% select(site, label, slope),
              by = c("site", "label")) %>%
    filter(!is.na(slope)) %>%
    mutate(
      yr_min  = as.numeric(t_min - t_med, units = "days") / 365.25,
      yr_max  = as.numeric(t_max - t_med, units = "days") / 365.25,
      y_start = y_med + slope * yr_min,
      y_end   = y_med + slope * yr_max
    )

  p_ts <- ggplot(plot_data, aes(x = date, y = value)) +
    geom_point(size = 0.9, colour = "steelblue3", alpha = 0.65) +
    geom_segment(data = trend_segs,
                 aes(x = t_min, xend = t_max, y = y_start, yend = y_end),
                 colour = "red", linewidth = 1, inherit.aes = FALSE) +
    geom_label(data = ts_stats,
               aes(x = -Inf, y = Inf, label = stat_text),
               hjust = -0.05, vjust = 1.4, size = 3.0,
               colour = "darkgreen", fill = "white", label.size = 0,
               inherit.aes = FALSE) +
    facet_wrap(~label, scales = "free", ncol = 2) +
    labs(x = NULL, y = paste0(md, " mode diameter (nm)"),
         title = paste0("Modal diameter trend – ", md, " (Theil-Sen)")) +
    theme_bw()

  ggsave(file.path(LTDIR, paste0("theil_sen_mode_", md, ".png")),
         p_ts, width = 11, height = 8.5, dpi = 150)
}

# Annual median mode diameter per site (compact cross-site view).
modes_annual <- modes_long %>%
  mutate(year = year(date)) %>%
  group_by(site, label, mode, year) %>%
  summarise(dp_median = median(dp, na.rm = TRUE),
            dp_q25    = quantile(dp, 0.25, na.rm = TRUE),
            dp_q75    = quantile(dp, 0.75, na.rm = TRUE),
            .groups   = "drop")

p_modes_annual <- ggplot(modes_annual, aes(x = year, y = dp_median, colour = label)) +
  geom_ribbon(aes(ymin = dp_q25, ymax = dp_q75, fill = label),
              alpha = 0.12, colour = NA) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1) +
  facet_wrap(~mode, scales = "free_y", ncol = 1) +
  labs(x = NULL, y = "Annual median modal diameter (nm)",
       colour = NULL, fill = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(LTDIR, "modes_annual_diameter.png"),
       p_modes_annual, width = 10, height = 10, dpi = 150)


# --- 1i. STL decomposition of monthly modal-diameter series ------------------
# Reuses run_stl() from 1g (expects month_date / N_median). Applied to each
# site × mode combination so the seasonal cycle in modal diameter is separated
# from the long-term trend. The same linear interpolation caveat as 1g applies:
# trend/seasonal values across the Marylebone/Harwell gap are modelled, not
# observed. Sites/modes with fewer than 24 valid months are skipped.
modes_stl_all <- modes_long %>%
  rename(month_date = date, N_median = dp) %>%
  group_by(site, label, mode) %>%
  group_modify(~run_stl(.x)) %>%
  ungroup() %>%
  pivot_longer(c(observed, trend, seasonal, remainder),
               names_to  = "component",
               values_to = "value") %>%
  mutate(component = factor(component,
                            levels = c("observed", "trend", "seasonal", "remainder"),
                            labels = c("Observed", "Trend", "Seasonal", "Remainder")))

# Trend component — one panel per site, one line per mode.
p_modes_stl_trend <- modes_stl_all %>%
  filter(component == "Trend") %>%
  ggplot(aes(x = month_date, y = value, colour = mode)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~label, scales = "free", ncol = 2) +
  scale_y_log10() +
  labs(x = NULL, y = "STL trend component – modal diameter (nm)",
       colour = NULL,
       caption = paste("STL trend extracted from monthly modal diameters (s.window = 13, robust).",
                       "Gaps linearly interpolated before decomposition.")) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(LTDIR, "modes_stl_trend.png"),
       p_modes_stl_trend, width = 12, height = 8, dpi = 150)

# Seasonal component only — facet_grid(mode ~ site) so each band gets its own
# y-scale; the within-year cycle amplitude is then comparable across sites.
p_modes_stl_seasonal <- modes_stl_all %>%
  filter(component == "Seasonal") %>%
  ggplot(aes(x = month_date, y = value, colour = mode)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
  geom_line(linewidth = 0.5) +
  facet_grid(mode ~ label, scales = "free_y") +
  labs(x = NULL, y = "STL seasonal component – modal diameter (nm)",
       colour = NULL,
       caption = "STL seasonal component (within-year cycle) of monthly modal diameters. Each row has its own y-scale.") +
  theme_bw() +
  theme(legend.position  = "none",
        strip.text       = element_text(size = 7),
        axis.text.x      = element_text(size = 6, angle = 30, hjust = 1))

ggsave(file.path(LTDIR, "modes_stl_seasonal.png"),
       p_modes_stl_seasonal, width = 14, height = 8, dpi = 150)

# Full decomposition — component × site grid, one colour per mode.
p_modes_stl_decomp <- modes_stl_all %>%
  ggplot(aes(x = month_date, y = value, colour = mode)) +
  geom_line(linewidth = 0.4) +
  facet_grid(component ~ label, scales = "free_y") +
  labs(x = NULL, y = NULL, colour = NULL) +
  theme_bw() +
  theme(legend.position  = "bottom",
        strip.text       = element_text(size = 7),
        axis.text.x      = element_text(size = 6, angle = 30, hjust = 1))

ggsave(file.path(LTDIR, "modes_stl_decomposition.png"),
       p_modes_stl_decomp, width = 18, height = 10, dpi = 150)


# --- 1j. Size-range concentration trends -------------------------------------
# Long-term trends in integrated particle number per size band for the four
# priority sites. Uses the nuc / acc / large columns from smps_sites (already
# Tukey-filtered hourly concentrations). Naming in smps_metrics(): nuc = <30 nm,
# acc = 30–100 nm (Aitken in aerosol convention), large = >100 nm (accumulation).

smps_priority <- smps_sites %>% filter(site %in% PRIORITY_SITES)

size_monthly <- bind_rows(
  monthly_summary(smps_priority, "nuc",   "Nucleation (<30 nm)"),
  monthly_summary(smps_priority, "acc",   "Aitken (30–100 nm)"),
  monthly_summary(smps_priority, "large", "Accumulation (>100 nm)")
) %>%
  mutate(band = factor(instrument,
                       levels = c("Nucleation (<30 nm)",
                                  "Aitken (30–100 nm)",
                                  "Accumulation (>100 nm)"))) %>%
  select(-instrument) %>%
  group_by(site, label, band) %>%
  complete(month_date = seq(min(month_date), max(month_date), by = "month")) %>%
  ungroup()

# Monthly median + IQR ribbon per band per site
p_size_monthly <- ggplot(size_monthly,
                         aes(x = month_date, colour = band, fill = band)) +
  geom_ribbon(aes(ymin = N_q25, ymax = N_q75), alpha = 0.15, colour = NA) +
  geom_line(aes(y = N_median), linewidth = 0.5) +
  facet_wrap(~label, scales = "free_y", ncol = 2) +
  labs(x = NULL, y = "Monthly median N (#/cm³)", colour = NULL, fill = NULL,
       caption = "Ribbon: interquartile range. Coverage filter: ≥50% of hours per month.") +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(LTDIR, "size_range_monthly.png"),
       p_size_monthly, width = 12, height = 8, dpi = 150)

# Theil-Sen trend per size band (seasonal Mann-Kendall, deseason = TRUE)

# -- openair version (kept for reference; uses hourly smps_priority,
#    bridges data gaps with connecting line)
# for (band_col in c("nuc", "acc", "large")) {
#   band_label <- switch(band_col,
#                        nuc   = "Nucleation (<30 nm)",
#                        acc   = "Aitken (30–100 nm)",
#                        large = "Accumulation (>100 nm)")
#   dd <- smps_priority %>% filter(!is.na(.data[[band_col]]))
#   if (nrow(dd) < 24) next
#   png(file.path(LTDIR, paste0("theil_sen_size_", band_col, "_openair.png")),
#       width = 11, height = 8.5, res = 300, units = "in")
#   TheilSen(as.data.frame(dd), pollutant = band_col, type = "label",
#            deseason = TRUE, ylab = paste0(band_label, " N (#/cm³)"),
#            main     = paste0("Size-range trend – ", band_label, " (Theil-Sen)"),
#            y.relation = "free", x.relation = "free")
#   dev.off()
# }

# -- Custom ggplot2 version: uses the already-monthly size_monthly so the same
#    complete()-filled NA rows that break lines in size_range_monthly.png also
#    prevent gap-bridging here. geom_point only (no connecting line).
band_file_key <- c("Nucleation (<30 nm)"    = "nuc",
                   "Aitken (30–100 nm)"     = "acc",
                   "Accumulation (>100 nm)" = "large")

for (bl in levels(size_monthly$band)) {
  plot_data <- size_monthly %>%
    filter(band == bl, !is.na(N_median)) %>%
    rename(date = month_date, value = N_median)

  if (nrow(plot_data) < 24) next

  ts_stats <- plot_data %>%
    group_by(site, label) %>%
    group_modify(~theilsen_stats(.x, "value")) %>%
    ungroup() %>%
    filter(!is.na(slope)) %>%
    mutate(stat_text = sprintf("%.0f [%.0f, %.0f] #/cm³/yr %s",
                               slope, ci_lo, ci_hi, signif))

  trend_segs <- plot_data %>%
    group_by(site, label) %>%
    summarise(t_min = min(date), t_max = max(date),
              y_med = median(value), t_med = median(date),
              .groups = "drop") %>%
    left_join(ts_stats %>% select(site, label, slope),
              by = c("site", "label")) %>%
    filter(!is.na(slope)) %>%
    mutate(
      yr_min  = as.numeric(t_min - t_med, units = "days") / 365.25,
      yr_max  = as.numeric(t_max - t_med, units = "days") / 365.25,
      y_start = y_med + slope * yr_min,
      y_end   = y_med + slope * yr_max
    )

  p_sz <- ggplot(plot_data, aes(x = date, y = value)) +
    geom_point(size = 0.7, colour = "steelblue3", alpha = 0.6) +
    geom_segment(data = trend_segs,
                 aes(x = t_min, xend = t_max, y = y_start, yend = y_end),
                 colour = "red", linewidth = 1, inherit.aes = FALSE) +
    geom_label(data = ts_stats,
               aes(x = -Inf, y = Inf, label = stat_text),
               hjust = -0.05, vjust = 1.4, size = 3.0,
               colour = "darkgreen", fill = "white", label.size = 0,
               inherit.aes = FALSE) +
    facet_wrap(~label, scales = "free", ncol = 2) +
    labs(x = NULL, y = paste0(bl, " N (#/cm³)"),
         title = paste0("Size-range trend – ", bl, " (Theil-Sen)")) +
    theme_bw()

  ggsave(file.path(LTDIR,
                   paste0("theil_sen_size_", band_file_key[[bl]], ".png")),
         p_sz, width = 11, height = 8.5, dpi = 150)
}

# STL decomposition of the monthly size-range series
size_stl_all <- size_monthly %>%
  rename(N_median_val = N_median) %>%
  group_by(site, label, band) %>%
  group_modify(~run_stl(.x %>% rename(N_median = N_median_val))) %>%
  ungroup() %>%
  pivot_longer(c(observed, trend, seasonal, remainder),
               names_to  = "component",
               values_to = "value") %>%
  mutate(component = factor(component,
                            levels = c("observed", "trend", "seasonal", "remainder"),
                            labels = c("Observed", "Trend", "Seasonal", "Remainder")))

# Trend component: one panel per site, one line per band
p_size_stl_trend <- size_stl_all %>%
  filter(component == "Trend") %>%
  ggplot(aes(x = month_date, y = value, colour = band)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~label, scales = "free_y", ncol = 2) +
  labs(x = NULL, y = "STL trend component N (#/cm³)", colour = NULL,
       caption = "STL trend from monthly size-range concentrations (s.window = 13, robust). Gaps linearly interpolated.") +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(LTDIR, "size_range_stl_trend.png"),
       p_size_stl_trend, width = 12, height = 8, dpi = 150)

# Seasonal component: facet_grid(band ~ site) so each band has its own y-scale
p_size_stl_seasonal <- size_stl_all %>%
  filter(component == "Seasonal") %>%
  ggplot(aes(x = month_date, y = value, colour = band)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
  geom_line(linewidth = 0.5) +
  facet_grid(band ~ label, scales = "free_y") +
  labs(x = NULL, y = "STL seasonal component N (#/cm³)", colour = NULL,
       caption = "STL seasonal component of monthly size-range concentrations. Each row has its own y-scale.") +
  theme_bw() +
  theme(legend.position  = "none",
        strip.text       = element_text(size = 7),
        axis.text.x      = element_text(size = 6, angle = 30, hjust = 1))

ggsave(file.path(LTDIR, "size_range_stl_seasonal.png"),
       p_size_stl_seasonal, width = 14, height = 8, dpi = 150)


# Long-format helper for the mode climatologies: pivots the per-band
# diameter (dp) and height (H) columns to one row per (key…, mode), carrying any
# grouping columns named in `keep` (e.g. "hour", "month", c("season","hour")).
pivot_modes <- function(df, keep) {
  df %>%
    select(all_of(keep), site, label,
           nuc_dp, nuc_H, ait_dp, ait_H, acc_dp, acc_H) %>%
    pivot_longer(c(nuc_dp, nuc_H, ait_dp, ait_H, acc_dp, acc_H),
                 names_to = c("mode", ".value"), names_sep = "_") %>%
    filter(!is.na(dp)) %>%
    mutate(mode = factor(mode,
                         levels = c("nuc", "ait", "acc"),
                         labels = c("Nucleation (<30 nm)",
                                    "Aitken (30–100 nm)",
                                    "Accumulation (>100 nm)")))
}


# =============================================================================
# 2. DIURNAL VARIATION (modal diameters)
# =============================================================================
# Modes fit to the hour-of-day mean spectrum (UTC), per priority site. Shows
# how each band's peak diameter (and strength) evolves over the day — e.g.
# nucleation-mode growth through midday or a traffic-driven Aitken peak.

diurnal_modes_long <- pivot_modes(smps_modes_diurnal, "hour")

p_modes_diurnal <- ggplot(diurnal_modes_long, aes(hour, dp, colour = mode)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1) +
  facet_wrap(~label, ncol = 2) +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0, 24, 6)) +
  labs(x = "Hour of day (UTC)", y = "Modal diameter (nm)", colour = NULL,
       caption = "Modes fit to the hour-of-day mean spectrum (full record pooled).") +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(LTDIR, "modes_diurnal_diameter.png"),
       p_modes_diurnal, width = 11, height = 8, dpi = 150)

# Mode strength (peak height) over the day — complements the diameter view.
p_modes_diurnal_H <- ggplot(diurnal_modes_long, aes(hour, H, colour = mode)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1) +
  facet_wrap(~label, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(0, 24, 6)) +
  labs(x = "Hour of day (UTC)", y = "Mode peak dN/d(log Dp) (#/cm³)",
       colour = NULL,
       caption = "Mode peak height fit to the hour-of-day mean spectrum.") +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(file.path(LTDIR, "modes_diurnal_height.png"),
       p_modes_diurnal_H, width = 11, height = 8, dpi = 150)


# =============================================================================
# 3. SEASONAL (MONTH-OF-YEAR) VARIATION (modal diameters)
# =============================================================================
# Modes fit to the calendar-month mean spectrum, per priority site. Shows the
# within-year cycle in each band's peak diameter and strength.

seasonal_modes_long <- pivot_modes(smps_modes_seasonal, "month")

p_modes_seasonal <- ggplot(seasonal_modes_long, aes(month, dp, colour = mode)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1) +
  facet_wrap(~label, ncol = 2) +
  scale_y_log10() +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = NULL, y = "Modal diameter (nm)", colour = NULL,
       caption = "Modes fit to the month-of-year mean spectrum (full record pooled).") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(size = 7))

ggsave(file.path(LTDIR, "modes_seasonal_diameter.png"),
       p_modes_seasonal, width = 11, height = 8, dpi = 150)

p_modes_seasonal_H <- ggplot(seasonal_modes_long, aes(month, H, colour = mode)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1) +
  facet_wrap(~label, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = NULL, y = "Mode peak dN/d(log Dp) (#/cm³)", colour = NULL,
       caption = "Mode peak height fit to the month-of-year mean spectrum.") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(size = 7))

ggsave(file.path(LTDIR, "modes_seasonal_height.png"),
       p_modes_seasonal_H, width = 11, height = 8, dpi = 150)


# =============================================================================
# 4. SEASONAL DIURNAL PATTERNS (modal diameters)
# =============================================================================
# Modes fit to the season × hour-of-day mean spectrum. Faceted site × season so
# the diurnal mode evolution can be compared across seasons (e.g. summer
# photochemical nucleation growth vs muted winter cycle).

seasonal_diurnal_modes_long <-
  pivot_modes(smps_modes_seasonal_diurnal, c("season", "hour"))

p_modes_seasonal_diurnal <-
  ggplot(seasonal_diurnal_modes_long, aes(hour, dp, colour = mode)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 0.6) +
  facet_grid(label ~ season) +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0, 24, 6)) +
  labs(x = "Hour of day (UTC)", y = "Modal diameter (nm)", colour = NULL,
       caption = "Modes fit to the season × hour mean spectrum (full record pooled).") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text      = element_text(size = 8))

ggsave(file.path(LTDIR, "modes_seasonal_diurnal_diameter.png"),
       p_modes_seasonal_diurnal, width = 12, height = 10, dpi = 150)
