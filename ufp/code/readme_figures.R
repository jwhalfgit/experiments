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
# Figure 2 — long-term CPC trend, all sites on one panel
# =============================================================================
# All 15 CPC sites, annual mean, single linear y-axis (0-based), one panel
# coloured by site, legend at the bottom with each site's coverage year span
# appended. Marylebone dominates the axis -- deliberately not rescaled or put
# on a secondary axis (that was tried; across all 15 sites Marylebone is only
# 1.4x the next-highest site, so a secondary axis buys little, unlike on the
# 6-site SMPS+CPC subset where it's 3.9x).
#
# Data: data/cache/cpc_all.Rds, written with save() (not saveRDS()) by
# cpc-analysis.R -- 15 sites, hourly, `site` column holds the display label
# (e.g. "London Marylebone Rd"), not a short key. Loaded with load(), same as
# cpc-polar-map.R does for the same file.

message("Figure 2: long-term CPC trend, all sites...")

cpc_all_cache <- file.path(CACHE_DIR, "cpc_all.Rds")
if (!file.exists(cpc_all_cache))
  stop("Missing ", cpc_all_cache, " -- generate via the cpc_all loading block in cpc-analysis.R.")

.cpc_env <- new.env()
load(cpc_all_cache, envir = .cpc_env)
cpc_all <- .cpc_env[["cpc_all"]]

cpc_all <- cpc_all %>%
  group_by(site) %>%
  mutate(conc = tukey_filter(conc, k = 3)) %>%
  ungroup()

cpc_annual <- cpc_all %>%
  filter(!is.na(conc)) %>%
  mutate(year = year(date)) %>%
  group_by(site, year) %>%
  summarise(cpc_mean = mean(conc, na.rm = TRUE),
            pct_coverage = n() / (365.25 * 24) * 100, .groups = "drop") %>%
  filter(pct_coverage >= 25)

# Legend order: descending max annual mean, so it reads in the same
# top-to-bottom order the lines sit on the plot.
site_rank <- cpc_annual %>%
  group_by(site) %>% summarise(peak = max(cpc_mean), .groups = "drop") %>%
  arrange(desc(peak)) %>% pull(site)
cpc_annual <- cpc_annual %>% mutate(site = factor(site, levels = site_rank))

# Interpolating an 8-colour Set2 palette up to 15 categories produces several
# near-duplicate muted tones, and colorspace's built-in qualitative palettes
# (e.g. "Dark 3") stay too close in chroma/luminance to separate reliably at
# 15 levels too. Full-chroma (c=100), mid-luminance (l=60) hues spaced evenly
# around the whole wheel hold up much better -- see also the year-span legend
# labels below, a second (non-colour) cue for telling similar hues apart.
#
# Sites with similar peak concentration land at adjacent ranks (site_rank is
# ordered by peak), so assigning hues in that same order puts visually/
# temporally similar sites next to each other on the hue wheel too --
# compounding rather than counteracting the confusion. Instead the hue
# sequence is assigned via a fixed step through the wheel (step size ~n/2,
# coprime with n so it still cycles through every hue exactly once) so that
# rank-neighbours land roughly opposite each other in hue.
n_sites  <- length(site_rank)
hue_step <- (n_sites %/% 2) + 1
hue_order <- ((seq_len(n_sites) - 1) * hue_step) %% n_sites + 1
site_colours <- setNames(
  colorspace::qualitative_hcl(n_sites, c = 100, l = 60)[hue_order],
  site_rank
)

# Legend labels get each site's coverage year span appended, since several
# colours are still close enough (15 categories) that the span is a useful
# second cue for telling two similar-hued lines apart.
site_years <- cpc_annual %>%
  group_by(site) %>%
  summarise(yr_min = min(year), yr_max = max(year), .groups = "drop") %>%
  mutate(site = as.character(site))

site_label_lookup <- setNames(
  paste0(site_years$site, " (", site_years$yr_min, "–", site_years$yr_max, ")"),
  site_years$site
)

cpc_annual <- cpc_annual %>%
  mutate(site_label = factor(site_label_lookup[as.character(site)],
                             levels = site_label_lookup[site_rank]))

site_colours_labelled <- setNames(site_colours, site_label_lookup[names(site_colours)])

p_trend <- ggplot(cpc_annual, aes(x = year, y = cpc_mean, colour = site_label,
                                  linetype = site == "London Marylebone Rd",
                                  linewidth = site == "London Marylebone Rd")) +
  geom_line() +
  geom_point(size = 1.3) +
  scale_y_continuous(labels = scales::label_comma(), limits = c(0, NA)) +
  scale_colour_manual(values = site_colours_labelled) +
  scale_linetype_manual(values = c(`TRUE` = "dashed", `FALSE` = "solid"), guide = "none") +
  scale_linewidth_manual(values = c(`TRUE` = 1.1, `FALSE` = 0.6), guide = "none") +
  labs(x = NULL, y = "Annual mean CPC (#/cm³)", colour = NULL,
       title = "Long-term ultrafine particle number trend, all sites",
       caption = "Annual mean, sites with ≥25% hourly coverage in a year.") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8)) +
  guides(colour = guide_legend(nrow = 5, byrow = TRUE))

ggsave(file.path(FIGDIR, "fig2_longterm_trend.png"), p_trend, width = 12, height = 8, dpi = 150)
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


# =============================================================================
# Figure 4 — size-resolved trend, Marylebone & Kensington (combined panels)
# =============================================================================
# Supports the finding that the decline is concentrated in the nucleation
# (<30 nm) and Aitken (30-100 nm) bands, with nucleation showing the widest
# IQR (episodic NPF events vs a steadier background) -- checked, not just
# assumed, below. Two-panel version (one per site, stacked, free y-scale
# since Marylebone's roadside concentrations run several times Kensington's
# urban-background ones) of trends-analysis.R's four-site
# size_range_monthly.png.
#
# Pipeline mirrors trends-analysis.R's Stage 2 exactly (outlier cap -> spline
# -> smps_metrics() -> Tukey filter -> monthly_summary(), the last two now in
# load_ufp.R). data/cache/marylebone_smps_raw.Rds is the list-of-tibbles
# output of load_raw_smps(), written with saveRDS() (confirmed via magic
# bytes) -- unlike cpc_all.Rds/the NPF caches above, a plain readRDS() works.

message("Figure 4: size-resolved trend, Marylebone...")

mb_smps_cache <- file.path(CACHE_DIR, "marylebone_smps_raw.Rds")
if (!file.exists(mb_smps_cache))
  stop("Missing ", mb_smps_cache, " -- generate via load_raw_smps() for marylebone in trends-analysis.R.")

mb_raw <- readRDS(mb_smps_cache)

mb_metrics <- smps_filter_outliers(mb_raw, max_dndlogdp = 1e5) %>%
  smps_spline() %>%
  smps_metrics() %>%
  mutate(across(c(nuc, acc, large), ~tukey_filter(.x, k = 3)),
         site = "marylebone", label = "London Marylebone Rd")

# Instrument-fault window, April 2003 - July 2004: masked, not dropped, from
# nuc/acc/large only. Confirmed this session by reading the raw source file
# directly (marylebone_smps_pmp_2003.csv) -- row counts collapse from
# ~2,400-2,900/month to 0-1,400/month starting April 2003 (several months
# missing entirely) and stay degraded through July 2004, coincident with
# values ~10x lower than the well-covered months on either side (nucleation
# ~1,100-2,400 #/cm3 here vs ~10,000-19,000 in 2002 and again from Aug 2004,
# where coverage and magnitude both recover together). A sustained
# order-of-magnitude drop occurring together with a data-recording collapse
# points to an instrument fault (blocked inlet / DMA or detector degradation
# / flow fault), not a real atmospheric event. Several months in this window
# (Oct 2003-Mar 2004, May 2004) still clear the >=50% monthly coverage filter
# below and would otherwise pull on the ribbon and the Theil-Sen slope --
# masked explicitly here rather than left to the coverage filter, which only
# incidentally catches some of the window's months and not others.
MARYLEBONE_FAULT_WINDOW <- as.POSIXct(c("2003-04-01", "2004-08-01"), tz = "UTC")
mb_metrics <- mb_metrics %>%
  mutate(across(c(nuc, acc, large),
                ~if_else(date >= MARYLEBONE_FAULT_WINDOW[1] & date < MARYLEBONE_FAULT_WINDOW[2],
                        NA_real_, .x)))

mb_bands <- bind_rows(
  monthly_summary(mb_metrics, "nuc",   "Nucleation (<30 nm)"),
  monthly_summary(mb_metrics, "acc",   "Aitken (30–100 nm)"),
  monthly_summary(mb_metrics, "large", "Accumulation (>100 nm)")
) %>%
  mutate(band = factor(instrument, levels = c("Nucleation (<30 nm)",
                                              "Aitken (30–100 nm)",
                                              "Accumulation (>100 nm)"))) %>%
  group_by(site, label, band) %>%
  complete(month_date = seq(min(month_date), max(month_date), by = "month")) %>%
  ungroup()

# Fixed categorical colours (dataviz skill reference palette, first 3 slots --
# validated to clear the CVD/normal-vision floors pairwise at n=3): blue,
# orange, aqua, assigned in fixed band order rather than ggplot's default hue
# rotation.
BAND_COLOURS <- c(
  "Nucleation (<30 nm)"     = "#2a78d6",
  "Aitken (30–100 nm)"      = "#eb6834",
  "Accumulation (>100 nm)"  = "#1baf7a"
)

# Kensington's data, for the second panel. Kensington isn't in TREND_SITES
# (no <site>_smps_raw.Rds cache from load_raw_smps()) -- it's the site the
# NPF classification workflow was built around instead, so the cache here is
# the 14 per-year data/cache/kensington_smps_npf_<year>.Rds files (2007-2020)
# written by npf_load_site() in npf-analysis.R. Kensington's bin structure
# never changes across the record (per SITES_NPF's comment in npf-analysis.R),
# so unlike Marylebone this skips smps_spline() entirely; smps_metrics()
# works directly off whatever numeric-named bin columns are present.
#
# NOTE: there is also a single combined data/cache/kensington_smps_npf.Rds
# (all years in one tibble) but it predates a unit-conversion fix in
# read_smps_files() (see the kensington/PMP comment there) -- for 2007-2008 it
# holds pre-fix values ~75-100x too low (verified this session: median
# ~33 #/cm3/log(nm) at the 16.55 nm bin in the combined cache vs ~2490 from a
# fresh read of the same source CSV, matching the per-year cache). The
# per-year caches are all dated one day later than the combined one and match
# the fresh re-read, so they're used here instead. The combined cache is left
# alone -- regenerating/deleting it is a separate decision, not part of this
# figure.

message("  loading Kensington SMPS data...")

KENS_NPF_YEARS <- 2007:2020

kens_raw <- map_dfr(KENS_NPF_YEARS, function(yr) {
  cache_path <- file.path(CACHE_DIR, paste0("kensington_smps_npf_", yr, ".Rds"))
  if (!file.exists(cache_path)) {
    message("  missing ", basename(cache_path), " -- skipping year ", yr)
    return(NULL)
  }
  env <- new.env()
  load(cache_path, envir = env)
  env[[paste0("kensington_smps_npf_", yr)]]
}) %>%
  distinct(date, .keep_all = TRUE) %>%
  arrange(date)

kens_metrics <- smps_filter_outliers(list(kens_raw), max_dndlogdp = 1e5)[[1]] %>%
  smps_metrics() %>%
  mutate(across(c(nuc, acc, large), ~tukey_filter(.x, k = 3)),
         site = "kensington", label = "London N. Kensington")

kens_bands <- bind_rows(
  monthly_summary(kens_metrics, "nuc",   "Nucleation (<30 nm)"),
  monthly_summary(kens_metrics, "acc",   "Aitken (30–100 nm)"),
  monthly_summary(kens_metrics, "large", "Accumulation (>100 nm)")
) %>%
  mutate(band = factor(instrument, levels = c("Nucleation (<30 nm)",
                                              "Aitken (30–100 nm)",
                                              "Accumulation (>100 nm)"))) %>%
  group_by(site, label, band) %>%
  complete(month_date = seq(min(month_date), max(month_date), by = "month")) %>%
  ungroup()

# Combined: both sites' band-labelled monthly series in one long frame,
# faceted rather than two separate images -- shares the one legend, and
# `label` (already carried through monthly_summary()'s group_by) drives the
# facet. free_y since Marylebone's roadside concentrations run several times
# Kensington's urban-background ones; x stays shared so each panel's actual
# record length reads directly off the same axis.
size_bands_both <- bind_rows(mb_bands, kens_bands) %>%
  mutate(label = factor(label, levels = c("London Marylebone Rd", "London N. Kensington")))

# Theil-Sen trend per (label, band): Sen's slope + seasonal Mann-Kendall on
# de-seasonalised monthly anomalies (theilsen_stats(), load_ufp.R) -- the
# same method used by every other long-term trend plot in this project
# (trends-analysis.R's theil_sen_size_*/mode_*/trends_* plots), so this is
# consistent with the rest of the analysis rather than a one-off fit.
# Requires >=12 valid months; bands that don't clear that come back with
# slope = NA and are dropped, not given a fabricated trend.
ts_stats_bands <- size_bands_both %>%
  rename(date = month_date) %>%
  group_by(label, band) %>%
  group_modify(~theilsen_stats(.x, "N_median")) %>%
  ungroup() %>%
  filter(!is.na(slope)) %>%
  mutate(stat_text = sprintf("%s: %+.0f [%+.0f, %+.0f] #/cm³/yr %s",
                             band, slope, ci_lo, ci_hi, signif))

# Trend-line segments spanning each series' own date range, anchored at its
# median date/value -- same construction used by trends-analysis.R's custom
# ggplot Theil-Sen plots.
trend_segs_bands <- size_bands_both %>%
  filter(!is.na(N_median)) %>%
  group_by(label, band) %>%
  summarise(t_min = min(month_date), t_max = max(month_date),
            y_med = median(N_median, na.rm = TRUE), t_med = median(month_date),
            .groups = "drop") %>%
  inner_join(ts_stats_bands %>% select(label, band, slope), by = c("label", "band")) %>%
  mutate(
    yr_min  = as.numeric(t_min - t_med, units = "days") / 365.25,
    yr_max  = as.numeric(t_max - t_med, units = "days") / 365.25,
    y_start = y_med + slope * yr_min,
    y_end   = y_med + slope * yr_max
  )

# Stacked annotation text, one line per band, colour-matched, anchored to
# each panel's top-left corner (x=-Inf/y=Inf works despite the very different
# y-ranges across panels under free_y) with a per-band vjust offset so the
# (up to) three lines stack downward without overlapping.
band_vjust <- setNames(seq_along(BAND_COLOURS) * 1.4 - 0.2, names(BAND_COLOURS))
ts_stats_bands <- ts_stats_bands %>% mutate(vj = band_vjust[as.character(band)])

p_size_bands <- ggplot(size_bands_both, aes(x = month_date, colour = band, fill = band)) +
  geom_ribbon(aes(ymin = N_q25, ymax = N_q75), alpha = 0.15, colour = NA) +
  geom_line(aes(y = N_median), linewidth = 0.7) +
  geom_segment(data = trend_segs_bands,
               aes(x = t_min, xend = t_max, y = y_start, yend = y_end),
               linewidth = 1.1, linetype = "dashed", show.legend = FALSE) +
  geom_text(data = ts_stats_bands,
            aes(x = -Inf, y = Inf, label = stat_text, vjust = vj),
            inherit.aes = TRUE, hjust = -0.05, size = 2.8, fontface = "bold",
            show.legend = FALSE) +
  facet_wrap(~label, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = BAND_COLOURS) +
  scale_fill_manual(values = BAND_COLOURS) +
  labs(x = NULL, y = "Monthly median N (#/cm³)", colour = NULL, fill = NULL,
       title = "Size-resolved particle number trend",
       caption = "Tukey-filtered (k=3) hourly concentrations. Ribbon: interquartile range. Coverage filter: ≥50% of hours per month.\nDashed: Sen's slope (seasonal Mann-Kendall, de-seasonalised monthly anomalies). * p<0.05, ** p<0.01, *** p<0.001 (unmarked = not significant).") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave(file.path(FIGDIR, "fig4_size_bands.png"), p_size_bands,
       width = 11, height = 10, dpi = 150)
message("  saved fig4_size_bands.png")

# Check the IQR-width claim directly rather than assuming it, per site --
# report widest band by median (Q75-Q25) width so the README text can be
# checked against it.
iqr_width <- size_bands_both %>%
  mutate(iqr = N_q75 - N_q25) %>%
  group_by(label, band) %>%
  summarise(median_iqr = median(iqr, na.rm = TRUE), .groups = "drop") %>%
  arrange(label, desc(median_iqr))
message("  median monthly IQR width by band (widest first, per site):")
for (i in seq_len(nrow(iqr_width)))
  message(sprintf("    %-22s %-24s %.0f", iqr_width$label[i], iqr_width$band[i], iqr_width$median_iqr[i]))

message("Done. Figures written to ", FIGDIR)
