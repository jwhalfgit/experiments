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

message("Done. Figures written to ", FIGDIR)
