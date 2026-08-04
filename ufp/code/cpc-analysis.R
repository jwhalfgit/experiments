# cpc-analysis.R
# CPC time series analysis across all UK monitoring sites, plus a per-file
# data-coverage audit and diurnal profiles for BAQS, MAQS, and HOP.
# Source sourceMeFirst_ufp.R before running.

source("sourceMeFirst_ufp.R")

# ── Site registry ─────────────────────────────────────────────────────────────
# pattern: regex passed to find_site_files(); NULL loads all CSVs.
# bic and marylebone both have a Beddows aggregate (*_cpc_2000_2009.csv) that
# overlaps with per-year Defra PMP files (*_cpc_pmp_*.csv); the aggregate is
# excluded here so the same period isn't double-counted.

SITES_CPC <- list(
  belfast        = list(dir = file.path(DATADIR, "belfast",        "cpc"), 
                        label = "Belfast"),
  glasgow        = list(dir = file.path(DATADIR, "glasgow",        "cpc"), 
                        label = "Glasgow"),
  manchester_pcc = list(dir = file.path(DATADIR, "manchester_pcc", "cpc"), 
                        label = "Manchester PCC"),
  maqs           = list(dir = file.path(DATADIR, "maqs",           "cpc"), 
                        label = "MAQS"),
  lincoln        = list(dir = file.path(DATADIR, "lincoln",        "cpc"), 
                        label = "Lincoln"),
  bic            = list(dir = file.path(DATADIR, "bic",            "cpc"), 
                        label = "Birmingham City Centre",
                        pattern = "pmp"),
  baqs           = list(dir = file.path(DATADIR, "baqs",           "cpc"), 
                        label = "BAQS"),
  tyb            = list(dir = file.path(DATADIR, "tyb",            "cpc"), 
                        label = "Birmingham Tyburn"),
  port_talbot    = list(dir = file.path(DATADIR, "port_talbot",    "cpc"), 
                        label = "Port Talbot"),
  bloomsbury     = list(dir = file.path(DATADIR, "bloomsbury",     "cpc"), 
                        label = "London Bloomsbury"),
  kensington     = list(dir = file.path(DATADIR, "kensington",     "cpc"), 
                        label = "London N. Kensington"),
  marylebone     = list(dir = file.path(DATADIR, "marylebone",     "cpc"), 
                        label = "London Marylebone Rd",
                        pattern = "cpc_pmp|cpc_20|ukair"),
  hop            = list(dir = file.path(DATADIR, "hop",            "cpc"), 
                        label = "London Honor Oak Park"),
  harwell        = list(dir = file.path(DATADIR, "harwell",        "cpc"), 
                        label = "Harwell"),
  chilbolton     = list(dir = file.path(DATADIR, "chilbolton",     "cpc"),
                        label = "Chilbolton")
)

SITE_LEVELS <- map_chr(SITES_CPC, "label")


# =============================================================================
# Data coverage audit: per-file inventory + availability plot
# =============================================================================
# Scans every CPC file per site (unfiltered by SITES_CPC's pattern, so
# overlapping/duplicate sources like the Beddows AURN aggregate are included)
# to report what data exists and where it came from.

# Source type from site name + filename.
# BAQS/MAQS are site-specific instruments; all others are classified by filename.
cpc_source_label <- function(site, bn) {
  if (site == "baqs") return("Site (BAQS)")
  if (site == "maqs") return("Site (MAQS)")
  case_when(
    grepl("_pmp_",                 bn, ignore.case = TRUE) ~ "PMP",
    grepl("_ukair_",               bn, ignore.case = TRUE) ~ "UK Air",
    grepl("_2000_2009|_2010_2020", bn)                    ~ "Beddows (AURN)",
    TRUE                                                   ~ "NPL"
  )
}

# Fast date extraction: MAQS daily files (YYYYMMDDHHMMSS_*.csv) are date-parsed
# from the filename to avoid reading 700+ individual files. All other formats
# are read by selecting only the date/datetime column.
read_cpc_dates_fast <- function(f) {
  if (grepl("^\\d{12}_", basename(f))) {
    d <- as.POSIXct(substr(basename(f), 1, 8), format = "%Y%m%d", tz = "UTC")
    return(c(d, d + 86399L))
  }
  tryCatch({
    peek <- read_csv(f, n_max = 1, col_types = cols(.default = col_character()),
                     show_col_types = FALSE)
    dcol <- if ("datetime" %in% names(peek)) "datetime" else
            if ("date"     %in% names(peek)) "date" else names(peek)[1]
    raw  <- read_csv(f, col_select = !!dcol,
                     col_types = cols(.default = col_character()),
                     show_col_types = FALSE)[[1]]
    raw  <- raw[!is.na(raw) & nzchar(raw)]
    if (length(raw) == 0L) return(NULL)
    suppressWarnings(
      if (grepl("^\\d{2}/\\d{2}/\\d{4}", raw[1]))
        dmy_hm(raw, tz = "UTC")
      else
        as.POSIXct(raw, tz = "UTC")
    )
  }, error = function(e) { message("  WARN: ", basename(f)); NULL })
}

# Build per-file metadata tibble
cpc_meta <- map_dfr(names(SITES_CPC), function(site) {
  ff <- find_site_files(file.path(DATADIR, site, "cpc"))
  if (length(ff) == 0L) { message("  CPC ", site, ": no files"); return(NULL) }
  message("  CPC ", site, " (", length(ff), " files)...")
  map_dfr(ff, function(f) {
    dates <- read_cpc_dates_fast(f)
    if (is.null(dates) || all(is.na(dates))) return(NULL)
    tibble(
      site       = site,
      file       = basename(f),
      date_start = min(dates, na.rm = TRUE),
      date_end   = max(dates, na.rm = TRUE),
      n_rows     = length(dates),
      source     = cpc_source_label(site, basename(f))
    )
  })
})

write_csv(cpc_meta, file.path(ROOT, "cpc_coverage_summary.csv"))
message("Saved cpc_coverage_summary.csv (", nrow(cpc_meta), " rows)")


# ── CPC data availability plot ─────────────────────────────────────────────────
# One segment per (site, source) coloured by data source.
# Multiple sources that overlap in time for the same site are shown as
# overlapping segments — this is intentional (e.g. Beddows + UK Air at
# national-network sites represent the same AURN data from different providers).

SOURCE_LEVELS_CPC <- c("PMP", "Beddows (AURN)", "UK Air", "NPL",
                        "Site (BAQS)", "Site (MAQS)")

cpc_avail <- cpc_meta %>%
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
    values = setNames(
      RColorBrewer::brewer.pal(6, "Set2"),
      SOURCE_LEVELS_CPC
    ),
    drop = FALSE
  ) +
  scale_fill_manual(
    values = setNames(
      RColorBrewer::brewer.pal(6, "Set2"),
      SOURCE_LEVELS_CPC
    ),
    drop = FALSE
  ) +
  labs(x = NULL, y = NULL, colour = "Data source", fill = "Data source") +
  theme_bw() +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank())

LTDIR <- file.path(PLOTDIR, "longterm_trends")
dir.create(LTDIR, showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(LTDIR, "cpc_data_availability.png"),
       p_cpc_availability, width = 14, height = 7, dpi = 150)
message("Saved cpc_data_availability.png")


# ── Load all CPC data ──────────────────────────────────────────────────────────
# Uncomment the loading block and comment out load() to regenerate from source.
# 
# cpc_all <- map_dfr(names(SITES_CPC), function(nm) {
#   s  <- SITES_CPC[[nm]]
#   ff <- find_site_files(s$dir, pattern = s$pattern)
#   if(length(ff) == 0){
#     warning("No CPC files found for: ", nm);
#     return(tibble())
#     }
#   read_cpc_files(ff) %>%
#     mutate(date = floor_date(date, "1 hour"), site = s$label) %>%
#     filter(complete.cases(.)) %>%
#     group_by(site, date) %>%
#     summarise(conc = mean(conc, na.rm = TRUE), .groups = "drop")
# })
# save(cpc_all, file = file.path(CACHE_DIR, "cpc_all.Rds"))
load(file.path(CACHE_DIR, "cpc_all.Rds"))


# ── Monthly summaries ──────────────────────────────────────────────────────────

cpc_monthly <- cpc_all %>%
  mutate(
    mo   = floor_date(date, "month"),
    site = factor(site, levels = SITE_LEVELS)
  ) %>%
  group_by(site, mo) %>%
  summarise(
    concMean = mean(conc, na.rm = TRUE),
    concSd   = sd(conc,   na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  # Expand to a complete monthly sequence per site so geom_line shows gaps
  group_by(site) %>%
  complete(mo = seq(min(mo), max(mo), by = "month")) %>%
  ungroup()


# ── Time series: monthly mean ± SD, faceted by site ────────────────────────────

cpcTs_all <- ggplot(cpc_monthly, aes(x = mo, y = concMean)) +
  geom_ribbon(aes(ymin = concMean - concSd, ymax = concMean + concSd),
              alpha = 0.2, fill = "steelblue", colour = NA) +
  geom_line(colour = "steelblue") +
  facet_wrap(~site, scales = "free_y", ncol = 3) +
  labs(x = NULL, y = "Particle concentration (#/cm³)") +
  theme_minimal() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(PLOTDIR, "cpc_ts_all_sites.png"), plot = cpcTs_all,
       width = 16, height = 16, units = "in", bg = "white")


# ── Time series: shared y-axis across non-MR sites; Marylebone Rd independent ──
# Marylebone Rd roadside concentrations are ~5x higher than background sites, so
# a shared axis would compress all other panels. Marylebone gets its own scale;
# all remaining sites share a common y range.

library(patchwork)
MR_LABEL <- "London Marylebone Rd"

cpc_monthly_no_mr <- cpc_monthly %>% filter(site != MR_LABEL)
cpc_monthly_mr    <- cpc_monthly %>% filter(site == MR_LABEL)

p_shared <- ggplot(cpc_monthly_no_mr, aes(x = mo, y = concMean)) +
  geom_ribbon(aes(ymin = concMean - concSd, ymax = concMean + concSd),
              alpha = 0.2, fill = "steelblue", colour = NA) +
  geom_line(colour = "steelblue") +
  facet_wrap(~site, ncol = 3) +
  coord_cartesian(ylim = c(0, 1e5)) +
  labs(x = NULL, y = "Particle concentration (#/cm³)") +
  theme_minimal() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1))

p_mr <- ggplot(cpc_monthly_mr, aes(x = mo, y = concMean)) +
  geom_ribbon(aes(ymin = concMean - concSd, ymax = concMean + concSd),
              alpha = 0.2, fill = "steelblue", colour = NA) +
  geom_line(colour = "steelblue") +
  facet_wrap(~site, ncol = 3) +
  labs(x = NULL, y = "Particle concentration (#/cm³)") +
  theme_minimal() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1))

n_rows_shared <- ceiling(length(unique(cpc_monthly_no_mr$site)) / 3)

cpcTs_shared_y <- p_shared / p_mr +
  plot_layout(heights = c(n_rows_shared, 1))

ggsave(file.path(PLOTDIR, "cpc_ts_all_sites_shared_y.png"), plot = cpcTs_shared_y,
       width = 16, height = 18, units = "in", bg = "white")


# ── Box-and-whisker: monthly distribution, faceted by site ─────────────────────

cpcBoxWhisk_all <- cpc_all %>%
  mutate(
    year_mo = as.Date(floor_date(date, "month")),
    site    = factor(site, levels = SITE_LEVELS)
  ) %>%
  ggplot(aes(x = year_mo, y = conc, group = year_mo)) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA,
               fill = "steelblue", colour = "steelblue4") +
  facet_wrap(~site, scales = "free", ncol = 3) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(x = NULL, y = "Particle concentration (#/cm³)") +
  theme_minimal() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(PLOTDIR, "cpc_bw_all_sites.png"), plot = cpcBoxWhisk_all,
       width = 16, height = 16, units = "in", bg = "white")


# ── All sites on one panel, coloured by site ───────────────────────────────────

cpcTs_overlay <- ggplot(cpc_monthly,
                        aes(x = mo, y = concMean, colour = site)) +
  geom_line(linewidth = 0.6, alpha = 0.85) +
  scale_colour_manual(values = setNames(
    colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(SITE_LEVELS)),
    SITE_LEVELS
  )) +
  labs(x = NULL, y = "Monthly mean CPC (#/cm³)", colour = NULL) +
  theme_bw() +
  theme(text            = element_text(size = 20),
        legend.position = "bottom",
        legend.text     = element_text(size = 16)) +
  guides(colour = guide_legend(ncol = 3))

ggsave(file.path(PLOTDIR, "cpc_ts_all_sites_overlay.png"), plot = cpcTs_overlay,
       width = 13, height = 6, units = "in", dpi = 150, bg = "white")


# =============================================================================
# Diurnal profiles: BAQS, MAQS, HOP — two most recent years compared
# =============================================================================
# HOP data ends mid-2024, so it shows 2023 vs 2024; BAQS and MAQS show 2024 vs 2025.

DIURNAL_SITES <- list(
  baqs = "BAQS",
  maqs = "MAQS",
  hop  = "London Honor Oak Park"
)

DIUR_DIR <- file.path(PLOTDIR, "diurnal")
dir.create(DIUR_DIR, showWarnings = FALSE, recursive = TRUE)

# For each site, find the two most recent years with at least MIN_ROWS hourly
# rows and overlay their diurnal profiles using timeVariation(group = "year").
MIN_ROWS <- 100

for (site_key in names(DIURNAL_SITES)) {
  site_label <- DIURNAL_SITES[[site_key]]

  df_site <- cpc_all %>%
    filter(site == site_label) %>%
    mutate(yr = year(date))

  recent_years <- df_site %>%
    group_by(yr) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n >= MIN_ROWS) %>%
    slice_max(yr, n = 2) %>%
    pull(yr) %>%
    sort()

  if (length(recent_years) < 2) {
    message("Fewer than 2 usable years for ", site_label, "; skipping")
    next
  }
  message(site_label, ": comparing ", recent_years[1], " vs ", recent_years[2])

  df_plot <- df_site %>%
    filter(yr %in% recent_years) %>%
    mutate(year = factor(yr))

  title_str <- paste0(site_label, "  (", recent_years[1], " vs ", recent_years[2], ")")

  # Seasonal diurnal: four panels (DJF / MAM / JJA / SON), two years overlaid.
  result_s <- timeVariation(df_plot,
                            pollutant = "conc",
                            group     = "year",
                            type      = "season",
                            ylab      = "Particle concentration (#/cm³)",
                            plot      = FALSE)

  out_file <- file.path(DIUR_DIR,
                        paste0("cpc_diurnal_seasonal_", site_key, "_",
                               recent_years[1], "_", recent_years[2], ".png"))
  png(out_file, width = 1800, height = 1200, res = 150)
  plot(result_s, subset = "hour")
  grid::grid.text(title_str, x = 0.5, y = 0.99,
                  gp = grid::gpar(fontsize = 13, fontface = "bold"))
  dev.off()
  message("Saved ", basename(out_file))

  # Full timeVariation output: diurnal + day-of-week + monthly + weekday/weekend
  result <- timeVariation(df_plot,
                          pollutant = "conc",
                          group     = "year",
                          ylab      = "Particle concentration (#/cm³)",
                          plot      = FALSE)

  out_full <- file.path(DIUR_DIR,
                        paste0("cpc_timevariation_", site_key, "_",
                               recent_years[1], "_", recent_years[2], ".png"))
  png(out_full, width = 1600, height = 1200, res = 150)
  plot(result)
  grid::grid.text(title_str, x = 0.5, y = 0.99,
                  gp = grid::gpar(fontsize = 13, fontface = "bold"))
  dev.off()
  message("Saved ", basename(out_full))

  # ── Median versions: same two plots, statistic = "median" (line = median,
  # ribbon = 25th/75th percentile) instead of mean ± 95% CI ──────────────────

  result_s_med <- timeVariation(df_plot,
                                pollutant = "conc",
                                group     = "year",
                                type      = "season",
                                statistic = "median",
                                ylab      = "Particle concentration (#/cm³)",
                                plot      = FALSE)

  out_file_med <- file.path(DIUR_DIR,
                            paste0("cpc_diurnal_seasonal_", site_key, "_",
                                   recent_years[1], "_", recent_years[2], "_median.png"))
  png(out_file_med, width = 1800, height = 1200, res = 150)
  plot(result_s_med, subset = "hour")
  grid::grid.text(paste0(title_str, " — median"), x = 0.5, y = 0.99,
                  gp = grid::gpar(fontsize = 13, fontface = "bold"))
  dev.off()
  message("Saved ", basename(out_file_med))

  result_med <- timeVariation(df_plot,
                              pollutant = "conc",
                              group     = "year",
                              statistic = "median",
                              ylab      = "Particle concentration (#/cm³)",
                              plot      = FALSE)

  out_full_med <- file.path(DIUR_DIR,
                            paste0("cpc_timevariation_", site_key, "_",
                                   recent_years[1], "_", recent_years[2], "_median.png"))
  png(out_full_med, width = 1600, height = 1200, res = 150)
  plot(result_med)
  grid::grid.text(paste0(title_str, " — median"), x = 0.5, y = 0.99,
                  gp = grid::gpar(fontsize = 13, fontface = "bold"))
  dev.off()
  message("Saved ", basename(out_full_med))
}


# =============================================================================
# Diurnal profiles by year × month: one file per site
# =============================================================================
# For every site with CPC data, the mean diurnal (hour-of-day, UTC) profile of
# particle number concentration, faceted as year (rows) × month (cols). Each
# panel shows the mean line plus a 25th–75th-percentile (IQR) ribbon of the
# day-to-day spread, and is annotated with the number of hourly points averaged.
# Separate files per site so each gets an independent y-axis scale.
#
# cpc_diurnal_grid() is called twice below: once for summer (May–Aug) and once
# for all twelve months. All CPC diurnal plots (this section and the JJA vs
# rest-of-year comparison below) share one output directory.

CPC_DIURNAL_DIR <- file.path(PLOTDIR, "diurnal_cpc")

# Require at least this many distinct days in a site-year-month before drawing
# its diurnal profile, so sparse cells don't produce noisy panels. Adjustable.
MIN_DAYS <- 5

cpc_diurnal_grid <- function(cpc_data, months, month_levels, out_dir,
                             file_suffix, title_suffix, min_days = MIN_DAYS,
                             statistic = c("mean", "median")) {

  statistic <- match.arg(statistic)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Map site label (as stored in cpc_all$site) back to its registry key for filenames
  label_to_key <- setNames(names(SITES_CPC), map_chr(SITES_CPC, "label"))

  # Per site-year-month-hour: mean/median + IQR across days
  diurnal <- cpc_data %>%
    mutate(
      hour = hour(date),
      day  = as.Date(date),
      mon  = month(date),
      yr   = year(date)
    ) %>%
    filter(mon %in% months) %>%
    group_by(site, yr, mon, hour) %>%
    summarise(
      centre_conc = if (statistic == "median") median(conc, na.rm = TRUE)
                    else                       mean(conc, na.rm = TRUE),
      q25       = quantile(conc, 0.25, na.rm = TRUE),
      q75       = quantile(conc, 0.75, na.rm = TRUE),
      n_days    = n_distinct(day),
      .groups   = "drop"
    ) %>%
    # Coverage filter: drop site-year-month cells with too few days
    group_by(site, yr, mon) %>%
    filter(max(n_days) >= min_days) %>%
    ungroup() %>%
    mutate(month = factor(month.abb[mon], levels = month_levels))

  # Per-panel point counts (hourly rows averaged) for the top-left annotation.
  counts <- diurnal %>%
    group_by(site, yr, month) %>%
    summarise(n_pts = sum(n_days), .groups = "drop")

  n_months <- length(month_levels)
  x_breaks  <- if (n_months <= 6) c(0, 6, 12, 18) else c(0, 12)
  ann_size  <- if (n_months <= 6) 2.8 else 2.2

  for (site_label in sort(unique(diurnal$site))) {
    site_key <- label_to_key[[site_label]]
    if (is.null(site_key) || is.na(site_key)) site_key <- make.names(site_label)

    df_site  <- diurnal %>% filter(site == site_label)
    ann_site <- counts  %>% filter(site == site_label)

    n_years <- n_distinct(df_site$yr)
    h <- max(4, 1.1 * n_years + 1)   # taller grids for sites with more years
    w <- max(9, 1.8 * n_months)      # wider grids for more month columns

    p <- ggplot(df_site, aes(x = hour)) +
      geom_ribbon(aes(ymin = q25, ymax = q75), fill = "steelblue",
                  alpha = 0.25, colour = NA) +
      geom_line(aes(y = centre_conc), colour = "steelblue", linewidth = 0.6) +
      geom_text(data = ann_site, aes(x = 0, y = Inf, label = paste0("n=", n_pts)),
                hjust = 0, vjust = 1.3, size = ann_size, colour = "grey30",
                inherit.aes = FALSE) +
      facet_grid(yr ~ month) +
      scale_x_continuous(breaks = x_breaks, limits = c(0, 23)) +
      labs(x = "Hour of day (UTC)", y = "CPC (#/cm³)",
           title = paste0(site_label, " — ", title_suffix)) +
      theme_bw(base_size = 11) +
      theme(panel.grid.minor = element_blank())

    out <- file.path(out_dir, paste0(site_key, file_suffix))
    ggsave(out, p, width = w, height = h, dpi = 150, bg = "white", limitsize = FALSE)
    message("Saved ", basename(out), " (", n_years, " years)")
  }
}

# Summer (May–Aug)
cpc_diurnal_grid(
  cpc_all,
  months       = 5:8,
  month_levels = c("May", "Jun", "Jul", "Aug"),
  out_dir      = CPC_DIURNAL_DIR,
  file_suffix  = "_cpc_diurnal_summer.png",
  title_suffix = "summer diurnal profiles (mean, IQR ribbon)"
)

# All twelve months
cpc_diurnal_grid(
  cpc_all,
  months       = 1:12,
  month_levels = month.abb,
  out_dir      = CPC_DIURNAL_DIR,
  file_suffix  = "_cpc_diurnal_allmonths.png",
  title_suffix = "diurnal profiles by month (mean, IQR ribbon)"
)

# Summer (May–Aug) — median
cpc_diurnal_grid(
  cpc_all,
  months       = 5:8,
  month_levels = c("May", "Jun", "Jul", "Aug"),
  out_dir      = CPC_DIURNAL_DIR,
  file_suffix  = "_cpc_diurnal_summer_median.png",
  title_suffix = "summer diurnal profiles (median, IQR ribbon)",
  statistic    = "median"
)

# All twelve months — median
cpc_diurnal_grid(
  cpc_all,
  months       = 1:12,
  month_levels = month.abb,
  out_dir      = CPC_DIURNAL_DIR,
  file_suffix  = "_cpc_diurnal_allmonths_median.png",
  title_suffix = "diurnal profiles by month (median, IQR ribbon)",
  statistic    = "median"
)


# =============================================================================
# Diurnal profiles: JJA vs rest-of-year, overlaid, one file per site
# =============================================================================
# Same mean + IQR-ribbon diurnal profile as above, but instead of faceting by
# month, each panel (one per year) overlays two curves: JJA (Jun/Jul/Aug) vs
# the rest of the year, so the summer/non-summer diurnal shape can be compared
# directly within a year.

cpc_diurnal_season_compare <- function(cpc_data, out_dir, min_days = MIN_DAYS,
                                       statistic = c("mean", "median")) {

  statistic <- match.arg(statistic)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  label_to_key <- setNames(names(SITES_CPC), map_chr(SITES_CPC, "label"))
  season_colours <- c("JJA" = "#d95f02", "Rest of year" = "#1f78b4")

  diurnal <- cpc_data %>%
    mutate(
      hour   = hour(date),
      day    = as.Date(date),
      mon    = month(date),
      yr     = year(date),
      season = if_else(mon %in% 6:8, "JJA", "Rest of year")
    ) %>%
    group_by(site, yr, season, hour) %>%
    summarise(
      centre_conc = if (statistic == "median") median(conc, na.rm = TRUE)
                    else                       mean(conc, na.rm = TRUE),
      q25       = quantile(conc, 0.25, na.rm = TRUE),
      q75       = quantile(conc, 0.75, na.rm = TRUE),
      n_days    = n_distinct(day),
      .groups   = "drop"
    ) %>%
    # Coverage filter: drop site-year-season cells with too few days
    group_by(site, yr, season) %>%
    filter(max(n_days) >= min_days) %>%
    ungroup() %>%
    mutate(season = factor(season, levels = c("JJA", "Rest of year")))

  # Per-panel point counts (hourly rows averaged), one line per season, stacked
  # top-left of each year panel.
  counts <- diurnal %>%
    group_by(site, yr, season) %>%
    summarise(n_pts = sum(n_days), .groups = "drop") %>%
    group_by(site, yr) %>%
    arrange(season, .by_group = TRUE) %>%
    mutate(
      lbl   = paste0(season, ": n=", n_pts),
      y_pos = row_number()   # stack successive season labels downward
    ) %>%
    ungroup()

  for (site_label in sort(unique(diurnal$site))) {
    site_key <- label_to_key[[site_label]]
    if (is.null(site_key) || is.na(site_key)) site_key <- make.names(site_label)

    df_site  <- diurnal %>% filter(site == site_label)
    ann_site <- counts  %>% filter(site == site_label)

    n_years <- n_distinct(df_site$yr)
    h <- max(4, 1.1 * n_years + 1)

    p <- ggplot(df_site, aes(x = hour, colour = season, fill = season)) +
      geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2, colour = NA) +
      geom_line(aes(y = centre_conc), linewidth = 0.6) +
      geom_text(data = ann_site,
                aes(x = 0, y = Inf, label = lbl, colour = season,
                    vjust = 1.3 + 1.3 * (y_pos - 1)),
                hjust = 0, size = 2.6, fontface = "plain",
                inherit.aes = FALSE, show.legend = FALSE) +
      facet_grid(yr ~ .) +
      scale_colour_manual(values = season_colours) +
      scale_fill_manual(values = season_colours) +
      scale_x_continuous(breaks = c(0, 6, 12, 18), limits = c(0, 23)) +
      labs(x = "Hour of day (UTC)", y = "CPC (#/cm³)", colour = NULL, fill = NULL,
           title = paste0(site_label, " — JJA vs rest-of-year diurnal profiles",
                          if (statistic == "median") " (median)" else "")) +
      theme_bw(base_size = 11) +
      theme(panel.grid.minor = element_blank(), legend.position = "top")

    file_suffix <- if (statistic == "median") "_cpc_diurnal_jja_vs_rest_median.png"
                   else                       "_cpc_diurnal_jja_vs_rest.png"
    out <- file.path(out_dir, paste0(site_key, file_suffix))
    ggsave(out, p, width = 7, height = h, dpi = 150, bg = "white", limitsize = FALSE)
    message("Saved ", basename(out), " (", n_years, " years)")
  }
}

cpc_diurnal_season_compare(cpc_all, out_dir = CPC_DIURNAL_DIR)
cpc_diurnal_season_compare(cpc_all, out_dir = CPC_DIURNAL_DIR, statistic = "median")
