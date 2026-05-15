# prepare_new_sites.R
# Reads raw data for non-baqs/maqs sites and writes standardised CSVs:
#   CPC  → date (POSIXct UTC), conc (#/cm³)
#   SMPS → date (POSIXct UTC), [numeric diameter-midpoint bin columns, nm]

source("sourceMeFirst_ufp.R")

write_site_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_csv(df, path)
  message("  wrote ", nrow(df), " rows  →  ", path)
}


# ── AURN 2000–2009 CPC ───────────────────────────────────────────────────────
# Source: data/unclassified/CPC/AURN_CPC_DATA2000_2009.csv
# Hourly data for 7 sites. Date column format: "YYYY-MM-DD HH".
# Each site is split into its own cpc/ subfolder under data/<site>/.
message("\n── AURN CPC 2000–2009")

site_map <- tribble(
  ~col,       ~folder,
  "BIC_PN",   "bic",
  "TYB_PN",   "tyb",
  "HWL_PN",   "harwell",
  "CHB_PN",   "chilbolton",
  "LNK_PN",   "lincoln",
  "LHP_PN",   "hop",
  "LMR_PN",   "marylebone"
)

raw <- read_csv(
  file.path(DATADIR, "unclassified", "beddows", "cpc", "AURN_CPC_DATA2000_2009.csv"),
  show_col_types = FALSE
) %>%
  mutate(date = ymd_h(Date, tz = "UTC")) %>%
  select(-Date)

for (ix in seq_len(nrow(site_map))) {
  col    <- site_map$col[ix]
  folder <- site_map$folder[ix]

  raw %>%
    select(date, conc = all_of(col)) %>%
    filter(!is.na(conc)) %>%
    write_site_csv(file.path(DATADIR, folder, "cpc",
                             paste0(folder, "_cpc_2000_2009.csv")))
}

# ── AURN 2010–2020 CPC ───────────────────────────────────────────────────────
# Source: data/unclassified/CPC/AURN_CPC_DATA2010_2020.csv
# Hourly data. Row-number column at position 1; Date (DD/MM/YYYY) and Time
# (HH:MM:SS) are separate columns. No BIC_PN. NAs are explicit "NA".
message("\n── AURN CPC 2010–2020")

site_map_2010 <- tribble(
  ~col,       ~folder,
  "TYB_PN",   "tyb",
  "HWL_PN",   "harwell",
  "CHB_PN",   "chilbolton",
  "LNK_PN",   "lincoln",
  "LHP_PN",   "hop",
  "LMR_PN",   "marylebone"
)

raw_2010 <- read_csv(
  file.path(DATADIR, "unclassified", "beddows", "cpc", "AURN_CPC_DATA2010_2020.csv"),
  show_col_types = FALSE
) %>%
  mutate(date = dmy_hms(paste(Date, Time), tz = "UTC")) %>%
  select(-`...1`, -Date, -Time)

for (ix in seq_len(nrow(site_map_2010))) {
  col    <- site_map_2010$col[ix]
  folder <- site_map_2010$folder[ix]

  raw_2010 %>%
    select(date, conc = all_of(col)) %>%
    filter(!is.na(conc)) %>%
    write_site_csv(file.path(DATADIR, folder, "cpc",
                             paste0(folder, "_cpc_2010_2020.csv")))
}

# ── NPL CPC 2019–2023 ────────────────────────────────────────────────────────
# Source: data/unclassified/npl/cpc/CPC <year>.txt
# Tab-delimited, multi-site (Marylebone Road, Honor Oak Park, Chilbolton),
# hourly. Start times are 00:00:01; floored to hour on output.
# 2019/2020 overlap with Beddows confirmed as rounding-only (<0.02% mean
# relative difference, max absolute 0.5 #/cm³) — same underlying AURN data.
# CPC 2022 is a CSV (same columns, no leading zeros in date/time); handled
# by the same reader via extension dispatch.
message("\n── NPL CPC 2019–2023")

npl_station_map <- tribble(
  ~station_name,                ~folder,
  "London Marylebone Road",     "marylebone",
  "London Honor Oak Park",      "hop",
  "Chilbolton Observatory",     "chilbolton"
)

read_npl_cpc_txt <- function(f) {
  reader <- if (grepl("\\.csv$", f, ignore.case = TRUE)) read_csv else read_tsv
  reader(f, show_col_types = FALSE) %>%
    filter(Validity_id == 1, measurement != -9999) %>%
    transmute(
      station_name = `Station name`,
      date = floor_date(
        dmy_hms(paste(`measurement start date`, `measurement start time`), tz = "UTC"),
        "hour"
      ),
      conc = measurement
    ) %>%
    filter(!is.na(date))
}

npl_files <- sort(list.files(
  file.path(DATADIR, "unclassified", "npl", "cpc"),
  pattern = "\\.(txt|csv)$", full.names = TRUE
))

for (f in npl_files) {
  yr  <- regmatches(basename(f), regexpr("\\d{4}", basename(f)))
  df  <- read_npl_cpc_txt(f)
  message("  processing NPL CPC ", yr)

  for (ix in seq_len(nrow(npl_station_map))) {
    stn    <- npl_station_map$station_name[ix]
    folder <- npl_station_map$folder[ix]

    df %>%
      filter(station_name == stn) %>%
      select(date, conc) %>%
      write_site_csv(file.path(DATADIR, folder, "cpc",
                               paste0(folder, "_cpc_", yr, ".csv")))
  }
}

message("\nDone.")
