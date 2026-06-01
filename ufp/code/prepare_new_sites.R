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
  mutate(date = as.POSIXct(Date, tz = "UTC")) %>%  # Date already POSIXct from readr auto-parse
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

# ── NPL SMPS 2020–2023 ───────────────────────────────────────────────────────
# Source: data/unclassified/npl/smps/SMPS_Size_<Site>_*.xl(s|sx)
# 15-minute data. Col 1 = Excel serial date (days since 1899-12-30); remaining
# cols = dN/d(log Dp) at each diameter midpoint (nm). Non-numeric trailing
# cols (NA, Total, TNC-SMPS) are excluded by the numeric-header filter.
# 2020–2022 and 2023 Jan-Feb cover ~16.55–604 nm; 2023 Mar+ covers ~10.18–
# 791 nm — written as separate CSVs (e.g. hop_smps_2023_jan-feb.csv).
message("\n── NPL SMPS 2020–2023")

site_from_smps_file <- function(f) {
  bn <- basename(f)
  if (grepl("Chilbolton",                    bn)) return("chilbolton")
  if (grepl("Honor.Oak.Park|Honor_Oak_Park", bn)) return("hop")
  if (grepl("Marylebone",                    bn)) return("marylebone")
  if (grepl("Harwell",                       bn)) return("harwell")
  if (grepl("North.Kensington",              bn)) return("kensington")
  stop("Unknown site in: ", bn)
}

label_from_smps_file <- function(f) {
  bn <- basename(f)
  yr <- regmatches(bn, regexpr("\\d{4}", bn))
  if (grepl("Jan.Feb", bn, ignore.case = TRUE)) return(paste0(yr, "_jan-feb"))
  if (grepl("Mar",     bn, ignore.case = TRUE)) return(paste0(yr, "_mar"))
  yr
}

read_npl_smps <- function(f) {
  sheet_nm  <- if ("Data" %in% excel_sheets(f)) "Data" else excel_sheets(f)[1]
  raw       <- read_excel(f, sheet = sheet_nm, col_names = FALSE, col_types = "text")
  hdrs      <- unlist(raw[1, ])
  keep_idx  <- which(!is.na(hdrs) & seq_along(hdrs) > 1)  # drop date col + blank cols
  keep_hdrs <- as.character(hdrs[keep_idx])
  data_rows <- raw[-1, ]
  dates     <- as.POSIXct(as.numeric(data_rows[[1]]) * 86400,
                           origin = "1899-12-30", tz = "UTC")
  keep_df        <- data_rows[, keep_idx] %>% mutate(across(everything(), as.numeric))
  names(keep_df) <- keep_hdrs
  bind_cols(tibble(date = dates), keep_df) %>% filter(!is.na(date))
}

npl_smps_files <- sort(list.files(
  file.path(DATADIR, "unclassified", "npl", "smps"),
  pattern = "\\.xls(x)?$", full.names = TRUE, ignore.case = TRUE
))

for (f in npl_smps_files) {
  folder <- site_from_smps_file(f)
  label  <- label_from_smps_file(f)
  message("  processing ", basename(f))
  df <- read_npl_smps(f)
  write_site_csv(df, file.path(DATADIR, folder, "smps",
                               paste0(folder, "_smps_", label, ".csv")))
}

# ── Beddows SMPS 2015–2020 ───────────────────────────────────────────────────
# Source: data/unclassified/beddows/smps/SMPS_Size_<Site>_*.xls
# Same format as NPL files above. Covers Chilbolton (2016–2020), Harwell
# (2015), HOP (2020), Marylebone Road (2015–2020), North Kensington (2015–2019).
# Years that overlap with NPL (2020 for CHB/HOP/MR) produce identical data;
# the NPL-written CSV is simply overwritten.
message("\n── Beddows SMPS 2015–2020")

beddows_smps_files <- sort(list.files(
  file.path(DATADIR, "unclassified", "beddows", "smps"),
  pattern = "\\.xls(x)?$", full.names = TRUE, ignore.case = TRUE
))

for (f in beddows_smps_files) {
  folder <- site_from_smps_file(f)
  label  <- label_from_smps_file(f)
  message("  processing ", basename(f))
  df <- read_npl_smps(f)
  write_site_csv(df, file.path(DATADIR, folder, "smps",
                               paste0(folder, "_smps_", label, ".csv")))
}

# ── Defra PMP CPC 2000–2009 ──────────────────────────────────────────────────
# Source: data/unclassified/defra_pmp/cpc/
# XLS files, 15-minute resolution. Col 1 is always the date; remaining cols are
# one site each. All formats store dates as Excel serial numbers; using
# col_types="text" returns them as numeric strings uniformly across all years.
# 2000–2004: header row has NA in col 1, site names contain "/ Particles cm" suffix.
# 2005: duplicate site columns follow the data columns (capture %); handled by
#   keeping only the first occurrence of each site.
# 2007–2009: row 2 contains notes or NAs; filtered out because date parses to NA.
# Output: averaged to hourly, NAs removed. Files named <site>_cpc_pmp_<year>.csv
#   to distinguish from Beddows data for overlapping sites (bic, marylebone).
message("\n── Defra PMP CPC 2000–2009")

defra_pmp_site_map <- tribble(
  ~pattern,      ~folder,
  "Belfast",     "belfast",
  "Birmingham",  "bic",
  "Glasgow",     "glasgow",
  "Bloomsbury",  "bloomsbury",
  "Manchester",  "manchester_pcc",
  "Kensington",  "kensington",
  "Port Talbot", "port_talbot",
  "Marylebone",  "marylebone",
  "Harwell",     "harwell"
)

folder_from_colname_pmp <- function(h) {
  for (i in seq_len(nrow(defra_pmp_site_map))) {
    if (grepl(defra_pmp_site_map$pattern[i], h, ignore.case = TRUE))
      return(defra_pmp_site_map$folder[i])
  }
  NA_character_
}

read_defra_pmp_cpc <- function(f) {
  raw  <- read_excel(f, sheet = "Data", col_names = FALSE, col_types = "text")
  hdrs <- as.character(unlist(raw[1, ]))

  # Build col → folder map; keep only the first occurrence of each site so that
  # the capture-% duplicate columns in 2005 are silently ignored.
  seen    <- character(0)
  col_map <- list()
  for (i in seq_along(hdrs)) {
    folder <- folder_from_colname_pmp(hdrs[i])
    if (!is.na(folder) && !folder %in% seen) {
      seen    <- c(seen, folder)
      col_map <- c(col_map, list(list(col = i, folder = folder)))
    }
  }

  if (length(col_map) == 0) return(list())

  # Skip header row; convert Excel serial dates; drop non-data rows (notes / NAs)
  data_rows <- raw[-1, ]
  dates     <- suppressWarnings(
    as.POSIXct(as.numeric(data_rows[[1]]) * 86400, origin = "1899-12-30", tz = "UTC")
  )

  lapply(col_map, function(m) {
    conc <- suppressWarnings(as.numeric(data_rows[[m$col]]))
    list(
      folder = m$folder,
      data   = tibble(date = dates, conc = conc) %>%
                 filter(!is.na(date), !is.na(conc)) %>%
                 mutate(date = floor_date(date, "hour")) %>%
                 group_by(date) %>%
                 summarise(conc = mean(conc), .groups = "drop")
    )
  })
}

defra_pmp_cpc_files <- sort(list.files(
  file.path(DATADIR, "unclassified", "defra_pmp", "cpc"),
  pattern = "\\.xls$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE
))

for (f in defra_pmp_cpc_files) {
  yr <- regmatches(basename(f), regexpr("\\d{4}", basename(f)))
  message("  processing ", basename(f))
  for (sd in read_defra_pmp_cpc(f)) {
    sd$data %>%
      write_site_csv(file.path(DATADIR, sd$folder, "cpc",
                               paste0(sd$folder, "_cpc_pmp_", yr, ".csv")))
  }
}

# ── Defra PMP SMPS 1998–2009 ──────────────────────────────────────────────────
# Source: data/unclassified/defra_pmp/smps/
# XLS files, 15-minute resolution, dN/d(log Dp). Format across all years:
#   Row 1  — col 1: date label (NA / "Date" / "Date/Time"), cols 2…N-1: diameter
#             midpoints (nm), col N: sum/total label or blank (excluded by
#             the numeric filter below).
#   Rows 2+— col 1: Excel serial date; cols 2…N-1: dN/d(log Dp) values.
# Some files have leading rows with data but no timestamp (col 1 blank); these
# are dropped naturally by the !is.na(date) filter.
# Diameter columns are identified as: header parses as numeric AND 1–10 000 nm.
# Output: 15-minute resolution (no pre-averaging) — use TIME_AVG in
#   read_smps_files() at load time. Format: date (POSIXct UTC) + numeric-named
#   diameter cols → loaded by the existing "maqs-ratified" branch without
#   modification.
# No time overlap with existing processed files (existing starts 2015, ends 2009).
# A file-existence check is included as a safety net.
message("\n── Defra PMP SMPS 1998–2009")

folder_from_smps_pmp <- function(f) {
  bn <- basename(f)
  if (grepl("Harwell",    bn, ignore.case = TRUE)) return("harwell")
  if (grepl("Marylebone", bn, ignore.case = TRUE)) return("marylebone")
  if (grepl("Kensington", bn, ignore.case = TRUE)) return("kensington")
  stop("Cannot determine site from: ", bn)
}

read_defra_pmp_smps <- function(f) {
  sh        <- excel_sheets(f)
  dat_sheet <- sh[grepl("note", sh, ignore.case = TRUE) == FALSE][1]
  raw       <- read_excel(f, sheet = dat_sheet, col_names = FALSE, col_types = "text")
  hdrs      <- as.character(unlist(raw[1, ]))

  # Diameter columns: header parses as numeric, within plausible nm range
  hdrs_num  <- suppressWarnings(as.numeric(hdrs))
  diam_idx  <- which(is.na(hdrs_num) == FALSE & hdrs_num > 1 & hdrs_num < 10000)
  diameters <- hdrs_num[diam_idx]

  if (length(diam_idx) == 0) stop("No diameter columns found in: ", basename(f))

  # Skip header row; parse Excel serial dates from col 1
  data_rows <- raw[-1, ]
  dates     <- suppressWarnings(
    as.POSIXct(as.numeric(data_rows[[1]]) * 86400, origin = "1899-12-30", tz = "UTC")
  )

  diam_df        <- data_rows[, diam_idx]
  diam_df        <- as.data.frame(lapply(diam_df, function(x) suppressWarnings(as.numeric(x))))
  names(diam_df) <- as.character(diameters)

  bind_cols(tibble(date = dates), as_tibble(diam_df)) %>%
    filter(is.na(date) == FALSE) %>%
    filter(rowSums(is.na(select(., -date))) < length(diameters))
}

defra_pmp_smps_files <- sort(list.files(
  file.path(DATADIR, "unclassified", "defra_pmp", "smps"),
  pattern = "\\.xls$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE
))

for (f in defra_pmp_smps_files) {
  yr      <- regmatches(basename(f), regexpr("\\d{4}", basename(f)))
  folder  <- folder_from_smps_pmp(f)
  outfile <- file.path(DATADIR, folder, "smps", paste0(folder, "_smps_pmp_", yr, ".csv"))

  if (file.exists(outfile)) {
    message("  skipping (already exists): ", basename(outfile))
    next
  }

  message("  processing ", basename(f))
  df <- read_defra_pmp_smps(f)
  if (is.null(df) == FALSE) write_site_csv(df, outfile)
}

# ── UK Air CPC 2010–2024 ─────────────────────────────────────────────────────
# Source: data/unclassified/ukair/CPC_*.csv
# Hourly data downloaded from UK-AIR portal. Wide format with 11 metadata rows.
# Status codes: V=Verified, P=Provisionally Verified, N=Not Verified, S=Suspect.
# Only V and P records are retained.
# Site names sit in row 4 at columns 3, 5, 7, ... (odd positions, 1-indexed).
# Conc/status pairs begin at column 3: col 3+4 = site 1, col 5+6 = site 2, etc.
# "No data" entries have an empty status column and are dropped by the V/P filter.
# Files written as <site>_cpc_ukair_<period>.csv; read_cpc_files() assigns these
# priority 1, so they win over Beddows/NPL data at overlapping timestamps.
message("\n── UK Air CPC 2010–2024")

ukair_site_map <- tribble(
  ~site_name,                 ~folder,
  "Birmingham Tyburn",        "tyb",
  "Harwell",                  "harwell",
  "London Marylebone Road",   "marylebone",
  "London N. Kensington",     "kensington",
  "Chilbolton Observatory",   "chilbolton",
  "London Honor Oak Park",    "hop"
)

read_ukair_cpc <- function(f) {
  # Extract site names from row 4: non-empty odd-indexed columns (3, 5, 7, ...)
  meta       <- read_csv(f, skip = 3, n_max = 1, col_names = FALSE,
                         col_types = cols(.default = col_character()),
                         show_col_types = FALSE)
  site_cols  <- seq(3, ncol(meta), by = 2)
  site_names <- as.character(meta[1, site_cols])
  site_names <- site_names[!is.na(site_names) & nzchar(trimws(site_names))]

  # Skip 11 rows (10 metadata + 1 column-header); read all as character
  raw <- read_csv(f, skip = 11, col_names = FALSE,
                  col_types = cols(.default = col_character()),
                  show_col_types = FALSE)

  dates <- as.POSIXct(paste(raw[[1]], raw[[2]]),
                      format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

  map_dfr(seq_along(site_names), function(i) {
    conc_col   <- 2L + (i - 1L) * 2L + 1L   # 3, 5, 7, ...
    status_col <- 2L + (i - 1L) * 2L + 2L   # 4, 6, 8, ...
    tibble(
      date      = dates,
      conc      = suppressWarnings(as.numeric(raw[[conc_col]])),
      status    = substr(trimws(as.character(raw[[status_col]])), 1L, 1L),
      site_name = site_names[i]
    ) %>%
      filter(status %in% c("V", "P"), !is.na(conc))
  })
}

ukair_files <- sort(list.files(
  file.path(DATADIR, "unclassified", "ukair"),
  pattern = "\\.csv$", full.names = TRUE
))

for (f in ukair_files) {
  period <- regmatches(basename(f), regexpr("\\d{4}-\\d{4}", basename(f)))
  message("  processing ", basename(f))
  df <- read_ukair_cpc(f)

  for (ix in seq_len(nrow(ukair_site_map))) {
    stn     <- ukair_site_map$site_name[ix]
    folder  <- ukair_site_map$folder[ix]
    site_df <- df %>% filter(site_name == stn) %>% select(date, conc)
    if (nrow(site_df) == 0) next
    write_site_csv(site_df,
                   file.path(DATADIR, folder, "cpc",
                             paste0(folder, "_cpc_ukair_", period, ".csv")))
  }
}

message("\nDone.")


