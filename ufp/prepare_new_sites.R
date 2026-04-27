# prepare_new_sites.R
# Reads raw data for Chilbolton, HOP, and Marylebone and writes standardised
# CSVs matching the baqs/maqs format conventions:
#   CPC  → date (POSIXct UTC), conc (#/cm³)
#   SMPS → date (POSIXct UTC), [numeric diameter-midpoint bin columns, nm]
#
# Redundancy resolution:
#   - _pc_Annual_ xlsx/xls (15-min) preferred over UK-AIR hourly/20-min uploads
#     where both cover the same period.
#   - _pc_Annual_ 2023 xlsx supersedes the Jan-Feb 2023 txt for Chilbolton/HOP.
#   - Unclassified SMPS monthly uploads are excluded (not time series).
#   - Harwell_pc_Annual_2020_v01.xls is excluded (Harwell, not HOP).
#   - "Honor Oak Park New SMPS 2023.txt" and "Marylebone Road New SMPS 2023.txt"
#     excluded — same data as the 2023 xlsx files, less complete.
#   - "2021 SMPS - 15min.txt" files contain full 51-bin SMPS distributions in
#     UK-AIR long format; they are NOT CPC files. Use read_ukair_tab_smps() to
#     extract SMPS data if needed (xls files already cover 2020-2022).
#   - 2021 CPC for all three sites comes from unclassified/CPC 2021.txt (TNC
#     parameter). All unclassified upload files are multi-site — always pass
#     the station argument to read_upload_txt_cpc / read_upload_xlsx_cpc.
#
# HOP SMPS 2023 is split into two files because the bin structure changed:
#   Jan-Feb  → 51 bins, 16.55–604 nm   (hop_smps_2023_jan_feb.csv)
#   Mar-Dec  → 122 bins, 10.18–~600 nm  (hop_smps_2023_mar_onwards.csv)

source("sourceMeFirst_ufp.R")
library(readxl)
library(lubridate)


# ── Helpers ───────────────────────────────────────────────────────────────────

# _pc_Annual_ xls/xlsx: "15Min_Data" sheet → date, conc
read_pc_annual_cpc <- function(f) {
  df <- read_excel(f, sheet = "15Min_Data")
  tibble(
    date = as.POSIXct(df[[1]], tz = "UTC"),
    conc = df[[2]]
  ) %>%
    filter(!is.na(date), !is.na(conc))
}

# UK-AIR tab-delimited CPC (PC_16_6 parameter from _pc_Annual_ txt files).
# These files contain only a single PC_16_6 parameter — total particle count
# above 16.6 nm — which is a true CPC-equivalent measurement.
# NOTE: the "2021 SMPS - 15min.txt" files are NOT the same format; they
# contain 51 size-bin parameters (full SMPS distribution) and must be read
# with read_ukair_tab_smps() instead.
read_ukair_tab_cpc <- function(f) {
  df <- read_tsv(f, show_col_types = FALSE)
  df %>%
    filter(parameter_id == "PC_16_6", Validity_id == 1, measurement != -9999) %>%
    transmute(
      date = parse_date_time(
        paste(`measurement start date`, `measurement start time`),
        orders = c("dmy HM", "dmy HMS"), tz = "UTC"
      ),
      conc = measurement
    ) %>%
    filter(!is.na(date))
}

# UK-AIR tab-delimited SMPS ("2021 SMPS - 15min.txt" files).
# These are 51-bin size distributions in long format: one row per
# timestamp × diameter bin. PC_xxx parameter names encode the diameter
# midpoint in nm (underscores replace decimal points: PC_16_6 = 16.6 nm).
# Pivots to wide format: date + one column per diameter midpoint.
read_ukair_tab_smps <- function(f) {
  df <- read_tsv(f, show_col_types = FALSE)
  df %>%
    filter(Validity_id == 1, measurement != -9999) %>%
    mutate(
      date = parse_date_time(
        paste(`measurement start date`, `measurement start time`),
        orders = c("dmy HM", "dmy HMS"), tz = "UTC"
      ),
      diameter = as.numeric(gsub("_", ".", sub("^PC_", "", parameter_id)))
    ) %>%
    filter(!is.na(date), !is.na(diameter)) %>%
    select(date, diameter, measurement) %>%
    pivot_wider(names_from = diameter, values_from = measurement)
}

# measurement_upload txt CPC (unclassified/, TNC parameter, hourly).
# All unclassified files are multi-site; use the station argument to filter
# to a single site by its "Station name" value.
# Duplicate timestamps (original + revisions) are resolved by taking the mean.
#
# station values: "London Marylebone Road", "London Honor Oak Park",
#                 "Chilbolton Observatory"
read_upload_txt_cpc <- function(f, station = NULL) {
  df <- read_tsv(f, show_col_types = FALSE)
  if (!is.null(station))
    df <- filter(df, `Station name` == station)
  df %>%
    filter(Validity_id == 1, measurement != -9999) %>%
    transmute(
      date = parse_date_time(
        paste(`measurement start date`, `measurement start time`),
        orders = c("dmy HMS", "dmy HM"), tz = "UTC"
      ),
      conc = measurement
    ) %>%
    filter(!is.na(date)) %>%
    group_by(date) %>%
    summarise(conc = mean(conc, na.rm = TRUE), .groups = "drop")
}

# measurement_upload xlsx CPC (unclassified/, for 2022 where no txt exists).
# All unclassified files are multi-site; use the station argument to filter.
# readxl gives start date as POSIXct at midnight; start time as POSIXct at
# 1899-12-31 + the actual time — combine by stripping each to its component.
read_upload_xlsx_cpc <- function(f, station = NULL) {
  df <- read_excel(f, sheet = 1)
  if (!is.null(station))
    df <- filter(df, `Station name` == station)
  df %>%
    filter(Validity_id == 1, measurement != -9999) %>%
    transmute(
      date = as.POSIXct(
        format(`measurement start date`, "%Y-%m-%d"),
        tz = "UTC"
      ) + as.numeric(
        difftime(`measurement start time`,
                 as.POSIXct("1899-12-31", tz = "UTC"),
                 units = "secs")
      ),
      conc = measurement
    ) %>%
    filter(!is.na(date)) %>%
    group_by(date) %>%
    summarise(conc = mean(conc, na.rm = TRUE), .groups = "drop")
}

# SMPS xls/xlsx ("Data" sheet): works for both old (2020-2022) and new (2023)
# formats — both have Date/Time as col 1 and numeric diameter names for bins.
# Drops Total, TNC-SMPS, and any empty/non-numeric columns.
read_smps_excel <- function(f) {
  df <- suppressWarnings(read_excel(f, sheet = "Data"))
  bin_cols <- names(df)[!is.na(suppressWarnings(as.numeric(names(df))))]
  bind_cols(
    tibble(date = as.POSIXct(df[[1]], tz = "UTC")),
    df[, bin_cols]
  ) %>%
    filter(!is.na(date))
}

write_site_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_csv(df, path)
  message("  wrote ", nrow(df), " rows  →  ", path)
}


# ── Chilbolton CPC ────────────────────────────────────────────────────────────
message("\n── Chilbolton CPC")

read_pc_annual_cpc(
  file.path(DATADIR, "chilbolton/Chilbolton_pc_Annual_2020_v01.xls")
) %>% write_site_csv(file.path(DATADIR, "chilbolton/cpc/chilbolton_cpc_2020.csv"))

# 2021: unclassified upload (all three sites in one file)
read_upload_txt_cpc(
  file.path(DATADIR, "unclassified/CPC 2021.txt"),
  station = "Chilbolton Observatory"
) %>% write_site_csv(file.path(DATADIR, "chilbolton/cpc/chilbolton_cpc_2021.csv"))

# 2023 annual xlsx covers full year; Jan-Feb txt is a subset — use xlsx only
read_pc_annual_cpc(
  file.path(DATADIR, "chilbolton/Chilbolton_pc_Annual_2023_v01.xlsx")
) %>% write_site_csv(file.path(DATADIR, "chilbolton/cpc/chilbolton_cpc_2023.csv"))


# ── Chilbolton SMPS ───────────────────────────────────────────────────────────
message("\n── Chilbolton SMPS")

for (yr in 2020:2022) {
  ff <- list.files(file.path(DATADIR, "chilbolton/smps"),
                   pattern = as.character(yr), full.names = TRUE)
  ff <- ff[grepl("\\.xls$", ff, ignore.case = TRUE)]
  if (length(ff))
    read_smps_excel(ff[1]) %>%
      write_site_csv(file.path(
        DATADIR, sprintf("chilbolton/smps/chilbolton_smps_%d.csv", yr)))
}

# 2023: Jan-Feb only (51-bin format identical to 2020-2022)
read_smps_excel(
  file.path(DATADIR, "chilbolton/smps/SMPS_Size_Chilbolton_2023_Jan-Feb_v06.xlsx")
) %>% write_site_csv(file.path(DATADIR, "chilbolton/smps/chilbolton_smps_2023.csv"))


# ── HOP CPC ───────────────────────────────────────────────────────────────────
message("\n── HOP CPC")
# Note: Harwell_pc_Annual_2020_v01.xls skipped (Harwell site, not HOP)

# 2021: unclassified upload (all three sites in one file)
read_upload_txt_cpc(
  file.path(DATADIR, "unclassified/CPC 2021.txt"),
  station = "London Honor Oak Park"
) %>% write_site_csv(file.path(DATADIR, "hop/cpc/hop_cpc_2021.csv"))

read_pc_annual_cpc(
  file.path(DATADIR, "hop/cpc/Honor Oak Park_pc_Annual_2023_v01.xlsx")
) %>% write_site_csv(file.path(DATADIR, "hop/cpc/hop_cpc_2023.csv"))


# ── HOP SMPS ─────────────────────────────────────────────────────────────────
message("\n── HOP SMPS")

for (yr in 2020:2022) {
  ff <- list.files(file.path(DATADIR, "hop/smps"),
                   pattern = as.character(yr), full.names = TRUE)
  ff <- ff[grepl("\\.xls$", ff, ignore.case = TRUE)]
  if (length(ff))
    read_smps_excel(ff[1]) %>%
      write_site_csv(file.path(
        DATADIR, sprintf("hop/smps/hop_smps_%d.csv", yr)))
}

# 2023: two files because the bin structure changed at the instrument swap
read_smps_excel(
  file.path(DATADIR, "hop/smps/SMPS_Size_Honor Oak Park_2023_Jan-Feb_v06.xlsx")
) %>% write_site_csv(
  file.path(DATADIR, "hop/smps/hop_smps_2023_jan_feb.csv"))

read_smps_excel(
  file.path(DATADIR, "hop/smps/SMPS_Size_Honor Oak Park_2023_Mar onwards_v01.xlsx")
) %>% write_site_csv(
  file.path(DATADIR, "hop/smps/hop_smps_2023_mar_onwards.csv"))


# ── Marylebone CPC ────────────────────────────────────────────────────────────
message("\n── Marylebone CPC")

# 2019: only source is hourly unclassified upload
read_upload_txt_cpc(
  file.path(DATADIR, "unclassified/CPC 2019.txt"),
  station = "London Marylebone Road"
) %>% write_site_csv(file.path(DATADIR, "marylebone/cpc/marylebone_cpc_2019.csv"))

# 2020: 15-min _pc_Annual_ preferred over hourly unclassified
read_pc_annual_cpc(
  file.path(DATADIR, "marylebone/Marylebone Road_pc_Annual_2020_v01.xls")
) %>% write_site_csv(file.path(DATADIR, "marylebone/cpc/marylebone_cpc_2020.csv"))

# 2021: unclassified upload (all three sites in one file)
read_upload_txt_cpc(
  file.path(DATADIR, "unclassified/CPC 2021.txt"),
  station = "London Marylebone Road"
) %>% write_site_csv(file.path(DATADIR, "marylebone/cpc/marylebone_cpc_2021.csv"))

# 2022: only source is hourly unclassified upload (xlsx only, no txt)
read_upload_xlsx_cpc(
  file.path(DATADIR, "unclassified/CPC 2022.xlsx"),
  station = "London Marylebone Road"
) %>% write_site_csv(file.path(DATADIR, "marylebone/cpc/marylebone_cpc_2022.csv"))

# 2023: 15-min _pc_Annual_ preferred over hourly unclassified
read_pc_annual_cpc(
  file.path(DATADIR, "marylebone/Marylebone Road_pc_Annual_2023_v01.xlsx")
) %>% write_site_csv(file.path(DATADIR, "marylebone/cpc/marylebone_cpc_2023.csv"))


# ── Marylebone SMPS ───────────────────────────────────────────────────────────
message("\n── Marylebone SMPS")

for (yr in 2020:2022) {
  ff <- list.files(file.path(DATADIR, "marylebone/smps"),
                   pattern = as.character(yr), full.names = TRUE)
  ff <- ff[grepl("\\.xls$", ff, ignore.case = TRUE)]
  if (length(ff))
    read_smps_excel(ff[1]) %>%
      write_site_csv(file.path(
        DATADIR, sprintf("marylebone/smps/marylebone_smps_%d.csv", yr)))
}

# 2023: 122-bin format; "New SMPS 2023.txt" has same data but less complete
read_smps_excel(
  file.path(DATADIR,
    "marylebone/smps/SMPS_Size_Marylebone Road_Annual_Ratified_2023_v01.xlsx")
) %>% write_site_csv(
  file.path(DATADIR, "marylebone/smps/marylebone_smps_2023.csv"))

message("\nDone.")
