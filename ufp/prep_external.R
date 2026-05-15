# prep_external.R
# File-preparation functions that reformat raw SMPS data into the layout
# expected by PyNSD:
#   - column named "date" (POSIXct UTC)
#   - remaining columns named as plain decimal diameter midpoints (nm)
#   - all other columns (prefixed raw counts, summary statistics,
#     QC flags, instrument metadata) dropped
#
# Use write_working_csv() from load_ufp.R to persist the result.


# prep_smps_external() --------------------------------------------------------
# Reads raw SMPS CSV files and returns a named list of tibbles in the
# external-tool format (date column + plain numeric diameter columns).
# One tibble is returned per input file, named by the file stem, so files
# with different bin structures are never silently merged. The caller can
# combine files that share identical bins with bind_rows() if desired.
#
# Handles four file formats automatically:
#   baqs           — date column is unnamed (read_csv assigns "...1"); bin
#                    columns named "PN_<diam>"; "RA_<diam>" raw-count columns
#                    are dropped.
#   maqs ratified  — date column named "datetime"; bin columns already numeric;
#                    summary-statistic and metadata columns are dropped.
#   maqs raw AIM   — 52 metadata header rows; date column "DateTime Sample
#                    Start" (DD/MM/YYYY HH:MM:SS); "_<diam>" raw-count columns
#                    are dropped.
#   new sites      — date column named "date"; bin columns already numeric;
#                    (written by prepare_new_sites.R — already nearly correct).
#
# Arguments:
#   FF — character vector of CSV file paths
#
# Returns: named list of tibbles, names are file stems (basename without ext)

prep_smps_external <- function(FF) {

  result <- vector("list", length(FF))

  for (ix in seq_along(FF)) {
    first_line <- readLines(FF[ix], n = 1)
    is_maqs    <- FALSE

    if (grepl("AIM Version", first_line, ignore.case = TRUE)) {
      # raw maqs AIM format: 52 metadata rows before the column header
      df       <- read_csv(FF[ix], skip = 52, show_col_types = FALSE)
      df       <- filter(df, `Detector Status` == "Normal Scan") %>% 
        filter(if_any(-date, ~. < 1e8)) # massive spike 9 April 2025 across lots of bins
      date_vec <- dmy_hms(df$`DateTime Sample Start`, tz = "UTC")
      is_maqs  <- TRUE
    } else {
      df <- read_csv(FF[ix], show_col_types = FALSE)
      if ("datetime" %in% names(df)) {
        date_vec <- as.POSIXct(df$datetime, tz = "UTC")
        is_maqs  <- TRUE
      } else if ("date" %in% names(df)) {
        date_vec <- as.POSIXct(df$date, tz = "UTC")
      } else {
        # baqs: first column is unnamed in the CSV, read_csv names it ...1
        date_vec <- as.POSIXct(df[[1]], tz = "UTC")
      }
    }

    # Strip PN_ prefix, then keep only columns whose name is a plain number
    # >= 10 nm. _-prefixed raw-count columns (AIM format) are also excluded.
    # Per James Brean, the SMPS can't reliably see particles < 10 nm.
    stripped <- sub("^PN_", "", names(df))
    num_vals <- suppressWarnings(as.numeric(stripped))
    bin_idx  <- which(!is.na(num_vals) & num_vals >= 10)

    bin_data        <- df[, bin_idx]
    names(bin_data) <- stripped[bin_idx]

    # PyNSD doesn't automatically remove all 0 rows.
    # Data from 13 Dec 2023 12:00 UTC through 31 Dec 2023 are also excluded
    # (instrument fault / bad data period).
    tbl <- bind_cols(tibble(date = date_vec), bin_data) %>%
      filter(!if_all(all_of(names(bin_data)), ~is.na(.x) | .x == 0)) %>%
      filter(!(date > as.POSIXct("2023-12-13 12:00:00", tz = "UTC") &
               date <= as.POSIXct("2023-12-31 23:59:59", tz = "UTC")))

    if (is_maqs) {
      # maqs SMPS is 5-minute resolution; average to 1-hour before export
      tbl <- tbl %>%
        mutate(date = floor_date(date, "1 hour")) %>%
        group_by(date) %>%
        summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
        mutate(across(-date, ~if_else(is.nan(.x), NA_real_, .x)))
    }

    result[[ix]]       <- tbl
    result[[ix]]$date  <- as.character(format(result[[ix]]$date))
  }

  setNames(result, tools::file_path_sans_ext(basename(FF)))
}


# smps_dataset_summary() ------------------------------------------------------
# Produces a summary tibble (and optionally a CSV) describing each SMPS
# dataset: time coverage and logarithmic bin width (mean Δlog10 Dp).
#
# Arguments:
#   datasets    — named list; names are dataset labels, values are character
#                 vectors of file paths belonging to that dataset.
#                 Group files by bin structure so each entry gets one row.
#   output_path — if provided, the summary is written to this CSV path.
#
# Example:
#   datasets <- list(
#     baqs_smps              = list.files(file.path(DATADIR, "baqs/smps/raw"),        full.names = TRUE),
#     maqs_smps              = list.files(file.path(DATADIR, "maqs/smps/raw"),        full.names = TRUE),
#     chilbolton_smps        = list.files(file.path(DATADIR, "chilbolton/smps/raw"),  full.names = TRUE),
#     hop_smps_51bin         = list.files(file.path(DATADIR, "hop/smps/raw"),         full.names = TRUE, pattern = "hop_smps_20(20|21|22)|jan_feb"),
#     hop_smps_122bin        = file.path(DATADIR,  "hop/smps/raw/hop_smps_2023_mar_onwards.csv"),
#     marylebone_smps_51bin  = list.files(file.path(DATADIR, "marylebone/smps/raw"),  full.names = TRUE, pattern = "marylebone_smps_20(20|21|22)"),
#     marylebone_smps_122bin = file.path(DATADIR,  "marylebone/smps/raw/marylebone_smps_2023.csv")
#   )
#   smps_dataset_summary(datasets, output_path = file.path(DATADIR, "smps_summary.csv"))

smps_dataset_summary <- function(datasets, output_path = NULL) {

  rows <- map(names(datasets), function(nm) {
    FF <- datasets[[nm]]

    # Bin width: read only the header of each file
    bin_widths <- map_dbl(FF, function(f) {
      first_line <- readLines(f, n = 1)
      skip_n <- if (grepl("AIM Version", first_line, ignore.case = TRUE)) 52L else 0L
      cols     <- names(read_csv(f, n_max = 0, skip = skip_n, show_col_types = FALSE))
      stripped <- sub("^PN_", "", cols)
      num_vals <- suppressWarnings(as.numeric(stripped))
      diams    <- sort(num_vals[!is.na(num_vals) & num_vals >= 10])
      if (length(diams) < 2) return(NA_real_)
      mean(diff(log10(diams)))
    })

    # Time coverage: read only the date column of each file
    date_lists <- map(FF, function(f) {
      first_line <- readLines(f, n = 1)
      if (grepl("AIM Version", first_line, ignore.case = TRUE)) {
        df <- read_csv(f, skip = 52,
                       col_select = c("DateTime Sample Start"),
                       show_col_types = FALSE)
        dmy_hms(df$`DateTime Sample Start`, tz = "UTC")
      } else {
        df  <- read_csv(f, col_select = any_of(c("date", "datetime")),
                        show_col_types = FALSE)
        col <- if ("datetime" %in% names(df)) df$datetime else df$date
        as.POSIXct(col, tz = "UTC")
      }
    })
    all_dates <- do.call(c, date_lists)

    tibble(
      dataset   = nm,
      starttime = min(all_dates, na.rm = TRUE),
      endtime   = max(all_dates, na.rm = TRUE),
      bin_width = round(mean(bin_widths, na.rm = TRUE), 4)
    )
  })

  result <- bind_rows(rows)

  if (!is.null(output_path)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    write_csv(result, output_path)
    message("wrote ", nrow(result), " rows  →  ", output_path)
  }

  invisible(result)
}
