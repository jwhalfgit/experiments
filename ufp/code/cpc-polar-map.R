# cpc-polar-map.R
# Seasonal polar-rose plots of CPC particle number concentration for the six
# focus sites, overlaid on a basemap using openairmaps::polarMap().
# Also saves per-site static PNG polar plots via openair::polarPlot().
#
# Source sourceMeFirst_ufp.R before running.
# Met data is cached to CACHE_DIR/polar_met_cache.Rds after the first download.

source("sourceMeFirst_ufp.R")

# ── Focus sites ────────────────────────────────────────────────────────────────
# Six sites with both CPC records (cpc_all.Rds) and nearby AURN met stations.
# met_code: passed to importUKAQ(); cpc_label: matches the site column in cpc_all.

POLAR_SITES <- tribble(
  ~site_key,    ~cpc_label,               ~met_code,
  "baqs",       "BAQS",                   "bmld",
  "maqs",       "MAQS",                   "mahg",
  "hop",        "London Honor Oak Park",  "hp1",
  "marylebone", "London Marylebone Rd",   "my1",
  "kensington", "London N. Kensington",   "kc1",
  "chilbolton", "Chilbolton",             "chbo"
)

# Pull accurate lat/lon from AURN metadata rather than hardcoding.
aurn_meta   <- importMeta(source = "aurn", all = TRUE)
aurn_coords <- aurn_meta %>%
  mutate(code_lc = tolower(code)) %>%
  filter(code_lc %in% POLAR_SITES$met_code) %>%
  select(code_lc, latitude, longitude) %>%
  distinct(code_lc, .keep_all = TRUE)

POLAR_SITES <- POLAR_SITES %>%
  left_join(aurn_coords, by = c("met_code" = "code_lc"))

# ── Load / cache met data ──────────────────────────────────────────────────────
MET_CACHE <- file.path(CACHE_DIR, "polar_met_cache.Rds")

if (file.exists(MET_CACHE)) {
  message("Loading cached met data from ", basename(MET_CACHE))
  met_all <- readRDS(MET_CACHE)
} else {
  message("Downloading met data from AURN (", nrow(POLAR_SITES), " sites)...")
  met_all <- map_dfr(seq_len(nrow(POLAR_SITES)), function(i) {
    info <- POLAR_SITES[i, ]
    message("  ", info$met_code, "...")
    importUKAQ(site = info$met_code, year = 2019:2025, source = "aurn") %>%
      select(date, ws, wd) %>%
      mutate(met_code = info$met_code)
  })
  saveRDS(met_all, MET_CACHE)
  message("Met data cached to ", basename(MET_CACHE))
}

# ── Load CPC data ──────────────────────────────────────────────────────────────
load(file.path(CACHE_DIR, "cpc_all.Rds"))   # → cpc_all: date, conc, site  (site == cpc_label)

# ── Join CPC + met per site, add coordinates ───────────────────────────────────
polar_data <- map_dfr(seq_len(nrow(POLAR_SITES)), function(i) {
  info <- POLAR_SITES[i, ]

  cpc_site <- cpc_all %>%
    filter(site == info$cpc_label) %>%
    mutate(date = floor_date(date, "1 hour")) %>%
    group_by(date) %>%
    summarise(conc = mean(conc, na.rm = TRUE), .groups = "drop")

  met_site <- met_all %>%
    filter(met_code == info$met_code) %>%
    select(date, ws, wd) %>%
    mutate(date = floor_date(date, "1 hour"))

  inner_join(cpc_site, met_site, by = "date") %>%
    mutate(
      site      = info$cpc_label,
      latitude  = info$latitude,
      longitude = info$longitude
    )
}) %>%
  filter(!is.na(ws), !is.na(wd), !is.na(conc), conc > 0)

message("Merged dataset: ", nrow(polar_data), " hourly rows across ",
        n_distinct(polar_data$site), " sites")

# ── Output directories ─────────────────────────────────────────────────────────
MAPDIR <- file.path(PLOTDIR, "polar_maps")
POLDIR <- file.path(PLOTDIR, "polar_plots")
dir.create(MAPDIR, showWarnings = FALSE, recursive = TRUE)
dir.create(POLDIR, showWarnings = FALSE, recursive = TRUE)

# ── Generate per-site × season polar plot PNGs for embedding in the map ────────
# polarMap() produces blank squares when it can't render embedded images, so we
# generate the PNGs ourselves with polarPlot() (which we know works) and embed
# them as base64 data URIs in a hand-built Leaflet map.

IMGDIR <- file.path(MAPDIR, "img")
dir.create(IMGDIR, showWarnings = FALSE, recursive = TRUE)

SEASON_LEVELS <- c("winter (DJF)", "spring (MAM)", "summer (JJA)", "autumn (SON)")

polar_data_s <- polar_data %>%
  mutate(season = case_when(
    month(date) %in% c(12, 1, 2) ~ "winter (DJF)",
    month(date) %in% c( 3, 4, 5) ~ "spring (MAM)",
    month(date) %in% c( 6, 7, 8) ~ "summer (JJA)",
    month(date) %in% c( 9,10,11) ~ "autumn (SON)"
  ))

img_registry <- list()

for (i in seq_len(nrow(POLAR_SITES))) {
  info    <- POLAR_SITES[i, ]
  df_site <- polar_data_s %>% filter(site == info$cpc_label)

  for (ssn in SEASON_LEVELS) {
    df_ssn <- df_site %>% filter(season == ssn)
    if (nrow(df_ssn) < 50) next

    img_path <- file.path(IMGDIR,
                          paste0(info$site_key, "_",
                                 gsub("[^a-z]", "_", ssn), ".png"))

    # 400×400 px source → displayed at 200×200 in the map (retina-quality).
    # bg = "transparent" makes the device canvas transparent; par.settings
    # stops lattice from filling it with its own background rectangle.
    png(img_path, width = 400, height = 400, res = 72, bg = "transparent")
    polarPlot(df_ssn,
              pollutant    = "conc",
              main         = paste0(info$cpc_label, "\n", ssn),
              angle.scale  = 315,
              statistic    = "mean",
              key          = FALSE,
              par.settings = list(background = list(col = "transparent")))
    dev.off()

    img_registry[[length(img_registry) + 1]] <- list(
      site_key  = info$site_key,
      site      = info$cpc_label,
      season    = ssn,
      latitude  = info$latitude,
      longitude = info$longitude,
      img_path  = img_path
    )
  }
}
message("Generated ", length(img_registry), " polar plot images")

# ── Build Leaflet map with seasonal toggle layers ──────────────────────────────
# Each season is a named layer group; the layer control shows them as radio
# buttons (baseGroups) so only one season is visible at a time.
m <- leaflet() %>%
  addProviderTiles("CartoDB.Positron")

for (ssn in SEASON_LEVELS) {
  entries <- Filter(function(x) x$season == ssn, img_registry)
  for (e in entries) {
    icon <- makeIcon(
      iconUrl     = paste0("data:image/png;base64,", base64encode(e$img_path)),
      iconWidth   = 200,
      iconHeight  = 200,
      iconAnchorX = 100,
      iconAnchorY = 100
    )
    m <- m %>%
      addMarkers(
        lat   = e$latitude,
        lng   = e$longitude,
        icon  = icon,
        label = paste0(e$site, " (", ssn, ")"),
        group = ssn
      )
  }
}

m <- m %>%
  addLayersControl(
    baseGroups = SEASON_LEVELS,
    options    = layersControlOptions(collapsed = FALSE)
  )

saveWidget(m,
           file          = file.path(MAPDIR, "cpc_polar_map_seasonal.html"),
           selfcontained = TRUE)
message("Saved cpc_polar_map_seasonal.html")

# ── Static seasonal polar plots per site (openair, PNG for papers) ─────────────
# openair::polarPlot calls print() internally so the active PNG device receives
# the output even inside a for loop.
for (i in seq_len(nrow(POLAR_SITES))) {
  info    <- POLAR_SITES[i, ]
  df_site <- polar_data %>% filter(site == info$cpc_label)

  if (nrow(df_site) < 100) {
    message("Skipping ", info$cpc_label, " (too few rows: ", nrow(df_site), ")")
    next
  }

  out_file <- file.path(POLDIR, paste0("cpc_polar_seasonal_", info$site_key, ".png"))
  png(out_file, width = 2400, height = 2400, res = 200)
  polarPlot(df_site,
            pollutant   = "conc",
            type        = "season",
            main        = info$cpc_label,
            key.footer  = "#/cm³",
            angle.scale = 315,
            statistic   = "mean")
  dev.off()
  message("Saved ", basename(out_file))
}
