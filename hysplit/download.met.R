# Used for downloading met files from the NOAA repository

METFILEDIR <- "C:/HYSPLIT/metfiles"


# getMet() function from David Carslaw's openair book:
# https://bookdown.org/david_carslaw/openair/sections/appendices/appendix-hysplit.html

getMet <- function(year = 2013, month = 1, path_met = METFILEDIR) {
  for (i in seq_along(year)) {
    for (j in seq_along(month)) {
      download.file(
        url = paste0(
          "https://www.ready.noaa.gov/data/archives/reanalysis/RP",
          year[i], sprintf("%02d", month[j]), ".gbl"
        ),
        destfile = paste0(
          path_met, "RP", year[i],
          sprintf("%02d", month[j]), ".gbl"
        ),
        mode = "wb"
      )
    }
  }
}


getMet(year = 2024, month = 1:12)
