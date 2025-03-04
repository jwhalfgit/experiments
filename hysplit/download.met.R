# Used for downloading met files from the NOAA repository

METFILEDIR <- "C:/HYSPLIT/metfiles/"


# getMet() function from David Carslaw's openair book:
# https://bookdown.org/david_carslaw/openair/sections/appendices/appendix-hysplit.html
#


getMet <- function(year = 2021, month = 1, path_met = METFILEDIR) {
  
  for (i in seq_along(year)) {
    for (j in seq_along(month)) {
      download.file(
        url = paste0(
          "https://www.ready.noaa.gov/data/archives/gdas1/gdas1",
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

# getMetGdas is an altered function of getMet for use with 1 degree GDAS
# files
getMetGdas <- function(year = 2021, month = 1:12, path_met = METFILEDIR) {
  selectMonths <- tolower(month.abb[month])
  for (i in seq_along(year)) {
    for (j in seq_along(month)) {
      if(selectMonths[j]=="feb"){
        for(k in 1:4){
        download.file(
          url = paste0(
            #"https://www.ready.noaa.gov/data/archives/gdas1/gdas1.",
            "ftp://ftp.arl.noaa.gov/archives/gdas1/gdas1.",
             selectMonths[j],sub("20","",year[i]), ".w",k
          ),
          destfile = paste0(
            path_met, year[i],
            sprintf("%02d", month[j]), ".w",k
          ),
          mode = "wb"
        )}}else{
          for(k in 1:5){
            download.file(
              url = paste0(
                #"https://www.ready.noaa.gov/data/archives/gdas1/gdas1.",
                "ftp://ftp.arl.noaa.gov/archives/gdas1/gdas1.",
                selectMonths[j],sub("20","",year[i]), ".w",k
              ),
              destfile = paste0(
                path_met, year[i],
                sprintf("%02d", month[j]), ".w",k
              ),
              mode = "wb"
            )}
      }
    }
  }
}

getMetGdas(year = 2024, month = 1:12)



