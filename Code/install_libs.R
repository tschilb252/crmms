## Packages used in the CRMMS repository 
listOfPackages <- c("remotes", "RWDataPlyr", "tidyverse", "lubridate", "RWcheck",
                    "readxl", "zoo", "rhdb", "crssplot")

# loop through libs to see if they are installed; if not download
for (i in listOfPackages){
  if( !(i %in% installed.packages()) )
    if (i %in% c('RWcheck', "rhdb")) {
      remotes::install_github(paste0("BoulderCodeHub/", i), ref = 'main')
    } else if (i %in% c("crssplot")) {
      remotes::install_github('rabutler-usbr/crssplot', ref = 'main')
    }  else {
      install.packages(i, dependencies = TRUE)
    }
}

# Notes: this will also install packages that are not on CRAN
# remotes::install_github("BoulderCodeHub/RWcheck", ref = 'main')
# remotes::install_github("BoulderCodeHub/rhdb", ref = 'main')
# remotes::install_github('rabutler-usbr/crssplot', ref = 'main')