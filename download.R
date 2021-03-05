##' Download Phenocam data
##'

URL="https://data.ecoforecast.org/targets/phenology/phenology-targets.csv.gz"

dat <- readr::read_csv(URL)
