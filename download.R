##' Download Phenocam data
##'
##' @param URL  web address where data is archived
##' @param skipNum The number of lines to skip (22 for 1day file and 17 for roistats file)
##' @export

download.phenocam <- function(URL,skipNum=0) {
  ## check that we've been passed a URL
  if (length(URL) == 1 & is.character(URL) & substr(URL,1,4)=="http") {
    
    ## read data
    dat <- readr::read.csv(URL)
    
    ## convert date
    dat$date <- as.Date(as.character(dat$date))
    
    return(dat)
  } else {
    print(paste("download.phenocam: Input URL not provided correctly",URL))
  }
}

URL= "https://data.ecoforecast.org/targets/phenology/phenology-targets.csv.gz"

