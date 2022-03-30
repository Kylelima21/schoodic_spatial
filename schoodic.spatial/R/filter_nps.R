#' @title filter_nps: Filter a dataset by records inside a national park/monument.
#' @author Kyle Lima
#'
#' @importFrom sp coordinates over
#' @importFrom downloader download
#' @importFrom utils unzip
#' @importFrom rgdal readOGR CRS
#' @importFrom tidyr %>%
#' @importFrom dplyr rename
#' @importFrom methods slot
#'
#' @description A simple function that will take a dataframe, filter by records inside ANP, and return a
#' cleaned dataframe. IMPORTANT: This function only work for lat long data seperated
#' in two different columns (one for lat and one for long).
#'
#' @param df Name of the dataframe you have read in.
#' @param park The quoted name of the national park/monument that you want to filter records by. REQUIRES
#' name format to be exact. Find a list of the 427 park names at this link: https://rpubs.com/klima21/filternps.
#' @param lat The quoted column name that is your latitude data.
#' @param long The quoted column name that is your longitude data.
#'
#' @return Returns a dataframe of the same structure, but filtered to records inside
#' the specified park/monument. Some column names may change.
#'
#' @example
#'
#' # Read in data from working directory
#' bird.dat <- read.csv("ebird_mappingloc_20220217.csv")
#'
#' # Use filter_nps function to filter the bird.dat dataframe to records inside Acadia National Park
#' bird.anp <- filter_nps(bird.dat, "Acadia National ParK", lat = "y", long = "x")
#'
#' @export

# require(downloader)
# require(dplyr)
# require(methods)
# require(rgdal)
# require(sp)
# require(tidyr)
# require(utils)



filter_nps <- function(df, park, lat, long) {

  if (file.exists("nps_bound.zip") == FALSE) {
    download('https://irma.nps.gov/DataStore/DownloadFile/668434', dest="nps_bound.zip")
  }


  if (file.exists("nps_boundary.shp") == FALSE) {
    unzip("nps_bound.zip")
  }


  nps.bounds <- readOGR(unzip("nps_bound.zip", "nps_boundary.shp"), verbose = FALSE)

  select.bounds <- nps.bounds[nps.bounds@data$UNIT_NAME==paste(park), ]


  if (length(select.bounds@polygons) < 1) {
    stop("Function returned a park with 0 polygons. The park name does not exist. Go to https://rpubs.com/klima21/filternps for a list of valid park names.")
  }


  df <- df %>% rename(latitude=paste(lat), longitude=paste(long))

  df$"long" <- df$longitude
  df$"lat" <- df$latitude

  coordinates(df) <- c("long", "lat")

  slot(df, "proj4string") <- slot(select.bounds, "proj4string")

  output <- over(select.bounds, df, returnList = TRUE)

  output.df <- as.data.frame(output$`403`)


  if(length(output.df) < 1) {
    stop("Function returned a dataframe with 0 records. Records may not be within the specifed park boundaries")
  }


  if(length(output.df) > 1) {
    message("Calculations complete!")
  }

  return(output.df)

}





