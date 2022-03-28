#' @title filter_nps: Filter a dataset by records inside a national park/monument.
#'
#' @importFrom rgdal readOGR
#' @importFrom sp coordinates proj4string over
#' @importFrom downloader download
#' @importFrom utils unzip
#' @importFrom rgdal readOGR
#' @importFrom tidyr %>%
#' @importFrom dplyr rename
#'
#' @description A simple function that will take a dataframe, filter by records inside ANP, and return a 
#' cleaned dataframe. IMPORTANT: This function rely's on the dataframe having lat long data seperated 
#' in two different columns (one for lat and one for long).
#'
#' @param df Name of the dataframe you have read in
#' @param park The quoted name of the national park/monument that you want to filter records by. REQUIRES
#' name format to be exact. Find a list of the 427 park names at this link: 
#' @param download.rm \code{TRUE} or \code{FALSE}. The function downloads spatial files to the working 
#' directory which used to filter the dataframe. If \code{TRUE} these will be removed from working 
#' directory. Defaults to \code{TRUE}.
#'
#' @return Returns the dataframe that was input with the same structure, but filtered to records inside 
#' the specified park/monument
#'
#' @export


filter_nps <- function(df, park, download.rm = TRUE){
  
  download('https://irma.nps.gov/DataStore/DownloadFile/668434', dest="nps_bound.zip") 
  
  unzip("nps_bound.zip")
  
  nps.bounds <- readOGR(unzip("nps_bound.zip", "nps_boundary.shp"), verbose = FALSE)
  
  select.bounds <- nps.bounds[nps.bounds@data$UNIT_NAME==paste(park), ]
  
  if("LATITUDE" %in% colnames(df) == TRUE) {
    df <- df %>% rename(latitude = LATITUDE)
  }
  
  if("Latitude" %in% colnames(df) == TRUE) {
    df <- df %>% rename(latitude = Latitude)
  }

  if("Lat" %in% colnames(df) == TRUE) {
    df <- df %>% rename(latitude = Lat)
  }
  
  if("lat" %in% colnames(df) == TRUE) {
    df <- df %>% rename(latitude = lat)
  }

  if("LONGITUDE" %in% colnames(df) == TRUE) {
    df <- df %>% rename(longitude = LONGITUDE)
  }
  
  if("Longitude" %in% colnames(df) == TRUE) {
    df <- df %>% rename(longitude = Longitude)
  }
  
  if("Long" %in% colnames(df) == TRUE) {
    df <- df %>% rename(longitude = Long)
  }
  
  if("long" %in% colnames(df) == TRUE) {
    df <- df %>% rename(longitude = long)
  }
  
  
  coordinates(df) <- c("longitude", "latitude")
  
  proj4string(df) <- CRS("+init=epsg:4326")
  proj4string(select.bounds) <- CRS("+init=epsg:4326")
  
  output <- over(select.bounds, df, returnList = TRUE) 
  
  output.df <- as.data.frame(output$`403`)
  
  
  if(download.rm == TRUE) {
    file.remove(c("nps_bound.zip", "nps_boundary.xml", "nps_boundary.dbf", "nps_boundary.prj", "nps_boundary.shp", "nps_boundary.shx"))}
  
  
  return(output.df)

}





prac <- read.csv("ebird_mappingloc_20220216 copy.csv", header = TRUE)

prac <- prac %>% rename(Latitude = latitude, Longitude = longitude)

eh <- filter_nps(prac, "Acadia National Park")



