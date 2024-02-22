#' Discover countries according latitude and longitude
#' @param assembleages Input data with values
#' @return assembleages assembleages with countrie filled
#' @export
#' @examples
#' fillCountryAccordingLatLon()
fillCountryAccordingLatLon <- function(assembleages){

  # Replace commas with dots in exact_lat and exact_long columns
  assembleages$exact_lat <- gsub(",", ".", assembleages$exact_lat)
  assembleages$exact_long <- gsub(",", ".", assembleages$exact_long)
  
  # Convert the columns to numeric
  assembleages$exact_lat <- as.numeric(assembleages$exact_lat)
  assembleages$exact_long <- as.numeric(assembleages$exact_long)
  com <- assembleages %>% 
    group_by(id_study) %>% 
    mutate(lon = mean(exact_long, na.rm = TRUE),
           lat = mean(exact_lat, na.rm = TRUE)) %>% 
    filter(!is.na(lon))
  
  
  #Country names
  library(rworldmap)
  
  countriesSP <- st_as_sf(getMap(resolution='low'))
  comesp2 <- st_as_sf(com, 
                      coords = c('lon', 'lat'),
                      crs = 4326, #WGS84
                      remove = F)
  sf_use_s2(FALSE)
  
  cat("fillCountryAccordingLatLon.R: Unifying CRS coordinates with WSG84 values\n")
  # Set CRS to WGS84 for comesp2
   comesp2 <- st_set_crs(comesp2, st_crs("+proj=longlat +datum=WGS84"))
  # Set CRS to WGS84 for countriesSP
   countriesSP <- st_set_crs(countriesSP, st_crs("+proj=longlat +datum=WGS84"))

  cat("fillCountryAccordingLatLon.R: Joining data from assembleages and general info from countries\n")
  country <- st_join(comesp2, countriesSP, join = st_within)
  
  # Update NA values in the country column with values from the NAME column
  country <- country %>%
    mutate(country = ifelse(is.na(country), as.character(NAME), country))

  #Check how many countries are NA
  country %>% select(country, NAME) %>% filter(is.na(country))

  rm(com, comesp2, countriesSP) #Remove variables that will not be used anymore
  #In case you waant to record the file because so many rows without contry
  #write.csv(country, file = "inst/comm_nodist_plants.csv", row.names = FALSE)
  # Convert sf object to dataframe
  country <- st_drop_geometry(country)

  return(country)
}
  


#CODE TO UPDATE COUNTRIS IN SOURCE WITH VALUES ==NA AND FILLING SEARCHING LAT AND LON POSITION
source("loadLibraries.R");loadLibraries() # Load all libraries
#Function to work only R without excel filling country
assembleages <- read.csv("inst/comm_nodist_plants.csv", sep = ";", header = TRUE)

#Replace values NA
assembleages2 <- fillCountryAccordingLatLon(assembleages)

#Check if any NA
assembleages2 %>% dplyr::select(country,NAME,exact_lat,exact_long) %>% dplyr::filter(is.na(country))

# Define the columns where you want to replace the semicolon
columns_to_replace <- c("organism_threshold", "sampling_method", "disturbance1_age")

# Loop through each column and replace the semicolon with a comma
for(column in columns_to_replace) {
  assembleages3[[column]] <- gsub(";", ",", assembleages2[[column]])
}


# Select the first 57 columns
assembleages4 <- assembleages3 %>% dplyr::select(1:57,lat,lon)

# 1. Replace commas with periods (if present)
assembleages4$exact_lat <- gsub(",", ".", assembleages4$exact_lat)
assembleages4$exact_long <- gsub(",", ".", assembleages4$exact_long)
assembleages4$lat <- gsub(",", ".", assembleages4$lat)
assembleages4$lon <- gsub(",", ".", assembleages4$lon)

# 2. Convert character strings to numeric values
assembleages4$exact_lat <- as.numeric(assembleages4$exact_lat)
assembleages4$exact_long <- as.numeric(assembleages4$exact_long)
assembleages4$lat <- as.numeric(assembleages4$lat)
assembleages4$lon <- as.numeric(assembleages4$lon)

# 3. Round numeric values to two decimal places
assembleages4$exact_lat <- round(assembleages4$exact_lat, 2)
assembleages4$exact_long <- round(assembleages4$exact_long, 2)
assembleages4$lat <- round(assembleages4$lat, 2)
assembleages4$lon <- round(assembleages4$lon, 2)

# Save the dataframe to a semicolon-separated CSV file
write.table(assembleages4, file = "inst/comm_nodist_plants.csv", sep = ";", row.names = FALSE, col.names = TRUE, quote = FALSE)


