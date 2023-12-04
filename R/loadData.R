#' Download all assemblages values
#' @param NULL No values input
#' @return list(taxon = taxon, assemblages = assemblages, index = index) No values output
#' @export
#' @examples
#' loadData()
loadData <- function(){
  library("readr")
  library("dplyr")

  # Create an empty data frame with the same structure as taxon
  taxon <- data.frame(
    id = integer(0),
    id_comm = character(0),
    id_study = character(0),
    taxon_level = character(0),
    taxon_ori = character(0),
    rank = character(0),
    error_ident = numeric(0),
    error_class = numeric(0),
    measurement = numeric(0),
    metric = character(0),
    taxon_clean = character(0),
    unidentified = logical(0),
    count0 = integer(0),
    count = integer(0)
  )
  
  # Create an empty data frame with the same structure as assemblages  
  
  assemblages <- data.frame(
    id = character(0),
    database = character(0),
    id_comm = character(0),
    id_study = character(0),
    id_study_ori = character(0),
    citation = character(0),
    study = character(0),
    site = character(0),
    plot = character(0),
    id_comm_source = character(0),
    exact_lat = numeric(0),
    exact_long = numeric(0),
    age = numeric(0),
    age_ori = character(0),
    study_year = integer(0),
    temp_persp_class2 = character(0),
    stage = character(0),
    study_common_taxon = character(0),
    organism_threshold = character(0),
    metric = character(0),
    metric_source = character(0),
    aggregated_data = character(0),
    sampling_method = character(0),
    sampling_effort_within = numeric(0),
    sampling_effort_among = logical(0),
    n_comm = integer(0),
    n_comm_available = integer(0),
    effort_ori = numeric(0),
    historic_impact = character(0),
    predisturbances = character(0),
    disturbance1_age = character(0),
    disturbance2 = character(0),
    n_disturbances = integer(0),
    current_impact = character(0),
    notes = character(0),
    priority = character(0),
    country = character(0),
    study_common_taxon_clean = character(0),
    study_common_taxon_strgroup = character(0),
    disturbance1_age_clean = character(0),
    disturbance2_clean = character(0),
    predisturbances_clean = character(0),
    current_impact_clean = character(0),
    postdisturbance = character(0),
    restoration = integer(0),
    protection = integer(0),
    use = character(0),
    recovering_cond = character(0),
    coord_accuracy = character(0),
    taxon_level = character(0),
    richness_study = integer(0),
    richness = integer(0),
    abund = numeric(0),
    shannon = numeric(0),
    PD = numeric(0),
    SR = integer(0),
    error_class = numeric(0)
  )
  
  # Create index data frame
  index <- data.frame(
    id_comm = character(0),
    id_study = character(0)
  )  
  
    
  #First check if data exists in file
    #Load taxons
      #File is divided because more than 50Mb
      if (nrow(taxon) == 0) {
      taxon1 <- read.csv("inst/tax_cleaned1.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")
      taxon2 <- read.csv("inst/tax_cleaned2.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")
      taxon <- bind_rows(taxon1,taxon2)
      }
    #Load assembleages
     if (nrow(assemblages) == 0) {
      assemblages <- read.csv("inst/comm_nodist_plants.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")
      }

    #Create index table with Common columns
      #check if exists index.csv
      if (nrow(assemblages) == 0) {
       index <- taxon %>% select(id_comm , id_study)
       index <- distinct(index)
       #Write index in order to have it easier and not merge everytime we need.
        write.table(index, file = "inst/index.csv", sep = ";", row.names = FALSE, col.names = TRUE, quote = TRUE)
       } else{ #Write index in order to have it easier and not merge everytime we need.
       index <- read_delim("inst/index.csv", delim = ";", show_col_types = FALSE)
       }

  # Create a list to hold the data frames
  result_list <- list(taxon = taxon, assemblages = assemblages, index = index)
  
  # Return the list
  return(result_list)
}

