#' treeDecission all assemblages values indexing by richness and abund
#' @param NULL No values input
#' @return NULL No values output
#' @export
#' @examples
#' treeDecission()
treeDecission <- function(){
  library("readr")
  library("dplyr")

# treeDecission evaluate the more importants variables in our data
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


  #Merge data of taxon and assemblages
  merged_data <- merge(assemblages, taxon, by = c("id_comm", "id_study"))
  
  
  
  #TREE DECISSION FOR RICHNESS
  # Remove rows with missing values in the target variable 'richness'
  filtered_data <- merged_data[!is.na(merged_data$richness), ]
  
  filtered_data2<-filtered_data%>% select(richness,age,age_ori,study_year,stage,study_common_taxon,metric.x,metric_source,sampling_method,predisturbances,disturbance1_age,disturbance2,disturbance1_age_clean,disturbance2_clean,n_disturbances,current_impact,richness_study,richness,abund,taxon_level.x)
  filtered_data3 <- filtered_data2 %>%  mutate_all(~ ifelse(is.na(.), "", as.character(.)))

  # Convert specific columns to numeric
  filtered_data3$age <- as.numeric(filtered_data3$age)
  filtered_data3$age_ori <- as.numeric(filtered_data3$age_ori)
  filtered_data3$study_year <- as.numeric(filtered_data3$study_year)
  filtered_data3$n_disturbances <- as.numeric(filtered_data3$n_disturbances)
  filtered_data3$richness_study <- as.numeric(filtered_data3$richness_study)
  filtered_data3$richness <- as.numeric(filtered_data3$richness)
  filtered_data3$abund <- as.numeric(filtered_data3$abund)
  
  filtered_data4 <- na.omit(filtered_data3)
  
  
  #Convert text values in categories
      # Identify non-numeric columns to convert to factors
      columns_to_convert <- c("metric.x","metric_source", "stage","sampling_method", "study_common_taxon", "predisturbances", 
                              "disturbance1_age", "disturbance2", 
                              "disturbance1_age_clean", "disturbance2_clean", 
                              "current_impact")
      # Convert specified columns to factors
      filtered_data4[columns_to_convert] <- lapply(filtered_data4[columns_to_convert], as.factor)

  
  
  #####################################
  # Fit a random forest model by richness
  #####################################
  library("randomForest")
  rf_model <- randomForest(richness ~ ., data = filtered_data4)

  # View variable importance
  var_importance_for_richness <- importance(rf_model)
  
  # Print the ordered variable importance
  print(var_importance_for_richness)
  
  #####################################
  # Fit a random forest model by abund
  #####################################
  library("randomForest")
  rf_model_abund <- randomForest(abund ~ ., data = filtered_data4)
  
  # View variable importance
  var_importance_for_abund <- importance(rf_model_abund)
  
  # Print the ordered variable importance
  print(var_importance_for_abund)
}

