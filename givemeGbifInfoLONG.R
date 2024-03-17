#' Returns table related to the id_comm clicked in the map
#' @param study Study that we want specie list
#' @param  taxon inventory of taxon
#' @param  assembleages inventory of asssembleages
#' @return result Return table with info
#' @export
#' @examples
#' givemeGbifInfo(specie)
  givemeGbifInfo <- function(taxon,assembleages, study) {
    cat("givemeGbifInfo.R: Discover info from species in GBIF\n")
    empty_data <- data.frame(
      kingdom = NA,
      phylum = NA,
      order = NA,
      family = NA,
      genus = NA,
      class = NA,
      IsGymnosperm = "No"  # Assuming "No" if no data is available
    )
    # Initialize an empty data frame to store the results
    result_df <- data.frame()
    
    # Filter to obtain the species list from the clicked study
    unique_id_study <- unique(study)
    filtered_taxon <- taxon %>% filter(id_study == unique_id_study) %>% dplyr::select(taxon_clean)
    speciesList <- filtered_taxon %>% distinct()
    
    
    # Loop through each species in the list
    for (taxon_clean in speciesList$taxon_clean) {
      # Search for species occurrences in GBIF
      occ_search_results <- occ_search(scientificName = taxon_clean, limit = 1)
      cat("givemeGbifInfo.R: Discover info from ", taxon_clean , " in GBIF\n")
      
      # Check if data is available
      if (!is.null(occ_search_results$data) && nrow(occ_search_results$data) > 0) {
        # Extract and filter specific columns
        filtered_data <- occ_search_results$data %>%
          dplyr::select(scientificName, kingdom, phylum, class, order, family, genus)
        
        # Remove duplicate rows
        species_info <- distinct(filtered_data) %>% mutate(scientificName = taxon_clean) %>% slice(1) 
        
        # Check if the species is a gymnosperm and add a corresponding column
        is_gymnosperm <- ifelse(
          species_info$phylum %in% c("Coniferophyta", "Other Gymnosperm Phyla") &&
            species_info$class %in% c("Pinopsida", "Other Gymnosperm Classes"),
          "Yes",
          "No"
        )
 
          
        # Call givemeAbundance to get the abundance value
          abundance_value <- givemeAbundance(taxon,study%>% unique(), taxon_clean)
        
        
        # Append the abundance value to the result data frame
        result_df <- bind_rows(result_df, cbind(species_info, Abundance = abundance_value, IsGymnosperm = is_gymnosperm))
        
  
      } else {
        # If no data is available, create a data frame with "No available" in each column
        empty_data <- empty_data %>% data.frame(
          kingdom = NA,
          phylum = NA,
          order = NA,
          family = NA,
          genus = NA,
          class = NA,
          IsGymnosperm = "No"  # Assuming "No" if no data is available
        )
        
        # Append the empty data to the result data frame
        result_df <- bind_rows(result_df, empty_data)
      }
    }
  
    # Return the result data frame
    return(result_df)
  }
  
  
  
  
  
  
  #====================NEW FUNCTION TO EXECUTE MANUALLY NOT CALLED FROM MAIN PROGRAM================
  
  # 1 SEARCHING UNIQUE SPECIES VALUES CREATING 2 FILES WITH ALL INFO AND WITH SPECIES NOT FOUNDED IN GBIF
  library("dplyr")
  library("progress")
  library("rgbif")
  library("stringr")
  
  # Function to perform GBIF occurrence search with error handling and retry
  perform_occ_search <- function(taxon_clean, max_retries = 3, retry_delay = 5) {
    retries <- 0
    result <- NULL
    
    while (retries < max_retries && is.null(result)) {
      tryCatch({
        # Try to perform GBIF occurrence search
        result <- occ_search(scientificName = taxon_clean, limit = 1)
      }, error = function(e) {
        # Handle the error (e.g., print a message)
        cat("Error in GBIF occurrence search for", taxon_clean, ": ", conditionMessage(e), "\n")
        retries <- retries + 1
        Sys.sleep(retry_delay)  # Sleep for specified seconds before retrying
      })
    }
    
    return(result)
  }
  
  # Read the CSV file into a data frame
  taxon_list <- read.csv("inst/tax_cleanedOriginalVeroConError.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1")
  
  # Assign new column names to the dataframe
  names(taxon_list) <- c("taxon_clean", "status")
  
  # Remove duplicated values and create a data frame
  unique_taxon_list <- data.frame(taxon_clean = unique(taxon_list$taxon_clean)) 
  unique_taxon_list <- unique_taxon_list  %>% unique()
  # Extract first word from taxon_clean column
  taxon_list$first_word <- str_extract(taxon_list$taxon_clean, "\\b\\w+\\b")
  
  # Remove duplicated first words and create a data frame
  unique_taxon_list <- unique(data.frame(taxon_clean = taxon_list$first_word))
  
  
  # Create an empty result data frame
  result_df <- data.frame()
  
  # Create a progress bar with the total number of iterations
  pb <- progress_bar$new(total = nrow(unique_taxon_list))
  

  # Open a connection to the output error CSV file
  error_file <- "inst/tax_cleanedOriginalVeroConError2.csv"
  error_conn <- file(error_file, "w")
  
  # Define a vector of expected column names
  expected_columns <- c("scientificName", "kingdom", "phylum", "class", "order", "family", "genus")
  
    
  # Loop through each species in the list
  for (taxon_clean in unique_taxon_list$taxon_clean) {
    pb$tick() # To see how the loop is progressing
    
    # Perform GBIF occurrence search for the current species with error handling and retry
    querySpecie <- perform_occ_search(taxon_clean)
    
    # Rest of the loop remains unchanged
    if (!is.null(querySpecie) && !is.null(querySpecie$data) && nrow(querySpecie$data) > 0) {
      # Extract and filter specific columns
      filtered_data <- querySpecie$data
      
      # Check if all expected columns are present
      if (all(expected_columns %in% colnames(filtered_data))) {
        filtered_data <- filtered_data %>%
          dplyr::select(expected_columns)
        
        # Remove duplicate rows
        species_info <- distinct(filtered_data) %>%
          mutate(scientificName = taxon_clean) %>%
          slice(1)
        
        # Check if the species is a gymnosperm and add a corresponding column
        is_gymnosperm <- ifelse(
          species_info$phylum %in% c("Coniferophyta", "Other Gymnosperm Phyla") &&
            species_info$class %in% c("Pinopsida", "Other Gymnosperm Classes"),
          "Yes",
          "No"
        )
        
        # Add the IsGymnosperm column to the species_info data frame
        species_info$IsGymnosperm <- is_gymnosperm
        
        # Append the species_info data frame to the result data frame
        result_df <- bind_rows(result_df, species_info)
        cat("\n Specie ", taxon_clean , " founded in GBIF \n")

      } else {
        # Handle the case when not all expected columns are present
        missing_columns <- setdiff(expected_columns, colnames(filtered_data))
        cat(paste("Warning: Missing columns", paste(missing_columns, collapse = ", "), "for", taxon_clean, ". Discarding this row.\n"))
        # Write the error to the error file
        write.table(data.frame(taxon_clean, paste("Missing columns:", paste(missing_columns, collapse = ", "))), error_conn, append = TRUE, sep = ";", row.names = FALSE, col.names = !file.exists(error_file))
      }
    } else {
      # If no data is available or an error occurred, create a data frame with "No available" in each column
      cat("Warning: No data available for", taxon_clean, ". Discarding this row.\n")
      # Write the error to the error file
      write.table(data.frame(taxon_clean, "No data available"), error_conn, append = TRUE, sep = ";", row.names = FALSE, col.names = !file.exists(error_file))
    }
  }
  
    # Write the current result to the output CSV file
  write.table(result_df, "inst/tax_cleanedOriginalVeroConTaxon2.csv", append = TRUE, sep = ";", row.names = FALSE)
  # Close the connection to the output CSV file
  close(output_conn)
  close(error_conn)
  
  # Close the progress bar after the loop completes
  pb$terminate()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # 3 SEARCHING SPECIES PREVIOUSLY FILTERED THAT RETURNED ERROR.IT'S NEEDED TO SELECT ONLY GENUS (FIRST WORD)
  library("dplyr")
  library("progress")
  library("rgbif")
  
  # Open a connection to the output error CSV file
  error_file <- "inst/tax_cleanedOriginalVeroConError3.csv"
  error_conn <- file(error_file, "w")
  
  # Function to perform GBIF occurrence search with error handling and retry
  perform_name_suggest <- function(taxon_clean, max_retries = 3, retry_delay = 5) {
    retries <- 0
    result <- NULL
    
    while (retries < max_retries && is.null(result)) {
      tryCatch({
        # Try to perform GBIF occurrence search
        result <- name_suggest(q = taxon_clean, limit = 1)
      }, error = function(e) {
        # Handle the error (e.g., print a message)
        cat("Error in GBIF occurrence search for", taxon_clean, ": ", conditionMessage(e), "\n")
        retries <- retries + 1
        Sys.sleep(retry_delay)  # Sleep for specified seconds before retrying
      })
    }
    
    return(result)
  }
  
  # Read the CSV file into a data frame
  taxon_list <- read.csv("inst/tax_cleanedOriginalVeroConError2.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1")
  
  # Assign new column names to the dataframe
  names(taxon_list) <- c("taxon_clean", "status")
  
  # Modify the taxon_clean column to only contain the first word
  taxon_list$taxon_clean <- sapply(strsplit(taxon_list$taxon_clean, "\\s+"), `[`, 1)
  taxon_list <- taxon_list %>% unique()
  
  # Create an empty result data frame
  result_df <- data.frame(taxon_clean = character(), rank = character(), stringsAsFactors = FALSE)
  
  # Create a progress bar with the total number of iterations
  pb <- progress_bar$new(total = nrow(taxon_list))
  
  # Loop through each species in the list
  # Loop through each species in the list
  for (i in 1:nrow(taxon_list)) {
    pb$tick() # To see how the loop is progressing
    
    # Get the current species name
    current_species <- taxon_list[i, "taxon_clean"]
    
    # Perform GBIF occurrence search for the current species with error handling and retry
    result <- perform_name_suggest(current_species)
    
    # If result is not NULL, extract the rank information and add it to the result dataframe
    if (!is.null(result)) {
      rank <- result$rank
      # Create a temporary dataframe with the current species and rank
      temp_df <- data.frame(taxon_clean = current_species, rank = rank, stringsAsFactors = FALSE)
      # Add the temporary dataframe to the result dataframe
      result_df <- rbind(result_df, temp_df)
    }
  }
  
  # Write the current result to the output CSV file
  write.csv(result_df, "inst/tax_cleanedOriginalVeroConTaxon3.csv", row.names = FALSE)
  
  # Close the progress bar after the loop completes
  pb$terminate()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # 3 FROM PENDING VALUES, CONFIRM IF AMES ARE GENUS
  library("dplyr")
  library("progress")
  library("rgbif")
  

  # Read the CSV file into a data frame
  taxon_list <- read.csv("inst/tax_cleanedOriginalVeroConError2.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1")
  
  # Assign new column names to the dataframe
  names(taxon_list) <- c("taxon_clean", "status")
  
  # Modify the taxon_clean column to only contain the first word
  taxon_list$taxon_clean <- sapply(strsplit(taxon_list$taxon_clean, "\\s+"), `[`, 1)
  taxon_list <- taxon_list %>% unique()
  taxon_list <- taxon_list %>% head(10)

  # Create a progress bar with the total number of iterations
  pb <- progress_bar$new(total = nrow(taxon_list))
  
  # Loop through each species in the list
  # Initialize result and error data frames
  result_df <- data.frame(taxon_clean = character(), rank = character(), kingdom = character(), phylum = character(), class = character(), order = character(), family = character(), genus = character(), stringsAsFactors = FALSE)
  error_df <- data.frame(taxon_clean = character(), stringsAsFactors = FALSE)
  
  # Loop through each species in the list
  for (i in 1:nrow(taxon_list)) {
    pb$tick() # Progress bar tick
    
    # Get the current species name
    current_species <- taxon_list[i, "taxon_clean"]
    
    # Perform GBIF occurrence search for the current species with error handling and retry
    result <- perform_name_suggest(current_species)
    
    # Check if any records are returned and if the rank is "GENUS"
    if (!is.null(result$data$rank) && result$data$rank == "GENUS") {
      
      cat("Species", current_species, "has rank GENUS\n")
      
      # Perform a name lookup for additional taxonomic information
      genus_info <- name_lookup(current_species)
      
      # Extract the taxonomic information from the result
      if (!is.null(genus_info) && !any(sapply(genus_info[c("kingdom", "phylum", "class", "order", "family", "genus")], is.null))) {
        kingdom <- genus_info$kingdom
        phylum <- genus_info$phylum
        class_level <- genus_info$class
        order <- genus_info$order
        family <- genus_info$family
        genus <- genus_info$genus
        
        # Add the taxonomic information to the result dataframe
        temp_df <- data.frame(taxon_clean = current_species, rank = "GENUS", kingdom = kingdom, phylum = phylum, class = class_level, order = order, family = family, genus = genus, stringsAsFactors = FALSE)
        result_df <- rbind(result_df, temp_df)
        
        # Print the updated result_df
        print(result_df)
      } else {
        # Record the species in the error dataframe if any required information is missing
        error_df <- rbind(error_df, data.frame(taxon_clean = current_species, stringsAsFactors = FALSE))
      }
    } else {
      # Record the species in the error dataframe if the rank is not GENUS
      error_df <- rbind(error_df, data.frame(taxon_clean = current_species, stringsAsFactors = FALSE))
    }
  }
  
  
  
  
  
  
  # Remove first row from result_df (initialized empty)
  result_df <- result_df[-1,]
  
  
  # Write the current result to the output CSV file
  write.csv(result_df, "inst/tax_cleanedOriginalVeroConTaxon3.csv", row.names = FALSE)
  # Write the errors to the error file
  write.csv(error_df, "inst/tax_cleanedOriginalVeroConError3.csv", row.names = FALSE)
  
  # Close the progress bar after the loop completes
  pb$terminate()
  
  
  
  
  
  
  
  
  # 4 COMBINE TAXON CLEAN WITH INFO FROM
  # Read the CSV file into a data frame
  taxon_list <- read.csv("inst/tax_cleaned.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1")
  tax_cleaned_unique   <- read.csv("inst/tax_cleaned_unique.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1")
  
  # Merge the data frames by taxon_clean and scientificName columns
  merged_data <- merge(taxon_list, tax_cleaned_unique, by.x = "taxon_clean", by.y = "scientificName", all.x = TRUE)
  filtered_data <- subset(merged_data, kingdom == "Plantae")
  
  # Reorder the columns to move taxon_clean after taxon_ori
  # Create a new data frame with the desired column order
  filtered_data <- filtered_data[, c("id_comm", "id_study", "taxon_level", "taxon_ori","taxon_clean", "rank", "error_ident", "error_class", "measurement", "metric", "unidentified", "count0", "count", "kingdom", "phylum", "class", "order", "family", "genus", "IsGymnosperm")]
  

  
  # Print the structure of the modified data frame
  str(filtered_data)
  
  
  #Write the current result to the output CSV file
  write.csv(filtered_data, "inst/tax_cleaned.csv", row.names = FALSE)
  write.csv2(filtered_data, "inst/tax_cleaned.csv", row.names = FALSE)
  
  
  
  ####################################
  #Review fungi values
  ####################################
  taxon2 <- taxon %>% mutate(across(c(kingdom, phylum, class, order, family, genus),if_else(grepl('^[0-9]+$', taxon_clean), "FUNGI", .)))
  write.csv2(taxon2, "inst/tax_cleaned2.csv", row.names = FALSE)
  