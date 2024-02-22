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
  
  
  
  
  
  
  #====================NEW FUNCTION ================
  library("dplyr")
  library("progress")
  library("rgbif")
  
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
  
  ## Read the CSV file into a data frame
  taxon_list <- read.csv("inst/tax_cleaned.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1")
  
  # Remove duplicated values and create a data frame
  unique_taxon_list <- data.frame(taxon_clean = unique(taxon_list$taxon_clean))
  
  # Create an empty result data frame
  result_df <- data.frame()
  
  # Create a progress bar with the total number of iterations
  pb <- progress_bar$new(total = nrow(unique_taxon_list))
  
  # Open a connection to the output CSV file
  output_file <- "inst/gbifInfo.csv"
  output_conn <- file(output_file, "w")
  
  # Open a connection to the output error CSV file
  error_file <- "inst/gbifError.csv"
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
        
        # Write the current result to the output CSV file
        write.table(result_df, output_conn, append = TRUE, sep = ";", row.names = FALSE, col.names = !file.exists(output_file))
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
  
  # Close the connection to the output CSV file
  close(output_conn)
  close(error_conn)
  
  # Close the progress bar after the loop completes
  pb$terminate()
  
  
  
  library("dplyr")
  library("readr")
  
  # Read the gbifError.csv file
  error_df <- read.csv("gbifError.csv", stringsAsFactors = FALSE, sep = ";")
  
  # Create new columns for Kingdom, Phylum, Class, Order, Family, and Genus
  error_df$Kingdom <- ""
  error_df$Phylum <- ""
  error_df$Class <- ""
  error_df$Order <- ""
  error_df$Family <- ""
  error_df$Genus <- ""
  
  # Loop through each row in the error_df
  for (i in 1:nrow(error_df)) {
    # Check conditions for assigning values
    if (grepl("\\d", error_df$scientificName[i]) && grepl("spp", error_df$scientificName[i])) {
      # If species has numbers and 'spp', fill with 'Fungi'
      error_df$Kingdom[i] <- "Fungi"
      error_df$Phylum[i] <- "Fungi"
      error_df$Class[i] <- "Fungi"
      error_df$Order[i] <- "Fungi"
      error_df$Family[i] <- "Fungi"
      error_df$Genus[i] <- "Fungi"
    } else {
      # If not, send to GBIF2.csv
      # (you might need to adjust the path or filename)
      write.table(error_df[i, ], "GBIF2.csv", append = TRUE, sep = ";", col.names = !file.exists("GBIF2.csv"))
    }
  }
  
  # Filter out correct rows (without spp and numbers)
  correct_rows <- filter(error_df, !(grepl("\\d", scientificName) & grepl("spp", scientificName)))
  
  # Write correct rows to GBIF2.csv
  write.table(correct_rows, "GBIF2.csv", append = TRUE, sep = ";", col.names = !file.exists("GBIF2.csv"))
  
  # Filter out incorrect rows (with spp and numbers)
  incorrect_rows <- filter(error_df, grepl("\\d", scientificName) & grepl("spp", scientificName))
  
  # Write incorrect rows to errorGBIF2.csv
  write.table(incorrect_rows, "errorGBIF2.csv", append = TRUE, sep = ";", col.names = !file.exists("errorGBIF2.csv"))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #PROGRAM TO CHECK ERRORS. IN THIS CASE SPP VALUES AND NUMBER VALUES
  
  library("dplyr")
  library("rgbif")
  library("progress")
  
 
  

  # Function to fill Genus column based on species names
  fill_genus_column <- function(taxon_df) {
    # Create a copy of the dataframe to avoid modifying the original
    updated_taxon_df <- taxon_df
    
    # Identify unique species with empty or not assigned Genus values and no Kingdom assigned
    unique_species <- unique(filter(updated_taxon_df, (Genus == "" | is.na(Genus)))$specie)
    
    # Create a progress bar with the total number of iterations
    pb <- progress_bar$new(total = length(unique_species))
    
    # Loop through each unique species
    for (species_name in unique_species) {
      pb$tick() # To see how the loop is progressing
      
      # Check if Genus is already filled for this species
      if (any(!is.na(updated_taxon_df$Genus[updated_taxon_df$specie == species_name]) & 
              updated_taxon_df$Genus[updated_taxon_df$specie == species_name] != "")) {
        next  # Skip to the next species if Genus is already filled
      }
      
      # Perform GBIF query for the current species
      suggestions <- name_suggest(species_name)
      
      # Check if suggestions are available and contain genus information
      if (length(suggestions$data$canonicalName) > 0 && 
          any(!is.na(suggestions$data$canonicalName))) {
        # Extract the first non-missing genus from suggestions
        selected_genus <- head(na.omit(suggestions$data$canonicalName), 1)
        cat("Before change: species_name ",species_name,"/"," selected_genus ",selected_genus, "\n")
        
        # Check if selected_genus is available
        if (length(selected_genus) > 0) {
          # Convert specie to character if it's a factor
          species_name <- as.character(species_name)
          
          # Print information for debugging
          cat("Updating Genus for species:", species_name, "\n")
          cat("Rows to be updated:", sum(grepl(species_name, updated_taxon_df$specie, ignore.case = TRUE)), "\n")
          if (selected_genus != "incertae sedis"){
              # Update all rows with the same species in the Genus column
              updated_taxon_df$Genus[grepl(species_name, updated_taxon_df$specie, ignore.case = TRUE)] <- selected_genus
          }
          # Print updated information for debugging
          cat("Rows updated:", sum(updated_taxon_df$Genus == selected_genus), "\n")
        } else {
          # If no genus information is available, fill with "Not available"
          cat("No genus information available for species:", species_name, "\n")
          updated_taxon_df$Genus[grepl(species_name, updated_taxon_df$specie, ignore.case = TRUE)] <- "Not available"
          
        }
      }
    }
    
    # Close the progress bar after the loop completes
    pb$terminate()

    return(updated_taxon_df)
  }
  
  
  
  
  
  
  # Function to perform GBIF query for Kingdom, Phylum, Class, Order, Family, and Gymnosperm
  perform_gbif_query <- function(taxon_df) {

    # Create a progress bar with the total number of iterations
    pb <- progress_bar$new(total = nrow(taxon_df))
    
    # Loop through each row with missing Kingdom values
    for (i in 1:nrow(taxon_df)) {
      pb$tick() # To see how the loop is progressing
      
      # Get the species name and genus from the respective columns
      species_name <- taxon_df[i, "taxon_clen"]
      genus_name <- taxon_df[i, "Genus"]
      Kingdom <- taxon_df[i, "Kingdom"]
        if (Kingdom == "") {
          # Perform GBIF query for the current species and genus
          gbif_result <- name_backbone(genus_name)
          
          # Check if a match was found
          if (!is.null(gbif_result) && 
              !is.null(gbif_result$kingdom) && 
              !is.null(gbif_result$phylum) && 
              !is.null(gbif_result$class) && 
              !is.null(gbif_result$order) && 
              !is.null(gbif_result$family)) {
            
            # Directly update the dataframe columns
            taxon_df$Kingdom[i] <- gbif_result$kingdom
            taxon_df$Phylum[i] <- gbif_result$phylum
            taxon_df$Class[i] <- gbif_result$class
            taxon_df$Order[i] <- gbif_result$order
            taxon_df$Family[i] <- gbif_result$family
            is_gymnosperm <- ifelse(
              gbif_result$phylum %in% c("Coniferophyta", "Other Gymnosperm Phyla") &&
                gbif_result$class %in% c("Pinopsida", "Other Gymnosperm Classes"),
              "Yes",
              "No"
            )
            taxon_df$Gymnosperm[i] <- is_gymnosperm
            
            cat("\n CORRECT: number:", i, "/", nrow(taxon_df), " Kingdom ", gbif_result$kingdom, " Phylum ", gbif_result$phylum, "Class", gbif_result$class, "Order", gbif_result$order, "Family", gbif_result$family, "Gymnosperm", gbif_result$Gymnosperm, "\n")
            
          } else {
          # Handle the case where no match was found
          cat("No match found for", genus_name, "\n")
          taxon_df$Kingdom[i] <- "No Kingdom available"
          taxon_df$Phylum[i] <- "No Kingdom available"
          taxon_df$Class[i] <- "No Kingdom available"
          taxon_df$Order[i] <- "No Kingdom available"
          taxon_df$Family[i] <- "No Kingdom available"
          taxon_df$Gymnosperm[i] <- is_gymnosperm
          
          cat("ERRORS: number:", i, "/", nrow(taxon_df), " Kingdom ", gbif_result$kingdom, " Phylum ", gbif_result$phylum, "Class", gbif_result$class, "Order", gbif_result$order, "Family", gbif_result$family, "Gymnosperm", gbif_result$Gymnosperm, "\n")
          
          # Empty the row if there's an issue
          taxon_df[taxon_df[i, "row_number"], ] <- NA
        }
      }
      else{
        cat("\n NOT NEEDED TO UPDATE ANYTHING : species_name:", species_name, "genus_name", genus_name, " Kingdom ", Kingdom, "\n")
      }      
      
    }
    
   
    # Close the progress bar after the loop completes
    pb$terminate()

    return(taxon_df)
  }
  


  # Read the taxon_cleaned.csv file
  
  
  taxon_df <- read.csv("inst/tax_cleaned.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1")
  
  taxon_df2  <- taxon_df  %>% head(50000)
  # Fill Genus column based on species names for rows with no Kingdom value assigned
  taxon_df3 <- fill_genus_column(taxon_df)
  # Write the updated data frame back to the same file
  write.table(taxon_df3, file = "inst/tax_cleaned.csv", row.names = FALSE, sep = ";", quote = FALSE)
  write.table(taxon_df3, file = "inst/tax_cleaned_withgenus.csv", row.names = FALSE, sep = ";", quote = FALSE)  
  
  
  taxon_df4 <- read.csv("inst/tax_cleaned_withgenus.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1")
  taxon_df4_nokingdom <- taxon_df4 %>% filter(Kingdom == "" | is.na(Kingdom))
  distinct_species_kingdom_unique <- taxon_df4_nokingdom %>%   distinct(taxon_clean , Kingdom,Phylum,Class,Genus, Order, Family,Gymnosperm)

  
  
  # Perform GBIF query for Kingdom, Phylum, Class, Order, Family, and Gymnosperm
  taxon_df6 <- taxon_df4_nokingdom %>% head(50)
  taxon_df7 <- perform_gbif_query(distinct_species_kingdom_unique)
  
  # Write the updated data frame back to the same file
  write.table(taxon_df7, file = "inst/tax_cleaned_distinct.csv", row.names = FALSE, sep = ";", quote = FALSE)  

  


  
  
  # Print the updated taxon_df
  print(taxon_df)
  
  
  
  
  #PROCESS TO REVIEW ERRONEOUS TAXONS
  # Function to perform GBIF query for Kingdom, Phylum, Class, Order, Family, and Gymnosperm
  search_suggest_gbif <- function(taxon_df) {
    library("dplyr")
    library("rgbif")
    library("progress")
    

    # Create a progress bar with the total number of iterations
    pb <- progress_bar$new(total = nrow(taxon_df))
    
    # Loop through each row with missing Kingdom values
    for (i in 1:nrow(taxon_df)) {
      pb$tick() # To see how the loop is progressing
      
      # Get the species name and genus from the respective columns
      species_name <- taxon_df[i, "specie"]
        # Perform GBIF query for the current species and genus
        gbif_result <- name_suggest(species_name) %>% slice(1)
        
        # Check if a match was found
        if (!is.null(gbif_result) && 
            !is.null(gbif_result$kingdom) && 
            !is.null(gbif_result$phylum) && 
            !is.null(gbif_result$class) && 
            !is.null(gbif_result$genus) && 
            !is.null(gbif_result$order) && 
            !is.null(gbif_result$family)) {
          
          # Directly update the dataframe columns
          taxon_df$GBIF_name[i] <- gbif_result$canonicalName
          taxon_df$Kingdom[i] <- gbif_result$kingdom
          taxon_df$Phylum[i] <- gbif_result$phylum
          taxon_df$Class[i] <- gbif_result$class
          taxon_df$Order[i] <- gbif_result$order
          taxon_df$Family[i] <- gbif_result$family
          taxon_df$Genus[i] <- gbif_result$genus
          is_gymnosperm <- ifelse(
            gbif_result$phylum %in% c("Coniferophyta", "Other Gymnosperm Phyla") &&
              gbif_result$class %in% c("Pinopsida", "Other Gymnosperm Classes"),
            "Yes",
            "No"
          )
          taxon_df$Gymnosperm[i] <- is_gymnosperm
          
          cat("\n CORRECT: number:", i, "/", nrow(taxon_df), " Kingdom ", gbif_result$kingdom, " Phylum ", gbif_result$phylum, "Class", gbif_result$class, "Order", gbif_result$order, "Family", gbif_result$family, "Gymnosperm", gbif_result$Gymnosperm, "\n")
          
        } 
      }

      

    # Close the progress bar after the loop completes
    pb$terminate()

    return(taxon_df)
  }
  
  search_backbone_gbif <- function(taxon_df) {
    library("dplyr")
    library("rgbif")
    library("progress")

    # Create a progress bar with the total number of iterations
    pb <- progress_bar$new(total = nrow(taxon_df))
    
    # Loop through each row with missing Kingdom values
    for (i in 1:nrow(taxon_df)) {
      pb$tick() # To see how the loop is progressing
      
      # Get the species name and genus from the respective columns
      species_name <- taxon_df[i, "specie"]
      # Perform GBIF query for the current species and genus
      gbif_result <- name_backbone(species_name)
      
      # Check if a match was found
      if (!is.null(gbif_result) && 
          !is.null(gbif_result$kingdom) && 
          !is.null(gbif_result$phylum) && 
          !is.null(gbif_result$class) && 
          !is.null(gbif_result$genus) && 
          !is.null(gbif_result$order) && 
          !is.null(gbif_result$family)) {
        
        # Directly update the dataframe columns
        taxon_df$GBIF_name[i] <- gbif_result$canonicalName
        taxon_df$Kingdom[i] <- gbif_result$kingdom
        taxon_df$Phylum[i] <- gbif_result$phylum
        taxon_df$Class[i] <- gbif_result$class
        taxon_df$Order[i] <- gbif_result$order
        taxon_df$Family[i] <- gbif_result$family
        taxon_df$Genus[i] <- gbif_result$genus
        is_gymnosperm <- ifelse(
          gbif_result$phylum %in% c("Coniferophyta", "Other Gymnosperm Phyla") &&
            gbif_result$class %in% c("Pinopsida", "Other Gymnosperm Classes"),
          "Yes",
          "No"
        )
        taxon_df$Gymnosperm[i] <- is_gymnosperm
        
        cat("\n CORRECT: number:", i, "/", nrow(taxon_df), " Kingdom ", gbif_result$kingdom, " Phylum ", gbif_result$phylum, "Class", gbif_result$class, "Order", gbif_result$order, "Family", gbif_result$family, "Gymnosperm", gbif_result$Gymnosperm, "\n")
        
      } 
    }
  }
    

##################################################################
#search gbif with lookup command
search_lookup_gbif <- function(taxon_df) {
      library("dplyr")
      library("rgbif")
      library("progress")
      
      
      # Create a progress bar with the total number of iterations
      pb <- progress_bar$new(total = nrow(taxon_df))
      
      # Loop through each row with missing Kingdom values
      for (i in 1:nrow(taxon_df)) {
        pb$tick() # To see how the loop is progressing
        
        # Get the species name and genus from the respective columns
        species_name <- taxon_df[i, "specie"] 
        # Perform GBIF query for the current species and genus
          result <- name_lookup(species_name)
          result2 <- result$data
          gbif_result <- result2[1,]
          
        # Check if a match was found
          if (nrow(gbif_result) > 0 &&
              !is.null(gbif_result) &&
              all(c("canonicalName", "kingdom", "phylum", "class", "order", "family") %in% names(gbif_result)) &&
              "genus" %in% names(gbif_result)) 
            {
            
           
            
          cat("\n CORRECT: number:", i, "/", nrow(taxon_df), " Kingdom ", gbif_result$kingdom, " Phylum ", gbif_result$phylum, "Class", gbif_result$class, "Order", gbif_result$order, "Family", gbif_result$family, "\n")
           
          # Directly update the dataframe columns
          taxon_df$GBIF_name[i] <- gbif_result$canonicalName
          taxon_df$Kingdom[i] <- gbif_result$kingdom
          taxon_df$Phylum[i] <- gbif_result$phylum
          taxon_df$Class[i] <- gbif_result$class
          taxon_df$Order[i] <- gbif_result$order
          taxon_df$Family[i] <- gbif_result$family
          taxon_df$Genus[i] <- gbif_result$genus
          cat("\n CORRECT: number:", i, "/", nrow(taxon_df), " Kingdom ", gbif_result$kingdom, " Phylum ", gbif_result$phylum, "Class", gbif_result$class, "Order", gbif_result$order, "Family", gbif_result$family, "\n")
          
          is_gymnosperm <- ifelse(
            gbif_result$phylum %in% c("Coniferophyta", "Other Gymnosperm Phyla") &&
              gbif_result$class %in% c("Pinopsida", "Other Gymnosperm Classes"),
            "Yes",
            "No"
          )
          taxon_df$Gymnosperm[i] <- is_gymnosperm
         }
         else{
           cat("no entra")
         }
      }
    
    # Close the progress bar after the loop completes
    pb$terminate()

    return(taxon_df)
    }
    
  taxon_erroneos <- read.csv("inst/taxon_erroneos.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1")
  taxon_erroneos_df1 <- search_suggest_gbif(taxon_erroneos)
  taxon_erroneos_df1 <- search_lookup_gbif(taxon_erroneos)
  taxon_erroneos_df1 <- search_backbone_gbif(taxon_erroneos)
  write.table(taxon_erroneos_df1, file = "inst/taxon_erroneos_rellenos.csv", row.names = FALSE, sep = ";", quote = FALSE)  
  