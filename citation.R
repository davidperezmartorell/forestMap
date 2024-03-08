# Instalar y cargar la librer??a
#install.packages("rcrossref")
library("rcrossref")
library("dplyr")

# Load citations automatically
cat("citation.R: Loading citations\n")
citation <- read.csv("inst/citation.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")
citation2 <- citation %>% dplyr::select(citation) %>% distinct()
# Function to get DOI for a given title
get_doi_for_citation <- function(title) {
  tryCatch({
    result <- cr_works(query = paste0("title:'", title, "'"), limit = 1)
    
    if (!is.null(result) && nrow(result$data) > 0) {
      return(result$data$doi[1])
    } else {
      cat("No DOI found for", title, "\n")
      return(NA)
    }
  }, error = function(e) {
    cat("Error in retrieving DOI for", title, ":", conditionMessage(e), "\n")
    return(NA)
  })
}

# Update 'doi' column in the data frame
citation2$doi <- sapply(citation2$citation, get_doi_for_citation)

# Function to get URL for a given title
get_url_for_citation <- function(title) {
  result <- cr_works(query = paste0("title:'", title, "'"), limit = 1)
  
  if (!is.null(result) && nrow(result$data) > 0) {
    return(result$data$url[1])
  } else {
    cat("No URL found for", title, "\n")
    return(NA)
  }
}

# Update 'url' column in the data frame
citation2$url <- sapply(citation2$citation, get_url_for_citation)

# Function to get author for a given title
get_author_for_citation <- function(title) {
  result <- cr_works(query = paste0("title:'", title, "'"), limit = 1)
  
  if (!is.null(result) && nrow(result$data) > 0) {
    # Extracting given name of the first author
    given_name <- result$data$author[[1]]$given[1]
    return(given_name)
  } else {
    cat("No information found for", title, "\n")
    return(NA)
  }
}

# Update 'author' column in the data frame
citation2$author <- sapply(citation2$citation, get_author_for_citation)

# Save the updated data frame to a new CSV file
write.csv(citation2, file = "inst/citation_updated.csv", row.names = FALSE)

# Print the updated data frame
print(citation2)
  




#SEARCH INFO AGAIN FROM DATA UNIFIED
library("rcrossref")
library("dplyr")

citation_data_ori <- read.csv("inst/citation_data.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")
citation_data <- citation_data_ori
citation_data <- citation_data %>% rename(author = ??..author) # Rename the column
#citation_data <- citation_data_ori %>% slice(1:20)
#citation_data <- citation_data %>% filter(Title == "The effect of land-use on the local distribution of palm species in an Andean rain forest fragment in northwestern Ecuador")





# Iterate through each row of citation_data
for (i in 1:nrow(citation_data)) {
  # Extract the title from Title column
  title <- citation_data$Title[i]
  
  # Perform the query
  works <- cr_works(query = title, limit = 1)
  
  # Log message
  cat("Searching ",i, "/",nrow(citation_data), " ", title, " doi ", works$data$doi , "\n")
  
  # Check if works were found
  if (nrow(works$data) > 0) {
    # Merge the extracted data with citation_data for the current row
    citation_data[i, names(works$data)] <- works$data[1, ]
  } else {
    warning(paste("No information found for publication with title:", title))
  }
}




# Write the dataframe to a CSV file
write.csv(citation_data, file = "inst/citation_updated_filled.csv", row.names = FALSE)

# View the updated citation_data
head(citation_data)



# Print citation_data to check the updated DOI column
print(citation_data)
citation_data$DOI













#UNIFYING DATA FROM DIFFERENTS DATASTREAMS
# import data from docs 
source("loadLibraries.R")
library("dplyr")

taxon <- read.csv("inst/tax_cleaned.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
assembleages <- read.csv("inst/comm_nodist_plants.csv", sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
citation_data <- assembleages %>% select (citation, year_column, database)
write.csv(citation_data, file = "inst/citation_data.csv", row.names = FALSE)

citation_cu <- read.csv("inst/citation_cu.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")
citation_hu <- read.csv("inst/citation_hu.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")
citation_ru <- read.csv("inst/citation_ru.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")
citation_pr <- read.csv("inst/citation_pr.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")

citation_VERO_cu <- read.csv("inst/citation_VERO_cu.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")
citation_VERO_hu <- read.csv("inst/citation_VERO_hu.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")
citation_VERO_ru <- read.csv("inst/citation_VERO_ru.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")
citation_VERO_pr <- read.csv("inst/citation_VERO_pr.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")
citation_VERO_base <- read.csv("inst/citation_VERO_base.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")

citation_data <- read.csv("inst/citation_data.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")


# Define the target author and year
target_author <- "Baeten"
target_year <- 2010

# Define the target author and year
target_author <- "Baeten"
target_year <- 2010

# List of dataframes
citation_dataframes <- list(
  citation_pr, citation_VERO_pr, citation_VERO_base
)

# Iterate over each dataframe
for (df in citation_dataframes) {
  # Filter rows based on matching author and year
  matching_rows <- df[grep(target_author, df$Authors) & df$Year == target_year, ]
  
  # Print the dataframe if there are matching rows
  print(paste("Searching in", deparse(substitute(df))))
  if (nrow(matching_rows) > 0) {
    print(paste("Matching rows in", deparse(substitute(df)), ":"))
    print(matching_rows)
  } else {
    print("No matching rows found.")
  }
}
