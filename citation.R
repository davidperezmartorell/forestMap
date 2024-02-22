# Instalar y cargar la librer??a
#install.packages("rcrossref")
library("rcrossref")
library("dplyr")

# Load citations
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
  
  
 