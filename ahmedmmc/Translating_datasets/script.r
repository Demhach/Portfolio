install.packages("httr")

# Load required library
library(httr)

# Set your DeepL API key
api_key <- "0180a73a-e7f7-428f-b297-5be634f33579:fx"

# Set API URL (use 'api.deepl.com' if you have a paid plan)
api_url <- "https://api-free.deepl.com/v2/document"

# Input and output file paths
input_file <- "For AMI_EJM_Morocco_aggregated_2024-05-18_2024-06-25.xlsx"          # Replace with your input file path
output_file <- "output_translated.xlsx"  # Desired output file name

# Upload the document for translation
cat("Uploading document for translation...\n")
response <- POST(
  url = api_url,
  body = list(
    auth_key = api_key,
    target_lang = "FR",  # Target language code for French
    file = upload_file(input_file)
  ),
  encode = "multipart"
)

# Check for errors in the upload response
if (http_error(response)) {
  stop("Failed to upload document: HTTP error ", status_code(response))
}

content <- content(response, "parsed")

if (!is.null(content$message)) {
  stop("API error: ", content$message)
}

document_id <- content$document_id
document_key <- content$document_key

cat("Document uploaded successfully. Document ID:", document_id, "\n")

# Poll the translation status
status_url <- paste0(api_url, "/", document_id)
status <- "translating"

while (status == "translating" || status == "queued") {
  Sys.sleep(5)  # Wait for 5 seconds before checking again
  
  status_response <- GET(
    url = status_url,
    query = list(
      auth_key = api_key,
      document_key = document_key
    )
  )
  
  if (http_error(status_response)) {
    stop("Failed to check document status: HTTP error ", status_code(status_response))
  }
  
  status_content <- content(status_response, "parsed")
  
  if (!is.null(status_content$message)) {
    stop("API error: ", status_content$message)
  }
  
  status <- status_content$status
  cat("Current status:", status, "\n")
}

if (status != "done") {
  stop("Translation failed with status: ", status)
}

# Download the translated document
cat("Downloading translated document...\n")
download_response <- GET(
  url = paste0(status_url, "/result"),
  query = list(
    auth_key = api_key,
    document_key = document_key
  )
)

if (http_error(download_response)) {
  stop("Failed to download translated document: HTTP error ", status_code(download_response))
}

# Save the content to a file
writeBin(content(download_response, "raw"), output_file)
cat("Translation complete. Translated file saved as", output_file, "\n")
