# Load necessary libraries
library(tidyverse)
library(httr)
library(readxl)
library(R.utils)
library(writexl)

# Read the dataset from an Excel file
df <- read_xlsx("data/dataset_noticias.xlsx")

# Then, put your API key in the quotes below: 
my_API <- ""

# The "hey_chatGPT" function will help you access the API and prompt GPT 
hey_chatGPT <- function(answer_my_question) {
  chat_GPT_answer <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", my_API)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      temperature = 0,
      messages = list(
        list(
          role = "user",
          content = answer_my_question
        )
      )
    )
  )
  str_trim(content(chat_GPT_answer)$choices[[1]]$message$content)
}

# Loop through the dataframe
for (i in 1:nrow(df)) {
  # Check if 'Accepted' is NA and 'bodyComplete' is not NA
  if (is.na(df$Accepted[i]) && !is.na(df$bodyComplete[i])) {
    print(i)
    
    question <- "I am going to pass you data about a query made on a cultural asset of the Camino de Santiago as it passes through Castilla y León. I am also going to give you the title, the body and the link to a news item. With this information I want you to indicate me with '1.0' if the news is related to any of the terms of the query and '0.0' if not. Answer only with one of these two options and nothing else."
    title <- paste("Title: ", df$title[i])
    body <- paste("Body: ", df$bodyComplete[i])   
    link <- paste("Link: ", df$link[i])
    query <- paste("Query: ", df$query[i])
    reminder <- "REMEMBER, RETURN ONLY '1.0' if there is a relationship and '0.0' if there is not, nothing else"
    concat <- paste(question, title, body, link, query, reminder, sep = ";")
    
    result <- NULL
    hadTimeout <- FALSE
    hadError <- FALSE
    
    tryCatch({
      withTimeout({
        repeat {
          result <- hey_chatGPT(concat)
          if (!is.null(result) && length(result) > 0) {
            break
          }
        }
      }, timeout = 75) # Set a maximum wait time of 75 seconds
    }, TimeoutException = function(ex) {
      hadTimeout <- TRUE
      cat("Timeout reached for row", i, ".\n")
    }, error = function(e) {
      hadError <- TRUE
      cat("Error encountered: ", conditionMessage(e), ". Retrying in 50 seconds...\n")
      Sys.sleep(50)
    })
    
    if (!hadTimeout && !hadError && !is.null(result) && length(result) > 0) {
      print(result)
      df$Accepted[i] <- result
    } else {
      cat("No valid result for row", i, "- either timeout, error, or empty result.\n")
    }
    
    Sys.sleep(10) # Pause before the next record
    
  } else {
    cat("Skipping row", i, "as it doesn't meet the criteria.\n")
  }
  
  # Save the dataframe every 250 iterations
  if (i %% 250 == 0) {
    saveRDS(df, paste0("backup_googleNewsUnifiedRefinement_", i, ".RDS"))
    cat("Saved backup for row", i, "\n")
  }
  
}

# Save the final dataframe to an Excel file
write_xlsx(df, "backup_googleNews.xlsx")
