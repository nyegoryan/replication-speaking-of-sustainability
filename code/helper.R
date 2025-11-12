# Function for removing line and paragraph breaks ------------------------------
# Arguments:
#   x: A character vector (string or vector of strings) from which to remove
#      line breaks, paragraph breaks, and extra whitespace
removeBreaks <- function(x) {
  x <- gsub("\r\n\r\n", " ", x)  # Remove Windows paragraph breaks
  x <- gsub("\r\n", " ", x)       # Remove Windows line breaks
  x <- gsub("\n\n", " ", x)       # Remove Unix/Mac paragraph breaks
  x <- gsub("\n", " ", x)         # Remove Unix/Mac line breaks
  # remove additional spaces
  x <- str_replace_all(x, "[\\s]+", " ")  # Replace multiple spaces with single space
  x <- trimws(x)                   # Trim leading/trailing whitespace
}

# Function prompting GPT (through the API) to spell check ----------------------
# Arguments:
#   text: A character string containing the text to be spell-checked
promptSpellcheck <- function(text) {
  answer = create_chat_completion(
    model = "gpt-3.5-turbo",
    temperature = 0,
    messages = list(
      list(
        "role" = "system",
        "content" = "You are a professional copy editor. Please only correct spelling mistakes in the following text:"
      ),
      list(
        "role" = "user",
        "content" = text
      )
    )
  )
  answer <- answer$choices$message.content
  return(answer)
}

# Function correcting misspellings in a list of text ---------------------------
# Arguments:
#   api_key: A character string containing the OpenAI API key for authentication
#   text_vec: A character vector where each element is a text document to be 
#             spell-checked
#   bysentence: Logical value indicating whether to spell-check sentence by 
#               sentence (TRUE, default) or the entire text at once (FALSE)
#   sleep: Numeric value specifying the number of seconds to wait between API 
#          calls to avoid rate limits (default: 2 seconds)
doSpellcheck <- function(api_key, text_vec, bysentence = TRUE, sleep = 2) {
  if(!exists("api_key") || (!is.character(api_key) || length(api_key) != 1)) {
    stop("api_key must be defined and must be a single character string")
  }

  if (!is.logical(bysentence) || length(bysentence) != 1) {
    stop("bysentence must be a single logical value (default: TRUE)")
  }  
  
  if(bysentence == TRUE) {
    cat("Checking spelling by sentence.\n")
  } else {
    cat("Checking spelling of the entire text.\n")
  }

  if (!is.character(text_vec)) {
    stop("text_vec must be a character vector")
  }  

  if (!is.numeric(sleep) || sleep < 0) {
    stop("sleep must be a non-negative number (default: 2)")
  }  
  
  # load openai package (installs if needed) and set the API key
  pacman::p_load(openai)
  
  # set the API key
  Sys.setenv(OPENAI_API_KEY = api_key)
  
  # initialize -----
  # create an empty log file 
  logFile <- "log_spellcheck.txt" 
  file.create(logFile)
  
  # number of documents
  I <- length(text_vec)
  
  # create an empty vector to save spell-checked text
  text_clean <- c(NA) 
  length(text_clean) <- I
  
  # loop over each element in the list
  for(i in 1:I) {
    prompt_i <- text_vec[[i]]
    
    if(bysentence == TRUE){
      prompt_i <- strsplit(prompt_i, "\\. ")[[1]]
    }
    
    # Number of prompts / sentences
    J <- length(prompt_i) 
    
    # empty vector to save gpt answer in
    answer_i <- c(NA)
    length(answer_i) <- J
    
    # loop over each sentence
    for(j in 1:J){
      Sys.sleep(sleep)
      answer_i[j] <- promptSpellcheck(prompt_i[j])
      
      # append log.txt file
      cat(paste0("document: ", i, " out of ", I, ", ",
                 "prompt: ", j, " out of ", J), 
          file = logFile, append = TRUE, sep = "\n")
    }
    
    # drop the period at the end, collapse, and save
    answer_i <- gsub("\\.$", "", answer_i)
    text_clean[i] <- paste(answer_i, collapse = ". ")
  }
  
  return(text_clean)
}


# Function loading or pre-processing data -------------------------------------
getData <- function(do = "load", filename) {
  if(do == "load") {
    file_path <- file.path(processedDataPath, filename)
    if (!file.exists(file_path)) {
      stop(sprintf("File not found: %s", file_path))
    }
    tryCatch({
      dt <- fread(file_path)
    }, error = function(e) {
      stop(sprintf("Failed to read file %s: %s", file_path, e$message))
    })
    return(dt)
  } else {
    cat(paste0("Pre-processing data: ", filename, "\n"))
    
    # source the function
    source(paste0("prep_", strsplit(filename, "\\.")[[1]][1], ".R"))
  }
  return(dt)
}


# Function for text analysis ---------------------------------------------------
# Arguments:
#   data: A data frame or data.table containing columns 'product_type', 
#         'product_id', and 'text' (will be converted to data.table if needed)
#   label: A character string identifying the data source (e.g., "reviews", 
#          "descriptions") to be added to output tables
#   keywords_vec: A character vector of keywords or multi-word phrases to be 
#                 treated as compound tokens during text processing
#   keywords_list: A named list where each element contains character vectors 
#                  of keywords, used as a dictionary for keyword frequency analysis
#   dimension_list: A named list where each element contains character vectors 
#                   of terms, used as a dictionary for dimension frequency analysis
analyzeText <- function(data, label, keywords_vec, keywords_list, 
                        dimension_list) {

  # convert to data.table if not already
  if (!is.data.table(data)) {
    message("Converting input data to data.table")
    data <- as.data.table(data)
  }
  # validate required columns
  if (!all(c("product_type", "product_id", "text") %in% names(data))) {
    stop("data must contain columns: product_type, product_id, text")
  }

  # unique counter for each row (document)
  data[, counter := .I] 
  
  # text column to lowercase
  data[, text := tolower(text)] 
  
  # create a corpus
  corpus <- corpus(data[, .(product_type, product_id, text)], 
                   text_field = "text")
  
  # change the naming of documents
  docid <- paste(data$product_type, data$product_id, data$counter, sep = "_")
  docnames(corpus) <- docid
  
  # tokenize
  tok <- tokens(corpus, remove_numbers = TRUE, remove_punct = TRUE, 
                remove_symbols = TRUE, remove_separators = TRUE) |>
    tokens_compound(pattern = phrase(keywords_vec)) |>
    tokens_remove(stopwords("en")) 
  docnames(tok) <- docid
  
  # sustainability terms / keywords in reviews
  dfm_keywords <- tok |>
    tokens_lookup(dictionary = dictionary(keywords_list)) |>
    dfm()
  
  # sustainability dimensions in reviews
  dfm_dimensions <- tok |>
    tokens_lookup(dictionary = dictionary(dimension_list)) |>
    dfm()
  
  # save dfm_keywords as a data table
  dt_keywords <- as.data.table(convert(dfm_keywords, to = "data.frame"))
  dt_keywords[, `:=`(product_type = dfm_dimensions$product_type,
                     product_id = dfm_dimensions$product_id,
                     counter = sapply(strsplit(doc_id, "_"), `[`, 3))]
  dt_keywords[, c("doc_id", "counter") := NULL] # cleanup
  dt_keywords[, total_tokens := ntoken(tok)] # add total tokens per text
  dt_keywords[, source := label]
  
  # save dfm_dimensions as a data table
  dt_dimensions <- as.data.table(convert(dfm_dimensions, to = "data.frame"))
  dt_dimensions[, `:=`(product_type = dfm_dimensions$product_type,
                       product_id = dfm_dimensions$product_id,
                       counter = sapply(strsplit(doc_id, "_"), `[`, 3))]
  dt_dimensions[, doc_id := NULL] # cleanup
  dt_dimensions[, total_tokens := ntoken(tok)] # add total tokens per text
  dt_dimensions[, source := label] # add source
  
  return(list(source = label, 
              keywords = dt_keywords, 
              dimensions = dt_dimensions))
}


# Function for Aggregating and Reshaping Results -------------------------------
# Arguments:
#   data: A data frame or data.table containing the data to aggregate (will be 
#         converted to data.table if needed)
#   by: A character vector specifying the column name(s) to group by for 
#       aggregation
#   cols: A character vector specifying the column names containing numeric 
#         values to sum during aggregation
#   variable.name: A character string specifying the name for the variable 
#                  column created when reshaping to long format
aggregateResults <- function(data, by, cols, variable.name) {
  
  # convert to data.table if not already
  if (!is.data.table(data)) {
    message("Converting input data to data.table")
    data <- as.data.table(data)
  }
  # validate required columns
  missing_cols <- setdiff(c(by, cols, "total_tokens"), names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing columns in data: %s", 
                 paste(missing_cols, collapse = ", ")))
  }  
  
  # aggregate to product category level
  data <- data[, lapply(.SD, sum), .SDcols = cols, by = by]
  
  # reshape to long format
  data <- melt.data.table(data, id.vars = c(by, "total_tokens"), 
                          variable.name = variable.name, value.name = "freq")
  
  # compute relative frequencies in total tokens
  data[, rel_freq_intokens := freq / total_tokens]
  
  # compute relative frequencies within sustainability keywords
  data[, rel_freq_within := sum(freq), keyby = by]
  data[, rel_freq_within := freq / rel_freq_within]
  
  # reorder columns
  setcolorder(data, c(by, variable.name, "total_tokens", "freq", 
                      "rel_freq_intokens", "rel_freq_within"))
  
  return(data)
}

# Function for running or loading zero-shot classification ---------------------
#   do: A character string specifying the action: "load" to load existing 
#       results from CSV files, or any other value to run the zero-shot 
#       classification
#   name: A character string identifying the dataset/analysis name, used to 
#         construct file paths (e.g., "reviews", "descriptions") and to source 
#         the corresponding R script
getZeroShot <- function(do = "load", name) {
  if(do == "load") {
    cat(paste0("Loading results", "\n"))
    namevec <- c("sustainability", "economic", "environmental", "social")
    dt <- lapply(namevec, function(x) {
      file_path <- file.path(processedDataPath, paste0("zs_", name, "_", x, ".csv"))
      if (!file.exists(file_path)) {
        stop(sprintf("File not found: %s", file_path))
      }
      fread(file_path)
    })      
    names(dt) <- namevec
  } else {
    cat(paste0("Running zero-shot classification: ", "\n"))
    
    # Set API key
    if(!is.null(api_key)) {
      stop("api_key must be defined in setup.R to run zero-shot classification")
    }else {
      Sys.setenv(OPENAI_API_KEY = api_key)
    }
    
    # source the function
    source(paste0("zero_shot_", name, ".R"))
  }
  return(dt)
}

# Function for reformating the zero-shot classification results ----------------
# Arguments:
#   data: A list of data.tables containing zero-shot classification 
#         results for different dimensions
#   source_name: A character string identifying the data source (e.g., 
#                "FGC: Website") to be added to output tables
#   prompt_list: A named list where each element contains the system message
#                used for zero-shot classification for each dimension
computeFreqZeroShot <- function(data, source_name, prompt_list = system_message) {
  freq <- foreach(i = names(prompt_list), .combine = "rbind")%do%{
    x <- data[[i]]
    
    # substitute [ and ] with ""
    x$answer <- gsub("\\[", "", x$answer)
    x$answer <- gsub("\\]", "", x$answer)
    
    # strinsplit
    temp <- strsplit(x$answer, ", ")
    
    # drop elements with more than one character
    temp <- sapply(temp, function(x) x[nchar(x) == 1])
    
    # compute sum of a, b, and c
    total <- foreach(i = c("a", "b", "c"), .combine = "rbind")%do%{
      sapply(temp, function(x) sum(x == i))
    }
    
    total <- rowSums(total)
    names(total) <- c("a", "b", "c")
    return(total)
  }
  
  freq <- data.table(freq)
  freq[, dimension := names(prompt_list)]
  freq[, source := source_name]
  
  setnames(freq, c("a", "b", "c"), 
           c("positive", "negative", "neutral"))

  return(freq)
}


# Function for creating frequency table from aggregated results ----------------
# Arguments:
#   data: A data.table containing columns 'source', 'dimension', 'total_tokens',
#         'freq', 'rel_freq_intokens', and 'rel_freq_within'
#   output_file: A character string specifying the full file path for the output
#                CSV file (optional, if NULL no file is saved)
createFrequencyTable <- function(data, output_file = NULL) {
  
  # convert to data.table if not already
  if (!is.data.table(data)) {
    message("Converting input data to data.table")
    data <- as.data.table(data)
  }
  
  # validate required columns
  required_cols <- c("source", "dimension", "total_tokens", "freq", 
                     "rel_freq_intokens", "rel_freq_within")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing columns in data: %s", 
                 paste(missing_cols, collapse = ", ")))
  }
  
  # separate the total rows and main data
  res_total <- data[dimension == "total"]
  res_main <- data[dimension != "total"]
  
  # create the main table (without totals)
  table_main <- dcast(res_main, 
                      dimension ~ source, 
                      value.var = c("freq", "rel_freq_intokens", "rel_freq_within"))
  
  # get unique sources in order
  sources <- unique(data$source)
  
  # build column order dynamically
  col_order <- c("dimension")
  new_names <- c("Dimension")
  old_names <- c("dimension")
  
  for (src in sources) {
    # clean source name for column names (replace spaces and colons)
    src_clean <- gsub(": ", "_", src)
    src_clean <- gsub(" ", "_", src_clean)
    
    col_order <- c(col_order, 
                   paste0("freq_", src),
                   paste0("rel_freq_intokens_", src),
                   paste0("rel_freq_within_", src))
    
    old_names <- c(old_names,
                   paste0("freq_", src),
                   paste0("rel_freq_intokens_", src),
                   paste0("rel_freq_within_", src))
    
    new_names <- c(new_names,
                   paste0(src_clean, "_Abs_freq"),
                   paste0(src_clean, "_Rel_freq_wrt_all"),
                   paste0(src_clean, "_Rel_freq_within"))
  }
  
  # reorder columns
  table_main <- table_main[, ..col_order]
  
  # rename columns
  setnames(table_main, old = old_names, new = new_names)
  
  # capitalize dimension names
  table_main[, Dimension := tools::toTitleCase(as.character(Dimension))]
  
  # create total keywords row dynamically
  total_row_data <- list(Dimension = "Total no. of keywords")
  for (src in sources) {
    src_clean <- gsub(": ", "_", src)
    src_clean <- gsub(" ", "_", src_clean)
    
    total_row_data[[paste0(src_clean, "_Abs_freq")]] <- 
      res_total[source == src, freq]
    total_row_data[[paste0(src_clean, "_Rel_freq_wrt_all")]] <- 
      res_total[source == src, rel_freq_intokens]
    total_row_data[[paste0(src_clean, "_Rel_freq_within")]] <- 
      res_total[source == src, rel_freq_within]
  }
  total_row <- as.data.table(total_row_data)
  
  # create tokens row dynamically
  tokens_row_data <- list(Dimension = "Total no. of tokens")
  for (src in sources) {
    src_clean <- gsub(": ", "_", src)
    src_clean <- gsub(" ", "_", src_clean)
    
    tokens_row_data[[paste0(src_clean, "_Abs_freq")]] <- 
      res_total[source == src, total_tokens]
    tokens_row_data[[paste0(src_clean, "_Rel_freq_wrt_all")]] <- 100.00
    tokens_row_data[[paste0(src_clean, "_Rel_freq_within")]] <- NA
  }
  tokens_row <- as.data.table(tokens_row_data)
  
  # combine all rows
  table_final <- rbindlist(list(table_main, total_row, tokens_row), 
                           use.names = TRUE)
  
  # round percentages to 2 decimal places
  percentage_cols <- grep("Rel_freq", names(table_final), value = TRUE)
  table_final[, (percentage_cols) := lapply(.SD, function(x) round(x, 2)), 
              .SDcols = percentage_cols]
  
  # format absolute frequency columns with commas
  abs_freq_cols <- grep("_Abs_freq", names(table_final), value = TRUE)
  table_final[, (abs_freq_cols) := lapply(.SD, function(x) {
    format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  }), .SDcols = abs_freq_cols]
  
  # format percentage columns with % sign
  table_final[, (percentage_cols) := lapply(.SD, function(x) {
    ifelse(is.na(x), NA_character_, paste0(sprintf("%.2f", x), "%"))
  }), .SDcols = percentage_cols]
  
  # replace any remaining NA with empty string
  for (col in names(table_final)) {
    set(table_final, which(is.na(table_final[[col]])), col, "")
  }
  
  # save to CSV if output_file is provided
  if (!is.null(output_file)) {
    fwrite(table_final, output_file)
    message(sprintf("Table saved to: %s", output_file))
  }
  
  return(table_final)
}

