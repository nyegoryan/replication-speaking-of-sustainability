# Load YouTube transcripts =====================================================
# Get all file names in path -> youtube folder
file_list <- list.files(file.path(rawDataPath, "ugc_youtube"), 
                        full.names = TRUE)

# Get transcript ids from the file names
transcript_ids <- gsub("transcript_", "", basename(file_list))
transcript_ids <- as.integer(gsub(".txt", "", transcript_ids))

# sort the file list and transcript_id by ascending order of transcript_id
file_list <- file_list[order(transcript_ids)]
transcript_ids <- transcript_ids[order(transcript_ids)]

# Load YouTube transcripts, directly remove line and paragraph breaks
transcripts <- sapply(file_list, function(x) {
  temp <- read_file(x)                  # load txt file
  temp <- gsub("\\[Music\\]", "", temp) # drop [Music] from text
  temp <- removeBreaks(temp)            # remove line and paragraph breaks
  return(temp)
})
names(transcripts) <- NULL


# Correct misspellings using chatGPT ===========================================
if(spellcheck == TRUE) {
  cat("Correcting misspellings using chatGPT 3.5. This may take some time.\n")
  
  if(is.null(api_key)) {
    stop("Please provide an OpenAI API key.")
  }
  
  # Correct misspellings using chatGPT 
  transcripts_clean <- doSpellcheck(api_key = api_key,
                                    text_list = transcripts,
                                    bysentence = TRUE)
} else {
  cat("No spell check will be conducted.\n")
}


# Combine in a data table ======================================================
dt <- data.table(transcript_id = transcript_ids, filename = file_list)
setkey(dt, transcript_id) # sort by transcript_id

# Save transcript text as a column in the data table 
if(spellcheck == TRUE) {
  dt[, text := transcripts_clean]
} else {
  dt[, text := transcripts]
}

# load and merge with the mapping file, reorder columns, 
# and drop unnecessary columns
mapping <- fread(file.path(processedDataPath, "mapping.csv")) 
dt <- merge(mapping, dt, by = "transcript_id") 
setcolorder(dt, c(names(mapping), setdiff(names(dt), names(mapping))))
dt[, filename := NULL]

# save pre-processed data
if(overwrite == TRUE) {
  write.table(dt, file = file.path(processedDataPath, "ugc_youtube_data.csv"), 
              sep = ";", row.names = FALSE)
}

# cleanup the environment
rm(mapping)
