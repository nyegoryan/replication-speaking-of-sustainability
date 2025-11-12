# zero shot using GPT 4o for FGC Amazon data ===================================
data = fgc_amazon_data
counter <- nrow(data)
chunk_size = 20

# loop over system message
dt <- NULL
for(m in names(system_message)){
  system_message_m <- system_message[[m]]
  
  dt[[m]] <- data.table(text = 1:counter, answer = as.character(NA))
  # loop over doc / text
  for(i in 1:counter) {
    # get text
    text <- data[i, text]
    
    # break into a vector of sentences
    sentences <- unlist(strsplit(text, "\n"))
    
    # get rid of spaces in the beginning and end of each sentence
    sentences <- trimws(sentences)
    
    # break into chunks
    if(length(sentences) > chunk_size){
      chunks <- split(sentences, ceiling(seq_along(sentences)/chunk_size))
    } else {
      chunks <- list(sentences)
    }
    
    # loop over chunks
    res <- data.table(text = i, chunk = 1:length(chunks), answer = as.character(NA))
    for(j in 1:length(chunks)){
      
      # create a list of user_messages
      user_message <- unlist(chunks[j])
      names(user_message) <- 1:length(user_message)
      user_message <- paste0(1:length(user_message), ". ", user_message)
      user_message <- paste(user_message, collapse = " ")
      user_message <- paste0("Classify each of the following sentences according to the given instructions: ",
                             user_message)
      
      
      # loop over system_message
      answer <- create_chat_completion(
        model = "gpt-4o",
        temperature = 0.2, # https://community.openai.com/t/cheat-sheet-mastering-temperature-and-top-p-in-chatgpt-api/172683
        # top_p = 0.1, # doc for arguments: https://platform.openai.com/docs/api-reference/chat/create
        messages = list(
          list(role = "system", content = system_message_m),
          list(role = "user", content = user_message)
        )
      )
      
      # save and clean the answer
      answer <- answer$choices$message.content
      answer <- gsub("\n", "", answer)
      answer <- gsub("\\d+\\.", "", answer)
      answer <- gsub(" +", " ", answer)
      answer <- trimws(answer)
      answer <- unlist(strsplit(answer, " "))
      
      answer_vec <- paste(answer, collapse = ", ")
      res[text == i & chunk == j, answer := answer_vec]
      
      # pause
      Sys.sleep(1)
    }
    
    # save current res
    write.table(res, file = file.path(processedDataPath, "temp", paste0("res_", m, "_", i,".csv")), 
                sep = ",", row.names = FALSE)
    
    test <- paste(res$answer, collapse = ", ")
    test <- gsub(",,", ",", test)
    dt[[m]][text == i, answer := test]
    
    rm(res)
  }
  
  # save current res
  write.table(dt[[m]], 
              file = file.path(processedDataPath, paste0("zs_fgc_amazon_", m,".csv")), 
              sep = ",", row.names = FALSE)
  
  # delete all files in the output folder starting with res_
  files <- list.files(file.path(processedDataPath, "temp"), pattern = "res_")
  file.remove(file.path(processedDataPath, "temp", files))
}

