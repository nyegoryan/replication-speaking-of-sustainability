# Load amazon reviews ==========================================================
dt <- fread(file.path(rawDataPath, "ugc_amazon_data.csv"))


# Delete unnecessary columns ===================================================
dropcol <- c("author_descriptor", "author_profile_img", "author_url",
             "image_url", "query", "official_comment_banner", "rating_text",
             "review_timestamp", "author_title", "url", "comments")
dt[, (dropcol) := NULL]


# Combine title and body as in one column ======================================
dt[, text := paste(title, body, sep = " ")]
dt[, c("title", "body") := NULL]

# Recode missing values for character columns properly =========================
# select all character columns
cols <- names(dt)[sapply(dt, is.character)]

# recode "" as NA
dt[, (cols) := lapply(.SD, function(x) ifelse(x == "", NA, x)), .SDcols = cols]

# Convert review_helpful and rating into integer vars ==========================
dt[helpful %like% "One", review_helpful := 1L]
dt[!helpful %like% "One", 
   review_helpful := as.integer(sapply(strsplit(helpful, " "), `[[`, 1))]

# delete the original column
dt[, helpful := NULL] 

# convert Rating to integer
dt[, rating := as.integer(rating)]


# Extract review region ========================================================
dt[, review_region := gsub("Reviewed in (.*)", "\\1", date)]
dt[, review_region := sapply(strsplit(review_region, "on"), `[[`, 1)]

# get rid of "the"
dt[, review_region := gsub("the ", "\\1", review_region)]

# remove leading or trailing spaces
dt[, review_region := trimws(review_region)] 

# unique regions?
unique(dt$review_region) 


# Extract review date and save in date format ==================================
dt[, review_date := gsub("Reviewed in (.*) on ", "", date)]
dt[, review_date := as.Date(review_date, format = "%B %d, %Y")]
dt[, date := NULL] 

# Renaming and reordering columns ==============================================
# rename columns
setnames(dt, c("rating", "badge", "id", "variation"),
         c("product_rating", "review_badge", "review_id", "product_variation"))


# Process review text ==========================================================
# all review text to lower case 
dt[, text := tolower(text)]

# detect language
dt[, review_language := textcat(text)]
unique(dt$review_language)
# [1] "english"         "scots"           "spanish"        
# [4] "catalan"         "afrikaans"       "middle_frisian" 
# [7] "italian"         "portuguese"      "latin"          
# [10] "romanian"        "rumantsch"       "manx"           
# [13] "dutch"           "french"          "danish"         
# [16] "slovenian-ascii" "swedish"         "frisian"        
# [19] "czech-iso8859_2" "slovak-ascii"  

# many detected are actually english
english <- c("english", "scots", "catalan", "afrikaans", "middle_frisian",
             "latin", "romanian", "rumantsch", "manx", "dutch",
             "french", "danish", "slovenian-ascii", "swedish", "frisian",
             "czech-iso8859_2", "slovak-ascii")
dt[review_language %in% english, review_language := "english"]

# if Italian or Portuguese need further testing
dt[review_language %in% c("italian", "portuguese"), 
        .(review_id, text)]

# manually set the language for specific review_ids
english_ids <- c("RDDZ64KRYETTF", "RGR48Q61209SQ", "R3LPE87X2GKYGH", 
                 "R1RZHEZZGBLCN7", "R2PJ0NJQU2JYUD", "R3ODCHZ6N37QPT")
dt[review_id %in% english_ids, review_language := "english"]
dt[review_language != "english", review_language := "other"]

# share of non-English reviews
round(prop.table(table(dt$review_language)) * 100, 2)
# english   other 
# 96.62    3.38

# keep only English reviews
dt <- dt[review_language == "english"]

# Overwrite review_id as an integer counter 
dt[, review_id := .I]
nrow(dt) # 2029 reviews in total

# Note: there may still be some non-English reviews. They don't effect the 
# text analysis results though.


# Correct misspellings using chatGPT ===========================================
if(spellcheck == TRUE) {
  cat("Correcting misspellings using chatGPT 3.5. This may take some time.\n")
  
  if(is.null(api_key)) {
    stop("Please provide an OpenAI API key.")
  }
  
  # Correct misspellings using chatGPT 
  text_clean <- doSpellcheck(api_key = api_key,
                             text_list = dt$text,
                             bysentence = TRUE)
  
  # save the results in the dataset (overwrite the original text)
  dt[, text := text_clean]
  
} else {
  cat("No spell check will be conducted.\n")
}


# Remove breaks and quotes in text =============================================
dt[, text := removeBreaks(text)]
dt[, text := gsub("\"", "", text)]


# Link reviews to product_ids ==================================================
mapping <- fread(file.path(processedDataPath, "mapping.csv"))
mapping <- unique(mapping[, .(product_id, product_asin, product_type)])

dt <- merge(mapping, dt, by = "product_asin", 
            all.y = TRUE, allow.cartesian = TRUE)


# Sort and reorder columns =====================================================
# sort
setkey(dt, product_id, review_id)

# Drop Product ASIN (don't need it anymore)
dt[, product_asin := NULL]

# reorder columns
setcolorder(dt, c("product_id", "product_type",
                  "product_url", "product_variation", "total_reviews",
                  "review_id", "review_badge", "review_helpful",
                  "review_date", "review_region", "review_language", 
                  "product_rating", "text"))

# save pre-processed data
if(overwrite == TRUE) {
  write.table(dt, file = file.path(processedDataPath, "ugc_amazon_data.csv"), 
              sep = ";", row.names = FALSE)
}


# cleanup: all objects except reviews and objects in setup.R and helper.R
rm(mapping, english_ids, cols, dropcol)

