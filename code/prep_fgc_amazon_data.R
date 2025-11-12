# Load Mapping Data ============================================================
dt <- fread(file.path(processedDataPath, "mapping.csv"))
dt <- unique(dt[, .(product_id, product_type)]) # only uniques
setkey(dt, product_id) # sort by product_id

# number of products
N <- max(dt$product_id)


# Load Product Descriptions on Amazon  =========================================
# initialize an empty vector
amazon_vec = rep(NA, N) 

# loop over products to fill mission vector
for(i in 1:N){
  file <- file.path(rawDataPath, "fgc_amazon",
                    paste0("product_", i, ".csv"))
  
  if(file.exists(file)) { # read the .txt file
    amazon_vec[i] <- read_file(file)
  }
}

# Save as columns in the dataset (directly merging with mapping) ===============
dt[, text := amazon_vec]


# Remove breaks and quotes in text =============================================
# substitute breaks with .
dt[, text := gsub("\n\n", "\n", text)]
dt[, text := gsub("\n", " \n ", text)]

# tolowercase
dt[, text := tolower(text)]


# Save pre-processed data ======================================================
if(overwrite == TRUE) {
  write.table(dt, file = file.path(processedDataPath, "fgc_amazon_data.csv"), 
              sep = ";", row.names = FALSE)
}

# cleanup: all objects except company_dt
rm(N, amazon_vec)
