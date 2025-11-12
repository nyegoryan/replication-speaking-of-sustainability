# Load Mapping Data ============================================================
dt <- fread(file.path(processedDataPath, "mapping.csv"))
dt <- unique(mapping[, .(product_id, product_type)]) # only uniques
setkey(dt, product_id) # sort by product_id

# number of products
N <- max(dt$product_id)


# Load Website Data  ===========================================================
# data is coming from (and saved in) two sources (folders)
# initialize empty vectors
mission_vec = sust_vec = rep(NA, N) 

# loop over products to fill mission vector
for(i in 1:N){
  file <- file.path(rawDataPath, "fgc_website", "mission", 
                    paste0("product_", i, ".csv"))
  
  if(file.exists(file)) { # read the .txt file
    mission_vec[i] <- read_file(file)
  }
}

# loop over products to fill sustainability vector
for(i in 1:N){
  file <- file.path(rawDataPath, "fgc_website", "sustainability", 
                    paste0("product_", i, ".csv"))
  
  if(file.exists(file)) { # read the .txt file
    sust_vec[i] <- read_file(file)
  }
}

# Save as columns in the dataset (directly merging with mapping) ===============
dt[, `:=`(mission = mission_vec, sustainability = sust_vec)]


# Reshape into long format =====================================================
dt <- melt.data.table(dt, id.vars = c("product_id", "product_type"),
                      variable.name = "document_type",
                      value.name = "text")


# Remove breaks and quotes in text =============================================
dt[, text := removeBreaks(text)]
dt[, text := gsub("\"", "", text)]


# Save pre-processed data ======================================================
if(overwrite == TRUE) {
  write.table(dt, file = file.path(processedDataPath, "fgc_website_data.csv"), 
              sep = ";", row.names = FALSE)
}

# cleanup: all objects except company_dt
rm(N, mission_vec, sust_vec)
