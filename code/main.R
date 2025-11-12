source("setup.R")
source("helper.R")

# Load FGC and UGC Data ========================================================
# You can either load the pre-processed data in the appropriate format for the 
# analysis or preprocess it from scratch: using do = "run" argument in getData 
# function

# UGC YouTube Review Data 
ugc_youtube_data <- getData(do = "load", filename = "ugc_youtube_data.csv")

# UGC Amazon Review Data 
ugc_amazon_data <- getData(do = "load", filename = "ugc_amazon_data.csv")

# FGC Website Data 
fgc_website_data <- getData(do = "load", filename = "fgc_website_data.csv")

# FGC Amazon (Product Description) Data 
fgc_amazon_data <- getData(do = "load", filename = "fgc_amazon_data.csv")


# Dictionary-based Text Analysis ===============================================
# Prepare the dictionary for text analysis -------------------------------------
# Load the dictionary
dict <- fread(file.path(processedDataPath, "dictionary.csv"))
dim(dict)
# there are 84 unique terms/keywords, each with one or more spelling versions

# Save unique keywords (terms) and associated keywords as a list
terms <- dict$term # 84 keywords

# Create a list of all keywords associated with each term to ensure that 
# different spelling versions are counted as the same term
keywords_list <- strsplit(dict$keywords, ", ")
names(keywords_list) <- terms

# Save sustainability dimensions and associated keywords as a list
# This is an aggregate analysis we are interested in
dimensions <- unique(dict$dimension)
dimension_list <- dict[, .(keywords = paste(keywords, collapse = ", ")), 
                       by = dimension]
dimension_list <- strsplit(dimension_list$keywords, ", ")
names(dimension_list) <- dimensions

# Print keywords (all spelling versions) in each dimensions 
# For Table B1 in Appendix B
sapply(dimension_list, function(x) paste(x, collapse = ", "))


# Reshape dictionary into a long format 
nK <- max(sapply(keywords_list, length)) # max number of keywords per term

# split keywords into separate columns
dict[, paste0("keyword_", 1:nK) := tstrsplit(keywords, ", ", fixed = TRUE)]
dict[, keywords := NULL] # cleanup

# reshape to long
dict <- melt.data.table(dict, 
                        id.vars = c("tid", "dimension", "term"), 
                        value.name = "keyword")
dict <- dict[!is.na(keyword)] # drop rows with NAs
dict[, variable := NULL] # cleanup

# create a unique ukid counter for each keyword (all versions)
dict[, ukid := .I]

# reorder columns
setcolorder(dict, c("tid", "ukid", "dimension", "term", "keyword"))
dict
# 184 rows (keywords with different spellings)

# Text analysis by Source ------------------------------------------------------
res_ugc_youtube <- analyzeText(data = ugc_youtube_data, 
                               label = "UGC: YouTube", 
                               keywords_vec = dict$keyword, 
                               keywords_list = keywords_list, 
                               dimension_list = dimension_list)

res_ugc_amazon <- analyzeText(data = ugc_amazon_data, 
                              label = "UGC: Amazon", 
                              keywords_vec = dict$keyword, 
                              keywords_list = keywords_list, 
                              dimension_list = dimension_list)

res_fgc_website <- analyzeText(data = fgc_website_data, 
                               label = "FGC: Website", 
                               keywords_vec = dict$keyword, 
                               keywords_list = keywords_list, 
                               dimension_list = dimension_list)

res_fgc_amazon <- analyzeText(data = fgc_amazon_data, 
                              label = "FGC: Amazon", 
                              keywords_vec = dict$keyword, 
                              keywords_list = keywords_list, 
                              dimension_list = dimension_list)


# combine the results from all sources
res_all <- list(keywords = rbind(res_fgc_website$keywords, 
                                 res_fgc_amazon$keywords, 
                                 res_ugc_youtube$keywords, 
                                 res_ugc_amazon$keywords),
                dimensions = rbind(res_fgc_website$dimensions, 
                                   res_fgc_amazon$dimensions, 
                                   res_ugc_youtube$dimensions, 
                                   res_ugc_amazon$dimensions))


# Overall Result by Source and Sustainability Dimension 
res_overall <- aggregateResults(res_all$dimensions, by = "source",
                                cols = c(tolower(dimensions), "total_tokens"),
                                variable.name = "dimension")

# Compute frequency and relative frequency within each source and combine with
# res_overall
total <- res_overall[, .(freq = sum(freq), 
                         rel_freq_intokens = sum(rel_freq_intokens), 
                         rel_freq_within = sum(rel_freq_within)), 
                     by = .(source, total_tokens)]

total[, dimension := "total"]
setcolorder(total, names(res_overall))
res_overall <- rbind(res_overall, total)

# Relative freq in % (multiple by 100 and round to 2 decimal places)
cols <- grep("^rel_", names(res_overall), value = TRUE) # columns with 'rel_'
res_overall[, (cols) := lapply(.SD, function(x) round(x * 100, 2)), 
            .SDcols = cols]

# Sort by dimension in a particular order
dimensions <- c("environmental", "social", "economic", "global", "total")
res_overall[, dimension := factor(dimension, levels = dimensions)]

# Set source as factor for sorting
res_overall[, source := factor(source, levels = c("FGC: Website", 
                                                  "FGC: Amazon", 
                                                  "UGC: YouTube", 
                                                  "UGC: Amazon"))]
setkey(res_overall, source, dimension)

# Print the resulting table
table_by_source <- createFrequencyTable(
  data = res_overall,
  output_file = file.path(outputPath, "tab_results_by_source.csv")
)
print(table_by_source)



# Overall Result by Product Category, Source, and Sustainability Dimension =====
dimensions <- c("environmental", "social", "economic", "global")
res_category <- aggregateResults(res_all$dimensions,
                                 by = c("product_type", "source"),
                                 cols = c(tolower(dimensions), "total_tokens"),
                                 variable.name = "dimension")

# relative freq in % (multiple by 100 and round to 2 decimal places)
cols <- grep("^rel_", names(res_category), value = TRUE) # columns with 'rel_'
res_category[, (cols) := lapply(.SD, function(x) round(x * 100, 2)),
             .SDcols = cols]

# sort by dimension in a particular order
res_category[, dimension := factor(dimension,
                                   levels = tolower(dimensions),
                                   labels = dimensions)]

setkey(res_category, product_type, source, dimension)

# print the resulting table
print(res_category)
total <- res_category[, .(rel_freq_intokens = sum(rel_freq_intokens)),
             by = .(product_type, source)]
total[, dimension := "total"]

res_category_plot <- rbind(res_category[, .(product_type, source,
                                            rel_freq_intokens, dimension)],
                           total)


# Visualize the results ========================================================
# Wordclouds by Source ---------------------------------------------------------
res_keywords <- aggregateResults(res_all$keywords, by = "source",
                                 cols = c(tolower(unique(dict$term)), 
                                          "total_tokens"),
                                 variable.name = "term")
dim(res_keywords) # 336      6

# add dimension grouping from dictionary
res_keywords <- merge(unique(dict[, .(term = tolower(term), 
                                      dimension = tolower(dimension))]), 
                      res_keywords, all.y = TRUE, by = "term")

# set source as factor for sorting
res_keywords[, source := factor(source, levels = c("FGC: Website", 
                                                   "FGC: Amazon", 
                                                   "UGC: YouTube", 
                                                   "UGC: Amazon"))]
res_keywords[, dimension := factor(dimension, 
                                   levels = tolower(dimensions),
                                   labels = dimensions)]

# sort by descending frequency and dimension
res_keywords <- res_keywords[order(-freq, -dimension)]

# drop * from term
res_keywords[, term := gsub("\\*", "", term)]

# number of unique keywords by source
res_keywords[freq > 0, .(N_terms = uniqueN(term)), by = source]
#          source N_terms
# 1: FGC: Website      47
# 2:  UGC: Amazon      30
# 3:  FGC: Amazon      19
# 4: UGC: YouTube      17

res_keywords[source == "FGC: Website" & freq > 0, 
             .(dimension, term, freq)]

res_keywords[source == "FGC: Amazon" & freq > 0, 
             .(dimension, term, freq)]

res_keywords[source == "UGC: Amazon" & freq > 0, 
             .(dimension, term, freq)]

res_keywords[source == "UGC: YouTube" & freq > 0, 
             .(dimension, term, freq)]


# plot based on absolute frequency
set.seed(13)
ggplot(res_keywords[freq >= 3], 
       aes(label = term, size = freq, 
           color = dimension)) +
  geom_text_wordcloud_area(show.legend = TRUE) +
  scale_size_area(max_size = 45) +
  scale_color_manual(values = cbp1[c(4, 7, 6, 1)]) +
  facet_wrap(source~., scales = "free") +
  theme_minimal() +
  theme(strip.text = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.position = "bottom",
        panel.spacing.y = unit(4, "lines")) +
  guides(size = "none",
         color = guide_legend(title = "Sustainability dimension:",
                              override.aes = list(size = 7)))

# Save the wordcloud
ggsave(file = file.path(outputPath, "wordcloud.png"), 
       width = 9, height = 7, dpi = 700)


# Plot Distribution across Sustainability Dimensions ---------------------------
# By Product-Category and Source 
# set source as factor for sorting
res_category_plot[, source := factor(source, levels = c("FGC: Website", 
                                                        "FGC: Amazon", 
                                                        "UGC: YouTube", 
                                                        "UGC: Amazon"))]

res_category_plot[, dimension := factor(dimension,
                                        levels = c("total", 
                                                   "environmental",
                                                   "social",
                                                   "economic",
                                                   "global"))]

# plot
ggplot(res_category_plot, aes(x = dimension, y = rel_freq_intokens, fill = dimension)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(aes(yintercept = -Inf)) + 
  coord_cartesian(clip = "off") +
  facet_grid(source ~ product_type,
             scales = "free_y",
             labeller = label_wrap_gen(width=10)) +
  scale_fill_manual(values = c("#000000", cbp1[c(4, 7, 6, 1)])) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.line.y = element_line(),
        axis.text.x = element_blank()) +
  labs(x = "", y = "Relative frequency wrt total tokens (in %)",
       fill = "Sustainability dimension")

# save in results folder
ggsave(file = file.path(outputPath, "boxplot_by_category.png"), 
       width = 6, height = 6, dpi = 700)


# Zero-shot Classification using GPT-4o ========================================
# Construct Prompts ------------------------------------------------------------
# Load system message
prompts <- fread(file.path(processedDataPath, "zero_shot_prompts.csv"))

# save as a system message list
system_message <- as.list(prompts[, prompt])
names(system_message) <- prompts[, dimension]


# get zero-shot classification results for FGC website and compute frequencies
zs_fgc_website <- getZeroShot(do = "load", name = "fgc_website")
total_zs_fgc_website <- computeFreqZeroShot(
  zs_fgc_website, source_name = "FGC: Website", prompt_list = system_message
)

# get zero-shot classification results for FGC amazon and compute frequencies
zs_fgc_amazon <- getZeroShot(do = "load", name = "fgc_amazon")
total_zs_fgc_amazon <- computeFreqZeroShot(
  zs_fgc_amazon, source_name = "FGC: Amazon", prompt_list = system_message
)

# get zero-shot classification results for UGC youtube and compute frequencies
zs_ugc_youtube <- getZeroShot(do = "load", name = "ugc_youtube")
total_zs_ugc_youtube <- computeFreqZeroShot(
  zs_ugc_youtube, source_name = "UGC: YouTube", prompt_list = system_message
)

# get zero-shot classification results for UGC amazon and compute frequencies
zs_ugc_amazon <- getZeroShot(do = "load", name = "ugc_amazon")
total_zs_ugc_amazon <- computeFreqZeroShot(
  zs_ugc_amazon, source_name = "UGC: Amazon", prompt_list = system_message
)


# Reshape and Prep Zero-shot results table: ------------------------------------
# Comparison with dictionary-based results
# combine all sources and drop neutral cases
zs_res <- rbind(total_zs_fgc_website, total_zs_fgc_amazon, 
                total_zs_ugc_youtube, total_zs_ugc_amazon)
zs_res[, neutral := NULL]

# compute total frequency of positive and negative cases
zs_res[, freq := positive + negative]

# merge total tokens information from res_overall
tokens <- unique(res_overall[, .(source, total_tokens)])
zs_res <- merge(zs_res, tokens, by = "source")

# drop general sustainability dimension and compute total across three dimensions
zs_res <- zs_res[dimension != "sustainability"]

# compute total frequency across three dimensions
zs_res[, total_freq := sum(freq), by = source]

# compute relative frequency in % in tokens and within
zs_res[, rel_freq_intokens := freq / total_tokens * 100]
zs_res[, rel_freq_within := freq / total_freq * 100]

# drop total_freq column and add additional row for total across dimensions for 
# each source
zs_res[, total_freq := NULL]
total_zs <- zs_res[, .(freq = sum(freq),
                       total_tokens = unique(total_tokens),
                      rel_freq_intokens = sum(rel_freq_intokens),
                      rel_freq_within = sum(rel_freq_within)),
                   by = source]
total_zs[, dimension := "total"]

# combine necessary columns in one table
zs_res <- rbind(zs_res[, .(source, dimension, freq, total_tokens, 
                           rel_freq_intokens, rel_freq_within)],
                total_zs)

# order by source and dimension
zs_res[, dimension := factor(dimension,
                             levels = c("environmental", "social", 
                                        "economic", "total"))]
zs_res[, source := factor(source, levels = c("FGC: Website", "FGC: Amazon", 
                                             "UGC: YouTube", "UGC: Amazon"))]

setkey(zs_res, source, dimension)

# drop total_tokens and freq columns
zs_res[, c("total_tokens", "freq") := NULL]

# rename columns
setnames(zs_res, old = c("rel_freq_intokens", "rel_freq_within"),
         new = c("zs_rel_freq_intokens", "zs_rel_freq_within"))


# prep dictionary-based results, recompute relative freq within 
# environmental, social, economic dimensions
dict_res <- res_overall[!dimension %in% c("global", "total")]

# recompute rel_freq_within
dict_res[, total_freq := sum(freq), by = source]
dict_res[, rel_freq_within := freq / total_freq * 100]

dict_res_total <- dict_res[, .(total_tokens = unique(total_tokens),
                               freq = sum(freq),
                               rel_freq_intokens = sum(rel_freq_intokens),
                               rel_freq_within = sum(rel_freq_within)),
                             by = source]
dict_res_total[, dimension := "total"]
setcolorder(dict_res_total, c("source", "dimension", "total_tokens", "freq", 
                              "rel_freq_intokens", "rel_freq_within"))
dict_res <- rbind(
  dict_res[, .(source, dimension, total_tokens, freq, 
               rel_freq_intokens, rel_freq_within)], 
  dict_res_total
)

# order by source and dimension
setkey(dict_res, source, dimension)

# drop total_tokens and freq columns
dict_res[, c("total_tokens", "freq") := NULL]

# rename columns
setnames(dict_res, old = c("rel_freq_intokens", "rel_freq_within"),
         new = c("dict_rel_freq_intokens", "dict_rel_freq_within"))


# combine in one table, retain only rel. freqs.
comparison_table <- merge(dict_res, zs_res, by = c("source", "dimension"))

# Columns containing rel_freq - format as percentages (keeping 2 decimal places)
cols <- names(comparison_table)[grepl("rel_freq", names(data))]
comparison_table[, (cols) := lapply(.SD, function(x) sprintf("%.2f%%", x)), .SDcols = cols]

# title case for dimension names
comparison_table[, dimension := tools::toTitleCase(as.character(dimension))]

# save the comparison table
fwrite(comparison_table, file.path(outputPath, "tab_comparison_dict_zeroshot.csv"))

# Print the comparison table
print(comparison_table)
