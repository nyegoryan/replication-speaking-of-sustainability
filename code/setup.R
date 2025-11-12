# Load packages ================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, dplyr, stringr, foreach, readr, quanteda, textcat,
               ggplot2, ggwordcloud, openai, Hmisc, here, tools)

# Paths =============================================================
PROJECT_ROOT <- dirname(here::here())

# Define data path =============================================================
processedDataPath <- file.path(PROJECT_ROOT, "data_processed")
rawDataPath <- file.path(PROJECT_ROOT, "data_raw")
outputPath <- file.path(PROJECT_ROOT, "output")

# Set some global parameters ===================================================
# do spell check using chatGPT?
spellcheck = FALSE # set to TRUE if you want to use GPT for spell check

# openAI API key
api_key <- NULL
if (is.null(api_key)) {
  cat("OPENAI_API_KEY environment variable not set. Please set it in setup.R if you want to run the spellcheck or zero-shot classification.")
}

# overwrite existing data file?
overwrite = FALSE # TRUE


# color-blind friendly palette for ggplot2 =====================================
# The palette with grey:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


message("Project setup complete!")