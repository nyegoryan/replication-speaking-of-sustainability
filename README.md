# Replication: Speaking of Sustainability

This repository contains the replication materials for the scientific article:

Blits, J., Yegoryan, N., Mandler, T. & Burmester, A.B. (2025). Speaking of Sustainability… 
The Triple Bottom Line in Firm- and User-Generated Content. 
*Schmalenbach J Bus Res* 77, 557–584. https://doi.org/10.1007/s41471-025-00215-8

---

## 🧾 Overview

This repository includes the data, code, and custom dictionary used to replicate 
the analyses reported in the paper. The study explores how firms and users 
communicate sustainability in online content, applying the triple bottom line 
framework (environmental, social, and economic sustainability).

---

## 📁 Repository Contents

- `code/` – R scripts used for data cleaning, analysis, and visualization, 
including the `setup.R` configuration file and custom-made functions in `helper.R` 
- `data_raw/` – Collected textual data from Amazon, YouTube, and corporate websites  
- `data_processed/` – Cleaned and processed datasets ready for analysis, 
including `dictionary.csv`, the sustainability dictionary developed for the study  
- `output/` – Tables and figures generated during analysis and used in the article
- `LICENSE` – Licensing information  
- `README.md` – Description and usage information  

---

### Quick Access

📖 [**Sustainability Dictionary**](https://github.com/nyegoryan/replication-speaking-of-sustainability/blob/main/data_processed/dictionary.csv) – 
The developed dictionary used in this study

---

## 📋 Requirements

- R version 4.0 or higher
- Required R packages:
  - `quanteda`, `textcat` (text analysis)
  - `data.table`, `dplyr`, `stringr` (data manipulation)
  - `readr` (data input/output)
  - `openai` (API access)
  - `foreach` (iteration)
  - `ggplot2`, `ggwordcloud` (visualization)
  - `pacman` (package management)
  - `here` (file path management)
  - `Hmisc`, `tools` (miscellaneous functions)
- Optional: OpenAI API key (for spell-checking and zero-shot classification)

Required packages will be automatically installed and loaded when sourcing `code/setup.R` script.

---

## 📄 License

This project is licensed under Creative Commons Attribution 4.0 International License -- 
see the [LICENSE](https://github.com/nyegoryan/replication-speaking-of-sustainability/blob/main/LICENSE) file for details.

If you use the included dictionary, please cite:

Blits, J., Yegoryan, N., Mandler, T., & Burmester, A.B. (2025). Speaking of Sustainability… 
The Triple Bottom Line in Firm- and User-Generated Content. 
*Schmalenbach Journal of Business Research* 77, 557–584. https://doi.org/10.1007/s41471-025-00215-8

---


## 🚀 Usage

### Basic Replication (Using Preprocessed Data)

1. **Clone the repository**
```bash
   git clone https://github.com/nyegoryan/replication-speaking-of-sustainability.git
   cd replication-speaking-of-sustainability
```

2. **Run the main analysis**
```r
   source("code/main.R")
```

### Advanced: Full Replication from Raw Data

To reprocess raw data and perform spell-checking using GPT-3.5 (requires OpenAI API key):

1. **Edit configuration in `code/setup.R`**
```r
   overwrite <- TRUE  # Allow overwriting preprocessed files
   spellcheck <- TRUE # to use GPT-3.5 for spell check, requires OpenAI API key
   api_key <- Sys.getenv("OPENAI_API_KEY") # Set your OpenAI API key
```

2. **Edit `code/main.R`**
```r
   getData(do = "run", name = ...)  # for each data source
```

To run zero-shot classification (requires OpenAI API key):

1. **Set up API key in configuration in `code/setup.R`**
```r
   api_key <- Sys.getenv("OPENAI_API_KEY") # Set your OpenAI API key
```
2. **Edit `code/main.R`**
```r
   getZeroShot(do = "run", name = ...) # for each data source
```


## 📁 Repository Structure
```
replication-speaking-of-sustainability/
│
├── README.md                        # This file
├── LICENSE                          # License information
│
├── code/
│   ├── main.R                       # Main analysis script (START HERE)
│   ├── setup.R                      # Configuration settings
│   ├── helper.R                     # Custom functions
│   ├── prep_fgc_amazon.R            # Preprocess FGC Amazon product descriptions
│   ├── prep_fgc_website.R           # Preprocess FGC website data
│   ├── prep_ugc_amazon.R            # Preprocess UGC Amazon customer reviews
│   ├── prep_ugc_youtube.R           # Preprocess UGC YouTube review transcripts
│   ├── zero_shot_fgc_amazon.R       # Zero-shot for FGC Amazon product descriptions
│   ├── zero_shot_fgc_website.R      # Zero-shot for FGC website data
│   ├── zero_shot_ugc_amazon.R       # Zero-shot for UGC Amazon customer reviews
│   └── zero_shot_ugc_youtube.R      # Zero-shot for UGC YouTube review transcripts
│
├── data_raw/
│   ├── ugc_amazon.csv               # UGC Amazon customer reviews
│   ├── fgc_amazon/                  # FGC Amazon product descriptions (by product)
│   ├── fgc_website/                 # FGC website data (by product)
│   └── ugc_youtube/                 # UGC YouTube review transcripts (by video)
│
├── data_processed/
│   ├── dictionary.csv               # ⭐ Sustainability dictionary
│   ├── mapping.csv                  # Product ID mappings
│   ├── zero_shot_prompts.csv        # Prompts for zero-shot classification
│   ├── fgc_amazon.csv               # Processed FGC Amazon product descriptions
│   ├── fgc_website.csv              # Processed FGC website data
│   ├── ugc_amazon.csv               # Processed UGC Amazon customer reviews
│   ├── ugc_youtube.csv              # Processed UGC YouTube review transcripts
│   └── zs_*.csv                     # Zero-shot results
│
└── output/
    ├── tab_results_by_source.csv         # Table 3 in paper
    ├── boxplot_by_category.png           # Figure B1 in paper
    ├── wordcloud.png                     # Figure B2 in paper
    └── tab_comparison_dict_zeroshot.csv  # Table B1 in paper
```


## Documentation of Custom-made Functions in `helper.R`

- [`removeBreaks`](#removebreaks) - Remove line breaks and extra whitespace
- [`promptSpellcheck`](#promptspellcheck) - Send spell-check request to GPT-3.5
- [`doSpellcheck`](#dospellcheck) - Batch spell-check documents via API
- [`getData`](#getdata) - Load or preprocess data files
- [`analyzeText`](#analyzetext) - Perform text analysis with quanteda
- [`aggregateResults`](#aggregateresults) - Aggregate and reshape frequency data
- [`getZeroShot`](#getzeroshot) - Run or load zero-shot classification
- [`computeFreqZeroShot`](#computefreqzeroshot) - Process zero-shot results
- [`createFrequencyTable`](#createfrequencytable) - Generate formatted frequency tables

---

#### `removeBreaks`

**Description**

This function removes line breaks, paragraph breaks, and extra spaces from a character string.

```r
removeBreaks(x)
```

|**Arguments**|                                                                |
|:------------|:---------------------------------------------------------------|
|`x`          | A character string to be processed                             |



#### `promptSpellcheck`

**Description**

This function sends a prompt to OpenAI's API to correct spelling mistakes in the provided text using GPT-3.5 and the following prompt:

*You are a professional copy editor. Please only correct spelling mistakes in the following text:*

An OpenAI account and API key are required to use this function.

```r
promptSpellcheck(text)
```

|**Arguments**|                                                                |
|:------------|:---------------------------------------------------------------|
|`text`       | A character string containing the text to be checked for spelling mistakes.|

<!-- ----------------------------------------------------------------------- -->

#### `doSpellcheck`

**Description**

This function performs spell-checking on a vector of text documents using OpenAI's GPT-3.5 API. It can process text either sentence-by-sentence or as complete documents, and creates a log file to track progress.

```r
doSpellcheck(api_key, text_vec, bysentence = TRUE, sleep = 2)
```

|**Arguments**|                                                                |
|:------------|:---------------------------------------------------------------|
|`api_key`    | A character string containing the OpenAI API key for authentication.|
|`text_vec`   | A character vector where each element is a text document to be spell-checked.|
|`bysentence` | Logical value indicating whether to spell-check sentence by sentence (`TRUE`, default) or the entire text at once (`FALSE`).|
|`sleep`      | Numeric value specifying the number of seconds to wait between API calls to avoid rate limits (default: 2 seconds).|

<!-- ----------------------------------------------------------------------- -->

#### `getData`

**Description**

This function either loads pre-processed data from a `.csv` file or runs a preprocessing script to generate the data.

```r
getData(do = "load", filename)

# or

getData(do = "run", filename)
```

|**Arguments**|                                                                |
|:------------|:---------------------------------------------------------------|
|`do`         | A character string specifying the action: `"load"` to load existing data from CSV, or `"run"` to run preprocessing.|
|`filename`   | A character string specifying the name of the file to load or the preprocessing script to run.|

<!-- ----------------------------------------------------------------------- -->

#### `analyzeText`

**Description**

This function performs text analysis, including tokenization, keyword extraction, and dimension frequency analysis using the `quanteda` package (https://quanteda.io). It creates document-feature matrices for both keywords and dimensions.

```r
analyzeText(data, label, keywords_vec, keywords_list, dimension_list)
```

|**Arguments**|                                                                |
|:------------|:---------------------------------------------------------------|
|`data`       | A data frame or data.table containing columns `product_type`, `product_id`, and `text`.|
|`label`      | A character string identifying the data source to be added to output tables.|
|`keywords_vec`| A character vector of keywords or multi-word phrases to be treated as compound tokens during text processing.|
|`keywords_list`| A named list where each element contains character vectors of keywords, used as a dictionary for keyword frequency analysis.|
|`dimension_list`| A named list where each element contains character vectors of terms, used as a dictionary for dimension frequency analysis.|

<!-- ----------------------------------------------------------------------- -->

#### `aggregateResults`

**Description**

This function aggregates frequency data by specified grouping variables, reshapes the data to long format, and computes both absolute and relative frequencies.

```r
aggregateResults(data, by, cols, variable.name)
```

|**Arguments**|                                                                |
|:------------|:---------------------------------------------------------------|
|`data`       | A data frame or data.table containing the data to aggregate.|
|`by`         | A character vector specifying the column name(s) to group by for aggregation.|
|`cols`       | A character vector specifying the column names containing numeric values to sum during aggregation.|
|`variable.name`| A character string specifying the name for the variable column created when reshaping to long format.|

<!-- ----------------------------------------------------------------------- -->

#### `getZeroShot`

**Description**

This function either loads existing zero-shot classification results from CSV 
files or runs zero-shot classification analysis using GTP-4o through OpenAI's API 
using the prompts saved in [`zero_shot_prompts.csv`](https://github.com/nyegoryan/replication-speaking-of-sustainability/blob/main/data_processed/zero_shot_prompts.csv) 
file in the `data_processed/` folder. 
An OpenAI account and API key are required to run the zero-shot classification.

```r
getZeroShot(do = "load", name = "fgc_website")
```

|**Arguments**|                                                                |
|:------------|:---------------------------------------------------------------|
|`do`         | A character string specifying the action: `"load"` to load existing results from CSV files, or `"run"` to run the zero-shot classification.|
|`name`       | A character string identifying the dataset/analysis name, used to construct file paths and to source the corresponding R script.|

<!-- ----------------------------------------------------------------------- -->

#### `computeFreqZeroShot`

**Description**

This function processes zero-shot classification results to compute frequency counts of positive, negative, and neutral classifications across different dimensions.

```r
computeFreqZeroShot(data, source_name, prompt_list = system_message)
```

|**Arguments**|                                                                |
|:------------|:---------------------------------------------------------------|
|`data`       | A list of data.tables containing zero-shot classification results: output of `getZeroShot` function|
|`source_name`| A character string identifying the data source (e.g., "FGC: Website") to be added to output tables.|
|`prompt_list`| A named list where each element contains the system message used for zero-shot classification for each dimension (default: `system_message`).|

<!-- ----------------------------------------------------------------------- -->

#### `createFrequencyTable`

**Description**

This function creates a formatted frequency table from aggregated text analysis results, including absolute frequencies, relative frequencies with respect to all tokens, and relative frequencies within sustainability keywords. The table can optionally be saved to a CSV file.

```r
createFrequencyTable(data, output_file = NULL)
```

|**Arguments**|                                                                |
|:------------|:---------------------------------------------------------------|
|`data`       | A data.table containing columns `source`, `dimension`, `total_tokens`, `freq`, `rel_freq_intokens`, and `rel_freq_within`.|
|`output_file`| A character string specifying the full file path for the output CSV file (optional, if `NULL` no file is saved).|
