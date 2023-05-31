# ==============================================================================
# Feature Engineering
# 
# . Calculating TF-IDF.
# ==============================================================================

# Loading Packages =============================================================
library(here)
library(glue)
library(scales)
library(ggforce)
library(feather)
library(tidytext)
library(tidyverse)

# Changing ggplot2 Defaults ====================================================
update_geom_defaults("bar", list(color = "black", fill = "salmon"))
update_geom_defaults("boxplot", list(fill = c("skyblue", "salmon")))
theme_set(theme_minimal())

# Constants ====================================================================
CLEAN_DATA <- here("data", 
                   "active", 
                   "clean", 
                   "cleaned_abstracts_lower-50_upper-825.feather")
OUTDIR <- here("data", "active", "feature_engineering")
TERM_COUNT_LOWER_THRESHOLD <- 5
TF_IDF_LOWER_THRESHOLD <- 0.1

LABEL_TF_IDF_LOWER_THRESHOLD <- as.character(TF_IDF_LOWER_THRESHOLD) %>%
  str_replace("\\.", "-")
FILE_SUFFIX <- glue("{TERM_COUNT_LOWER_THRESHOLD}_{LABEL_TF_IDF_LOWER_THRESHOLD}")
FILENAME <- glue("tf_idf_matrix_long_{FILE_SUFFIX}.feather")

# Loading Data Sets ============================================================
tokens <- read_feather(CLEAN_DATA)

# Filtering Low Count Terms ====================================================
token_corpus_counts <- tokens %>%
  select(id, term) %>%
  add_count(term) %>%
  filter(n > TERM_COUNT_LOWER_THRESHOLD)

# Visualization ================================================================
token_corpus_counts %>%
  distinct(term, n) %>%
  arrange(desc(n)) %>%
  head()

token_corpus_counts %>%
  distinct(term, n) %>%
  head(n = 20) %>%
  mutate(term = str_to_title(term),
         term = fct_reorder(term, n)) %>%
  ggplot(aes(x = term, y = n)) +
    geom_col() +
    scale_y_continuous(labels = label_comma()) +
    coord_flip() +
    labs(y = "Word Count", x = "Term")

# TF-IDF =======================================================================
tf_idf_matrix_long <- tokens %>%
  count(id, term) %>%
  bind_tf_idf(term, id, n) %>%
  filter(tf_idf > TF_IDF_LOWER_THRESHOLD)

# Saving TF-IDF Matrix =========================================================
write_feather(tf_idf_matrix_long, here(OUTDIR, FILENAME))
