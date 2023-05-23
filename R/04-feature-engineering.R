# ==============================================================================
# Feature Engineering
# ==============================================================================

# Loading Packages =============================================================
library(here)
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
ACTIVE_DATA <- here("data", "active")

# Loading Data Sets ============================================================
tokens <- read_feather(here(ACTIVE_DATA, "normalized_tokens.feather"))

token_corpus_counts <- tokens %>%
  select(id, term) %>%
  add_count(term) %>%
  filter(n > 1)

# Visualization ================================================================
token_corpus_counts %>%
  arrange(desc(n)) %>%
  head()

token_corpus_counts %>%
  distinct(term, n) %>%
  head(n = 20) %>%
  mutate(term = str_to_title(term),
         term = fct_reorder(term, n)) %>%
  ggplot(aes(x = term, y = n)) +
    geom_col() + 
    labs(x = "Word Count", y = "Term") +
    scale_y_continuous(labels = label_comma())

# TF-IDF =======================================================================
tf_idf_matrix_long <- tokens %>%
  count(id, term) %>%
  bind_tf_idf(term, id, n) %>%
  filter(tf_idf > 0.05)

# Saving TF-IDF Matrix =========================================================
write_feather(tf_idf_matrix_long, here(ACTIVE_DATA, "tf_idf_matrix_long.feather"))
