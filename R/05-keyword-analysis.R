# ==============================================================================
# Keyword Analysis
# ==============================================================================

# Loading Packages =============================================================
library(here)
library(feather)
library(tidytext)
library(tidyverse)
library(humaniformat)

# Setting ggplot2 Defaults =====================================================
update_geom_defaults("bar", list(color = "black", fill = "salmon"))
update_geom_defaults("point", list(size = 3, color = "salmon"))
theme_set(theme_minimal())
theme_update(panel.border = element_rect(color = "black", fill = NA))

# Constants ====================================================================
ACTIVE_DATA <- here("data", "active")
TFIDF <- here(ACTIVE_DATA, "feature_engineering")

# Helper Functions =============================================================
extract_keywords <- function(tbl, group = NULL, n_keywords = 10) {
  tbl %>%
    mutate(aggregated_tf_idf = sum(tf_idf), .by = c({{ group }}, term)) %>%
    distinct(across({{ group }}), term, aggregated_tf_idf) %>%
    slice_max(order_by = aggregated_tf_idf, n = n_keywords, by = {{ group }}) %>%
    mutate(term = reorder_within(term, by = aggregated_tf_idf, 
                                 within = {{ group }},
                                 sep = ifelse(is.null({{ group }}), "", "___")))
}

select_from <- function(tbl, group, from, n = 12, random = FALSE, seed = NULL) {
  if (random) {
    set.seed(seed)
    slicer <- partial(sample_n, size = n)
  } else {
    slicer <- partial(slice_max, order_by = group_count, n = n, with_ties = FALSE)
  }
  
  tbl %>%
    filter({{ group }} %in% (from %>%
                               # Unnecessary counting if random...
                               count({{ group }}, name = "group_count") %>%
                               slicer() %>%
                               pull({{ group }})))
}

keyword_barplot <- function(tbl) {
  ggplot(data = tbl, aes(x = term, y = aggregated_tf_idf)) +
    geom_col() +
    scale_x_reordered() +
    coord_flip() +
    labs(x = "Aggregated TF-IDF", y = "Keyword")
}

extract_authors <- function(tbl) {
  tbl %>%
    separate_longer_delim(cols = author, delim = ",") %>%
    mutate(author = str_replace(author, "^ ", ""))
}

# Loading Data =================================================================
metadata <- read_feather(here(ACTIVE_DATA, "metadata.feather")) %>%
  select(id, journal, publication_year, author, reference_manager)

tf_idf_matrix_long <- read_feather(here(TFIDF, "tf_idf_matrix_long_3_0-05.feather")) %>%
  left_join(metadata, by = "id") %>%
  select(-c(n, tf, idf))

# Keyword Analysis =============================================================
# . Sum of TF-IDF
tf_idf_matrix_long %>%
  extract_keywords(n_keywords = 25) %>%
  keyword_barplot()

# . Sum of TF-IDF by Journal
tf_idf_matrix_long %>%
  extract_keywords(journal, n_keywords = 10) %>%
  select_from(journal, from = tf_idf_matrix_long["journal"], 
              random = TRUE, seed = 123) %>%
  keyword_barplot() +
    facet_wrap(~journal, scales = "free_y")

# . Sum of TF-IDF by Author
tf_idf_matrix_long %>%
  extract_authors() %>%
  extract_keywords(author) %>%
  select_from(author, from = extract_authors(metadata)) %>%
  keyword_barplot() +
    facet_wrap(~author, scales = "free_y")


