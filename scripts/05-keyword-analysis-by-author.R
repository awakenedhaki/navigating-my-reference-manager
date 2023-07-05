# ==============================================================================
# Keyword Analysis
# ==============================================================================

# Loading Packages =============================================================
library(here)
library(udpipe)
library(tidylo)
library(feather)
library(tidytext)
library(tidyverse)

# Setting ggplot2 Defaults =====================================================
update_geom_defaults("bar", list(color = "black", fill = "salmon"))
update_geom_defaults("point", list(size = 3, color = "salmon"))
theme_set(theme_minimal())
theme_update(panel.border = element_rect(color = "black", fill = NA))

# Constants ====================================================================
DATA <- here("data")
TOP_N_AUTHORS <- 12

# Loading Data =================================================================
metadata <- read_feather(here(DATA, "metadata.feather")) %>%
  select(id, journal, publication_year, reference_manager)

authors <- read_feather(here(DATA, "cleaned", "author_names.feather")) 

counts <- read_feather(here(DATA, "features", "counts.feather")) %>%
  select(id, term, n)

most_common_authors <- authors %>%
  count(author) %>%
  slice_max(order_by = n, n = TOP_N_AUTHORS, with_ties = FALSE) %>%
  pull(author)

n_documents <- counts %>%
  distinct(id) %>%
  nrow()

# Helper Functions =============================================================
plot_keywords <- function(tbl, metric) {
  tbl %>%
    filter(author %in% most_common_authors) %>%
    slice_max(order_by = {{ metric }}, n = 10, with_ties = FALSE, by = author) %>% 
    mutate(term = reorder_within(x = str_to_upper(term), by = {{ metric }}, within = author)) %>%
    ggplot(aes(x = term, y = {{ metric }})) +
      geom_col() +
      scale_x_reordered() +
      coord_flip() +
      facet_wrap(~author, scales = "free_y")
}

# Keyword Extraction ===========================================================
# . TF-IDF
keywords_by_tf_idf <- counts %>%
  left_join(authors, by = "id", relationship = "many-to-many") %>%
  select(author, term) %>%
  count(author, term) %>%
  bind_tf_idf(term = term, document = author, n = n) 

keywords_by_tf_idf %>%
  plot_keywords(metric = tf_idf)

# . Author Importance
preprocessed_authors <- authors %>%
  add_count(author, name = "total_n_authors") %>%
  mutate(author_freq = total_n_authors / sum(total_n_authors),
         author_position = row_number(),
         abs_centered_position = abs(author_position - floor(median(author_position))),
         .by = id)

# . . Regularized Exponential Model of Importance
regularized_exponential_author_weighter <- function(tbl) {
  tbl %>%
    mutate(unregularized_weights = exp(abs_centered_position),
           regularized_weights = unregularized_weights * author_freq,
           normalized_weights = regularized_weights / sum(regularized_weights),
           .by = id) %>%
    select(id, author, unregularized_weights, regularized_weights, normalized_weights)
}

author_exponential_weights <- preprocessed_authors %>%
  regularized_exponential_author_weighter() %>%
  select(id, author, normalized_weights) %>%
  right_join(counts, by = "id", relationship = "many-to-many") 

# . . Applying author weights on raw term count
exp_weighted_term_count <- author_exponential_weights %>%
  mutate(weighted_term_count = (n * normalized_weights)) %>%
  group_by(author, term) %>%
  summarize(weighted_count = sum(weighted_term_count)) %>%
  ungroup() %>%
  mutate(normalized_weighted_count = weighted_count / sum(weighted_count),
         .by = author)

exp_weighted_term_count %>%
  plot_keywords(metric = normalized_weighted_count)
  
# . . Applying author and IDF weights on term frequency
exp_weighted_term_freq <- author_exponential_weights %>%
  bind_tf_idf(term = term, document = id, n = n) %>%
  mutate(weighted_term_count = (tf * idf) * normalized_weights) %>%
  group_by(author, term) %>%
  summarize(weighted_count = sum(weighted_term_count)) %>%
  ungroup() %>%
  mutate(normalized_weighted_count = weighted_count / sum(weighted_count),
         .by = author)
  
exp_weighted_term_freq %>%
  plot_keywords(metric = normalized_weighted_count)

# . . Sigmoid Model for Author Importance
sigmoid <- function(author_position, slope = 1, midpoint = 3) {
  1 / (1 + exp(-slope * (author_position - midpoint)))
}

sigmoid_author_weighter <- function(tbl) {
  tbl %>%
    mutate(weight = sigmoid(author_position),
           normalized_weight = weight / sum(weight),
           .by = id) %>%
    select(id, author, weight, normalized_weights)
}

author_sigmoid_weights <- preprocessed_authors %>%
  sigmoid_author_weighter() %>%
  select(id, author, normalized_weights) %>%
  right_join(counts, by = "id", relationship = "many-to-many")
  
# . . Applying author weights on raw term count
sigmoid_weighted_term_count <- author_sigmoid_weights %>%
  mutate(weighted_term_count = (n * normalized_weights)) %>%
  group_by(author, term) %>%
  summarize(weighted_count = sum(weighted_term_count)) %>%
  ungroup() %>%
  mutate(normalized_weighted_count = weighted_count / sum(weighted_count),
         .by = author)

sigmoid_weighted_term_count %>%
  plot_keywords(metric = normalized_weighted_count)

# . . Applying author and IDF weights on term frequency
sigmoid_weighted_term_freq <- author_exponential_weights %>%
  bind_tf_idf(term = term, document = id, n = n) %>%
  mutate(weighted_term_count = (tf * idf) * normalized_weights) %>%
  group_by(author, term) %>%
  summarize(weighted_count = sum(weighted_term_count)) %>%
  ungroup() %>%
  mutate(normalized_weighted_count = weighted_count / sum(weighted_count),
         .by = author)
  
exp_weighted_term_freq %>%
  plot_keywords(metric = normalized_weighted_count)