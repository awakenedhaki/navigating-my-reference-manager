# ==============================================================================
# Keyword Analysis
# ==============================================================================

# Loading Packages =============================================================
library(here)
library(udpipe)
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
MIN_AUTHOR_APPEARANCE <- 3

# Loading Data =================================================================
metadata <- read_feather(here(DATA, "metadata.feather")) %>%
  select(id, journal, publication_year, reference_manager)

authors <- read_feather(here(DATA, "cleaned", "author_names.feather")) %>%
  add_count(author)

counts <- read_feather(here(DATA, "features", "counts.feather")) %>%
  select(id, term, n, tf_idf)

# Helper Functions =============================================================
prevalence_weights <- function(position) {
  0.5 * exp(abs(position))
}

min_max_scaler <- function(x) {
  minimum <- min(x)
  maximum <- max(x)
  
  (x - minimum) / (maximum - minimum)
}

# Keyword Extraction ===========================================================
# . By Author ==================================================================
prevalent_authors <- authors %>%
  group_by(id) %>%
  mutate(author_order = row_number(),
         median_centered_order = author_order - median(author_order),
         weight = prevalence_weights(median_centered_order),
         scaled_weight = min_max_scaler(weight)) %>%
  ungroup() %>%
  summarize(prevalence = sum(scaled_weight), .by = author) %>%
  slice_max(order_by = prevalence, n = 16, with_ties = FALSE) %>%
  pull(author)

author_tf_idf <- counts %>%
  select(id, term) %>%
  inner_join(authors[authors$n > MIN_AUTHOR_APPEARANCE, ], 
             by = "id", relationship = "many-to-many") %>%
  select(-c(id, n)) %>%
  count(author, term) %>%
  bind_tf_idf(term = term, document = author, n = n)

author_tf_idf %>%
  filter(author %in% prevalent_authors) %>%
  slice_max(order_by = tf_idf, n = 10, by = author, with_ties = FALSE) %>%
  mutate(term = reorder_within(str_to_upper(term), 
                               by = tf_idf, 
                               within = author)) %>%
  ggplot(aes(x = tf_idf, y = term)) +
    geom_col() +
    facet_wrap(~author, scales = "free_y", ncol = 4) +
    scale_y_reordered()