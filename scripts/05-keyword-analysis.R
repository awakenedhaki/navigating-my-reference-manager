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
salience_weights <- function(position) {
  0.5 * exp(abs(position))
}

min_max_scaler <- function(x) {
  minimum <- min(x)
  maximum <- max(x)
  
  (x - minimum) / (maximum - minimum)
}

# Keyword Extraction ===========================================================
# . By Author ==================================================================
author_salience <- authors %>%
  group_by(id) %>%
  mutate(author_order = row_number(),
         median_centered_order = author_order - floor(median(author_order)),
         weights = salience_weights(median_centered_order),
         scaled_weights = min_max_scaler(weights)) %>%
  ungroup() 

counts %>%
  left_join(author_salience[, c("id", "author", "scaled_weights")],
            by = "id",
            relationship = "many-to-many") %>%
  mutate(tmp = n * scaled_weights) %>%
  summarize(tmp1 = sum(tmp), .by = c(author, term)) %>%
  bind_tf_idf(term, author, tmp1) %>%
  mutate(tf_idf_tmp = tmp1 * log(idf)) %>%
  filter(author %in% (author_salience %>%
                        summarize(salience = sum(scaled_weights), .by = author) %>%
                        slice_max(order_by = salience, n = 12, with_ties = FALSE) %>%
                        pull(author))) %>%
  slice_max(order_by = tf_idf_tmp, n = 10, by = author, with_ties = FALSE) %>%
  mutate(term = reorder_within(str_to_upper(term), tf_idf_tmp, author)) %>%
  ggplot(aes(x = tf_idf_tmp, y = term)) +
    geom_col() +
    facet_wrap(~author, scales = "free_y") +
    scale_y_reordered()
