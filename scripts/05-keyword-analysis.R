# ==============================================================================
# Keyword Analysis
# ==============================================================================

# Loading Packages =============================================================
library(here)
library(udpipe)
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
DATA <- here("data")
MIN_TFIDF_WITHIN_DOC <- 0.2

# Helper Functions =============================================================
extract_authors <- function(tbl) {
  tbl %>%
    separate_longer_delim(cols = author, delim = ",") %>%
    mutate(author = str_remove(author, "^ "))
}

extract_keywords <- function(tbl, group = NULL, n_keywords = 10) {
  tbl %>%
    count({{ group }}, term, wt = n) %>%
    distinct(across({{ group }}), term, n) %>%
    slice_max(order_by = n, n = n_keywords, by = {{ group }}) %>%
    mutate(term = reorder_within(term, by = n, within = {{ group }},
                                 sep = ifelse(is.null({{ group }}), "", "___")))
}

select_from <- function(tbl, group, from, n = 12, random = FALSE, seed = NULL) {
  if (random) {
    set.seed(seed)
    selected_members <- from %>%
      distinct() %>%
      slice_sample(n = n) %>%
      pull({{ group }})
    
  } else {
    slicer <- partial(slice_max, order_by = group_count, n = n, with_ties = FALSE)
    selected_members <- from %>%
      count({{ group }}, name = "nn") %>%
      slice_max(order_by = nn, n = n, with_ties = FALSE) %>%
      pull({{ group }})
  }
  
  tbl %>%
    filter({{ group }} %in% selected_members)
}

keyword_barplot <- function(tbl) {
  ggplot(data = tbl, aes(x = term, y = n)) +
    geom_col() +
    scale_x_reordered() +
    labs(x = "Keyword", y = "Aggregated Counts") +
    coord_flip()
}

# Loading Data =================================================================
metadata <- read_feather(here(DATA, "metadata.feather")) %>%
  select(id, journal, publication_year, author, reference_manager)

counts <- read_feather(here(DATA, "counts.feather")) %>%
  select(id, term, n, tf_idf) %>%
  left_join(metadata, by = "id")

# Keyword Analysis =============================================================
# . Global Keywords
counts %>%
  filter(tf_idf > MIN_TFIDF_WITHIN_DOC) %>%
  extract_keywords(n_keywords = 25) %>%
  keyword_barplot()

# . Journal Keywords
counts %>%
  filter(tf_idf > MIN_TFIDF_WITHIN_DOC) %>%
  extract_keywords(journal, n_keywords = 10) %>%
  select_from(journal, from = counts["journal"], n = 12) %>%
  keyword_barplot() +
    facet_wrap(~journal, scales = "free_y")

# . Author Keywords
counts %>%
  extract_authors() %>%
  select_from(author, from = extract_authors(metadata)) %>%
  filter(tf_idf > MIN_TFIDF_WITHIN_DOC) %>%
  extract_keywords(author) %>%
  keyword_barplot() +
    facet_wrap(~author, scales = "free_y")

# . Publication year 
counts %>%
  extract_keywords(publication_year) %>%
  keyword_barplot() +
    facet_wrap(~publication_year, scales = "free_y")

