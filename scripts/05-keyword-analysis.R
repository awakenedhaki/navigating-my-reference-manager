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

# Loading Data =================================================================
metadata <- read_feather(here(DATA, "metadata.feather")) %>%
  select(id, journal, publication_year, author, reference_manager)

counts <- read_feather(here(DATA, "counts.feather")) %>%
  select(id, term, n, tf_idf)

# Keyword By Count =============================================================
# . Helper Functions
extract_authors <- function(tbl) {
  tbl %>%
    separate_longer_delim(cols = author, delim = ",") %>%
    mutate(author = str_remove(author, "^ "))
}

extract_keywords <- function(tbl, group = NULL, n_keywords = 10) {
  tbl %>%
    count({{ group }}, term, wt = n) %>%
    distinct(across({{ group }}), term, n) %>%
    slice_max(order_by = n, 
              n = n_keywords, 
              by = {{ group }},
              with_ties = FALSE) %>%
    mutate(term = reorder_within(str_to_upper(term), 
                                 by = n, 
                                 within = {{ group }},
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

# . Keyword Extraction
term_by_authors <- counts %>%
  left_join(metadata %>%
              select(id, author) %>%
              extract_authors() %>%
              slice_tail(n = 3, by = id),
            by = "id",
            relationship = "many-to-many") %>%
  select(-c(n, tf_idf))

most_common_authors <- term_by_authors %>%
  count(author) %>%
  slice_max(order_by = n, n = 16, with_ties = FALSE) %>%
  pull(author)

term_by_authors %>%
  count(author, term)  %>%
  bind_tf_idf(term = term, document = author, n = n) %>%
  filter(author %in% most_common_authors,
         tf_idf > 0.01) %>%
  extract_keywords(group = author, n_keywords = 8) %>%
  keyword_barplot() +
    facet_wrap(~author, scales = "free_y")

# Collocation ==================================================================
collocation <- counts %>%
  filter(tf_idf > 0.25) %>%
  mutate(term = str_to_upper(term)) %>%  filter(tf_idf > 0.25) %>%
  keywords_collocation(term = "term",  group = "id", 
                       ngram_max = 3) %>%
  as_tibble()
