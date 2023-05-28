# ==============================================================================
# Cleaning: Removing "Low-Quality" Abstracts
# 
# . Flagging abstracts with unusual lengths (i.e. too long or short)
# ==============================================================================

# Loading Packages ============================================================= 
library(here)
library(glue)
library(scales)
library(feather)
library(tidyverse)

# Changing ggplot2 Defaults ====================================================
update_geom_defaults("bar", list(color = "black", fill = "salmon"))
update_geom_defaults("boxplot", list(fill = c("skyblue", "salmon")))
theme_set(theme_minimal())

# Constants ====================================================================
ACTIVE_DATA <- here("data", "active")
OUTDIR <- here(ACTIVE_DATA, "clean")
LOWER_THRESHOLD <- 75
UPPER_THRESHOLD <- 825

THRESHOLDS <- glue("lower-{LOWER_THRESHOLD}_upper-{UPPER_THRESHOLD}")

# Loading Data =================================================================
metadata <- read_feather(here(ACTIVE_DATA, "metadata.feather")) %>%
  mutate(reference_manager = str_to_title(reference_manager),
         journal = str_to_title(journal))
abstracts <- read_feather(here(ACTIVE_DATA, "abstracts.feather"))
normalized_tokens <- read_feather(here(ACTIVE_DATA, "normalized_tokens.feather"))

# Counting Words in Abstracts ==================================================
abstracts_to_clean <- abstracts %>%
  mutate(n_words = str_count(abstract, pattern = "\\w+")) %>%
  right_join(metadata, by = "id") %>%
  select(id, n_words, abstract, publication_year, journal, reference_manager)

# Visualizations
abstracts_to_clean %>%
  arrange(n_words) %>%
  select(reference_manager, n_words, abstract, journal)

abstracts_to_clean %>%
  arrange(desc(n_words)) %>%
  select(reference_manager, n_words, journal)

# . Number of Words Distribution
abstracts_to_clean %>%
  ggplot(aes(x = n_words)) +
    geom_histogram(binwidth = 25) +
    scale_x_continuous(labels = label_comma(), breaks = seq(0, 1000, 100)) +
    labs(x = "Number of Words within Abstracts", y = "Occurrence")

# . Number of Words Distribution by Reference Manager
abstracts_to_clean %>%
  ggplot(aes(x = reference_manager, y = n_words)) +
    geom_boxplot() +
    scale_y_continuous(labels = label_comma(), breaks = seq(0, 1000, 100)) +
    labs(x = "Reference Manager", y = "Number of Words within Abstracts") +
    coord_flip()

# . Number of Words Distribution by Publishing Journal
abstracts_to_clean %>%
  mutate(journal = fct_lump(journal, n = 19, w = n_words)) %>% 
  mutate(n = n(), journal = glue("{journal} (n = {n})"), .by = "journal") %>%
  mutate(journal = fct_reorder(journal, n)) %>%
  ggplot(aes(x = reference_manager, y = n_words, fill = journal)) +
    geom_boxplot() +
    facet_wrap(~journal) +
    scale_y_continuous(labels = label_comma()) +
    labs(x = "Publishing Journal", y = "Number of Words within Abstracts") +
    coord_flip() +
    theme(legend.position = "none", 
          panel.border = element_rect(color = "black", fill = NA))

# . Number of Words Distribution in Review Articles by Reference Managers
abstracts_to_clean %>%
  filter(str_detect(journal, "Review")) %>%
  ggplot(aes(x = reference_manager, y = n_words)) +
    geom_boxplot() +
    labs(x = "Reference Manager", y = "Number of Words within Abstracts") +
    coord_flip()

# . Bottom 10 Journals by Median Words
abstracts_to_clean %>%
  mutate(median_words = median(n_words), .by = "journal") %>%
  distinct(journal, median_words) %>%
  arrange(median_words)

# Removing Abstracts ===========================================================
cleaned_abstracts <- normalized_tokens %>%
  filter(id %in% (abstracts_to_clean %>%
           filter(LOWER_THRESHOLD <= n_words, n_words <= UPPER_THRESHOLD) %>%
           pull(id)))

write_feather(cleaned_abstracts, 
              here(OUTDIR, glue("cleaned_abstracts_{THRESHOLDS}.feather",)))
