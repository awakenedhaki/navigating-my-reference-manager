# ==============================================================================
# Text Normalization
# 
# . Tokenization, lemmatization, and manual preprocessing of abstract text.
# ==============================================================================

# Loading Packages =============================================================
library(here)
library(spacyr)
library(feather)
library(tidytext)
library(fuzzyjoin)
library(tidyverse)

# Constants ====================================================================
ACTIVE_DATA <- here("data", "active")
RESOURCES <- here("resources")
CORPUS <- read_feather(here(ACTIVE_DATA, "abstracts.feather"))

# Loading Resources ============================================================
TERM_CORRECTIONS <- rbind(read_csv(here(RESOURCES, "gene_name_corrections.csv")),
                          read_csv(here(RESOURCES, "collapsing_tokens.csv")),
                          read_csv(here(RESOURCES, "plural_removal.csv")))

REMOVE_WORDS <- read_csv(here(RESOURCES, "remove_words.csv"))

# Tokenization & Normalization =================================================
spacy_initialize(model = "en_core_web_lg")

rm_pos <- c("PART", "AUX", "SCONJ")
normalized_tokens <- CORPUS %>%
  pull(abstract, id) %>%
  spacy_parse(pos = TRUE, tags = FALSE, lemma = TRUE, 
              entity = FALSE, nounphrase = FALSE,
              additional_attributes = "is_stop",
              multithreaded = TRUE) %>%
  as_tibble() %>%
  mutate(token = str_to_lower(token), lemma = str_to_lower(lemma)) %>%
  rename(id = doc_id) %>%
  anti_join(stop_words, by = c("lemma" = "word")) %>%
  filter(!is_stop, nchar(lemma) > 2, !str_detect(lemma, "\\d")) %>%
  regex_left_join(TERM_CORRECTION, by = c("lemma" = "regex")) %>%
  mutate(term = ifelse(is.na(term), lemma, term)) %>%
  regex_anti_join(REMOVE_WORDS, by = c("lemma" = "regex"))
  
spacy_finalize()

# Saving Count Matrix ==========================================================
write_feather(normalized_tokens, here(ACTIVE_DATA, "normalized_tokens.feather"))
