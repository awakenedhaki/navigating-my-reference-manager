# ==============================================================================
# Cleaning
# ==============================================================================

# Loading Packages ============================================================= 
library(here)
library(glue)
library(scales)
library(feather)
library(tidytext)
library(tidyverse)

# Constants ====================================================================
DATA <- here("data")
PREPROCESSED <- here(DATA, "preprocessed")
CLEANED <- here(DATA, "cleaned")

# Loading Data =================================================================
normalized_tokens <- read_feather(here(PREPROCESSED, "normalized_tokens.feather"))

# Helper Functions =============================================================
filter_by_lemma_length <- function(tbl, length) {
  tbl %>%
    mutate(token_length = nchar(lemma)) %>%
    filter(token_length > length) %>%
    select(-token_length)
}

regex_extract <- function(tbl, pattern) {
  tbl %>%
    mutate(lemma = ifelse(str_detect(lemma, pattern),
                          str_extract(lemma, pattern),
                          lemma))
}

regex_split_longer <- function(tbl, pattern) {
  tbl %>%
    mutate(lemma = ifelse(str_detect(lemma, pattern),
                          str_split(lemma, pattern),
                          lemma)) %>%
    unnest(cols = lemma) 
}

regex_remove <- function(tbl, pattern) {
  tbl %>%
    mutate(lemma = ifelse(str_detect(lemma, pattern),
                          str_remove(lemma, pattern),
                          lemma))
}

regex_replace <- function(tbl, pattern, replace) {
  tbl %>%
    mutate(lemma = ifelse(str_detect(lemma, pattern),
                          gsub(pattern = pattern, replacement = replace, x = lemma),
                          lemma))
}

# Cleaning =====================================================================
tokens <- normalized_tokens %>%
  select(id = doc_id, sentence_id, lemma, pos, is_stop, like_email, like_url) %>%
  anti_join(stop_words, by = c("lemma" = "word")) %>%
  filter(!like_email, !like_url) %>%
  select(-c(is_stop, like_email, like_url))

verbs <- tokens %>%
  filter(pos == "VERB")

filtered_tokens <- tokens %>%
  filter(!(pos %in% c("X", "PRON", "DET", "SYM", "NUM", "ADV", 
                      "SCONJ", "SPACE", "INTJ", "ADP", "PART", 
                      "CCONJ", "PUNCT", "AUX", "VERB"))) %>%
  filter_by_lemma_length(length = 2)  %>%
  mutate(lemma = str_to_lower(lemma)) %>%
  # Splitting by delimiters
  regex_split_longer(pattern = "-/-(?:;)?") %>%
  regex_split_longer(pattern = "/") %>% 
  regex_split_longer(pattern = ";") %>% 
  regex_split_longer(pattern = "\\+|high|bright|dim") %>%
  regex_split_longer(pattern = "\\.") %>%
  # Extracting gene and proteins names
  regex_extract(pattern = "cd\\d+") %>%
  regex_replace(pattern = "(\\w+)-(\\d+)", replace = "\\1\\2") %>%
  regex_replace(pattern = "(?:\\()?(\\w+)(?:\\))?(?:-)?(\\d+)", replace = "\\1\\2") %>%
  regex_extract(pattern = "tgf") %>%
  regex_extract(pattern = "p53") %>%
  regex_extract(pattern = "yap") %>%
  regex_extract(pattern = "fap") %>%
  regex_extract(pattern = "^egfr|^ras|^raf|^mek") %>%
  regex_split_longer(pattern = "-|‐|–") %>% 
  # Standardizing nomenclature
  regex_extract(pattern = "lgs|hgs|sbt") %>%
  regex_extract(pattern = "stroma") %>%
  regex_replace(pattern = "(?:.*)?(sb)o(t)(?:.*)?", replace = "\\1\\2") %>%
  regex_replace(pattern = "^(?:o)?(ccc)$", replace = "\\1") %>%
  regex_replace(pattern = "(\\w+)(?:\\d+,)*", "\\1") %>%
  regex_replace(pattern = "(tumo)(?:u)?(.*)", replace = "\\1u\\2") %>%
  regex_replace(pattern = "^(\\d{1,2}[pq])(?:\\d+)?(?:.*)?", "chromosome") %>%
  # Removing undesired strings
  regex_remove(pattern = "[:punct:]") %>%
  regex_remove(pattern = "[\\(\\)]") %>%
  regex_remove(pattern = "[\\{\\}\\$\\]]") %>%
  regex_remove(pattern = "g12c|g12v|g12d|v600e|v599e") %>%
  regex_remove(pattern = "background|result|aacr|acknowledgement|macmillan") %>%
  regex_remove(pattern = "john|sons|wiley|ltd|society|pathological|ireland") %>%
  regex_remove(pattern = "online|supplementary|motivation|limited|oxford") %>%
  regex_remove(pattern = "university|copyright|publishers|center|usa|press") %>%
  regex_remove(pattern = "american|association|email|abstract|suppl") %>%
  filter(!str_detect(lemma, "^\\d+$")) %>%
  filter(!str_detect(lemma, "^(ci|p|pequals|nequals|hr|fdr)[=<>;,]")) %>%
  # Redoing initial filters
  filter_by_lemma_length(length = 2) %>%
  anti_join(stop_words, by = c("lemma" = "word")) %>%
  select(id, term = lemma)

# Saving Cleaned Data ==========================================================
write_feather(verbs, here(CLEANED, "verbs.feather"))
write_feather(filtered_tokens, here(CLEANED, "cleaned_tokens.feather"))
