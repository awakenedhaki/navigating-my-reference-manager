# ==============================================================================
# Text Preprocessing 
# ==============================================================================

# Loading Packages =============================================================
library(here)
library(spacyr)
library(feather)
library(tidyverse)

# Constants ====================================================================
DATA <- here("data")

# Loading Data =================================================================
abstracts <- read_feather(here(ACTIVE_DATA, "abstracts.feather"))

# Text Preprocessing ===========================================================
spacy_initialize(model = "en_core_sci_md")

normalized_tokens <- abstracts %>%
  pull(abstract, id) %>%
  spacy_parse(pos = TRUE, lemma = TRUE, entity = TRUE,
              tags = FALSE, nounphrase = FALSE,
              additional_attributes = c("is_stop", "like_email", "like_url"),
              multithreaded = TRUE)

entities <- entity_extract(normalized_tokens)

spacy_finalize()

# Saving Preprocessed Data =====================================================
write_feather(normalized_tokens, here(DATA, "normalized_tokens.feather"))
write_feather(entities, here(DATA, "entities.feather"))
