# ==============================================================================
# Text Normalization
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
CORPUS <- read_feather(here(ACTIVE_DATA, "cleaned_abstracts.feather"))
TERM_CORRECTION <- tribble(~regex,                ~term,
                           # Renaming
                           "low.grade|lgs(oc)?",    "lgsc",
                           "high.grade|hgs(o)?c",   "hgsc",
                           "sb(o)?t(s)?",           "sbt",
                           "th\\d|treg",            "t-cell",
                           "mutat",                 "mutation",
                           "taxon",                 "taxonomy",
                           "transcript(?:ome)?",    "transcript",
                           "translat",              "translation",
                           "hsv",                   "hsv",
                           "genom",                 "genome",
                           "transcriptom",          "transcriptome",
                           "gynecolo",              "gynecologic",
                           "conserved",             "conserve",
                           "stiffness",             "stiff",
                           "stromal",               "stroma",
                           "metastatic",            "metastasis",
                           "herpe",                 "herpes",
                           "^vivo",                 "vivo",
                           "tum[ou]r",              "tumor",
                           "^onco",                 "onco",
                           "^vascular(?:.*)?",      "vascular",
                           "mechano",               "mechano",
                           "\\(|non-",              "",
                           # Collapsing Gene/Protein names
                           "^fap",                  "fap",
                           "^pdgf.*",               "pdgf",
                           "^smad.*",               "smad",
                           "^brca.*",               "brca",
                           "^wnt.*",                "wnt",
                           "^tgf.*",                "tgf",
                           "^braf.*",               "raf",
                           "^erk.*",                "erk",
                           "^[kn]ras.*",            "ras",
                           "^yap1",                 "yap",
                           "^igf.*",                "igf",
                           "^mmp[^i]",              "mmp",
                           "rsf.1",                 "rsf1",
                           "pd.1",                  "pd1",
                           "il.{0,2}\\d+",          "interleukin",
                           "parp-",                 "parp",
                           "rhoa",                  "rho",
                           "tp53",                  "p53",
                           # Removing plurals
                           "^caf(s)?",              "caf",
                           "author(.)?s",           "author",
                           "lncrnas",               "lncrna",
                           "mi(cro)?rnas",          "mirna",
                           "mrnas",                 "mrna",
                           "mpscs",                 "mpsc",
                           "mscs",                  "msc",
                           "umis",                  "umi",
                           "bioinformatics",        "bioinformatic",
                           # Remove token
                           "\\.\\.\\.|i\\.e\\.",    "__remove",
                           "britain|ireland|aacr",  "__remove",
                           "john|sons|wiley|©",     "__remove",
                           "publishers|macmillan",  "__remove",
                           "inc|elsevier|central",  "__remove",
                           "publish|copyright",     "__remove",
                           "pathological|ltd|\\s+", "__remove",
                           "approve|recent|define", "__remove",
                           "objective|method",      "__remove",
                           "result|background",     "__remove",
                           "university|press",      "__remove",
                           "licensee|pubmed|clin$", "__remove",
                           "society|center|iii",    "__remove",
                           "discovery|bhw|nhbs",    "__remove")

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
  filter(term != "__remove")
  
spacy_finalize()

# Saving Count Matrix ==========================================================
write_feather(normalized_tokens, here(ACTIVE_DATA, "normalized_tokens.feather"))
