# ==============================================================================
# Cleaning Author Names
# ==============================================================================

# Loading Packages =============================================================
library(here)
library(glue)
library(feather)
library(tidyverse)
library(humaniformat)

# Constants ====================================================================
DATA <- here("data")

# Loading Data =================================================================
metadata <- read_feather(here(DATA, "metadata.feather")) %>%
  select(id, journal, publication_year, author, title, reference_manager)

# Cleaning Authors Names =======================================================
authors <- metadata %>%
  select(id, author) %>%
  separate_longer_delim(cols = author, delim = ",") %>%
  mutate(first_name = first_name(author),
         middle_name = middle_name(author),
         last_name = last_name(author)) %>%
  mutate_at(.vars = vars(contains("name")),
            .funs = \(x) x <- str_remove_all(x, "\\.")) %>%
  mutate(author = str_remove(author, "^ "))

# . Shift Names
shifted_names <- which(nchar(authors$last_name) == 1)

first_name <- authors[shifted_names, "middle_name"]
middle_name <- authors[shifted_names, "last_name"]
last_name <- authors[shifted_names, "first_name"]

authors[shifted_names, "first_name"] <- first_name
authors[shifted_names, "middle_name"] <- middle_name
authors[shifted_names, "last_name"] <- last_name

# . Middle to First
swapped_given_names <- which(nchar(authors$first_name) == 1 & 
                               nchar(authors$middle_name) > 1)

first_name <- authors[swapped_given_names, "middle_name"]
middle_name <- authors[swapped_given_names, "first_name"]

authors[swapped_given_names, "first_name"] <- first_name
authors[swapped_given_names, "middle_name"] <- middle_name

# . Manual Correction
# . . Eric Z. Ma
eric_z_ma <- which(authors$author == "Eric Z. Ma")
authors[eric_z_ma, "first_name"] <- "Eric"
authors[eric_z_ma, "last_name"] <- "Ma"

# . . John S Coon
john_s_coon <- which(authors$author == "John S. Coon V")
authors[john_s_coon, "first_name"] <- "John"
authors[john_s_coon, "last_name"] <- "Coon"

# . . Stephen S T Wong
stephen_s_t_wong <- which(authors$last_name == "ST Wong")
authors[stephen_s_t_wong, "last_name"] <- "Wong"

# . . Michael S. Waterman
michael_s_waterman <- which(authors$author == "M.S Waterman")
authors[michael_s_waterman, "first_name"] <- "Michael"
authors[michael_s_waterman, "last_name"] <- "Waterman"

# . . David R. Rowley
david_r_rowley <- which(authors$author == "DR Rowley")
authors[david_r_rowley, "first_name"] <- "David"
authors[david_r_rowley, "last_name"] <- "Rowley"

# . . Document Specific Issue
missing_last_name <- which(authors$id == "fbeece21-eb59-412a-b803-6595e25a2fc7")

authors[missing_last_name, ] <- authors[missing_last_name, ] %>%
  fill(first_name, .direction = "down")
authors <- authors %>%
  filter(!(id == "fbeece21-eb59-412a-b803-6595e25a2fc7" & is.na(last_name)))

shifted_names <- which(authors$id == "fbeece21-eb59-412a-b803-6595e25a2fc7")
first_name <- authors[shifted_names, ]$last_name
last_name <- authors[shifted_names, ]$first_name

authors[shifted_names, "first_name"] <- first_name
authors[shifted_names, "last_name"] <- last_name

# . Removing Initials from First Names
authors <- authors %>%
  mutate(first_name = ifelse(str_detect(first_name, "[A-Z ] \\w{2,}"),
                             str_remove(first_name, "[A-Z ] "),
                             first_name))

# . Replacing First Name Initals
authors %>%
  filter(str_detect(first_name, "^[A-Z]$"))