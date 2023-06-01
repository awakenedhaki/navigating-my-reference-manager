# Navigating My Reference Manager

## Introduction

## Repository Structure

```         
.
├── README.md
├── navigating-my-reference-manager.Rproj
├── R/
│   ├── 01-wrangling.R
│   ├── 02-normalization.R
│   ├── 03-cleaning.R
│   ├── 04-feature-engineering.R
│   ├── 05-keyword-analysis.R
│   ├── 06-correlation-network.R
│   ├── 07-topic-modelling.R
│   └── 08-latent-semantic-analysis.R
├── resources/
|   ├── collapsing_tokens.csv
|   ├── gene_name_corrections.csv
|   ├── plural_removal.csv
|   └── remove_words.csv
└── data
    └── raw/
    |   ├── mendeley.bib
    |   └── papers.csv
    └── active/
        ├── abstracts.feather
        ├── metadata.feather
        ├── normalized_tokens.feather
        ├── clean/
        │   ├── cleaned_abstracts_lower-50_upper-825.feather
        │   └── cleaned_abstracts_lower-75_upper-825.feather
        └── feature_engineering/
            ├── tf_idf_matrix_long_1_0_0-01.feather
            ├── tf_idf_matrix_long_1_0_0-05.feather
            ├── tf_idf_matrix_long_1_0_0-1.feather
            ├── tf_idf_matrix_long_1_2_0-01.feather
            ├── tf_idf_matrix_long_1_2_0-05.feather
            ├── tf_idf_matrix_long_1_2_0-1.feather
            ├── tf_idf_matrix_long_1_3_0-01.feather
            ├── tf_idf_matrix_long_1_3_0-05.feather
            ├── tf_idf_matrix_long_1_3_0-1.feather
            ├── tf_idf_matrix_long_3_2_0-01.feather
            ├── tf_idf_matrix_long_3_2_0-05.feather
            ├── tf_idf_matrix_long_3_2_0-1.feather
            ├── tf_idf_matrix_long_5_0_0-01.feather
            ├── tf_idf_matrix_long_5_0_0-05.feather
            ├── tf_idf_matrix_long_5_0_0-1.feather
            ├── tf_idf_matrix_long_5_3_0-01.feather
            ├── tf_idf_matrix_long_5_3_0-05.feather
            └── tf_idf_matrix_long_5_3_0-1.feather
```
