
<!-- README.md is generated from README.Rmd. Please edit that file -->

# abcdutils

<!-- badges: start -->

<!-- badges: end -->

A collection of utility functions for working with the Adolescent Brain
and Cognitive Development dataset.

## Installation

You can install the development version of abcdutils from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("psvelayudhan/abcdutils")
```

## Usage

### Working with the data dictionary

``` r
library(abcdutils)
library(readr)

# Search the NDA's data dictionary from R
search_dd("traumatic brain injury")

# Go to the data dictionary page of a dataframe based on its short name
abcd_dd("abcd_otbi01")

# Remove the data dictionary (yes, this is just df[-1, ]
abcd_otbi01 <- read_csv("abcd_otbi01.txt")
remove_dd(abcd_otbi01)
```

### Extract cleaned dataframes

Subcortical volumes (structural MRI)

``` r
subc_v <- get_subc_v(smrip10201)
```

Subcortical volumes for a predefined set of subjects and a specific
collection event

``` r
# Dataframe containing "subjectkey" column
subject_df <- read_csv("subjectlist.csv")

subc_v <- get_subc_v(smrip10201, subjects = subject_df, t = 0)
```

(where `t = 0` refers to baseline data)

Extraction available for a wide range of variables related to
neuroimaging, demographics, psychosocial resilience, and medical
history:

  - `get_cbcl_aggressive_r`
  - `get_cbcl_anxiety_r`
  - `get_cbcl_attention_r`
  - `get_cbcl_depress_r`
  - `get_cbcl_dizzy`
  - `get_cbcl_headaches`
  - `get_cbcl_overtired`
  - `get_cbcl_sleeping_less`
  - `get_cbcl_sleeping_more`
  - `get_cbcl_vomiting`
  - `get_cort_sa`
  - `get_cort_t`
  - `get_exercise`
  - `get_family_function`
  - `get_full_sleep_df`
  - `get_gord_cor`
  - `get_gord_var`
  - `get_headaches`
  - `get_income`
  - `get_loneliness`
  - `get_mtbi_age`
  - `get_mtbi_count`
  - `get_mtbi_loc`
  - `get_mtbi_mechanism`
  - `get_mtbi_mem_daze`
  - `get_nihtbx_cardsort_fc`
  - `get_nihtbx_list_fc`
  - `get_nihtbx_pattern_fc`
  - `get_parent_psychopathology`
  - `get_prosocial_behaviour`
  - `get_pubertal_status`
  - `get_race`
  - `get_screen_time`
  - `get_sex`
  - `get_sports_and_activities`
  - `get_subc_cor`
  - `get_subc_v`
  - `get_subc_var`
  - `get_wmnd`

### Helpful subsetting

``` r
abcd_otbi01 <- read_csv("abcd_otbi01.txt")

# Subset to just baseline data
filter_timepoint(abcd_otbi01, 0)

# Or just year 1 follow-up data
filter_timepoint(abcd_otbi01, 1)

# Or just subjects of interest
filter_subjects(abcd_otbi01, subject_list)
```

### Concussion data prep

``` r
abcd_otbi01 <- read_csv("abcd_otbi01.txt")

# Renaming columns to be easily interpretable:
rename_tbi(abcd_otbi01)

# Identify which subjects had an mTBI and which had a moderate+ head injury:
identify_all_tbi(abcd_otbi01)

# Identify which head injuries were mTBIs
identify_mtbi(abcd_otbi01)

# When did the mTBIs occur
identify_mtbi_times(abcd_otbi01)

# What mechanism caused their latest mTBI
identify_latest_mtbi_mechanism(abcd_otbi01)

# How many mTBIs did the subject experience
identify_num_mtbi(abcd_otbi01)

# How much LOC occurred for the subject's most recent injury
identify_latest_mtbi_loc(abcd_otbi01)

# Did memory loss / feeling dazed or confused occur on the most recent injury
identify_latest_mtbi_mem_daze(abcd_otbi01)

# Combine all the functions above
detail_mtbi(abcd_otbi01)
```
