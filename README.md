
<!-- README.md is generated from README.Rmd. Please edit that file -->

# abcdutils

<!-- badges: start -->
<!-- badges: end -->

A collection of utility functions for working with the Adolescent Brain
and Cognitive Development dataset.

## Installation

You can install abcdutils from [GitHub](https://github.com/) with:

``` r
# Latest development version
devtools::install_github("psvelayudhan/abcdutils")

# Install a specific tagged version
devtools::install_github("psvelayudhan/abcdutils@v0.3.0")
```

## Usage

### Working with the data dictionary

``` r
library(abcdutils)
library(readr)

# Search the NDA's data dictionary from R
search_dd("traumatic brain injury")

# Launch the data dictionary in your browser
open_dd()
```

### Extract cleaned dataframes

Subcortical volumes (structural MRI)

``` r
subc_v <- get_subc_v(mri_y_smr_vol_aseg)
```

Subcortical volumes for a predefined set of subjects and a specific
collection event

``` r
# Dataframe containing "subjectkey" column
subject_df <- read_csv("subjectlist.csv")

subc_v <- get_subc_v(
    mri_y_smr_vol_aseg,
    subjects = subject_df,
    t = 0
)
```

(where `t = 0` refers to baseline data)

Extraction available for a wide range of variables related to
neuroimaging, demographics, psychosocial resilience, and medical
history:

- `get_all_wmnd()`
- `get_cbcl_syndrome_scale()`
- `get_cort_sa()`
- `get_cort_t()`
- `get_exercise()`
- `get_family_function()`
- `get_friends()`
- `get_gord_cor()`
- `get_gord_var()`
- `get_headaches()`
- `get_income()`
- `get_interview_age()`
- `get_mtbi_age()`
- `get_mtbi_count()`
- `get_mtbi_loc()`
- `get_mtbi_mechanism()`
- `get_mtbi_mem_daze()`
- `get_mtbi_subjects()`
- `get_nihtbx_cardsort_fc()`
- `get_nihtbx_list_fc()`
- `get_p_gender()`
- `get_parent_psychopathology()`
- `get_prosocial_behaviour_p()`
- `get_prosocial_behaviour_y()`
- `get_pubertal_status_p()`
- `get_pubertal_status_y()`
- `get_race()`
- `get_screen_time()`
- `get_sds_total_probs()`
- `get_sex()`
- `get_sleep_disturbance()`
- `get_sports_and_activities()`
- `get_subc_cor()`
- `get_subc_v()`
- `get_subc_var()`
- `get_uninjured_subjects()`
- `get_y_gender()`

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

### Plotting

``` r
# Visualize missing data across several dataframes
df_list <- list(
    "loss of consciousness" = as_mtbi_loc,
    "mechanism of injury" = as_mtbi_mechanism,
    "memory loss / dazed" = as_mtbi_mem_daze,
    "income" = d_income)

vis_missing_by_df(df_list)
```
