
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
devtools::install_github("psvelayudhan/abcdutils@v0.4.0")
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

| Function                   | Description                                                                                                     |
|----------------------------|-----------------------------------------------------------------------------------------------------------------|
| get_cbcl_syndrome_scale    | Raw or T-scores of the anxdep, withdep, somatic, social, thought, attention, rulebreak, and attention subscales |
| get_cort_sa                | Cortical surface area                                                                                           |
| get_cort_t                 | Cortical thickness                                                                                              |
| get_exercise               | A composite measure of exercise time from the Youth Risk Behavior Survey                                        |
| get_family_function        | Parent and youth responses to the Famiily Environment Scale                                                     |
| get_family_id              | Family-specific ID variable                                                                                     |
| get_friends                | Number of same and opposite sex close and total friends                                                         |
| get_gender_p               | Parent report of child’s gender                                                                                 |
| get_gender_y               | Youth’s self report of gender                                                                                   |
| get_gord_cor               | Cortical-cortical rs-fMRI network correlations                                                                  |
| get_gord_var               | Cortical-cortical rs-fMRI temporal variances                                                                    |
| get_headaches              | Headache history                                                                                                |
| get_income                 | Three-tiered family income                                                                                      |
| get_interview_age          | Child age at data collection                                                                                    |
| get_mtbi_age               | Age at latest concussion                                                                                        |
| get_mtbi_count             | Total number of concussions                                                                                     |
| get_mtbi_loc               | LOC status for last concussion                                                                                  |
| get_mtbi_mechanism         | Mechanism of acquiring last concussion                                                                          |
| get_mtbi_mem_daze          | Memory loss / dazed status of last concussion                                                                   |
| get_mtbi_subjects          | Identify children who have had a concussion                                                                     |
| get_nihtbx_cardsort_fc     | NIH toolbox card sorting task                                                                                   |
| get_nihtbx_list_fc         | NIH toolbox list sorting task                                                                                   |
| get_parent_psychopathology | Parent self report scores from ASEBA                                                                            |
| get_prosocial_behaviour_p  | Parent reports of child prosocial behaviour                                                                     |
| get_prosocial_behaviour_y  | Child self reports of prosocial behaviour                                                                       |
| get_pubertal_status_p      | Parent report of child pubertal status                                                                          |
| get_pubertal_status_y      | Youth self report of pubertal status                                                                            |
| get_race                   | Child ethnicity                                                                                                 |
| get_scanner_id             | Scanner-specific ID variable                                                                                    |
| get_screen_time            | Screentime on weekdays and weekends in hours                                                                    |
| get_sex                    | Sex at birth                                                                                                    |
| get_sleep_disturbance      | Sum of problems on Sleep Disturbance Scale                                                                      |
| get_sports_and_activities  | Total participation in sports and activities                                                                    |
| get_subc_cor               | Subcortical-cortical rs-fMRI network correlations                                                               |
| get_subc_v                 | Subcortical volume                                                                                              |
| get_subc_var               | Subcortical-cortical rs-fMRI temporal variances                                                                 |
| get_uninjured_subjects     | Identify children who have not had any head injuries                                                            |
| get_wmnd                   | Neurite densities of major and peri-cortical white matter regions                                               |

### Simple subsetting

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
    "income" = d_income
)

vis_missing_by_df(df_list)
```
