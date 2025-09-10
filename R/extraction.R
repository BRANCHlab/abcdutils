#' Extract CBCL syndrome scale data
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param mh_p_cbcl Data frame containing ABCD CBCL data
#' @param raw Boolean indicating if extracted data should be raw (TRUE) or
#'  t-scores (FALSE). Defaults to TRUE.
#' @return A data frame containing syndrome scale data
#' @export
get_cbcl_syndrome_scale <- function(mh_p_cbcl,
                                    raw = TRUE,
                                    t = NULL,
                                    subjects = NULL) {
    mh_p_cbcl <- swap_src_subjectkey(mh_p_cbcl)
    # Check that the provided syndrome is present
    syndromes <- c(
        "anxdep",
        "withdep",
        "somatic",
        "social",
        "thought",
        "attention",
        "rulebreak",
        "aggressive"
    )
    # Initial cleaning and filtering of the data frame
    mh_p_cbcl <- mh_p_cbcl |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    if (raw == TRUE) {
        suffix <- "_r"
    } else if (raw == FALSE) {
        suffix <- "_t"
    }
    syndromes <- paste0("cbcl_scr_syn_", syndromes, suffix)
    cols_to_keep <- c("subjectkey", syndromes)
    ss_data <- mh_p_cbcl[, cols_to_keep]
    return(ss_data)
}

#' Extract cortical surface areas
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param mri_y_smr_area_dsk Data file containing cortical data
#' @param only_total Logical indicating whether to return only the total
#'  cortical surface area or all cortical surface areas. Defaults to TRUE
#' @return A data frame of cortical surface areas
#' @export
get_cort_sa <- function(mri_y_smr_area_dsk,
                        subjects = NULL,
                        t = NULL,
                        only_total = TRUE) {
    mri_y_smr_area_dsk <- swap_src_subjectkey(mri_y_smr_area_dsk)
    cort_raw <- mri_y_smr_area_dsk |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    if (only_total) {
        cort_sa_df <- cort_raw |>
            dplyr::select(
                "subjectkey",
                "smri_area_cdk_total"
            )
    } else {
        cort_sa_df <- cort_raw |>
            dplyr::select(-"eventname") 
    }
    return(cort_sa_df)
}

#' Extract cortical thicknesses
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param mri_y_smr_thk_dsk Data file containing cortical data
#' @param only_mean Logical indicating whether to return only the mean cortical
#'  thickness or all cortical thicknesses. Defaults to TRUE.
#' @return A data frame of cortical thicknesses
#' @export
get_cort_t <- function(mri_y_smr_thk_dsk,
                       subjects = NULL,
                       t = NULL,
                       only_mean = TRUE) {
    mri_y_smr_thk_dsk <- swap_src_subjectkey(mri_y_smr_thk_dsk)
    cort_raw <- mri_y_smr_thk_dsk |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    if (only_mean) {
        cort_t_df <- cort_raw |>
            dplyr::select(
                "subjectkey",
                "smri_thick_cdk_mean"
            )
    } else {
        cort_t_df <- cort_raw |>
            dplyr::select(
                -"eventname"
            )
    }
    return(cort_t_df)
}

#' Extract healthy behaviours: exercise questionnaire
#'
#' This function returns a measure of exercise time from the ABCD Youth Risk
#' Behavior Survey. The values are stored in the ABCD table `ph_y_yrb.txt`
#' under the names `physical_activity1_y`, `physical_activity2_y`, and
#' `physical_activity5_y`. The final measure is the average of these three
#' variables.
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param ph_y_yrb ABCD Youth Risk Behavior Survey Exercise Physical Activity.
#' @return A data frame containing exercise data.
#' @export
get_exercise <- function(ph_y_yrb,
                         subjects = NULL,
                         t = NULL) {
    ph_y_yrb <- swap_src_subjectkey(ph_y_yrb)
    exercise <- ph_y_yrb |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects) |>
        dplyr::select("subjectkey", dplyr::ends_with("y"))
    # Taking the scaled average of all physical activity scores
    exercise <- exercise |>
        dplyr::mutate(
            physical_activity = (
                exercise$"physical_activity1_y" / 7 +
                    exercise$"physical_activity2_y" / 7 +
                    exercise$"physical_activity5_y" / 5
            ) / 3
        ) |>
        dplyr::select("subjectkey", "physical_activity")
    return(exercise)
}

#' Extract family function
#'
#' This function returns parent and youth responses to the ABCD Parent Family
#' Environment Scale-Family Conflict Subscale Modified from PhenX. The values
#' are stored in the ABCD tables ce_y_fes.txt and ce_p_fes.txt under the names
#' `fes_youth_q1` to `fes_youth_q9` and `fam_enviro1_p` to `fam_enviro9r_p`.
#'
#' The function also renames the variables as follows:
#' - "fes_youth_q1" -> "q1_fight_y"
#' - "fam_enviro1_p" -> "q1_fight_p"
#' - "fes_youth_q2" -> "q2_angry_y"
#' - "fam_enviro2r_p" -> "q2_angry_p"
#' - "fes_youth_q3" -> "q3_throw_y"
#' - "fam_enviro3_p" -> "q3_throw_p"
#' - "fes_youth_q4" -> "q4_temper_y"
#' - "fam_enviro4r_p" -> "q4_temper_p"
#' - "fes_youth_q5" -> "q5_criticize_y"
#' - "fam_enviro5_p" -> "q5_criticize_p"
#' - "fes_youth_q6" -> "q6_hit_y"
#' - "fam_enviro6_p" -> "q6_hit_p"
#' - "fes_youth_q7" -> "q7_peaceful_y"
#' - "fam_enviro7r_p" -> "q7_peaceful_p"
#' - "fes_youth_q8" -> "q8_outdo_y"
#' - "fam_enviro8_p" -> "q8_outdo_p"
#' - "fes_youth_q9" -> "q9_yell_y"
#' - "fam_enviro9r_p" -> "q9_yell_p"
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param ce_y_fes ABCD table containing Parent Family Environment Scale-Family
#'  Conflict Subscale Modified from PhenX (youth report).
#' @param ce_p_fes ABCD table containing Parent Family Environment Scale-Family
#'  Conflict Subscale Modified from PhenX (parent report).
#' @return A data frame containing 18 response (9 parent, 9 youth) columns for
#'  family environment.
#' @export
get_family_function <- function(ce_y_fes,
                                ce_p_fes,
                                subjects = NULL,
                                t = NULL) {
    ce_p_fes <- swap_src_subjectkey(ce_p_fes)
    ce_y_fes <- swap_src_subjectkey(ce_y_fes)
    p_family_function  <- ce_y_fes |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    y_family_function  <- ce_p_fes |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    family_function  <- dplyr::inner_join(
        p_family_function,
        y_family_function,
        by = "subjectkey"
    ) |>
        dplyr::select(
            "subjectkey",
            "fes_youth_q1",
            "fes_youth_q2",
            "fes_youth_q3",
            "fes_youth_q4",
            "fes_youth_q5",
            "fes_youth_q6",
            "fes_youth_q7",
            "fes_youth_q8",
            "fes_youth_q9",
            "fam_enviro1_p",
            "fam_enviro2r_p",
            "fam_enviro3_p",
            "fam_enviro4r_p",
            "fam_enviro5_p",
            "fam_enviro6_p",
            "fam_enviro7r_p",
            "fam_enviro8_p",
            "fam_enviro9r_p"
        )
    # Convert columns to numeric for subsequent averaging
    family_function <- col_to_num(family_function, 2:length(family_function))
    # Average the reports from youth and parents
    family_function <- family_function |>
        dplyr::rename(
            "q1_fight_y" = "fes_youth_q1",
            "q1_fight_p" = "fam_enviro1_p",
            "q2_angry_y" = "fes_youth_q2",
            "q2_angry_p" = "fam_enviro2r_p",
            "q3_throw_y" = "fes_youth_q3",
            "q3_throw_p" = "fam_enviro3_p",
            "q4_temper_y" = "fes_youth_q4",
            "q4_temper_p" = "fam_enviro4r_p",
            "q5_criticize_y" = "fes_youth_q5",
            "q5_criticize_p" = "fam_enviro5_p",
            "q6_hit_y" = "fes_youth_q6",
            "q6_hit_p" = "fam_enviro6_p",
            "q7_peaceful_y" = "fes_youth_q7",
            "q7_peaceful_p" = "fam_enviro7r_p",
            "q8_outdo_y" = "fes_youth_q8",
            "q8_outdo_p" = "fam_enviro8_p",
            "q9_yell_y" = "fes_youth_q9",
            "q9_yell_p" = "fam_enviro9r_p"
        ) |>
        dplyr::select("subjectkey", dplyr::starts_with("q"))
    return(family_function)
}

#' Extract family ID
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param abcd_y_lt Data frame storing family information.
#' @return A data frame containing subjectkey and family ID.
#' @export
get_family_id <- function(abcd_y_lt, subjects = NULL, t = NULL) {
    abcd_y_lt <- swap_src_subjectkey(abcd_y_lt)
    family_df <- abcd_y_lt |>
        dplyr::rename(
            "family_id" = "rel_family_id"
        ) |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects) |>
        dplyr::select("subjectkey", "family_id")
    return(family_df)
}

#' Extract number of friends
#'
#' This function returns the number of friends and close friends of the same
#' and opposite sex as reported by the ABCD Parent Resilience Survey. The values
#' are stored in the ABCD table `mh_y_or.txt` under the names `resiliency5a_y`,
#' '`resiliency5b_y`, `resiliency6a_y`, and `resiliency6b_y`. The variables
#' can also be discretized into quartiles relative to the entire sample at
#' the specified timepoint.
#' 
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param mh_y_or ABCD Other Resilience.
#' @param gish_p_gi Parent report of sex and gender data frame.
#' @param discretize Boolean indicating if the loneliness measures should be
#' discretized into quartiles.
#' @return A data frame containing number of friends.
#' @export
get_friends <- function(mh_y_or,
                        gish_p_gi,
                        subjects = NULL,
                        t = NULL,
                        discretize = FALSE) {
    gish_p_gi <- swap_src_subjectkey(gish_p_gi)
    mh_y_or <- swap_src_subjectkey(mh_y_or)
    sex <- get_sex(gish_p_gi, subjects, t = 0)
    friends <- mh_y_or |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    friends <- dplyr::full_join(friends, sex, by = "subjectkey")
    friends <- friends |>
        dplyr::rename(
            "friend_boy" = "resiliency5a_y",
            "close_friend_boy" = "resiliency5b_y",
            "friend_girl" = "resiliency6a_y",
            "close_friend_girl" = "resiliency6b_y"
        )
    friends <- numcol_to_numeric(friends)
    friends <- friends |>
        dplyr::mutate(
            "ss_friend" = dplyr::case_when(
                friends$"sex" == "M" ~ friends$"friend_boy",
                friends$"sex" == "F" ~ friends$"friend_girl"
            ),
            "os_friend" = dplyr::case_when(
                friends$"sex" == "M" ~ friends$"friend_girl",
                friends$"sex" == "F" ~ friends$"friend_boy"
            ),
            "ss_close_friend" = dplyr::case_when(
                friends$"sex" == "M" ~ friends$"close_friend_boy",
                friends$"sex" == "F" ~ friends$"close_friend_girl"
            ),
            "os_close_friend" = dplyr::case_when(
                friends$"sex" == "M" ~ friends$"close_friend_girl",
                friends$"sex" == "F" ~ friends$"close_friend_boy"
            )
        )
    friend_fts <- c(
        "ss_friend", "os_friend", "ss_close_friend", "os_close_friend"
    )
    if (is.null(t)) {
        features <- c("subjectkey", "eventname", friend_fts)
    } else {
        features <- c("subjectkey", friend_fts)
    }
    friends <- dplyr::select(friends, dplyr::all_of(features))
    if (discretize) {
        # Based on the quantiles of these measures on all baseline subjects.
        # This can be re-obtained by running `get_friends` at bl without
        # specifying any subjects.
        disc_df <- get_friends(mh_y_or, gish_p_gi, t = t)
        ss_qs <- stats::quantile(stats::na.omit(disc_df$"ss_friend"))[2:4]
        os_qs <- stats::quantile(stats::na.omit(disc_df$"os_friend"))[2:4]
        ss_close_qs <- stats::quantile(stats::na.omit(disc_df$"ss_close_friend"))[2:4]
        os_close_qs <- stats::quantile(stats::na.omit(disc_df$"os_close_friend"))[2:4]
        friends <- friends |>
            dplyr::mutate(
                ss_friend = dplyr::case_when(
                    ss_friend <= ss_qs[1] ~ 1,
                    ss_friend <= ss_qs[2] ~ 2,
                    ss_friend <= ss_qs[3] ~ 3,
                    ss_friend > ss_qs[3] ~ 4,
                    TRUE ~ NA
                ),
                os_friend = dplyr::case_when(
                    os_friend <= os_qs[1] ~ 1,
                    os_friend <= os_qs[2] ~ 2,
                    os_friend <= os_qs[3] ~ 3,
                    os_friend > os_qs[3] ~ 4,
                    TRUE ~ NA
                ),
                ss_close_friend = dplyr::case_when(
                    ss_close_friend <= ss_close_qs[1] ~ 1,
                    ss_close_friend <= ss_close_qs[2] ~ 2,
                    ss_close_friend <= ss_close_qs[3] ~ 3,
                    ss_close_friend > ss_close_qs[3] ~ 4,
                    TRUE ~ NA
                ),
                os_close_friend = dplyr::case_when(
                    os_close_friend <= os_close_qs[1] ~ 1,
                    os_close_friend <= os_close_qs[2] ~ 2,
                    os_close_friend <= os_close_qs[3] ~ 3,
                    os_close_friend > os_close_qs[3] ~ 4,
                    TRUE ~ NA
                )
            )
    }
    return(friends)
}

#' return A data frame containing parent-reported genders
#'
#' Collected parent reported gender information from the `demo_gender_id_v2`
#' column of the `gish_p_gi` table.
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param gish_p_gi Parent report of sex and gender data frame
#' @param match_y_report If TRUE, codes outcomes to match youth report options
#' at follow up timepoints (male, female, non-binary, NA). If FALSE, uses
#' response options provided to parents (male, female, trans male, trans
#' female, gender queer, and NA). In both cases, "refuse to answer" and
#' "don't know" are pooled with the NA option.
#' @export
get_gender_p <- function(gish_p_gi,
                         subjects = NULL,
                         t = NULL,
                         match_y_report = FALSE) {
    gish_p_gi <- swap_src_subjectkey(gish_p_gi)
    pgi <- gish_p_gi |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects) |>
        dplyr::select(
            "subjectkey",
            "demo_gender_id_v2", # what is their assigned sex
        )
    gender <- pgi |>
        dplyr::rename(
            "p_gender" = "demo_gender_id_v2",
        )
    if (match_y_report) {
        gender <- gender |>
            dplyr::mutate(
                p_gender = dplyr::case_when(
                    p_gender == 1 ~ "M",
                    p_gender == 2 ~ "F",
                    p_gender == 3 ~ "NB",
                    p_gender == 4 ~ "NB",
                    p_gender == 5 ~ "NB",
                    p_gender == 6 ~ "NB",
                    TRUE ~ NA
                )
            )
    } else {
        gender <- gender |>
            dplyr::mutate(
                p_gender = dplyr::case_when(
                    p_gender == 1 ~ "M",
                    p_gender == 2 ~ "F",
                    p_gender == 3 ~ "TM",
                    p_gender == 4 ~ "TF",
                    p_gender == 5 ~ "Q",
                    p_gender == 6 ~ "ONB",
                    TRUE ~ NA
                )
            )
    }
    return(gender)
}

#' return A data frame containing youth-reported genders
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param gish_y_gi Youth-reported gender data frame
#' @return A data frame containing sex
#' @export
get_gender_y <- function(gish_y_gi,
                         subjects = NULL,
                         t = NULL) {
    gish_y_gi <- swap_src_subjectkey(gish_y_gi)
    pgi <- gish_y_gi |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects) |>
        dplyr::select(
            "subjectkey",
            "kbi_gender", # what is their assigned sex
        )
    gender <- pgi |>
        dplyr::rename(
            "y_gender" = "kbi_gender",
        )
    gender <- gender |>
        dplyr::mutate(
            y_gender = dplyr::case_when(
                y_gender == 1 ~ "M",
                y_gender == 2 ~ "F",
                y_gender == 3 ~ "NB",
                TRUE ~ NA
            )
        )
    return(gender)
}

#' Extract cortical network correlations
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param mri_y_rsfmr_cor_gp_gp Data file containing neurite density data
#' @param roi_pairs List where each item in the list is a 2-item vector of ROIs
#' to select a correlation variable for. Possible ROIs are:
#'     - "auditory"
#'     - "cingulo_opercular"
#'     - "cingulo_parietal"
#'     - "default"
#'     - "dorsal_attention"
#'     - "fronto_parietal"
#'     - "none"
#'     - "retrosplenial_temporal"
#'     - "salience"
#'     - "sensorimotor_hand"
#'     - "sensorimotor_mouth"
#'     - "ventral_attention"
#'     - "visual"
#' @return A data frame of white matter neurite densities
#' @export
get_gord_cor <- function(mri_y_rsfmr_cor_gp_gp,
                         subjects = NULL,
                         t = NULL,
                         roi_pairs) {
    mri_y_rsfmr_cor_gp_gp <- swap_src_subjectkey(mri_y_rsfmr_cor_gp_gp)
    gord_cor <- mri_y_rsfmr_cor_gp_gp |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    networks <- c(
        "auditory" = "ad",
        "cingulo_opercular" = "cgc",
        "cingulo_parietal" = "ca",
        "default" = "dt",
        "dorsal_attention" = "dla",
        "fronto_parietal" = "fo",
        "none" = "n",
        "retrosplenial_temporal" = "rspltp",
        "salience" = "sa",
        "sensorimotor_hand" = "smh",
        "sensorimotor_mouth" = "smm",
        "ventral_attention" = "vta",
        "visual" = "vs"
    )
    keep_vars <- lapply(
        roi_pairs,
        function(x) {
            paste0(
                "rsfmri_c_ngd_",
                networks[[x[[1]]]],
                "_ngd_",
                networks[[x[[2]]]]
            )
        }
    ) |> unlist()
    # Remove the subjectkeys
    gord_cor <- gord_cor |>
        dplyr::select(
            "subjectkey",
            dplyr::all_of(keep_vars)
        )
    return(gord_cor)
}

#' Get subject headache history
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param ph_p_mhx Data frame containing medical history
#' @return A data frame containing headache history
#' @export
get_headaches <- function(ph_p_mhx,
                          subjects = NULL,
                          t = NULL) {
    ph_p_mhx <- swap_src_subjectkey(ph_p_mhx)
    headaches <- ph_p_mhx |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    if (isTRUE(t > 0)) {
        headaches <- headaches |>
            dplyr::rename("headache" = "medhx_2q_l") |>
            dplyr::select("subjectkey", "headache")
    } else {
        headaches <- headaches |>
            dplyr::rename("headache" = "medhx_2q") |>
            dplyr::select("subjectkey", "headache")
    }
    return(headaches)
}

#' Returns combined household incomes split into low, medium, and high groups
#'
#' Low: $0 - $50k, Medium: $50k - $100k, High: > $100k
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param abcd_p_demo Data frame containing parent demographic information
#' @return A data frame containing household incomes
#' @export
get_income <- function(abcd_p_demo,
                       subjects = NULL,
                       t = NULL) {
    abcd_p_demo <- swap_src_subjectkey(abcd_p_demo)
    income <- ""
    income_df <- abcd_p_demo |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    if (isTRUE(t > 0)) {
        income_df$"income" <- income_df$"demo_comb_income_v2_l"
    } else {
        income_df$"income" <- income_df$"demo_comb_income_v2"
    }
    income_df$"income" <- as.numeric(income_df$"income")
    income_df <- income_df |>
        dplyr::select(
            "subjectkey",
            "income"
        )
    income_df <- income_df |>
        dplyr::mutate(
            household_income = dplyr::case_when(
                income == 777 ~ NA_real_,
                income == 999 ~ NA_real_,
                income < 7 ~ 1,
                income < 9 ~ 2,
                income < 11 ~ 3,
                TRUE ~ NA_real_,
            )
        )
    income_df <- income_df |> dplyr::select("subjectkey", "household_income")
    return(income_df)
}

#' return A data frame containing interview age of specified subjects
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param abcd_y_lt Data frame containing interview age.
#' @param as_years Logical indicating whether to convert age to years.
#' @return A data frame containing interview age.
#' @export
get_interview_age <- function(abcd_y_lt,
                              subjects = NULL,
                              t = NULL,
                              as_years = TRUE) {
    abcd_y_lt <- swap_src_subjectkey(abcd_y_lt)
    interview_age <- abcd_y_lt |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects) |>
        dplyr::select("subjectkey", "interview_age")
    if (as_years == TRUE) {
        interview_age$"interview_age" <- round(
            interview_age$"interview_age" / 12
        )
    }
    return(interview_age)
}

#' Extract age at latest mTBI
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param ph_p_otbi TBI data frame
#' @param abcd_y_lt Data frame containing age information
#' @param as_years Logical indicating whether to convert age to years
#' @return A data frame containing latest_mtbi_age
#' @export
get_mtbi_age <- function(ph_p_otbi,
                         abcd_y_lt,
                         subjects = NULL,
                         t = NULL,
                         as_years = TRUE) {
    abcd_y_lt <- swap_src_subjectkey(abcd_y_lt)
    ph_p_otbi <- swap_src_subjectkey(ph_p_otbi)
    mtbi_age <- detail_mtbi(ph_p_otbi, abcd_y_lt, subjects, t = t) |>
        dplyr::select(
            "subjectkey",
            "latest_mtbi_age"
        )
    if (as_years) {
        mtbi_age$"latest_mtbi_age" <- round(
            mtbi_age$"latest_mtbi_age" / 12
        )
    }
    return(mtbi_age)
}

#' Extract number of mTBIs sustained by subject
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param ph_p_otbi TBI data frame
#' @param abcd_y_lt Data frame containing age information
#' @param cutoff Maximum number of mtbis to be reported
#' @return A data frame containing number of previous mTBIs
#' @export
get_mtbi_count <- function(ph_p_otbi,
                           abcd_y_lt,
                           subjects = NULL,
                           t = NULL,
                           cutoff = NULL) {
    abcd_y_lt <- swap_src_subjectkey(abcd_y_lt)
    ph_p_otbi <- swap_src_subjectkey(ph_p_otbi)
    mtbi_count_df <- ph_p_otbi |>
        detail_mtbi(
            abcd_y_lt,
            subjects,
            t = t
        ) |>
        dplyr::select(
            "subjectkey",
            "mtbi_count"
        )
    if (!is.null(cutoff)) {
        mtbi_count_df <- mtbi_count_df |>
            dplyr::mutate(
                mtbi_count = dplyr::case_when(
                    mtbi_count > cutoff ~ cutoff,
                    TRUE ~ mtbi_count
                )
            )
    }
    return(mtbi_count_df)
}

#' Extract LOC of latest mTBI
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param ph_p_otbi TBI data frame
#' @param abcd_y_lt Data frame containing age information
#' @return A data frame containing latest_mtbi_loc
#' @export
get_mtbi_loc <- function(ph_p_otbi, abcd_y_lt, subjects = NULL, t = NULL) {
    abcd_y_lt <- swap_src_subjectkey(abcd_y_lt)
    ph_p_otbi <- swap_src_subjectkey(ph_p_otbi)
    mtbi_loc <- detail_mtbi(
        ph_p_otbi,
        abcd_y_lt,
        subjects = subjects,
        t = t
    ) |>
        dplyr::select(
            "subjectkey",
            "latest_mtbi_loc"
        )
    return(mtbi_loc)
}

#' Extract mechanism of latest mTBI
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param ph_p_otbi TBI data frame
#' @param abcd_y_lt Data frame containing age information
#' @param format Variable indicating if df is dummied or undummied
#' @return A data frame containing latest_mtbi_mechanism
#' @export
get_mtbi_mechanism <- function(ph_p_otbi,
                               abcd_y_lt,
                               subjects = NULL,
                               t = NULL,
                               format = "undummied") {
    abcd_y_lt <- swap_src_subjectkey(abcd_y_lt)
    ph_p_otbi <- swap_src_subjectkey(ph_p_otbi)
    mtbi_mechanism <- detail_mtbi(
        ph_p_otbi,
        abcd_y_lt,
        subjects = subjects,
        t = t
    ) |>
        dplyr::select(
            "subjectkey",
            "latest_mtbi_mechanism"
        )
    if (format != "dummied" && format != "undummied") {
        print("Fomat must either be 'dummied' or 'undummied'.")
        return(NULL)
    } else if (format == "dummied") {
        mtbi_mechanism <- fastDummies::dummy_cols(
            .data = mtbi_mechanism,
            select_columns = "latest_mtbi_mechanism",
            remove_selected_columns = TRUE
        )
    }
    return(mtbi_mechanism)
}

#' Extract memory loss / dazed status of latest mTBI
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param ph_p_otbi ABCD table containing TBI information.
#' @param abcd_y_lt ABCD table containing age information.
#' @return A data frame containing latest_mtbi_mem_daze.
#' @export
get_mtbi_mem_daze <- function(ph_p_otbi,
                              abcd_y_lt,
                              subjects = NULL,
                              t = NULL) {
    abcd_y_lt <- swap_src_subjectkey(abcd_y_lt)
    ph_p_otbi <- swap_src_subjectkey(ph_p_otbi)
    mtbi_mem_daze <- detail_mtbi(
        ph_p_otbi,
        abcd_y_lt,
        subjects = subjects,
        t = t
    ) |>
        dplyr::select(
            "subjectkey",
            "latest_mtbi_mem_daze"
        )
    return(mtbi_mem_daze)
}

#' Extract NIH toolbox cardsort data
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param abcd_tbss01 NDA nihtbx data frame
#' @return A data frame containing nihtbx cardsort data.
#' @export
get_nihtbx_cardsort_fc <- function(abcd_tbss01,
                                   subjects = NULL,
                                   t = NULL) {
    abcd_tbss01 <- swap_src_subjectkey(abcd_tbss01)
    nihtbx_full <- abcd_tbss01 |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    nihtbx_cardsort_fc <- nihtbx_full |>
        dplyr::select(
            "subjectkey",
            "nihtbx_cardsort_fc"
        )
    return(nihtbx_cardsort_fc)
}

#' Extract NIH toolbox list sorting data
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param abcd_tbss01 NDA nihtbx data frame
#' @return A data frame containing nihtbx list sorting data.
#' @export
get_nihtbx_list_fc <- function(abcd_tbss01,
                               subjects = NULL,
                               t = NULL) {
    abcd_tbss01 <- swap_src_subjectkey(abcd_tbss01)
    nihtbx_full <- abcd_tbss01 |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    nihtbx_list_fc <- nihtbx_full |>
        dplyr::select(
            "subjectkey",
            "nihtbx_list_fc"
        )
    return(nihtbx_list_fc)
}


#' Extract parent psychopathology
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param mh_p_asr ABCD Parent Adult Self Report Scores Aseba
#' @param raw Boolean indicating if extracted data should be raw (TRUE) or
#'  t-scores (FALSE). Defaults to TRUE.
#' @return A data frame containing parent psychopathology data.
#' @export
get_parent_psychopathology <- function(mh_p_asr,
                                       subjects = NULL,
                                       t = NULL,
                                       raw = TRUE) {
    mh_p_asr <- swap_src_subjectkey(mh_p_asr)
    if (raw == TRUE) {
        parent_psychopathology <- mh_p_asr |>
            filter_timepoint(t = t) |>
            filter_subjects(subjects = subjects) |>
            dplyr::select("subjectkey", dplyr::ends_with("r")) |>
            dplyr::select(
                -c(
                    "asr_scr_totprob_r",
                    "asr_scr_internal_r",
                    "asr_scr_external_r",
                    "asr_scr_inattention_r", # same as attention
                    "asr_scr_adhd_r", # same as attention
                    "asr_scr_rulebreak_r", # same as antisocial
                    "asr_scr_somaticpr_r" # same as same as somatic
                )
            )
    } else {
        parent_psychopathology <- mh_p_asr |>
            filter_timepoint(t = t) |>
            filter_subjects(subjects = subjects) |>
            dplyr::select("subjectkey", dplyr::ends_with("t")) |>
            dplyr::select(
                -c(
                    "asr_scr_totprob_t",
                    "asr_scr_internal_t",
                    "asr_scr_external_t",
                    "asr_scr_inattention_t", # same as attention
                    "asr_scr_adhd_t", # same as attention
                    "asr_scr_rulebreak_t", # same as antisocial
                    "asr_scr_somaticpr_t" # same as same as somatic
                )
            )
    }
    return(parent_psychopathology)
}

#' Parent report of prosocial behaviour
#'
#' This function returns parent responses to the ABCD Parent Prosocial Behavior
#' Survey. The values are stored in the ABCD table `ce_p_psb.txt` under the
#' names `prosocial_q1_p`, `prosocial_q2_p`, and `prosocial_q3_p`.'
#' The function also renames the variables as follows:
#' - "prosocial_q1_p" -> "considerate_p"
#' - "prosocial_q2_p" -> "helps_hurt_p"
#' - "prosocial_q3_p" -> "helpful_p"
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param ce_p_psb Parent Prosocial Behavior Survey.
#' @param no_zero Boolean indicating if zero values should be replaced with 1.
#' @return A data frame containing 3 parent responses concerning prosocial
#'  behaviour in their children.
#' @export
get_prosocial_behaviour_p <- function(ce_p_psb,
                                      subjects = NULL,
                                      t = NULL,
                                      no_zero = FALSE) {
    ce_p_psb <- swap_src_subjectkey(ce_p_psb)
    prosocial <- ce_p_psb |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    prosocial <- numcol_to_numeric(prosocial)
    prosocial <- prosocial |>
        dplyr::rename(
            "considerate_p" = "prosocial_q1_p",
            "helps_hurt_p" = "prosocial_q2_p",
            "helpful_p" = "prosocial_q3_p"
        ) |>
        dplyr::select(
            "subjectkey",
            "considerate_p",
            "helps_hurt_p",
            "helpful_p"
        )
    if (no_zero) {
        prosocial[prosocial == 0] <- 1
    }
    return(prosocial)
}

#' Youth report of prosocial behaviour
#'
#' This function returns youth responses to the ABCD Parent Prosocial Behavior
#' Survey. The values are stored in the ABCD table `ce_y_psb.txt` under the
#' names `prosocial_q1_y`, `prosocial_q2_y`, and `prosocial_q3_y`.
#' The function also renames the variables as follows:
#' - "prosocial_q1_y" -> "considerate_y"
#' - "prosocial_q2_y" -> "helps_hurt_y"
#' - "prosocial_q3_y" -> "helpful_y"
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param ce_y_psb Youth Prosocial Behavior Survey.
#' @param no_zero Boolean indicating if zero values should be replaced with 1
#' @return A data frame containing 3 youth responses concerning prosocial
#'  behaviour.
#' @export
get_prosocial_behaviour_y <- function(ce_y_psb,
                                      subjects = NULL,
                                      t = NULL,
                                      no_zero = FALSE) {
    ce_y_psb <- swap_src_subjectkey(ce_y_psb)
    prosocial <- ce_y_psb |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    prosocial <- numcol_to_numeric(prosocial)
    prosocial <- prosocial |>
        dplyr::rename(
            "considerate_y" = "prosocial_q1_y",
            "helps_hurt_y" = "prosocial_q2_y",
            "helpful_y" = "prosocial_q3_y"
        ) |>
        dplyr::select(
            "subjectkey",
            "considerate_y",
            "helps_hurt_y",
            "helpful_y"
        )
    if (no_zero) {
        prosocial[prosocial == 0] <- 1
    }
    return(prosocial)
}

#' Parent report of pubertal status
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param ph_p_pds Data frame containing youth pubertal status report
#' @param max_value Maximum value for pubertal status
#' @return A data frame containing average pubertal status
#' @export
get_pubertal_status_p <- function(ph_p_pds,
                                  subjects = NULL,
                                  t = NULL,
                                  max_value = NULL) {
    ph_p_pds <- swap_src_subjectkey(ph_p_pds)
    pubertal_status_df <- ph_p_pds |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    pubertal_status_df$pubertal_status_p <- rowMeans(
        pubertal_status_df[,
            c(
                "pds_p_ss_female_category_2",
                "pds_p_ss_male_category_2"
            )
        ],
        na.rm = TRUE
    )
    if (!is.null(max_value)) {
        pubertal_status_df <- pubertal_status_df |>
            dplyr::mutate(
                pubertal_status_p = dplyr::case_when(
                    pubertal_status_p > max_value ~ max_value,
                    TRUE ~ pubertal_status_p
                )
            )
    }
    pubertal_status_df <- pubertal_status_df |>
        dplyr::select(
            "subjectkey",
            "pubertal_status_p"
        )
    return(pubertal_status_df)
}

#' Youth report of pubertal status
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param ph_y_pds Data frame containing youth pubertal status report
#' @param max_value Maximum value for pubertal status
#' @return A data frame containing average pubertal status
#' @export
get_pubertal_status_y <- function(ph_y_pds,
                                  subjects = NULL,
                                  t = NULL,
                                  max_value = NULL) {
    ph_y_pds <- swap_src_subjectkey(ph_y_pds)
    pubertal_status_df <- ph_y_pds |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    # Composite pubertal status by averaging parent and youth reports
    pubertal_status_df$pubertal_status_y <- rowMeans(
        pubertal_status_df[,
            c(
                "pds_y_ss_female_category_2",
                "pds_y_ss_male_cat_2"
            )
        ],
        na.rm = TRUE
    )
    if (!is.null(max_value)) {
        pubertal_status_df <- pubertal_status_df |>
            dplyr::mutate(
                pubertal_status_y = dplyr::case_when(
                    pubertal_status_y > max_value ~ max_value,
                    TRUE ~ pubertal_status_y
                )
            )
    }
    pubertal_status_df <- pubertal_status_df |>
        dplyr::select(
            "subjectkey",
            "pubertal_status_y"
        )
    return(pubertal_status_df)
}

#' Returns race information
#'
#' Returns race data categorized by the ABCD Study based on the NIH Minimum
#' Reporting guidelines and the US Census Bureau's OMB standards.
#' If dummy = FALSE, five categorical variables for different race groups
#' are provided. Otherwise, a single categorical variable is returned.
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param abcd_p_demo Data frame containing parent demographic information
#' @param dummy String indicating format to output race data
#' @param asian_as_other If TRUE, Asian category is included in "other".
#' @return A data frame containing subject race
#' @export
get_race <- function(abcd_p_demo,
                     subjects = NULL,
                     t = NULL,
                     dummy = FALSE,
                     asian_as_other = FALSE) {
    abcd_p_demo <- swap_src_subjectkey(abcd_p_demo)
    race_df <- abcd_p_demo |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects) |>
        dplyr::select(
            "subjectkey",
            "race_ethnicity",
        )
    colnames(race_df) <- c("subjectkey", "race")
    if (dummy) {
        if (asian_as_other) {
            race_df <- race_df |>
                dplyr::mutate(
                    white = dplyr::case_when(
                        is.na(race_df$"race") ~ NA_real_,
                        race_df$"race" == 1 ~ 1,
                        TRUE ~ 0
                    ),
                    black = dplyr::case_when(
                        is.na(race_df$"race") ~ NA_real_,
                        race_df$"race" == 2 ~ 1,
                        TRUE ~ 0
                    ),
                    hispanic = dplyr::case_when(
                        is.na(race_df$"race") ~ NA_real_,
                        race_df$"race" == 3 ~ 1,
                        TRUE ~ 0
                    ),
                    other = dplyr::case_when(
                        is.na(race_df$"race") ~ NA_real_,
                        race_df$"race" == 4 ~ 1,
                        race_df$"race" == 5 ~ 1,
                        TRUE ~ 0
                    ),
                )
            race_df <- race_df |>
                dplyr::select(
                    "subjectkey",
                    "white",
                    "black",
                    "hispanic",
                    "other"
                )
        } else {
            race_df <- race_df |>
                dplyr::mutate(
                    white = dplyr::case_when(
                        is.na(race_df$"race") ~ NA_real_,
                        race_df$"race" == 1 ~ 1,
                        TRUE ~ 0
                    ),
                    black = dplyr::case_when(
                        is.na(race_df$"race") ~ NA_real_,
                        race_df$"race" == 2 ~ 1,
                        TRUE ~ 0
                    ),
                    hispanic = dplyr::case_when(
                        is.na(race_df$"race") ~ NA_real_,
                        race_df$"race" == 3 ~ 1,
                        TRUE ~ 0
                    ),
                    asian = dplyr::case_when(
                        is.na(race_df$"race") ~ NA_real_,
                        race_df$"race" == 4 ~ 1,
                        TRUE ~ 0
                    ),
                    other = dplyr::case_when(
                        is.na(race_df$"race") ~ NA_real_,
                        race_df$"race" == 5 ~ 1,
                        TRUE ~ 0
                    ),
                )
            race_df <- race_df |>
                dplyr::select(
                    "subjectkey",
                    "white",
                    "black",
                    "hispanic",
                    "asian",
                    "other"
                )
        }
    } else {
        if (asian_as_other) {
            race_df <- race_df |>
                dplyr::mutate(
                    race = dplyr::case_when(
                        race_df$"race" == 1 ~ "white",
                        race_df$"race" == 2 ~ "black",
                        race_df$"race" == 3 ~ "hispanic",
                        race_df$"race" == 4 ~ "other",
                        race_df$"race" == 5 ~ "other",
                        TRUE ~ NA_character_
                    )
                )
        } else {
            race_df <- race_df |>
                dplyr::mutate(
                    race = dplyr::case_when(
                        race_df$"race" == 1 ~ "white",
                        race_df$"race" == 2 ~ "black",
                        race_df$"race" == 3 ~ "hispanic",
                        race_df$"race" == 4 ~ "asian",
                        race_df$"race" == 5 ~ "other",
                        TRUE ~ NA_character_
                    )
                )
        }
    }
    return(race_df)
}

#' Extract MRI scanner serial number
#'
#' Extracts variable `mri_info_deviceserialnumber` from table `mri_y_adm_info`
#' as `scanner_id`.
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @inheritParams get_site_id
#' @param mri_y_adm_info Data frame storing MRI administrative information.
#' @param sample_missing If TRUE, populates missing scanner IDs based on the
#'  scanners present at each observation's site of data collection. Sampling
#'  is weighted according to prevalence of that scanner among all ABCD subjects
#'  with scanner ID values from that site.
#' @return A data frame containing subjectkey and scanner ID.
#' @export
get_scanner_id <- function(mri_y_adm_info,
                           subjects = NULL,
                           t = NULL,
                           abcd_y_lt = NULL,
                           sample_missing = FALSE) {
    mri_y_adm_info <- swap_src_subjectkey(mri_y_adm_info)
    scanner_df <- mri_y_adm_info |>
        dplyr::rename(
            "scanner_id" = "mri_info_deviceserialnumber"
        ) |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects) |>
        dplyr::select("subjectkey", "scanner_id")
    if (sample_missing) {
        if (is.null(abcd_y_lt)) {
            abcdutils_error(
                "`abcd_y_lt` argument must be provided to sample missing",
                " scanner IDs."
            )
        }
        assigned_scanners <- assign_site_scanner(
            mri_y_adm_info,
            abcd_y_lt,
            subjects = subjects,
            t = t
        )
        cat(
            "Sampled scanner ID for ",
            nrow(assigned_scanners),
            " subjects.\n",
            sep = ""
        )
        scanner_df <- rbind(
            stats::na.omit(scanner_df),
            dplyr::select(assigned_scanners, "subjectkey", "scanner_id")
        )
        row.names(scanner_df) <- NULL
        # Ensure no duplicate subjects
        if (nrow(scanner_df) != length(unique(scanner_df$"subjectkey"))) {
            abcdutils_error(
                "Duplicate subjects occurred during scanner sampling. Please",
                " file a GitHub issue describing how to reproduce this error."
            )
        }
    }
    return(scanner_df)
}

#' Extract healthy behaviours: screen time questionnaire
#'
#' This function returns the screen time data from the ABCD Parent Screen Time
#' Survey. The values are stored in the ABCD table `nt_p_stq.txt` under the
#' names `screentime1_p_hours`, `screentime1_p_minutes`, `screentime2_p_hours`,
#' `screentime2_p_minutes`, `screentime_1_wknd_hrs_p`,
#' `screentime_1_wknd_min_p`, `screentime_1_wkdy_hrs_p`, and
#' `screentime_1_wkdy_min_p`. The final measures are reported in hours.
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param nt_p_stq ABCD Parent Screen Time Survey.
#' @return A data frame containing screen time data.
#' @export
get_screen_time <- function(nt_p_stq,
                            subjects = NULL,
                            t = NULL) {
    nt_p_stq <- swap_src_subjectkey(nt_p_stq)
    weekend_hours <- ""
    weekday_hours <- ""
    weekend_mins <- ""
    weekday_mins <- ""
    screen_time <- nt_p_stq |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    if (isTRUE(t > 0)) {
        screen_time <- screen_time |>
            dplyr::rename(
                "weekend_hours" = "screentime_1_wknd_hrs_p",
                "weekend_mins" = "screentime_1_wknd_min_p",
                "weekday_hours" = "screentime_1_wkdy_hrs_p",
                "weekday_mins" = "screentime_1_wkdy_min_p"
            )
    } else {
        screen_time <- screen_time |>
            dplyr::rename(
                "weekend_hours" = "screentime1_p_hours",
                "weekend_mins" = "screentime1_p_minutes",
                "weekday_hours" = "screentime2_p_hours",
                "weekday_mins" = "screentime2_p_minutes"
            )
    }
    screen_time <- screen_time |> 
        dplyr::select(
            "subjectkey",
            "weekend_hours",
            "weekend_mins",
            "weekday_hours",
            "weekday_mins"
        )
    # Convert columns to numeric
    screen_time <- numcol_to_numeric(screen_time)
    # Convert to hours
    screen_time <- screen_time |>
        dplyr::mutate(
            screentime_wknd_hrs = weekend_hours + (weekend_mins / 60),
            screentime_wkday_hrs = weekday_hours + (weekday_mins / 60),
        ) |>
        dplyr::select(
            "subjectkey", "screentime_wknd_hrs", "screentime_wkday_hrs"
        )
    return(screen_time)
}

#' return A data frame containing sex of specified subjects
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param gish_p_gi Parent report of sex and gender data frame
#' @param format String indicating format to output sex data:
#'     * `"dummied"` single binary column `M`. This is the default.
#'     * `"undummied"` single column `sex` containing factor values `M` and `F`.
#' @param only_m_f Logical indicating whether to remove non M/F values
#' @param as_numeric Logical indicating whether to convert M/F to 1/0 values
#' @return A data frame containing sex
#' @export
get_sex <- function(gish_p_gi,
                    subjects = NULL,
                    t = NULL,
                    format = "dummied",
                    only_m_f = FALSE,
                    as_numeric = FALSE) {
    gish_p_gi <- swap_src_subjectkey(gish_p_gi)
    options <- c("undummied", "dummied")
    if (!(format %in% options)) {
        print("The 'format argument should be one of the following options:")
        print("[1] 'dummied'")
        print("[2] 'undummied'")
        print("See ?get_sex for more information about these options.")
        return(NULL)
    }
    pgi <- gish_p_gi |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects) |>
        dplyr::select(
            "subjectkey",
            "demo_sex_v2", # what is their assigned sex
        )
    sex <- pgi |>
        dplyr::rename(
            "sex" = "demo_sex_v2",
        )
    sex <- sex |>
        dplyr::mutate(
            sex = dplyr::case_when(
                sex == 1 ~ "M",
                sex == 2 ~ "F",
                sex == 3 ~ "IM",
                sex == 4 ~ "IF",
                TRUE ~ NA
            )
        )
    if (only_m_f) {
        sex <- sex |>
            dplyr::mutate(
                sex = dplyr::case_when(
                    sex %in% c("IM", "IF") ~ NA,
                    TRUE ~ sex
                )
            )
    }
    if (as_numeric) {
        sex <- sex |>
            dplyr::mutate(
                sex = dplyr::case_when(
                    sex == "F" ~ 0,
                    sex == "M" ~ 1,
                    sex == "IM" ~ 2,
                    sex == "IF" ~ 3,
                    TRUE ~ NA
                )
            )
    }
    return(sex)
}

#' Extract site ID
#'
#' The extracted `site_id` variable is equal to the `site_id_l` variable in the
#' ABCD table `abcd_y_lt`.
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param abcd_y_lt Data frame storing site information.
#' @return A data frame containing subjectkey and site ID.
#' @export
get_site_id <- function(abcd_y_lt, subjects = NULL, t = NULL) {
    abcd_y_lt <- swap_src_subjectkey(abcd_y_lt)
    family_df <- abcd_y_lt |>
        dplyr::rename(
            "site_id" = "site_id_l"
        ) |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects) |>
        dplyr::select("subjectkey", "site_id")
    return(family_df)
}

#' General factor for Sleep Disturbance from the sleep disturbance scale.
#'
#' This function returns a total measure for sleep disturbance from
#'  the Sleep Disturbance Scale as defined in Bruni et al., 1996 The measure
#'  is stored in the ABCD table `ph_p_sds.txt` under the variable name 
#'  `sds_p_ss_total`.
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param ph_p_sds ABCD table containing sleep disturbance scale data.
#' @return A data frame containing sleep data.
#' @export
get_sleep_disturbance <- function(ph_p_sds,
                                  subjects = NULL,
                                  t = NULL) {
    ph_p_sds <- swap_src_subjectkey(ph_p_sds)
    sds_df <- ph_p_sds |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    sds_total <- sds_df |>
        dplyr::select(
            "subjectkey",
            "sds_p_ss_total"
        )
    # Convert to numeric
    sds_total$"sds_p_ss_total" <- as.numeric(sds_df$"sds_p_ss_total")
    return(sds_total)
}

#' Extract healthy behaviours: spots and activities questionnaire
#'
#' This function returns the sports and activities data from the ABCD Parent
#' Sports and Activities Involvement Questionnaire. The values are stored in
#' the ABCD table `ph_p_saiq.txt` under several column names ending with 
#' `_school`, `_school_l`, `_outside`, `_outside_l`, `_private`, `_private_l`,
#' `_p12`, and `_nmonth_p_l`. The final measures are reported as the number of
#' school activities, extracurricular activities, activities receiving private
#' instruction, and activities participated in the last year. These variables
#' can be discretized into quartiles relative to the entire sample at the
#' specified timepoint.
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param ph_p_saiq ABCD Parent Sports and Activities Involvement
#'  Questionnaire
#' @param discretize Boolean indicating if measures should be discretized into
#'  quartiles
#' @return A data frame containing sports and activities data.
#' @export
get_sports_and_activities <- function(ph_p_saiq,
                                      subjects = NULL,
                                      t = NULL,
                                      discretize = FALSE) {
    ph_p_saiq <- swap_src_subjectkey(ph_p_saiq)
    saiq <- ph_p_saiq |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    saiq <- numcol_to_numeric(saiq)
    ###########################################################################
    # Unusual specific variable selection:
    # For whatever reason, the lacross variable uses 11 to code yes and 0 to
    # code no.
    saiq$"sai_p_lax_school"[saiq$"sai_p_lax_school" == 11] <- 1
    saiq$"sai_p_lax_school_l"[saiq$"sai_p_lax_school_l" == 11] <- 1
    ###########################################################################
    # Number of activities organized inside of school
    school_idx <- endsWith(colnames(saiq), "school")
    school_l_idx <- endsWith(colnames(saiq), "school_l")
    school_all_idx <- school_idx | school_l_idx
    school_cols <- colnames(saiq)[school_all_idx]
    saiq$"school_activities" <- rowSums(
        saiq[, school_cols],
        na.rm = TRUE
    )
    ###########################################################################
    # Number of activities organized outside of school
    outside_idx <- endsWith(colnames(saiq), "outside")
    outside_l_idx <- endsWith(colnames(saiq), "outside_l")
    outside_all_idx <- outside_idx | outside_l_idx
    outside_cols <- colnames(saiq)[outside_all_idx]
    saiq$"extracurriculars" <- rowSums(
        saiq[, outside_cols],
        na.rm = TRUE
    )
    ###########################################################################
    # Number of activities receiving private instruction
    private_idx <- endsWith(colnames(saiq), "private")
    private_l_idx <- endsWith(colnames(saiq), "private_l")
    private_all_idx <- private_idx | private_l_idx
    private_cols <- colnames(saiq)[private_all_idx]
    saiq$"private_instruction" <- rowSums(
        saiq[, private_cols],
        na.rm = TRUE
    )
    ###########################################################################
    # Number of activities participated in the last year
    if (t == 0) {
        p12_idx <- endsWith(colnames(saiq), "p12")
        p12_all_idx <- p12_idx
        p12_cols <- colnames(saiq)[p12_all_idx]
        saiq$"activities_last_year" <- rowSums(
            saiq[, p12_cols],
            na.rm = TRUE
        )
    } else {
        p12_idx <- endsWith(colnames(saiq), "nmonth_p_l")
        p12_all_idx <- p12_idx
        p12_cols <- colnames(saiq)[p12_all_idx]
        saiq$"activities_last_year" <- rowSums(
            saiq[, p12_cols],
            na.rm = TRUE
        )
    }
    ###########################################################################
    # Select columns
    saiq <- dplyr::select(
        saiq,
        "subjectkey",
        "school_activities",
        "extracurriculars",
        "private_instruction",
        "activities_last_year"
    )
    if (discretize) {
        # Based on the quantiles of these measures on all baseline subjects.
        # This can be re-obtained by running `get_saiq` at bl without
        # specifying any subjects.
        disc_df <- get_sports_and_activities(
            ph_p_saiq,
            t = t,
            discretize = FALSE
        )
        school_qs <- stats::quantile(
            disc_df$"school_activities",
            na.rm = TRUE,
            probs = c(0.25, 0.5, 0.75)
        )
        extra_qs <- stats::quantile(
            disc_df$"extracurriculars",
            na.rm = TRUE,
            probs = c(0.25, 0.5, 0.75)
        )
        private_qs <- stats::quantile(
            disc_df$"private_instruction",
            na.rm = TRUE,
            probs = c(0.25, 0.5, 0.75)
        )
        ly_qs <- stats::quantile(
            disc_df$"activities_last_year",
            na.rm = TRUE,
            probs = c(0.25, 0.5, 0.75)
        )
        saiq <- saiq |>
            dplyr::mutate(
                school_activities = dplyr::case_when(
                    school_activities <= school_qs[1] ~ 1,
                    school_activities <= school_qs[2] ~ 2,
                    school_activities <= school_qs[3] ~ 3,
                    school_activities > school_qs[3] ~ 4,
                    TRUE ~ NA
                ),
                extracurriculars = dplyr::case_when(
                    extracurriculars <= extra_qs[1] ~ 1,
                    extracurriculars <= extra_qs[2] ~ 2,
                    extracurriculars <= extra_qs[3] ~ 3,
                    extracurriculars > extra_qs[3] ~ 4,
                    TRUE ~ NA
                ),
                private_instruction = dplyr::case_when(
                    private_instruction <= private_qs[1] ~ 1,
                    private_instruction <= private_qs[2] ~ 2,
                    private_instruction <= private_qs[3] ~ 3,
                    private_instruction > private_qs[3] ~ 4,
                    TRUE ~ NA
                ),
                activities_last_year = dplyr::case_when(
                    activities_last_year <= ly_qs[1] ~ 1,
                    activities_last_year <= ly_qs[2] ~ 2,
                    activities_last_year <= ly_qs[3] ~ 3,
                    activities_last_year > ly_qs[3] ~ 4,
                    TRUE ~ NA
                )
            )
    }
    return(saiq)
}

#' Extract subcortical network correlations
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param mri_y_rsfmr_cor_gp_aseg Data file containing neurite density data
#' @param roi_pairs List where each item in the list is a 2-item vector of ROIs
#' to select a correlation variable for. Possible ROIs are:
#' Network ROIs:
#'     - auditory
#'     - cingulo_opercular
#'     - cingulo_parietal
#'     - default
#'     - dorsal_attention
#'     - fronto_parietal
#'     - none
#'     - retrosplenial_temporal
#'     - salience
#'     - sensorimotor_hand
#'     - sensorimotor_mouth
#'     - ventral_attention
#'     - visual
#' Subcortical structure ROIs:
#'     - left_accumbens_area
#'     - right_accumbens_area
#'     - left_amygdala
#'     - right_amygdala
#'     - brain_stem
#'     - left_caudate
#'     - right_caudate
#'     - left_cerebellum_cortex
#'     - right_cerebellum_cortex
#'     - left_hippocampus
#'     - right_hippocampus
#'     - left_pallidum
#'     - right_pallidum
#'     - left_putamen
#'     - right_putamen
#'     - left_thalamus_proper
#'     - right_thalamus_proper
#'     - left_ventraldc
#'     - right_ventraldc
#' @return A data frame of white matter neurite densities
#' @export
get_subc_cor <- function(mri_y_rsfmr_cor_gp_aseg,
                         subjects = NULL,
                         t = NULL,
                         roi_pairs) {
    mri_y_rsfmr_cor_gp_aseg <- swap_src_subjectkey(mri_y_rsfmr_cor_gp_aseg)
    subc_cor <- mri_y_rsfmr_cor_gp_aseg |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
        rois <- c(
            "auditory" = "au",
            "cingulo_opercular" = "cerc",
            "cingulo_parietal" = "copa",
            "default" = "df",
            "dorsal_attention" = "dsa",
            "fronto_parietal" = "fopa",
            "none" = "none",
            "retrosplenial_temporal" = "rst",
            "salience" = "sa",
            "sensorimotor_hand" = "smh",
            "sensorimotor_mouth" = "smm",
            "ventral_attention" = "vta",
            "visual" = "vs",
            "left_accumbens_area" = "aalh",
            "right_accumbens_area" = "aarh",
            "left_amygdala" = "aglh",
            "right_amygdala" = "agrh",
            "brain_stem" = "bs",
            "left_caudate" = "cdelh",
            "right_caudate" = "cderh",
            "left_cerebellum_cortex" = "crcxlh",
            "right_cerebellum_cortex" = "crcxrh",
            "left_hippocampus" = "hplh",
            "right_hippocampus" = "hprh",
            "left_pallidum" = "pllh",
            "right_pallidum" = "plrh",
            "left_putamen" = "ptlh",
            "right_putamen" = "ptrh",
            "left_thalamus_proper" = "thplh",
            "right_thalamus_proper" = "thprh",
            "left_ventraldc" = "vtdclh",
            "right_ventraldc" = "vtdcrh"
        )
    keep_vars <- lapply(
        roi_pairs,
        function(x) {
            paste0("rsfmri_cor_ngd_", rois[[x[[1]]]], "_scs_", rois[[x[[2]]]])
        }
    ) |> unlist()
    subc_cor <- subc_cor |>
        dplyr::select(
            "subjectkey",
            dplyr::all_of(keep_vars)
        )
    return(subc_cor)
}

#' Extract subcortical volumes
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param mri_y_smr_vol_aseg Data file containing subcortical data
#' @param only_whole_brain If TRUE, collects only whole brain volume. Else,
#' collects all fts. Whole brain volume is extracted as the column
#' "smri_vol_scs_wholeb".
#' @return A data frame containing subcortical volumes.
#' @export
get_subc_v <- function(mri_y_smr_vol_aseg,
                       subjects = NULL,
                       t = NULL,
                       only_whole_brain = TRUE) {
    mri_y_smr_vol_aseg <- swap_src_subjectkey(mri_y_smr_vol_aseg)
    smri_raw <- mri_y_smr_vol_aseg |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    if (only_whole_brain) {
        subc_v_df <- smri_raw |>
            dplyr::select("subjectkey", "smri_vol_scs_wholeb")
    } else {
        subc_v_df <- smri_raw |>
            dplyr::select(-"eventname")
    }
    return(subc_v_df)
}

#' Extract cortical temporal variances
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param mrirstv02 Data file containing neurite density data
#' @return A data frame of white matter neurite densities
#' @export
get_subc_var <- function(mrirstv02,
                         subjects = NULL,
                         t = NULL) {
    mrirstv02 <- swap_src_subjectkey(mrirstv02)
    subc_var_raw <- mrirstv02 |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    subc_var <- subc_var_raw |>
        dplyr::select(
            "subjectkey",
            dplyr::contains("rsfmri_var_scs_")
        ) |>
        dplyr::select(
            -c("rsfmri_var_scs_wmhypin",
               "rsfmri_var_scs_wmhypinrh",
               "rsfmri_var_scs_wmhypinlh",
               "rsfmri_var_scs_lesionlh",
               "rsfmri_var_scs_lesionrh")
        )
    return(subc_var)
}

#' Extract white matter neurite densities
#'
#' Extract neurite densities for major white matter tracts (AtlasTrack ROIs) as
#'  well as peri-cortical/sub-adjacent white matter structures defined relative
#'  to the Desikan Cortical Parcellation.
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param mri_y_rsi_rnd_at Data file containing neurite density data
#' @param mri_y_rsi_rnd_wm_dsk Data file containing neurite density data
#' @param only_mean Logical indicating whether to return only the mean cortical
#'  thickness or all cortical thicknesses. Defaults to TRUE.
#' @return A data frame of white matter neurite densities
#' @export
get_wmnd <- function(mri_y_rsi_rnd_at,
                     mri_y_rsi_rnd_wm_dsk,
                     subjects = NULL,
                     t = NULL,
                     only_mean = TRUE) {
    mri_y_rsi_rnd_at <- swap_src_subjectkey(mri_y_rsi_rnd_at)
    mri_y_rsi_rnd_wm_dsk <- swap_src_subjectkey(mri_y_rsi_rnd_wm_dsk)
    if (only_mean) {
        mri_y_rsi_rnd_at <- mri_y_rsi_rnd_at |>
            filter_timepoint(t = t) |>
            filter_subjects(subjects = subjects) |>
            dplyr::select("subjectkey", "dmri_rsirnd_fib_allfib")
        mri_y_rsi_rnd_wm_dsk <- mri_y_rsi_rnd_wm_dsk |>
            filter_timepoint(t = t) |>
            filter_subjects(subjects = subjects) |>
            dplyr::select("subjectkey", "dmri_rsirndwm_cdk_mean")
    } else {
        mri_y_rsi_rnd_at <- mri_y_rsi_rnd_at |>
            filter_timepoint(t = t) |>
            filter_subjects(subjects = subjects) |>
            dplyr::select("subjectkey", dplyr::contains("dmri_rsirnd_fib_"))
        mri_y_rsi_rnd_wm_dsk <- mri_y_rsi_rnd_wm_dsk |>
            filter_timepoint(t = t) |>
            filter_subjects(subjects = subjects) |>
            dplyr::select("subjectkey", dplyr::contains("dmri_rsirndwm_cdk_"))
    }
    df_list <- list(mri_y_rsi_rnd_at, mri_y_rsi_rnd_wm_dsk)
    wmnd_df <- merge_df_list(df_list, join = "full")
    return(wmnd_df)
}
