#' General factor for sleep disturbance scale
#'
#' Factor loadings based on Mancini et al., 2019
#'
#' @param ph_p_sds Dataframe containing sleep disturbance scale data.
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return full_sleep_df Dataframe containing sleep data
#'
#' @export
get_sds_total_probs <- function(ph_p_sds, subjects = NULL, t = NULL) {
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

#' Extract family function
#'
#' @param ce_y_fes ABCD Parent Family Environment Scale-Family Conflict Subscale
#' Modified from PhenX
#' @param ce_p_fes ABCD Parent Family Environment Scale-Family Conflict
#' Subscale Modified from PhenX
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return family_function
#'
#' @export
get_family_function <- function(ce_y_fes, ce_p_fes, subjects = NULL, t = NULL) {
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

#' Parent report of prosocial behaviour
#'
#' @param ce_p_psb Parent Prosocial Behavior Survey
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#' @param no_zero Boolean indicating if zero values should be replaced with 1
#'
#' @return prosocial_behaviour
#'
#' @export
get_prosocial_behaviour_p <- function(ce_p_psb,
                                      subjects = NULL,
                                      no_zero = FALSE,
                                      t = NULL) {
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
#' @param ce_y_psb Parent Prosocial Behavior Survey
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#' @param no_zero Boolean indicating if zero values should be replaced with 1
#'
#' @return prosocial_behaviour
#'
#' @export
get_prosocial_behaviour_y <- function(ce_y_psb,
                                      subjects = NULL,
                                      no_zero = FALSE,
                                      t = NULL) {
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

#' Extract number of friends
#'
#' @param mh_y_or ABCD Other Resilience
#' @param gish_p_gi Parent report of sex and gender dataframe
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#' @param discretize Boolean indicating if the loneliness measures should be
#' discretized into quartiles
#'
#' @export
get_friends <- function(mh_y_or,
                        gish_p_gi,
                        subjects = NULL,
                        discretize = FALSE,
                        t = NULL) {
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

#' Extract healthy behaviours: screen time questionnaire
#'
#' @param nt_p_stq ABCD Parent Screen Time Survey
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return screen_time
#'
#' @export
get_screen_time <- function(nt_p_stq, subjects = NULL, t = NULL) {
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

#' Extract healthy behaviours: spots and activities questionnaire
#'
#' @param ph_p_saiq ABCD Parent Sports and Activities Involvement
#' Questionnaire
#'
#' @param subjects Vector of subjectkeys.
#'
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @param discretize Boolean indicating if measures should be discretized into
#' quartiles
#'
#' @return activities
#'
#' @export
get_sports_and_activities <- function(ph_p_saiq,
                                      subjects = NULL,
                                      t = NULL,
                                      discretize = FALSE) {
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

#' Extract healthy behaviours: exercise questionnaire
#'
#' @param ph_y_yrb ABCD Youth Youth Risk Behavior Survey Exercise Physical
#' Activity
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return exercise
#'
#' @export
get_exercise <- function(ph_y_yrb, subjects = NULL, t = NULL) {
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

#' Extract parent psychopathology
#'
#' @param mh_p_asr ABCD Parent Adult Self Report Scores Aseba
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#' @param raw Boolean indicating if extracted data should be raw (TRUE) or
#'  t-scores (FALSE). Defaults to TRUE.
#'
#' @return parent_psychopathology
#'
#' @export
get_parent_psychopathology <- function(mh_p_asr,
                                       subjects = NULL,
                                       t = NULL,
                                       raw = TRUE) {
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

#' Get nihtbx list sorting data
#'
#' @param abcd_tbss01 NDA nihtbx dataframe
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return nihtbx_list_fc list sorting data
#'
#' @export
get_nihtbx_list_fc <- function(abcd_tbss01, subjects = NULL, t = NULL) {
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

#' Get nihtbx cardsort data
#'
#' @param abcd_tbss01 NDA nihtbx dataframe
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return nihtbx_cardsort_fc cardsort data
#'
#' @export
get_nihtbx_cardsort_fc <- function(abcd_tbss01, subjects = NULL, t = NULL) {
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

#' Extract subcortical volumes
#'
#' @param mri_y_smr_vol_aseg Data file containing subcortical data
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return subc_v_df Dataframe of subcortical volumes
#'
#' @export
get_subc_v <- function(mri_y_smr_vol_aseg, subjects = NULL, t = NULL) {
    smri_raw <- mri_y_smr_vol_aseg |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    subc_v_df <- smri_raw |>
        dplyr::select("subjectkey", "smri_vol_scs_wholeb")
    return(subc_v_df)
}

#' Extract cortical thicknesses
#'
#' @param mri_y_smr_thk_dsk Data file containing cortical data
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return cort_t_df Dataframe of cortical thicknesses
#'
#' @export
get_cort_t <- function(mri_y_smr_thk_dsk, subjects = NULL, t = NULL) {
    cort_raw <- mri_y_smr_thk_dsk |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    cort_t_df <- cort_raw |>
        dplyr::select(
            "subjectkey",
            "smri_thick_cdk_mean"
        )
    return(cort_t_df)
}

#' Extract cortical surface areas
#'
#' @param mri_y_smr_area_dsk Data file containing cortical data
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return cort_sa_df Dataframe of cortical surface areas
#'
#' @export
get_cort_sa <- function(mri_y_smr_area_dsk, subjects = NULL, t = NULL) {
    cort_raw <- mri_y_smr_area_dsk |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    cort_sa_df <- cort_raw |>
        dplyr::select(
            "subjectkey",
            "smri_area_cdk_total"
        )
    return(cort_sa_df)
}

#' Extract white matter neurite densities
#'
#' Extract neurite densities for major white matter tracts (AtlasTrack ROIs) as
#'  well as peri-cortical/sub-adjacent white matter structures defined relative
#'  to the Desikan Cortical Parcellation.
#'
#' @param mri_y_rsi_rnd_at Data file containing neurite density data
#' @param mri_y_rsi_rnd_wm_dsk Data file containing neurite density data
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return wmnd_df Dataframe of white matter neurite densities
#'
#' @export
get_all_wmnd <- function(mri_y_rsi_rnd_at,
                         mri_y_rsi_rnd_wm_dsk,
                         subjects = NULL,
                         t = NULL) {
    mri_y_rsi_rnd_at <- mri_y_rsi_rnd_at |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects) |>
        dplyr::select("subjectkey", "dmri_rsirnd_fib_allfib")
    mri_y_rsi_rnd_wm_dsk <- mri_y_rsi_rnd_wm_dsk |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects) |>
        dplyr::select("subjectkey", "dmri_rsirndwm_cdk_mean")
    df_list <- list(mri_y_rsi_rnd_at, mri_y_rsi_rnd_wm_dsk)
    wmnd_df <- merge_df_list(df_list, join = "full")
    return(wmnd_df)
}

#' Extract cortical network correlations
#'
#' @param mri_y_rsfmr_cor_gp_gp Data file containing neurite density data
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return gord_cor Dataframe of white matter neurite densities
#'
#' @export
get_gord_cor <- function(mri_y_rsfmr_cor_gp_gp, subjects = NULL, t = NULL) {
    gord_cor <- mri_y_rsfmr_cor_gp_gp |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    # Store the subjectkeys in the rownames
    row.names(gord_cor) <- gord_cor$"subjectkey"
    # Remove the subjectkeys
    gord_cor <- gord_cor |>
        dplyr::select(-c("subjectkey", "eventname"))
    # Dataframe of just rowmeans
    gord_cor <- data.frame(rowMeans(gord_cor))
    gord_cor$"subjectkey" <- rownames(gord_cor)
    colnames(gord_cor) <- c("avg_gord_cor", "subjectkey")
    rownames(gord_cor) <- NULL
    gord_cor <- gord_cor |> dplyr::select(
        "subjectkey",
        "avg_gord_cor"
    )
    return(gord_cor)
}

#' Extract subcortical network correlations
#'
#' @param mri_y_rsfmr_cor_gp_aseg Data file containing neurite density data
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return subc_cor Dataframe of white matter neurite densities
#'
#' @export
get_subc_cor <- function(mri_y_rsfmr_cor_gp_aseg, subjects = NULL, t = NULL) {
    subc_cor <- mri_y_rsfmr_cor_gp_aseg |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    # Store the subjectkeys in the rownames
    row.names(subc_cor) <- subc_cor$"subjectkey"
    # Remove the subjectkeys
    subc_cor <- subc_cor |>
        dplyr::select(-c("subjectkey", "eventname"))
    # Dataframe of just rowmeans
    subc_cor <- data.frame(rowMeans(subc_cor))
    subc_cor$"subjectkey" <- rownames(subc_cor)
    colnames(subc_cor) <- c("avg_subc_cor", "subjectkey")
    rownames(subc_cor) <- NULL
    subc_cor <- subc_cor |> dplyr::select(
        "subjectkey",
        "avg_subc_cor"
    )
    return(subc_cor)
}

#' Extract cortical temporal variances
#'
#' @param mri_y_rsfmr_var_gp Data file containing neurite density data
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return subc_cor Dataframe of white matter neurite densities
#'
#' @export
get_gord_var <- function(mri_y_rsfmr_var_gp, subjects = NULL, t = NULL) {
    gord_var_raw <- mri_y_rsfmr_var_gp |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    gord_var <- gord_var_raw |>
        dplyr::select(
            "subjectkey",
            dplyr::contains("rsfmri_var_cortgordon_")
        )
    return(gord_var)
}

#' Extract cortical temporal variances
#'
#' @param mrirstv02 Data file containing neurite density data
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return subc_cor Dataframe of white matter neurite densities
#'
#' @export
get_subc_var <- function(mrirstv02, subjects = NULL, t = NULL) {
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

#' Get number of mtbis sustained by subject
#'
#' @param ph_p_otbi TBI dataframe
#' @param abcd_y_lt Dataframe containing age information
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#' @param cutoff Maximum number of mtbis to be reported
#'
#' @return mtbi_count Dataframe containing number of previous mTBIs
#'
#' @export
get_mtbi_count <- function(ph_p_otbi,
                           abcd_y_lt,
                           subjects = NULL,
                           cutoff = NULL,
                           t = NULL) {
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

#' Get subject headache history
#'
#' @param ph_p_mhx Dataframe containing medical history
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return headaches Dataframe containing headache history
#'
#' @export
get_headaches <- function(ph_p_mhx, subjects = NULL, t = NULL) {
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

#' Youth report of pubertal status
#'
#' @param ph_y_pds Dataframe containing youth pubertal status report
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#' @param max_value Maximum value for pubertal status
#'
#' @return pubertal_status Dataframe containing average pubertal status
#'
#' @export
get_pubertal_status_y <- function(ph_y_pds,
                                  subjects = NULL,
                                  max_value = NULL,
                                  t = NULL) {
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

#' Parent report of pubertal status
#'
#' @param ph_p_pds Dataframe containing youth pubertal status report
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#' @param max_value Maximum value for pubertal status
#'
#' @return pubertal_status Dataframe containing average pubertal status
#'
#' @export
get_pubertal_status_p <- function(ph_p_pds,
                                  subjects = NULL,
                                  max_value = NULL,
                                  t = NULL) {
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

#' Returns combined household incomes split into low, medium, and high groups
#'
#' Low: $0 - $50k, Medium: $50k - $100k, High: > $100k
#'
#' @param abcd_p_demo Dataframe containing parent demographic information
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return income_df Dataframe containing household incomes
#'
#' @export
get_income <- function(abcd_p_demo, subjects = NULL, t = NULL) {
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

#' Returns race information
#'
#' Returns race data categorized by the ABCD Study based on the NIH Minimum
#' Reporting guidelines and the US Census Bureau's OMB standards.
#' If dummy = FALSE, five categorical variables for different race groups
#' are provided. Otherwise, a single categorical variable is returned.
#'
#' @param abcd_p_demo Dataframe containing parent demographic information
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#' @param dummy String indicating format to output race data
#' @param asian_as_other If TRUE, Asian category is included in "other".
#'
#' @return race_df Dataframe containing subject race
#'
#' @export
get_race <- function(abcd_p_demo,
                     subjects = NULL,
                     t = t,
                     dummy = FALSE,
                     asian_as_other = FALSE) {
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

#' Print summary of subjects by race
#'
#' @param race_df Dataframe generated by get_race function
#' @param format Variable indicating if df is dummied or undummied
#'
#' @return race_table Summary table of subjects by race
#'
#' @export
format_race <- function(race_df, format) {
    options <- c("undummied", "dummied")
    if (!(format %in% options)) {
        print("The 'format argument should be one of the following options:")
        print("[1] 'dummied'")
        print("[2] 'undummied'")
        print("See ?format_race for more information about these options.")
        return(NULL)
    }
    if (format == "undummied") {
        race_table <- dplyr::count(race_df, race_df$"race")
    }
    if (format == "dummied") {
        race_table <- colSums(race_df |> dplyr::select(-"subjectkey"))
    }
    return(race_table) # TO-DO
}

#' Return dataframe containing interview age of specified subjects
#'
#' @param abcd_df Any ABCD dataframe containing interview age
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#' @param as_years Logical indicating whether to convert age to years
#'
#' @return interview_age Dataframe containing interview age
#'
#' @export
get_interview_age <- function(abcd_df,
                              subjects = NULL,
                              t = NULL,
                              as_years = TRUE) {
    interview_age <- abcd_df |>
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

#' Return dataframe containing sex of specified subjects
#'
#' @param gish_p_gi Parent report of sex and gender dataframe
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#' @param format
#'     String indicating format to output sex data.
#'
#'     * `"dummied"` single binary column `M`. This is the default.
#'     * `"undummied"` single column `sex` containing factor values `M` and `F`.
#'
#' @param only_m_f Logical indicating whether to remove non M/F values
#' @param as_numeric Logical indicating whether to convert M/F to 1/0 values
#' @return sex Dataframe containing sex
#'
#' @export
get_sex <- function(gish_p_gi,
                    subjects = NULL,
                    t = t,
                    format = "dummied",
                    only_m_f = FALSE,
                    as_numeric = FALSE) {
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

#' Return dataframe containing parent-reported genders
#'
#' Collected parent reported gender information from the `demo_gender_id_v2`
#' column of the `gish_p_gi` table.
#'
#' @param gish_p_gi Parent report of sex and gender dataframe
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#' @param match_y_report If TRUE, codes outcomes to match youth report options
#' at follow up timepoints (male, female, non-binary, NA). If FALSE, uses
#' response options provided to parents (male, female, trans male, trans
#' female, gender queer, and NA). In both cases, "refuse to answer" and
#' "don't know" are pooled with the NA option.
#'
#' @export
get_p_gender <- function(gish_p_gi,
                         subjects = NULL,
                         t = t,
                         match_y_report = FALSE) {
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

#' Return dataframe containing youth-reported genders
#'
#' @param gish_y_gi Youth-reported gender dataframe
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return sex Dataframe containing sex
#'
#' @export
get_y_gender <- function(gish_y_gi, subjects = NULL, t = t) {
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

#' Get acute symptom input variable 'latest_mtbi_age'
#'
#' @param ph_p_otbi TBI dataframe
#' @param abcd_y_lt Dataframe containing age information
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#' @param as_years Logical indicating whether to convert age to years
#'
#' @return mtbi_age Dataframe containing latest_mtbi_age
#'
#' @export
get_mtbi_age <- function(ph_p_otbi,
                         abcd_y_lt,
                         subjects = NULL,
                         t = NULL,
                         as_years = TRUE) {
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

#' Get age data for subjects who exclusively had a latest mtbi post bl
#'
#' @param abcd_lpohstbi01 longitudinal tbi dataframe
#' @param l_subs subjects who had an mTBI exclusively in a longitudinal tp
#' @param y1_subs subjects who had an mTBI at year 1
#' @param y2_subs subjects who had an mTBI at year 2
#'
#' @return l_df latest mTBI age data for l subjects
#'
#' @export
get_mtbi_age_l <- function(abcd_lpohstbi01, l_subs, y1_subs, y2_subs) {
    # Collect data for 1yfu and 2yfu
    age_l_1 <- get_mtbi_age(abcd_lpohstbi01, l_subs, t = 1)
    age_l_2 <- get_mtbi_age(abcd_lpohstbi01, l_subs, t = 2)
    colnames(age_l_1) <- c("subjectkey", "latest_mtbi_age_1")
    colnames(age_l_2) <- c("subjectkey", "latest_mtbi_age_2")
    # Determine which children need data from 2yfu and which need from 1yfu
    l_df <- l_subs
    l_df$"collect_2" <- l_df$"subjectkey" %in% y2_subs$"subjectkey"
    l_df <- merge_df_list(
        list(
            l_df,
            age_l_1,
            age_l_2
        ),
        join = "full"
    )
    l_df <- l_df |>
        dplyr::mutate(
            "latest_mtbi_age" = dplyr::case_when(
                l_df$"collect_2" == TRUE ~ l_df$"latest_mtbi_age_2",
                l_df$"collect_2" == FALSE ~ l_df$"latest_mtbi_age_1",
                TRUE ~ NA
            )
        ) |>
        dplyr::select("subjectkey", "latest_mtbi_age")
    return(l_df)
}

#' Extract CBCL syndrome scale data
#'
#' @param mh_p_cbcl Dataframe containing ABCD CBCL data
#' @param raw Boolean indicating if extracted data should be raw (TRUE) or
#'  t-scores (FALSE). Defaults to TRUE.
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return ss_data Dataframe containing syndrome scale data
#'
#' @export
get_cbcl_syndrome_scale <- function(mh_p_cbcl,
                                    raw = TRUE,
                                    t = NULL,
                                    subjects = NULL) {
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
    # Initial cleaning and filtering of the dataframe
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

#' Get CBCL headache data
#'
#' @param mh_p_cbcl NDA cbcl dataframe
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return cbcl_headaches headache data
#'
#' @export
get_cbcl_headaches <- function(mh_p_cbcl, subjects = NULL, t = NULL) {
    cbcl_full <- mh_p_cbcl |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    cbcl_headaches <- cbcl_full |>
        dplyr::select(
            "subjectkey",
            "cbcl_q56b_p"
        ) |>
        dplyr::rename("cbcl_headaches" = "cbcl_q56b_p")
    return(cbcl_headaches)
}

#' Get CBCL nausea data
#'
#' @param mh_p_cbcl NDA cbcl dataframe
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return cbcl_nausea nausea data
#'
#' @export
get_cbcl_nausea <- function(mh_p_cbcl, subjects = NULL, t = NULL) {
    cbcl_full <- mh_p_cbcl |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    cbcl_nausea <- cbcl_full |>
        dplyr::select(
            "subjectkey",
            "cbcl_q56c_p"
        ) |>
        dplyr::rename("cbcl_nausea" = "cbcl_q56c_p")
    return(cbcl_nausea)
}

#' Get CBCL vomiting data
#'
#' @param mh_p_cbcl NDA cbcl dataframe
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return cbcl_vomiting vomiting data
#'
#' @export
get_cbcl_vomiting <- function(mh_p_cbcl, subjects = NULL, t = NULL) {
    cbcl_full <- mh_p_cbcl |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    cbcl_vomiting <- cbcl_full |>
        dplyr::select(
            "subjectkey",
            "cbcl_q56g_p"
        ) |>
        dplyr::rename("cbcl_vomiting" = "cbcl_q56g_p")
    return(cbcl_vomiting)
}

#' Get CBCL dizzy data
#'
#' @param mh_p_cbcl NDA cbcl dataframe
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return cbcl_dizzy dizzy data
#'
#' @export
get_cbcl_dizzy <- function(mh_p_cbcl, subjects = NULL, t = NULL) {
    cbcl_full <- mh_p_cbcl |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    cbcl_dizzy <- cbcl_full |>
        dplyr::select(
            "subjectkey",
            "cbcl_q51_p"
        ) |>
        dplyr::rename("cbcl_dizzy" = "cbcl_q51_p")
    return(cbcl_dizzy)
}

#' Get CBCL overtired data
#'
#' @param mh_p_cbcl NDA cbcl dataframe
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return cbcl_overtired overtired data
#'
#' @export
get_cbcl_overtired <- function(mh_p_cbcl, subjects = NULL, t = NULL) {
    cbcl_full <- mh_p_cbcl |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    cbcl_overtired <- cbcl_full |>
        dplyr::select(
            "subjectkey",
            "cbcl_q54_p"
        ) |>
        dplyr::rename("cbcl_overtired" = "cbcl_q54_p")
    return(cbcl_overtired)
}

#' Get CBCL sleeping_more data
#'
#' @param mh_p_cbcl NDA cbcl dataframe
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return cbcl_sleeping_more sleeping_more data
#'
#' @export
get_cbcl_sleeping_more <- function(mh_p_cbcl, subjects = NULL, t = NULL) {
    cbcl_full <- mh_p_cbcl |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    cbcl_sleeping_more <- cbcl_full |>
        dplyr::select(
            "subjectkey",
            "cbcl_q77_p"
        ) |>
        dplyr::rename("cbcl_sleeping_more" = "cbcl_q77_p")
    return(cbcl_sleeping_more)
}

#' Get CBCL sleeping_less data
#'
#' @param mh_p_cbcl NDA cbcl dataframe
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return cbcl_sleeping_less sleeping_less data
#'
#' @export
get_cbcl_sleeping_less <- function(mh_p_cbcl, subjects = NULL, t = NULL) {
    cbcl_full <- mh_p_cbcl |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    cbcl_sleeping_less <- cbcl_full |>
        dplyr::select(
            "subjectkey",
            "cbcl_q76_p"
        ) |>
        dplyr::rename("cbcl_sleeping_less" = "cbcl_q76_p")
    return(cbcl_sleeping_less)
}

#' Get acute symptom input variable 'latest_mtbi_mechanism'
#'
#' @param ph_p_otbi TBI dataframe
#' @param abcd_y_lt Dataframe containing age information
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#' @param format Variable indicating if df is dummied or undummied
#'
#' @return mtbi_mechanism Dataframe containing latest_mtbi_mechanism
#'
#' @export
get_mtbi_mechanism <- function(ph_p_otbi,
                               abcd_y_lt,
                               subjects = NULL,
                               t = NULL,
                               format = "undummied") {
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

#' Get acute symptom input variable 'latest_mtbi_loc'
#'
#' @param ph_p_otbi TBI dataframe
#' @param abcd_y_lt Dataframe containing age information
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return mtbi_loc Dataframe containing latest_mtbi_loc
#'
#' @export
get_mtbi_loc <- function(ph_p_otbi, abcd_y_lt, subjects = NULL, t = NULL) {
    mtbi_loc <-
        detail_mtbi(
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

#' Get LOC data for subjects who exclusively had a latest injury in follow up
#'
#' @param ph_p_otbi TBI dataframe
#' @param abcd_y_lt Dataframe containing age information
#' @param l_subs subjects who had an mTBI exclusively in a longitudinal tp
#' @param y1_subs subjects who had an mTBI at year 1
#' @param y2_subs subjects who had an mTBI at year 2
#'
#' @return l_df latest mTBI LOC data for l subjects
#'
#' @export
get_mtbi_loc_l <- function(ph_p_otbi, abcd_y_lt, l_subs, y1_subs, y2_subs) {
    # Collect data for 1yfu and 2yfu
    loc_l_1 <- get_mtbi_loc(ph_p_otbi, abcd_y_lt, l_subs, t = 1)
    loc_l_2 <- get_mtbi_loc(ph_p_otbi, abcd_y_lt, l_subs, t = 2)
    colnames(loc_l_1) <- c("subjectkey", "latest_mtbi_loc_1")
    colnames(loc_l_2) <- c("subjectkey", "latest_mtbi_loc_2")
    # Determine which children need data from 2yfu and which need from 1yfu
    l_df <- l_subs
    l_df$"collect_2" <- l_df$"subjectkey" %in% y2_subs$"subjectkey"
    l_df <- merge_df_list(
        list(
            l_df,
            loc_l_1,
            loc_l_2
        ),
        join = "full"
    )
    l_df <- l_df |>
        dplyr::mutate(
            "latest_mtbi_loc" = dplyr::case_when(
                l_df$"collect_2" == TRUE ~ l_df$"latest_mtbi_loc_2",
                l_df$"collect_2" == FALSE ~ l_df$"latest_mtbi_loc_1",
                TRUE ~ NA
            )
        ) |>
        dplyr::select("subjectkey", "latest_mtbi_loc")
    return(l_df)
}

#' Get acute symptom input variable 'latest_mtbi_mem_daze'
#'
#' @param ph_p_otbi TBI dataframe
#' @param abcd_y_lt Dataframe containing age information
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return mtbi_mem_daze Dataframe containing latest_mtbi_mem_daze
#'
#' @export
get_mtbi_mem_daze <- function(ph_p_otbi, abcd_y_lt, subjects = NULL, t = NULL) {
    mtbi_mem_daze <-
        detail_mtbi(
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

#' Get mem daze data for subjects who exclusively had a latest mtbi post bl
#'
#' @param ph_p_otbi TBI dataframe
#' @param abcd_y_lt Dataframe containing age information
#' @param l_subs subjects who had an mTBI exclusively in a longitudinal tp
#' @param y1_subs subjects who had an mTBI at year 1
#' @param y2_subs subjects who had an mTBI at year 2
#' @param keep_collect_cols boolean on whether or not where each var came from
#'  should be stored
#'
#' @return l_df latest mTBI mem daze data for l subjects
#'
#' @export
get_mtbi_mem_daze_l <- function(ph_p_otbi, abcd_y_lt, l_subs, y1_subs, y2_subs,
                                keep_collect_cols = FALSE) {
    # Collect data for 1yfu and 2yfu
    mem_daze_l_1 <- get_mtbi_mem_daze(ph_p_otbi, abcd_y_lt, l_subs, t = 1)
    mem_daze_l_2 <- get_mtbi_mem_daze(ph_p_otbi, abcd_y_lt, l_subs, t = 2)
    colnames(mem_daze_l_1) <- c("subjectkey", "latest_mtbi_mem_daze_1")
    colnames(mem_daze_l_2) <- c("subjectkey", "latest_mtbi_mem_daze_2")
    # Determine which children need data from 2yfu and which need from 1yfu
    l_df <- l_subs
    l_df$"collect_2" <- l_df$"subjectkey" %in% y2_subs$"subjectkey"
    l_df <- merge_df_list(
        list(
            l_df,
            mem_daze_l_1,
            mem_daze_l_2
        ),
        join = "full"
    )
    l_df <- l_df |>
        dplyr::mutate(
            "latest_mtbi_mem_daze" = dplyr::case_when(
                l_df$"collect_2" == TRUE ~ l_df$"latest_mtbi_mem_daze_2",
                l_df$"collect_2" == FALSE ~ l_df$"latest_mtbi_mem_daze_1",
                TRUE ~ NA
            )
        )
    if (keep_collect_cols == FALSE) {
        l_df <- l_df |> dplyr::select("subjectkey", "latest_mtbi_mem_daze")
    } else {
        l_df <- l_df |>
            dplyr::mutate(
                collect_time = dplyr::case_when(
                    l_df$"collect_2" == FALSE ~ 1,
                    l_df$"collect_2" == TRUE ~ 2
                )
            ) |>
            dplyr::select("subjectkey", "latest_mtbi_mem_daze", "collect_time")
    }
    return(l_df)
}

#' Get mechanism data for subjects who exclusively had a latest mtbi post bl
#'
#' @param ph_p_otbi TBI dataframe
#' @param abcd_y_lt Dataframe containing age information
#' @param l_subs subjects who had an mTBI exclusively in a longitudinal tp
#' @param y1_subs subjects who had an mTBI at year 1
#' @param y2_subs subjects who had an mTBI at year 2
#'
#' @return l_df latest mTBI mechanism data for l subjects
#'
#' @export
get_mtbi_mechanism_l <- function(ph_p_otbi,
                                 abcd_y_lt,
                                 l_subs,
                                 y1_subs,
                                 y2_subs) {
    # Collect data for 1yfu and 2yfu
    mechanism_l_1 <- get_mtbi_mechanism(ph_p_otbi, abcd_y_lt, l_subs, t = 1)
    mechanism_l_2 <- get_mtbi_mechanism(ph_p_otbi, abcd_y_lt, l_subs, t = 2)
    colnames(mechanism_l_1) <- c("subjectkey", "latest_mtbi_mechanism_1")
    colnames(mechanism_l_2) <- c("subjectkey", "latest_mtbi_mechanism_2")
    # Determine which children need data from 2yfu and which need from 1yfu
    l_df <- l_subs
    l_df$"collect_2" <- l_df$"subjectkey" %in% y2_subs$"subjectkey"
    l_df <- merge_df_list(
        list(
            l_df,
            mechanism_l_1,
            mechanism_l_2
        ),
        join = "full"
    )
    l_df <- l_df |>
        dplyr::mutate(
            "latest_mtbi_mechanism" = dplyr::case_when(
                l_df$"collect_2" == TRUE ~ l_df$"latest_mtbi_mechanism_2",
                l_df$"collect_2" == FALSE ~ l_df$"latest_mtbi_mechanism_1",
                TRUE ~ NA
            )
        ) |>
        dplyr::select("subjectkey", "latest_mtbi_mechanism")
    return(l_df)
}
