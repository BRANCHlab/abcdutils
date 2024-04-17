#' Rename non-descriptive columns in ABCD's TBI file.
#'
#' Renames column names of ABCD's Ohio TBI Screen data to meaningful names
#' that are easier to work with.
#' Use 'original_otbi_names()' to print out a conversion table of old and new
#'  column names.
#'
#' @param tbi_df The complete file abcd_otbi01.txt
#'
#' @return renamed_tbi A modified form of tbi_df with clearer column names
#' @export
rename_tbi <- function(tbi_df) {
    if (!is.data.frame(tbi_df)) {
        rlang::abort("Object is not a dataframe.", class = "non_df")
    }
    renamed_tbi <- tbi_df |>
        col_collapse("tbi_1", "tbi_1_l", "hosp_er_inj") |>
        col_collapse("tbi_1b", "tbi_1b_l", "hosp_er_loc") |>
        col_collapse("tbi_1c", "tbi_1c_l", "hosp_er_mem_daze") |>
        col_collapse("tbi_1d", "tbi_1d_l", "hosp_er_age") |>
        col_collapse("tbi_2", "tbi_2_l", "vehicle_inj") |>
        col_collapse("tbi_2b", "tbi_2b_l", "vehicle_loc") |>
        col_collapse("tbi_2c", "tbi_2c_l", "vehicle_mem_daze") |>
        col_collapse("tbi_2d", "tbi_2d_l", "vehicle_age") |>
        col_collapse("tbi_3", "tbi_3_l", "fall_hit_inj") |>
        col_collapse("tbi_3b", "tbi_3b_l", "fall_hit_loc") |>
        col_collapse("tbi_3c", "tbi_3c_l", "fall_hit_mem_daze") |>
        col_collapse("tbi_3d", "tbi_3d_l", "fall_hit_age") |>
        col_collapse("tbi_4", "tbi_4_l", "violent_inj") |>
        col_collapse("tbi_4b", "tbi_4b_l", "violent_loc") |>
        col_collapse("tbi_4c", "tbi_4c_l", "violent_mem_daze") |>
        col_collapse("tbi_4d", "tbi_4d_l", "violent_age") |>
        col_collapse("tbi_5", "tbi_5_l", "blast_inj") |>
        col_collapse("tbi_5b", "tbi_5b_l", "blast_loc") |>
        col_collapse("tbi_5c", "tbi_5c_l", "blast_mem_daze") |>
        col_collapse("tbi_5d", "tbi_5d_l", "blast_age") |>
        col_collapse("tbi_6o", "tbi_6o_l", "other_loc_inj") |>
        col_collapse("tbi_6p", "tbi_6p_l", "other_loc_num") |>
        col_collapse("tbi_6q", "tbi_6q_l", "other_loc_max_loc_mins") |>
        col_collapse("tbi_6r", "tbi_6r_l", "other_loc_num_over_30") |>
        col_collapse("tbi_6s", "tbi_6s_l", "other_loc_min_age") |>
        col_collapse("tbi_7a", "tbi_7a_l", "multi_inj") |>
        col_collapse("tbi_7c1", "tbi_7c1_l", "multi_loc") |>
        col_collapse("tbl_7c2", "tbl_7c2_l", "multi_mem_daze") |>
        col_collapse("tbi_7e", "tbi_7e_l", "multi_effect_start_age") |>
        col_collapse("tbi_7f", "tbi_7f_l", "multi_effect_end_age") |>
        dplyr::rename_with(
            ~ dplyr::case_when(
                grepl("tbi_7g", .) ~ "other_multi_inj",
                grepl("tbi_7i", .) ~ "other_multi_effect_type",
                grepl("tbi_7k", .) ~ "other_multi_effect_start_age",
                grepl("tbi_7l", .) ~ "other_multi_effect_end_age",
                . == "tbi_8" ~ "num_sport_concussions",
                . == "tbi_8a" ~ "school_missed_sport_concussion",
                . == "tbi_8b" ~ "school_missed_worst_sport_concussion",
                grepl("tbi_8g", .) ~ "other_other_multi_inj",
                grepl("tbi_8i", .) ~ "other_other_multi_effect_type",
                grepl("tbi_8k", .) ~ "other_other_multi_effect_start_age",
                grepl("tbi_8l", .) ~ "other_other_multi_effect_end_age",
                TRUE ~ .
            )
        )
    if (identical(renamed_tbi, tbi_df)) {
        rlang::warn("No changes were made to the object.", class = "no_effect")
    }
    return(renamed_tbi)
}

#' Return conversion table of original and new otbi names
#'
#' @export
original_tbi_names <- function() {
    df <- data.frame(
        new_name = c(
            "hosp_er_inj",
            "hosp_er_loc",
            "hosp_er_mem_daze",
            "hosp_er_age",
            "vehicle_inj",
            "vehicle_loc",
            "vehicle_mem_daze",
            "vehicle_age",
            "fall_hit_inj",
            "fall_hit_loc",
            "fall_hit_mem_daze",
            "fall_hit_age",
            "violent_inj",
            "violent_loc",
            "violent_mem_daze",
            "violent_age",
            "blast_inj",
            "blast_loc",
            "blast_mem_daze",
            "blast_age",
            "other_loc_inj",
            "other_loc_num",
            "other_loc_max_loc_mins",
            "other_loc_num_over_30",
            "other_loc_min_age",
            "multi_inj",
            "multi_loc",
            "multi_mem_daze",
            "multi_effect_start_age",
            "multi_effect_end_age",
            "other_multi_inj",
            "other_multi_effect_type",
            "other_multi_effect_start_age",
            "other_multi_effect_end_age",
            "num_sport_concussions",
            "school_missed_sport_concussion",
            "school_missed_worst_sport_concussion",
            "other_other_multi_inj",
            "other_other_multi_effect_type",
            "other_other_multi_effect_start_age",
            "other_other_multi_effect_end_age"
        ),
        old_name = c(
            "tbi_1",
            "tbi_1b",
            "tbi_1c",
            "tbi_1d",
            "tbi_2",
            "tbi_2b",
            "tbi_2c",
            "tbi_2d",
            "tbi_3",
            "tbi_3b",
            "tbi_3c",
            "tbi_3d",
            "tbi_4",
            "tbi_4b",
            "tbi_4c",
            "tbi_4d",
            "tbi_5",
            "tbi_5b",
            "tbi_5c",
            "tbi_5d",
            "tbi_6o",
            "tbi_6p",
            "tbi_6q",
            "tbi_6r",
            "tbi_6s",
            "tbi_7a",
            "tbi_7c1",
            "tbl_7c2",
            "tbi_7e",
            "tbi_7f",
            "tbi_7g",
            "tbi_7i",
            "tbi_7k",
            "tbi_7l",
            "tbi_8",
            "tbi_8a",
            "tbi_8b",
            "tbi_8g",
            "tbi_8i",
            "tbi_8k",
            "tbi_8l"
        ),
        description = c(
            "ever hospitalized/ER for head/neck injury?",
            "if LOC, how long?",
            "were they dazed or have memory gap?",
            "how old were they?",
            "ever injured in a vehicle accident?",
            "if LOC, how long?",
            "were they dazed or have memory gap?",
            "how old were they?",
            "ever injured head/neck from fall or hit?",
            "if LOC, how long?",
            "were they dazed or have memory gap?",
            "how old were they?",
            "ever injure head/neck from violence?",
            "if LOC, how long?",
            "were they dazed or have memory gap?",
            "how old were they?",
            "ever injure head or neck from blast?",
            "if LOC, how long?",
            "were they dazed or have memory gap?",
            "how old were they?",
            "any other injuries with LOC?",
            "how many more?",
            "how long was longest LOC?",
            "how many were >= 30 min?",
            "what was their youngest age?",
            "did they have a period of multiple injuries?",
            "if LOC, how long?",
            "were they dazed or have memory gap?",
            "at what age did the effects begin?",
            "at what age did the effects end?",
            "was there another multiple injury period?",
            "typical effect of the injury?",
            "start age of those effects?",
            "end age of those effects?",
            "how many sport/activity-related concussions?*",
            "days of school missed due to worst concussion (if multiple)?",
            "days of school missed due to concussion?",
            "another period of multiple inj?",
            "typical effects?",
            "start age of effects?",
            "end age of effects?"
        )
    )
    options(width = 110)
    print(df)
    options(width = 80)
}

#' Adds columns to tbi_df showing if a child has had an mTBI or moderate+ TBI
#'
#' Calculates whether or not child had an mTBI using the following definition:
#' (0 mins < LOC < 30 mins) OR (LOC < 30 mins AND felt dazed or confused)
#'
#' Calculates moderate or severe TBIs using the following definition:
#' (LOC > 30 mins)
#'
#' These definitions aim to match typical GCS-based definitions as well as
#' possible.
#'
#' @param tbi_df A TBI dataframe
#'
#' @return tbi_df A modified form of tbi_df added tbi columns
#'
#' @export
identify_all_tbi <- function(tbi_df) {
    # Assign column types
    tbi_df <- numcol_to_numeric(tbi_df)
    # Generate mtbi and moderate_or_severe_tbi columns
    tbi_df <- tbi_df |> dplyr::mutate(
        mtbi = dplyr::case_when(
            (tbi_df$"hosp_er_loc" < 2 & tbi_df$"hosp_er_mem_daze" == 1) |
                tbi_df$"hosp_er_loc" == 1 ~ 1,
            (tbi_df$"vehicle_loc" < 2 & tbi_df$"vehicle_mem_daze" == 1) |
                tbi_df$"vehicle_loc" == 1 ~ 1,
            (tbi_df$"fall_hit_loc" < 2 & tbi_df$"fall_hit_mem_daze" == 1) |
                tbi_df$"fall_hit_loc" == 1 ~ 1,
            (tbi_df$"violent_loc" < 2 & tbi_df$"violent_mem_daze" == 1) |
                tbi_df$"violent_loc" == 1 ~ 1,
            (tbi_df$"blast_loc" < 2 & tbi_df$"blast_mem_daze" == 1) |
                tbi_df$"blast_loc" == 1 ~ 1,
            (tbi_df$"other_loc_num" - tbi_df$"other_loc_num_over_30") > 0 ~ 1,
            (tbi_df$"multi_loc" < 2 & tbi_df$"multi_mem_daze" == 1) |
                tbi_df$"multi_loc" == 1 ~ 1,
            flex_cond(tbi_df$"num_sport_concussions" > 0) ~ 1,
            flex_cond(tbi_df$"other_multi_inj" == 1) ~ 0.5,
            flex_cond(tbi_df$"other_other_multi_inj" == 1) ~ 0.5,
            TRUE ~ 0
        ),
        moderate_or_severe_tbi = dplyr::case_when(
            (
                tbi_df$"hosp_er_loc" > 1 |
                    tbi_df$"vehicle_loc" > 1 |
                    tbi_df$"fall_hit_loc" > 1 |
                    tbi_df$"violent_loc" > 1 |
                    tbi_df$"blast_loc" > 1 |
                    tbi_df$"other_loc_num_over_30" > 0 |
                    tbi_df$"multi_loc" > 1
            ) ~ 1,
            flex_cond(tbi_df$"other_multi_inj" == 1) ~ 0.5,
            flex_cond(tbi_df$"other_other_multi_inj" == 1) ~ 0.5,
            TRUE ~ 0
        )
    )
    return(tbi_df)
}

#' Generate columns indicating which injury types were mTBIs
#'
#' @param tbi_df A TBI dataframe
#'
#' @return df_mtbi The modified dataframe
#'
#' @export
identify_mtbi <- function(tbi_df) {
    df_mtbi <- tbi_df |> dplyr::mutate(
        hosp_er_mtbi = dplyr::case_when(
            (tbi_df$"hosp_er_loc" < 2 & tbi_df$"hosp_er_mem_daze" == 1) |
                tbi_df$"hosp_er_loc" == 1 ~ 1,
            TRUE ~ 0
        ),
        vehicle_mtbi = dplyr::case_when(
            (tbi_df$"vehicle_loc" < 2 & tbi_df$"vehicle_mem_daze" == 1) |
                tbi_df$"vehicle_loc" == 1 ~ 1,
            TRUE ~ 0
        ),
        fall_hit_mtbi = dplyr::case_when(
            (tbi_df$"fall_hit_loc" < 2 & tbi_df$"fall_hit_mem_daze" == 1) |
                tbi_df$"fall_hit_loc" == 1 ~ 1,
            TRUE ~ 0
        ),
        violent_mtbi = dplyr::case_when(
            (tbi_df$"violent_loc" < 2 & tbi_df$"violent_mem_daze" == 1) |
                tbi_df$"violent_loc" == 1 ~ 1,
            TRUE ~ 0
        ),
        blast_mtbi = dplyr::case_when(
            (tbi_df$"blast_loc" < 2 & tbi_df$"blast_mem_daze" == 1) |
                blast_loc == 1 ~ 1,
            TRUE ~ 0
        ),
        other_loc_mtbi_num = dplyr::case_when(
            (tbi_df$"other_loc_num" - tbi_df$"other_loc_num_over_30") > 0 ~
                tbi_df$"other_loc_num" - tbi_df$"other_loc_num_over_30",
            TRUE ~ 0
        ),
        other_loc_mtbi = dplyr::case_when(
            tbi_df$"other_loc_num" - tbi_df$"other_loc_num_over_30" > 0 ~ 1,
            TRUE ~ 0
        ),
        multi_mtbi = dplyr::case_when(
            (tbi_df$"multi_loc" < 2 & tbi_df$"multi_mem_daze" == 1) |
                tbi_df$"multi_loc" == 1 ~ 1,
            TRUE ~ 0
        )
    )
    return(df_mtbi)
}

#' Identify time since and age at each mTBI / most recent mTBI
#'
#' @param tbi_df A TBI dataframe
#'
#' @return dfa The modified dataframe
#'
#' @export
identify_mtbi_times <- function(tbi_df) {
    # Scale injury ages to match interview ages if necessary
    if (mean(tbi_df$"hosp_er_age", na.rm = TRUE) < 20) {
        tbi_df$"blast_age" <- tbi_df$"blast_age" * 12
        tbi_df$"hosp_er_age" <- tbi_df$"hosp_er_age" * 12
        tbi_df$"vehicle_age" <- tbi_df$"vehicle_age" * 12
        tbi_df$"fall_hit_age" <- tbi_df$"fall_hit_age" * 12
        tbi_df$"violent_age" <- tbi_df$"violent_age" * 12
        tbi_df$"other_loc_min_age" <- tbi_df$"other_loc_min_age" * 12
        tbi_df$"multi_effect_end_age" <- tbi_df$"multi_effect_end_age" * 12
    }
    # Time since each type of mTBI
    dft <- tbi_df |> dplyr::mutate(
        hosp_er_mtbi_mpi = dplyr::case_when(
            tbi_df$"hosp_er_mtbi" == 1 ~
                tbi_df$"interview_age" - tbi_df$"hosp_er_age"
        ),
        vehicle_mtbi_mpi = dplyr::case_when(
            tbi_df$"vehicle_mtbi" == 1 ~
                tbi_df$"interview_age" - tbi_df$"vehicle_age"
        ),
        fall_hit_mtbi_mpi = dplyr::case_when(
            tbi_df$"fall_hit_mtbi" == 1 ~
                tbi_df$"interview_age" - tbi_df$"fall_hit_age"
        ),
        violent_mtbi_mpi = dplyr::case_when(
            tbi_df$"violent_mtbi" == 1 ~
                tbi_df$"interview_age" - tbi_df$"violent_age"
        ),
        blast_mtbi_mpi = dplyr::case_when(
            tbi_df$"blast_mtbi" == 1 ~
                tbi_df$"interview_age" - tbi_df$"blast_age"
        ),
        other_loc_mtbi_mpi = dplyr::case_when(
            tbi_df$"other_loc_mtbi" > 0 ~
                tbi_df$"interview_age" - tbi_df$"other_loc_min_age"
        ),
        multi_mtbi_mpi = dplyr::case_when(
            tbi_df$"multi_mtbi" == 1 ~
                tbi_df$"interview_age" - tbi_df$"multi_effect_end_age"
        )
    )
    # Time since latest mTBI
    dft2 <- dft |>
        dplyr::mutate(latest_mtbi_mpi = pmin(
            dft$"hosp_er_mtbi_mpi",
            dft$"vehicle_mtbi_mpi",
            dft$"fall_hit_mtbi_mpi",
            dft$"violent_mtbi_mpi",
            dft$"blast_mtbi_mpi",
            dft$"other_loc_mtbi_mpi",
            dft$"multi_mtbi_mpi",
            na.rm = TRUE
        ))
    # Age at latest mTBI
    mtbi_ages <- list(
        (dft2$"hosp_er_age" * dft2$"hosp_er_mtbi"),
        (dft2$"vehicle_age" * dft2$"vehicle_mtbi"),
        (dft2$"fall_hit_age" * dft2$"fall_hit_mtbi"),
        (dft2$"violent_age" * dft2$"violent_mtbi"),
        (dft2$"blast_age" * dft2$"blast_mtbi"),
        # The only mTBI variable that has count info beyond the binary 0/1
        (dft2$"other_loc_min_age" * dft2$"other_loc_mtbi"),
        (dft2$"multi_effect_end_age" * dft2$"multi_mtbi")
    )
    mtbi_ages_max <- do.call(pmax, c(mtbi_ages, na.rm = TRUE))
    dft2$latest_mtbi_age <- mtbi_ages_max
    return(dft2)
}

#' Add columns to a TBI dataframe indicating mechanism of the latest mTBI
#'
#' @param tbi_df A TBI dataframe
#'
#' @return df_mech The modified dataframe
#'
#' @export
identify_latest_mtbi_mechanism <- function(tbi_df) {
    # Generate column indicating mechanism of latest mTBI
    df_mech <- tbi_df |> dplyr::mutate(latest_mtbi_mechanism = dplyr::case_when(
        tbi_df$"latest_mtbi_age" == tbi_df$"hosp_er_age" ~ "hosp_er",
        tbi_df$"latest_mtbi_age" == tbi_df$"vehicle_age" ~ "vehicle",
        tbi_df$"latest_mtbi_age" == tbi_df$"fall_hit_age" ~ "fall_hit",
        tbi_df$"latest_mtbi_age" == tbi_df$"violent_age" ~ "violent",
        tbi_df$"latest_mtbi_age" == tbi_df$"blast_age" ~ "blast",
        tbi_df$"latest_mtbi_age" == tbi_df$"other_loc_min_age" ~ "other_loc",
        tbi_df$"latest_mtbi_age" == tbi_df$"multi_effect_end_age" ~ "multi"
    ))
    return(df_mech)
}

#' Add columns to a TBI dataframe indicating a subject's estimated mTBI count
#'
#' @param tbi_df A TBI dataframe
#'
#' @return df_num_mtbi The modified dataframe
#'
#' @export
identify_num_mtbi <- function(tbi_df) {
    # Generate column for best guess of number of mTBIs sustained
    df_num_mtbi <- tbi_df |>
        dplyr::mutate(
            mtbi_count = (
                tbi_df$"hosp_er_mtbi" +
                    tbi_df$"vehicle_mtbi" +
                    tbi_df$"fall_hit_mtbi" +
                    tbi_df$"violent_mtbi" +
                    tbi_df$"blast_mtbi" +
                    tbi_df$"other_loc_mtbi_num" +
                    tbi_df$"multi_mtbi"
            )
        )
    return(df_num_mtbi)
}

#' Add columns to a TBI dataframe indicating the LOC of their latest mTBI
#'
#' @param tbi_df A TBI dataframe
#'
#' @return dfll The modified dataframe
#'
#' @export
identify_latest_mtbi_loc <- function(tbi_df) {
    # Create LOC duration variable for the 'other_loc' category
    dfol <- tbi_df |>
        dplyr::mutate(other_loc_loc = dplyr::case_when(
            tbi_df$"other_loc_max_loc_mins" == 0 ~ 0,
            tbi_df$"other_loc_max_loc_mins" > 0 &
                tbi_df$"other_loc_max_loc_mins" <= 30 ~ 1,
            tbi_df$"other_loc_max_loc_mins" > 30 &
                tbi_df$"other_loc_max_loc_mins" <= 1440 ~ 2,
            tbi_df$"other_loc_max_loc_mins" > 1440 ~ 3,
            TRUE ~ NA_real_
        ))
    # Create column indicating loc of latest mTBI
    dfll <- dfol |>
        dplyr::mutate(latest_mtbi_loc = dplyr::case_when(
            dfol$"latest_mtbi_mechanism" == "hosp_er" ~ hosp_er_loc,
            dfol$"latest_mtbi_mechanism" == "vehicle" ~ vehicle_loc,
            dfol$"latest_mtbi_mechanism" == "fall_hit" ~ fall_hit_loc,
            dfol$"latest_mtbi_mechanism" == "violent" ~ violent_loc,
            dfol$"latest_mtbi_mechanism" == "blast" ~ blast_loc,
            dfol$"latest_mtbi_mechanism" == "other_loc" ~ other_loc_loc,
            dfol$"latest_mtbi_mechanism" == "multi" ~ multi_loc,
            TRUE ~ NA_real_
        ))
    return(dfll)
}

#' Add columns to a TBI dataframe indicating if their latest mTBI had mem/daze
#'
#' @param tbi_df A TBI dataframe
#'
#' @return dfmd The modified dataframe
#'
#' @export
identify_latest_mtbi_mem_daze <- function(tbi_df) {
    dfmd <- tbi_df |>
        dplyr::mutate(latest_mtbi_mem_daze = dplyr::case_when(
            tbi_df$"latest_mtbi_mechanism" == "hosp_er" ~ hosp_er_mem_daze,
            tbi_df$"latest_mtbi_mechanism" == "vehicle" ~ vehicle_mem_daze,
            tbi_df$"latest_mtbi_mechanism" == "fall_hit" ~ fall_hit_mem_daze,
            tbi_df$"latest_mtbi_mechanism" == "violent" ~ violent_mem_daze,
            tbi_df$"latest_mtbi_mechanism" == "blast" ~ blast_mem_daze,
            tbi_df$"latest_mtbi_mechanism" == "other_loc" ~ NA_real_,
            tbi_df$"latest_mtbi_mechanism" == "multi" ~ multi_mem_daze,
            TRUE ~ NA_real_
        ))
    return(dfmd)
}

#' Extract mTBI subjects with a minimum time-since-last-mtbi threshold
#'
#' @param ph_p_otbi TBI dataframe
#' @param abcd_y_lt Dataframe containing age information
#' @param subjects Vector of subjects to extract data for.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return subjects Dataframe containing list of required subjects
#'
#' @export
detail_mtbi <- function(ph_p_otbi,
                        abcd_y_lt,
                        subjects = NULL,
                        t = 0) {
    ph_p_otbi_filtered <- ph_p_otbi |>
        filter_timepoint(t) |>
        filter_subjects(subjects)
    ###########################################################################
    # Integrate age information
    ###########################################################################
    abcd_y_lt_filtered <- abcd_y_lt |>
        filter_timepoint(t) |>
        filter_subjects(subjects) |>
        dplyr::select("subjectkey", "interview_age", "eventname")
    tbi_df <- dplyr::inner_join(
        ph_p_otbi_filtered,
        abcd_y_lt_filtered,
        by = c(
            "subjectkey",
            "eventname"
        )
    )
    ###########################################################################
    # Ensure all columns that may be numeric are treated as numeric
    ###########################################################################
    tbi_df <- numcol_to_numeric(tbi_df)
    ###########################################################################
    # Sorting by subjectkey
    ###########################################################################
    tbi_df <- dplyr::arrange(tbi_df, tbi_df$"subjectkey")
    ###########################################################################
    # Renaming ambiguous columns
    ###########################################################################
    renamed_tbi <- rename_tbi(tbi_df)
    identified_tbi <- identify_all_tbi(renamed_tbi)
    identified_mtbi <- identify_mtbi(identified_tbi)
    identified_mtbi_times <- identify_mtbi_times(identified_mtbi)
    identified_mech <- identify_latest_mtbi_mechanism(identified_mtbi_times)
    identified_num_mtbi <- identify_num_mtbi(identified_mech)
    identified_latest_loc <- identify_latest_mtbi_loc(identified_num_mtbi)
    detailed_mtbi <- identify_latest_mtbi_mem_daze(identified_latest_loc)
    ###########################################################################
    # Checking for impossible age values
    ###########################################################################
    if (t > 0) {
        zero_age <- detailed_mtbi$"latest_mtbi_age" == 0
        detailed_mtbi$"latest_mtbi_age"[zero_age] <- NA
    }
    if (t > 0) {
        previous_t_age <- abcd_y_lt |>
            filter_timepoint(t - 1) |>
            filter_subjects(subjects) |>
            dplyr::select("subjectkey", "interview_age")
        current_t_age <- abcd_y_lt_filtered |>
            dplyr::select(-"eventname")
        current_tbi_times <- detailed_mtbi |>
            dplyr::select("subjectkey", "latest_mtbi_age")
        colnames(previous_t_age) <- c("subjectkey", "last_years_age")
        colnames(current_t_age) <- c("subjectkey", "current_age")
        colnames(current_tbi_times) <- c("subjectkey", "mtbi_lower_bound")
        age_gap_df <- dplyr::full_join(
            current_tbi_times,
            previous_t_age,
            by = "subjectkey"
        )
        age_gap_df <- dplyr::full_join(
            age_gap_df,
            current_t_age,
            by = "subjectkey"
        )
        age_gap_df$"mtbi_upper_bound" <- age_gap_df$"mtbi_lower_bound" + 11
        age_gap_df <- age_gap_df |>
            dplyr::mutate(
                plausible_age = dplyr::case_when(
                    current_age >= mtbi_lower_bound &
                        last_years_age <= mtbi_upper_bound ~ TRUE,
                    current_age < mtbi_lower_bound |
                        last_years_age > mtbi_upper_bound ~ FALSE
                )
            )
        impossible_age_ind <- which(age_gap_df$"plausible_age" == FALSE)
        impossible_ages <- age_gap_df[impossible_age_ind, ]
        impossible_age_subs <- impossible_ages$"subjectkey"
        if (nrow(impossible_ages) > 0) {
            warning(
                "The following subjects were flagged as having reported",
                " impossible ages: ",
                paste(impossible_age_subs, collapse = ", ")
            )
            detailed_mtbi <- detailed_mtbi |>
                dplyr::filter(
                    !(detailed_mtbi$"subjectkey" %in% impossible_age_subs)
                )
        }
    }
    return(detailed_mtbi)
}

#' Extract mTBI subjects with a minimum time-since-last-mtbi threshold
#'
#' @param ph_p_otbi TBI dataframe
#' @param abcd_y_lt Dataframe containing age information
#' @param min_mpi The minimum time-since-last-mtbi to be selected
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return subjects Dataframe containing list of required subjects
#'
#' @export
get_mtbi_subjects <- function(ph_p_otbi,
                              abcd_y_lt,
                              min_mpi = -10,
                              t = NULL) {
    ph_p_otbi <- detail_mtbi(
        ph_p_otbi,
        abcd_y_lt,
        t = t
    )
    subjects <- ph_p_otbi |>
        dplyr::filter(
            ph_p_otbi$"mtbi" == 1 &
                ph_p_otbi$"moderate_or_severe_tbi" == 0 &
                ph_p_otbi$"latest_mtbi_mpi" >= min_mpi
        ) |>
        dplyr::select("subjectkey")
    return(subjects)
}

#' Extract list of ABCD subjects who have not sustained any head injury
#'
#' @param ph_p_otbi TBI dataframe
#' @param abcd_y_lt Dataframe containing age information
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @return uninjured_subjects List of uninjured subjects
#'
#' @export
get_uninjured_subjects <- function(ph_p_otbi, abcd_y_lt, t = NULL) {
    ph_p_otbi <- detail_mtbi(
        ph_p_otbi,
        abcd_y_lt,
        t = t
    )
    subjectkey <- ""
    mtbi <- ""
    moderate_or_severe_tbi <- ""
    all_inj <- ""
    ph_p_otbi <- ph_p_otbi |>
        dplyr::group_by(subjectkey) |>
        dplyr::summarize(
            mtbi_sum = sum(mtbi),
            mod_sev_sum = sum(moderate_or_severe_tbi)
        )
    ph_p_otbi$"all_inj" <- ph_p_otbi$"mtbi_sum" + ph_p_otbi$"mod_sev_sum"
    uninjured_subjects <- ph_p_otbi |>
        dplyr::filter(all_inj == 0) |>
        dplyr::select(subjectkey)
    return(uninjured_subjects)
}
