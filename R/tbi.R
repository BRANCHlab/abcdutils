#' Rename ambiguous columns in otbi01 file.
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
#'
#' @examples
#' # Mock abcd_otbi01.txt
#' abcd_otbi01 <- data.frame(matrix(NA, nrow = 2, ncol = 49))
#' colnames(abcd_otbi01) <- c("collection_id", "abcd_otbi01_id", "dataset_id",
#'     "subjectkey", "src_subject_id", "interview_date", "interview_age", "sex",
#'     "eventname", "tbi_select_language___1", "tbi_1", "tbi_1b", "tbi_1c",
#'     "tbi_1d", "tbi_2", "tbi_2b", "tbi_2c", "tbi_2d", "tbi_3", "tbi_3b",
#'     "tbi_3c", "tbi_3d", "tbi_4", "tbi_4b", "tbi_4c", "tbi_4d", "tbi_5",
#'     "tbi_5b", "tbi_5c", "tbi_5d", "tbi_6o", "tbi_6p", "tbi_6q", "tbi_6r",
#'     "tbi_6s", "tbi_7a", "tbi_7c1", "tbl_7c2", "tbi_7e", "tbi_7f", "tbi_7g",
#'     "tbi_7i", "tbi_7k", "tbi_7l", "tbi_8g", "tbi_8i", "tbi_8k", "tbi_8l",
#'     "collection_title")
#'
#' otbi01_renamed <- rename_tbi(abcd_otbi01)
#' otbi01_renamed
rename_tbi <- function(tbi_df) {
    if (!is.data.frame(tbi_df)) {
        rlang::abort("Object is not a dataframe.",
            class = "non_df")
    }
    renamed_tbi <- tbi_df |>
        dplyr::rename_with(
            ~ dplyr::case_when(
                . == "tbi_1" | . == "tbi_1_l" ~ "hosp_er_inj",
                grepl("tbi_1b", .) ~ "hosp_er_loc",
                grepl("tbi_1c", .) ~ "hosp_er_mem_daze",
                grepl("tbi_1d", .) ~ "hosp_er_age",
                . == "tbi_2" | . == "tbi_2_l" ~ "vehicle_inj",
                grepl("tbi_2b", .) ~ "vehicle_loc",
                grepl("tbi_2c", .) ~ "vehicle_mem_daze",
                grepl("tbi_2d", .) ~ "vehicle_age",
                . == "tbi_3" | . == "tbi_3_l" ~ "fall_hit_inj",
                grepl("tbi_3b", .) ~ "fall_hit_loc",
                grepl("tbi_3c", .) ~ "fall_hit_mem_daze",
                grepl("tbi_3d", .) ~ "fall_hit_age",
                . == "tbi_4" | . == "tbi_4_l" ~ "violent_inj",
                grepl("tbi_4b", .) ~ "violent_loc",
                grepl("tbi_4c", .) ~ "violent_mem_daze",
                grepl("tbi_4d", .) ~ "violent_age",
                . == "tbi_5" | . == "tbi_5_l" ~ "blast_inj",
                grepl("tbi_5b", .) ~ "blast_loc",
                grepl("tbi_5c", .) ~ "blast_mem_daze",
                grepl("tbi_5d", .) ~ "blast_age",
                grepl("tbi_6o", .) ~ "other_loc_inj",
                grepl("tbi_6p", .) ~ "other_loc_num",
                grepl("tbi_6q", .) ~ "other_loc_max_loc_mins",
                grepl("tbi_6r", .) ~ "other_loc_num_over_30",
                grepl("tbi_6s", .) ~ "other_loc_min_age",
                grepl("tbi_7a", .) ~ "multi_inj",
                grepl("tbi_7c1", .) ~ "multi_loc",
                grepl("tbl_7c2", .) ~ "multi_mem_daze",
                grepl("tbi_7e", .) ~ "multi_effect_start_age",
                grepl("tbi_7f", .) ~ "multi_effect_end_age",
                grepl("tbi_7g", .) ~ "other_multi_inj",
                grepl("tbi_7i", .) ~ "other_multi_effect_type",
                grepl("tbi_7k", .) ~ "other_multi_effect_start_age",
                grepl("tbi_7l", .) ~ "other_multi_effect_end_age",
                . == "tbi_8" ~ "num_sport_concussions",
                . == "tbi_8a" ~ "school_missed_sport_concussion",
                . == "tbi_8b" ~ "school_missed_worst_recent_concussion",
                grepl("tbi_8g", .) ~ "other_other_multi_inj",
                grepl("tbi_8i", .) ~ "other_other_multi_effect_type",
                grepl("tbi_8k", .) ~ "other_other_multi_effect_start_age",
                grepl("tbi_8l", .) ~ "other_other_multi_effect_end_age",
                TRUE ~ .)
            )
    if (identical(renamed_tbi, tbi_df)) {
        rlang::warn("No changes were made to the object.",
            class = "no_effect")
    }
    return(renamed_tbi)
}


#' Return conversion table of original and new otbi names
#'
#' @export
original_otbi_names <- function() {
    print("| New name                           | Old name | Description                                  |")
    print("|------------------------------------+----------+----------------------------------------------|")
    print("| hosp_er_inj                        | tbi_1    | ever hospitalized/ER for head/neck injury?   |")
    print("| hosp_er_loc                        | tbi_1b   | if LOC, how long?                            |")
    print("| hosp_er_mem_daze                   | tbi_1c   | were they dazed or have memory gap?          |")
    print("| hosp_er_age                        | tbi_1d   | how old were they?                           |")
    print("| vehicle_inj                        | tbi_2    | ever injured in a vehicle accident?          |")
    print("| vehicle_loc                        | tbi_2b   | if LOC, how long?                            |")
    print("| vehicle_mem_daze                   | tbi_2c   | were they dazed or have memory gap?          |")
    print("| vehicle_age                        | tbi_2d   | how old were they?                           |")
    print("| fall_hit_inj                       | tbi_3    | ever injured head/neck from fall or hit?     |")
    print("| fall_hit_loc                       | tbi_3b   | if LOC, how long?                            |")
    print("| fall_hit_mem_daze                  | tbi_3c   | were they dazed or have memory gap?          |")
    print("| fall_hit_age                       | tbi_3d   | how old were they?                           |")
    print("| violent_inj                    | tbi_4    | ever injure head/neck from violence?         |")
    print("| violent_loc                    | tbi_4b   | if LOC, how long?                            |")
    print("| violent_mem_daze               | tbi_4c   | were they dazed or have memory gap?          |")
    print("| violent_age                    | tbi_4d   | how old were they?                           |")
    print("| blast_inj                          | tbi_5    | ever injure head or neck from blast?         |")
    print("| blast_loc                          | tbi_5b   | if LOC, how long?                            |")
    print("| blast_mem_daze                     | tbi_5c   | were they dazed or have memory gap?          |")
    print("| blast_age                          | tbi_5d   | how old were they?                           |")
    print("| other_loc_inj                      | tbi_6o   | any other injuries with LOC?                 |")
    print("| other_loc_num                      | tbi_6p   | how many more?                               |")
    print("| other_loc_max_loc_mins             | tbi_6q   | how long was longest LOC?                    |")
    print("| other_loc_num_over_30              | tbi_6r   | how many were >= 30 min?                     |")
    print("| other_loc_min_age                  | tbi_6s   | what was their youngest age?                 |")
    print("| multi_inj                          | tbi_7a   | did they have a period of multiple injuries? |")
    print("| multi_loc                          | tbi_7c1  | if LOC, how long?                            |")
    print("| multi_mem_daze                     | tbl_7c2  | were they dazed or have memory gap?          |")
    print("| multi_effect_start_age             | tbi_7e   | at what age did the effects begin?           |")
    print("| multi_effect_end_age               | tbi_7f   | at what age did the effects end?             |")
    print("| other_multi_inj                    | tbi_7g   | was there another multiple injury period?    |")
    print("| other_multi_effect_type            | tbi_7i   | typical effect of the injury?                |")
    print("| other_multi_effect_start_age       | tbi_7k   | start age of those effects?                  |")
    print("| other_multi_effect_end_age         | tbi_7l   | end age of those effects?                    |")
    print("| other_other_multi_inj              | tbi_8g   | another period of multiple inj?              |")
    print("| other_other_multi_effect_type      | tbi_8i   | typical effects?                             |")
    print("| other_other_multi_effect_start_age | tbi_8k   | start age of effects?                        |")
    print("| other_other_multi_effect_end_age   | tbi_8l   | end age of effects?                          |")
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
#' @return renamed_tbi A modified form of tbi_df with clearer column names
#'
#' @export
#'
identify_all_tbi <- function(tbi_df) {
    # Assign column types
    dfct <- tbi_df |>
        dplyr::mutate(
            dplyr::across(
                "hosp_er_inj":"other_other_multi_effect_end_age", as.numeric
            )
        )
    dfct$"interview_age" <- as.numeric(dfct$"interview_age")
    # Generate mtbi and moderate_or_severe_tbi columns
    df_all_tbi <- dfct |> dplyr::mutate(
        mtbi = dplyr::case_when(
            (dfct$"hosp_er_loc" < 2 & dfct$"hosp_er_mem_daze" == 1) |
                dfct$"hosp_er_loc" == 1 ~ 1,
            (dfct$"vehicle_loc" < 2 & dfct$"vehicle_mem_daze" == 1) |
                dfct$"vehicle_loc" == 1 ~ 1,
            (dfct$"fall_hit_loc" < 2 & dfct$"fall_hit_mem_daze" == 1) |
                dfct$"fall_hit_loc" == 1 ~ 1,
            (dfct$"violent_loc" < 2 & dfct$"violent_mem_daze" == 1) |
                dfct$"violent_loc" == 1 ~ 1,
            (dfct$"blast_loc" < 2 & dfct$"blast_mem_daze" == 1) |
                dfct$"blast_loc" == 1 ~ 1,
            (dfct$"other_loc_num" - dfct$"other_loc_num_over_30") > 0 ~ 1,
            (dfct$"multi_loc" < 2 & dfct$"multi_mem_daze" == 1) |
                dfct$"multi_loc" == 1 ~ 1,
            dfct$"other_multi_inj" == 1 ~ 0.5,
            dfct$"other_other_multi_inj" == 1 ~ 0.5,
            TRUE ~ 0
        ),
        moderate_or_severe_tbi = dplyr::case_when(
            (dfct$"hosp_er_loc" > 1 |
                dfct$"vehicle_loc" > 1 |
                dfct$"fall_hit_loc" > 1 |
                dfct$"violent_loc" > 1 |
                dfct$"blast_loc" > 1 |
                dfct$"other_loc_num_over_30" > 0 |
                dfct$"multi_loc" > 1) ~ 1,
            dfct$"other_multi_inj" == 1 ~ 0.5,
            dfct$"other_other_multi_inj" == 1 ~ 0.5,
            TRUE ~ 0
        )
    )
    return(df_all_tbi)
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
            (
                tbi_df$"hosp_er_loc" < 2 &
                    tbi_df$"hosp_er_mem_daze" == 1) |
                tbi_df$"hosp_er_loc" == 1 ~ 1,
            TRUE ~ 0
        ),
        vehicle_mtbi = dplyr::case_when(
            (
                tbi_df$"vehicle_loc" < 2 & tbi_df$"vehicle_mem_daze" == 1) |
                tbi_df$"vehicle_loc" == 1 ~ 1,
            TRUE ~ 0
        ),
        fall_hit_mtbi = dplyr::case_when(
            (
                tbi_df$"fall_hit_loc" < 2 & tbi_df$"fall_hit_mem_daze" == 1) |
                tbi_df$"fall_hit_loc" == 1 ~ 1,
            TRUE ~ 0
        ),
        violent_mtbi = dplyr::case_when(
            (
                tbi_df$"violent_loc" < 2 & tbi_df$"violent_mem_daze" == 1) |
                tbi_df$"violent_loc" == 1 ~ 1,
            TRUE ~ 0
        ),
        blast_mtbi = dplyr::case_when(
            (
                tbi_df$"blast_loc" < 2 & tbi_df$"blast_mem_daze" == 1) |
                blast_loc == 1 ~ 1,
            TRUE ~ 0
        ),
        other_loc_mtbi = dplyr::case_when(
            (
                tbi_df$"other_loc_num" - tbi_df$"other_loc_num_over_30") > 0 ~
                tbi_df$"other_loc_num" - tbi_df$"other_loc_num_over_30",
            TRUE ~ 0
        ),
        multi_mtbi = dplyr::case_when(
            (
                tbi_df$"multi_loc" < 2 & tbi_df$"multi_mem_daze" == 1) |
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
        tbi_df$"blast_age" <-
            tbi_df$"blast_age" * 12
        tbi_df$"hosp_er_age" <-
            tbi_df$"hosp_er_age" * 12
        tbi_df$"vehicle_age" <-
            tbi_df$"vehicle_age" * 12
        tbi_df$"fall_hit_age" <-
            tbi_df$"fall_hit_age" * 12
        tbi_df$"violent_age" <-
            tbi_df$"violent_age" * 12
        tbi_df$"other_loc_min_age" <-
            tbi_df$"other_loc_min_age" * 12
        tbi_df$"multi_effect_end_age" <-
            tbi_df$"multi_effect_end_age" * 12
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
    dfa <- dft2 |>
        dplyr::mutate(latest_mtbi_age = pmax(
            dft2$"hosp_er_age",
            dft2$"vehicle_age",
            dft2$"fall_hit_age",
            dft2$"violent_age",
            dft2$"blast_age",
            dft2$"other_loc_min_age",
            dft2$"multi_effect_end_age",
            na.rm = TRUE
        ))
    return(dfa)
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
        dplyr::mutate(mtbi_count = (
            tbi_df$"hosp_er_mtbi" +
                tbi_df$"vehicle_mtbi" +
                tbi_df$"fall_hit_mtbi" +
                tbi_df$"violent_mtbi" +
                tbi_df$"blast_mtbi" +
                tbi_df$"other_loc_mtbi" +
                tbi_df$"multi_mtbi"))
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


#' Chain several TBI annotation functions to add multiple helpful mTBI columns
#'
#' @param otbi01 The baseline TBI dataframe
#' @param subjects Dataframe containing list of required subjects
#' @param t Integer representing which timepoint to filter to:
#'  - 0: baseline
#'  - 1: 1-year follow-up
#'  - 2: 2-year follow-up
#'  - 3: 3-year follow-up
#'
#' @return detailed_otbi01 The modified dataframe
#'
#' @export
detail_mtbi <- function(otbi01, subjects, t = NULL) {
    detailed_otbi01 <- abcd_import(otbi01, subjects, t = t) |>
        rename_tbi() |>
        identify_all_tbi() |>
        identify_mtbi() |>
        identify_mtbi_times() |>
        identify_latest_mtbi_mechanism() |>
        identify_num_mtbi() |>
        identify_latest_mtbi_loc() |>
        identify_latest_mtbi_mem_daze()
    return(detailed_otbi01)
}


#' Extract mTBI subjects with a minimum time-since-last-mtbi threshold
#'
#' @param abcd_otbi01 A TBI dataframe
#' @param min_latest_mtbi_mpi The minimum time-since-last-mtbi to be selected
#' @param t Integer representing which timepoint to filter to:
#'  - 0: baseline
#'  - 1: 1-year follow-up
#'  - 2: 2-year follow-up
#'  - 3: 3-year follow-up
#'
#' @return subjects Dataframe containing list of required subjects
#'
#' @export
#'
get_mtbi_subjects <- function(abcd_otbi01, min_latest_mtbi_mpi, t = NULL) {
    abcd_otbi01 <- abcd_import(abcd_otbi01, t = t) |>
        rename_tbi() |>
        identify_all_tbi() |>
        identify_mtbi() |>
        identify_mtbi_times()
    subjects <- abcd_otbi01 |>
        dplyr::filter(abcd_otbi01$"mtbi" == 1 &
                      abcd_otbi01$"moderate_or_severe_tbi" == 0 &
                      abcd_otbi01$"latest_mtbi_mpi" >= min_latest_mtbi_mpi) |>
        dplyr::select("subjectkey")
    return(subjects)
}
