
#' Subset dataframe to a collection event
#'
#' @param abcd_df An ABCD dataframe containing a data dictionary
#' @param t Integer representing which timepoint to filter to:
#'  - 0: baseline
#'  - 1: 1-year follow-up
#'  - 2: 2-year follow-up
#'  - 3: 3-year follow-up
#'
#' @return timepoint_abcd_df Baseline data only
#'
#' @export
filter_timepoint <- function(abcd_df, t) {
    if (!is.null(t)) {
        t <- dplyr::case_when(
            t == 0 ~ "baseline_year_1_arm_1",
            t == 1 ~ "1_year_follow_up_y_arm_1",
            t == 2 ~ "2_year_follow_up_y_arm_1",
            t == 3 ~ "3_year_follow_up_y_arm_1")
        t_abcd_df <- abcd_df |>
            dplyr::filter(abcd_df$"eventname" == t)
        return(t_abcd_df)
    } else {
        print("All collection timepoints included.")
        return(abcd_df)
    }
}

#' Subset a dataframe to a given subject list
#'
#' @param abcd_df An ABCD dataframe
#' @param subjects Dataframe containing list of required subjects
#'
#' @return filtered_df The subsetted dataframe
#'
#' @export
filter_subjects <- function(abcd_df, subjects = NULL) {
    if (is.null(subjects)) {
        return(abcd_df)
    } else {
        filtered_df <- dplyr::inner_join(abcd_df, subjects, by = "subjectkey")
        return(filtered_df)
    }
}

#' Get common subjects
#'
#' @description
#' Extract subjects common across a list of dataframes
#'
#' @param list List of dataframes
#'
#' @return common_subs Subjects common across list of dataframes
#'
#' @export
common_subjects <- function(list) {
    shared_df <- list |>
        purrr::reduce(dplyr::inner_join, by = "subjectkey")
    common_subs <- shared_df$subjectkey
    return(common_subs)
}


#' Subset a raw ABCD dataframe by time and subjects
#'
#' @param abcd_df A raw ABCD dataframe
#' @param t Integer representing which timepoint to filter to:
#'  - 0: baseline
#'  - 1: 1-year follow-up
#'  - 2: 2-year follow-up
#'  - 3: 3-year follow-up
#' @param subjects Dataframe containing list of required subjects
#'
#' @return abcd_clean_df The subsetted dataframe
#'
#' @export
abcd_import <- function(abcd_df, t = NULL, subjects = NULL) {
    abcd_clean_df <- abcd_df |>
        remove_dd() |>
        filter_timepoint(t) |>
        filter_subjects(subjects)
    abcd_clean_df <- abcd_clean_df |>
        dplyr::arrange(abcd_clean_df$"subjectkey")
    return(abcd_clean_df)
}
