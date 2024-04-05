#' Subset dataframe to a collection event
#'
#' @param abcd_df An ABCD dataframe.
#'
#' @param t Integer representing which follow-up year to filter to. Defaults
#' to 0 (baseline).
#'
#' @export
filter_timepoint <- function(abcd_df, t) {
    if (!is.null(t)) {
        t <- dplyr::case_when(
            t == 0 ~ "baseline_year_1_arm_1",
            t == 1 ~ "1_year_follow_up_y_arm_1",
            t == 2 ~ "2_year_follow_up_y_arm_1",
            t == 3 ~ "3_year_follow_up_y_arm_1",
            t == 4 ~ "4_year_follow_up_y_arm_1")
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
#' @param abcd_df An ABCD dataframe.
#'
#' @param subject_list Dataframe containing list of required subjects.
#'
#' @export
filter_subjects <- function(abcd_df, subject_list = NULL) {
    if (is.null(subject_list)) {
        return(abcd_df)
    } else {
        keep_subs <- abcd_df$"subjectkey" %in% subject_list
        subject_filtered_df <- abcd_df[keep_subs, ]
        return(subject_filtered_df)
    }
}

#' Filter dataframe by time and subjectkey, then sort by subjectkey
#'
#' @param abcd_df A raw ABCD dataframe.
#'
#' @param t Integer representing follow-up year to filter to. Defaults to 0
#' (baseline).
#'
#' @param subject_list List of subjects to filter to.
#'
#' @export
time_subject_filter_sort <- function(abcd_df, t = NULL, subject_list = NULL) {
    abcd_filtered_df <- abcd_df |>
        filter_timepoint(t) |>
        filter_subjects(subject_list) |>
        numcol_to_numeric()
    abcd_filtered_and_sorted_df <- abcd_filtered_df |>
        dplyr::arrange(abcd_filtered_df$"subjectkey")
    return(abcd_filtered_and_sorted_df)
}
