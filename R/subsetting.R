#' Subset dataframe to a collection event
#'
#' @param abcd_df An ABCD dataframe.
#'
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
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
#' @param subjects Vector of subjects to filter to.
#'
#' @export
filter_subjects <- function(abcd_df, subjects = NULL) {
    if (is.null(subjects)) {
        return(abcd_df)
    } else {
        keep_subs <- abcd_df$"subjectkey" %in% subjects
        subject_filtered_df <- abcd_df[keep_subs, ]
        return(subject_filtered_df)
    }
}

#' Filter dataframe by time and subjectkey, then sort by subjectkey
#'
#' @param abcd_df A raw ABCD dataframe.
#'
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @param subjects Vector of subjects to filter to.
#'
#' @export
time_subject_filter_sort <- function(abcd_df, t = NULL, subjects = NULL) {
    abcd_filtered_df <- abcd_df |>
        filter_timepoint(t) |>
        filter_subjects(subjects) |>
        numcol_to_numeric()
    abcd_filtered_and_sorted_df <- abcd_filtered_df |>
        dplyr::arrange(abcd_filtered_df$"subjectkey")
    return(abcd_filtered_and_sorted_df)
}
