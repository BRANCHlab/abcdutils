#' Deprecated function for pulling sleep disturbance scale total problems.
#'
#' @param ph_p_sds Dataframe containing sleep disturbance scale data.
#' @param subjects Vector of subjectkeys.
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...).
#' @return Data frame containing sleep data.
#' @export
get_sds_total_probs <- function(ph_p_sds, subjects = NULL, t = NULL) {
    abcdutils_deprecated(
        version = "0.2.0",
        alternative = "Please use `get_sleep_disturbance()` instead."
    )
}

#' Open data dictionary in browser
#'
#' This function will launch the ABCD interactive data dictionary in a browser.
#' @export
open_dict <- function() {
    abcdutils_deprecated(
        version = "0.2.0",
        alternative = "Please use `open_dd()` instead."
    )
}
