#' Extract parent-reported gender of youth
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated function for extracting parent-reported gender.
#' Please use `get_gender_p()` instead.
#'
#' @param ... Arguments passed to `get_gender_p()`.
#' @return Data frame containing youth-reported gender.
get_p_gender <- function(...) {
    abcdutils_deprecated(
        version = "0.4.0",
        alternative = "Please use `get_gender_p()` instead."
    )
    get_gender_p(...)
}

#' Deprecated function for pulling sleep disturbance scale total problems.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Defunct function for converting a data list into a data frame. Please
#'  use `as.data.frame()` instead.
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

#' Extract youth-reported gender of self
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated function for extracting youth-reported gender.
#' Please use `get_gender_y()` instead.
#'
#' @param ... Arguments passed to `get_gender_y()`.
#' @return Data frame containing youth-reported gender.
get_y_gender <- function(...) {
    abcdutils_deprecated(
        version = "0.4.0",
        alternative = "Please use `get_gender_y()` instead."
    )
    get_gender_y(...)
}

#' Open data dictionary in browser
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated function for launching the ABCD data dictionary in browser.
#' Please use `open_dd()` instead.
#'
#' @export
open_dict <- function() {
    abcdutils_deprecated(
        version = "0.2.0",
        alternative = "Please use `open_dd()` instead."
    )
}

#' Extract white matter neurite densities
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated function for extracting white matter neurite densities.
#' Please use `get_wmnd()` instead.
#'
#' @param ... Arguments passed to `get_gender_y()`.
#' @return Data frame containing youth-reported gender.
get_all_wmnd <- function(...) {
    abcdutils_deprecated(
        version = "0.4.0",
        alternative = "Please use `get_wmnd()` instead."
    )
    get_wmnd(...)
}

