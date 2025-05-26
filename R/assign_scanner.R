#' Assign scanner hash IDs by randomly sampling from site
#'
#' Some subjects in the ABCD study have no neuroimaging data, including no
#' scanner ID variable. Scanner effect adjustment approaches may require 
#' scanner ID to function, but imputing a random scanner ID may be undesirable.
#' To avoid the need to drop observations from the imputation that do not have
#' scanner IDs or to impute missing scanner IDs, this function randomly 
#' provides a scanner to subjects by sampling from the scanners available at
#' their site of data collection. Ensure a random seed is set prior to running
#' this function to obtain replicable results. Sampling of scanner ID when
#' multiple scanners are present at a site is weighted by the frequency of
#' other observations at that site with that scanner ID for that time point.
#'
#' @inheritParams get_scanner_id
#' @inheritParams get_site_id
#' @param return_scanner_map Logical indicating whether to return the scanner
#'  map alongside the assigned scanner IDs. Defaults to `FALSE`.
#' @return If return_scanner_map is FALSE, returns a data frame of observations
#'  with assigned scanner IDs. Otherwise, returns a list containing that data
#'  frame as well as the scanner map used to assign scanner IDs.
#' @export
assign_site_scanner <- function(mri_y_adm_info,
                                abcd_y_lt,
                                subjects = NULL,
                                t = NULL,
                                return_scanner_map = FALSE) {
    site_id <- ""
    scanner_id <- ""
    scanners <- ""
    weights <- ""
    n <- ""
    percent <- ""
    total <- ""
    if (is.null(t)) {
        stop("Please specify a timepoint with the `t` argument.")
    }
    scanner_df <- get_scanner_id(mri_y_adm_info, t = t)
    site_df <- get_site_id(abcd_y_lt, t = t)
    # Site-scanner map
    site_scanner_df <- dplyr::full_join(
        scanner_df,
        site_df,
        by = "subjectkey"
    ) |>
        stats::na.omit()
    scanner_summary <- site_scanner_df |>
        dplyr::group_by(site_id, scanner_id) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::group_by(site_id) |>
        dplyr::mutate(
            total = sum(n),
            percent = round(100 * n / total, 1)
        ) 
    scanner_map <- scanner_summary |>
        dplyr::group_by(site_id) |>
        dplyr::summarise(
            scanners = list(scanner_id),
            weights = list(percent),
            .groups = "drop"
        )
    subject_scanner_df <- get_scanner_id(
        mri_y_adm_info,
        subjects = subjects,
        t = t
    )
    subject_site_df <- get_site_id(
        abcd_y_lt,
        subjects = subjects,
        t = t
    )
    subject_site_scanner_df <- dplyr::full_join(
        subject_scanner_df,
        subject_site_df,
        by = "subjectkey"
    )
    subject_site_no_scanner_df <- dplyr::filter(
        subject_site_scanner_df,
        is.na(subject_site_scanner_df$"scanner_id")
    )
    if (nrow(subject_site_no_scanner_df) == 0) {
        cat("Provided subjects are not missing scanner IDs.\n")
        return(NULL)
    }
    assigned_scanners <- subject_site_no_scanner_df |>
        dplyr::left_join(scanner_map, by = "site_id") |>
        dplyr::mutate(
            scanner_id = purrr::pmap_chr(
                list(scanners, weights),
                function(scanners, weights) {
                    if (length(scanners) == 0 || length(weights) == 0) {
                        NA_character_
                    } else {
                        sample(scanners, size = 1, prob = weights)
                    }
                }
            )
        ) |>
        dplyr::select(-scanners, -weights)
    names(scanner_map$"scanners") <- scanner_map$"site_id"
    if (return_scanner_map) {
        return(
            list(
                assigned_scanners = assigned_scanners,
                scanner_map = scanner_map
            )
        )
    } else {
        return(assigned_scanners)
    }
}
