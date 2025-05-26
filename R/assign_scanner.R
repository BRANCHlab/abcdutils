#' Assign scanner hash IDs by randomly sampling from site
#'
#' Some subjects in the ABCD study have no neuroimaging data, including no
#' scanner ID variable. Scanner effect adjustment approaches may require 
#' scanner ID to function, but imputing a random scanner ID may be undesirable.
#' To avoid the need to drop observations from the imputation that do not have
#' scanner IDs or to impute missing scanner IDs, this function randomly 
#' provides a scanner to subjects by sampling from the scanners available at
#' their site of data collection. Ensure a random seed is set prior to running
#' this function to obtain replicable results.
#'
#' @inheritParams get_scanner_id
#' @inheritParams get_site_id
#' @return A list containing a data frame of observations with assigned scanner
#' IDs as well as the scanner map that those IDs were sampled from.
#' @export
assign_site_scanner <- function(mri_y_adm_info,
                                abcd_y_lt,
                                subjects = NULL,
                                t = NULL) {
    site_id <- ""
    scanner_id <- ""
    scanners <- ""
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
        stats::na.omit() |>
        dplyr::select(-"subjectkey") |> 
        unique() |>
        dplyr::arrange(site_id)
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
    scanner_map <- site_scanner_df |>
        dplyr::group_by(site_id)
    scanner_map <- scanner_map |>
        dplyr::summarise(
            scanners = list(unique(scanner_id)),
            .groups = "drop"
        )
    assigned_scanners <- subject_site_no_scanner_df |>
        dplyr::left_join(scanner_map, by = "site_id") |>
        dplyr::mutate(
            scanner_id = purrr::map_chr(
                scanners,
                ~ if (length(.x) == 0) NA_character_ else sample(.x, 1)
            )
        ) |>
        dplyr::select(-"scanners")
    names(scanner_map$"scanners") <- scanner_map$"site_id"
    return(
        list(
            assigned_scanners = assigned_scanners,
            scanner_map = scanner_map
        )
    )
}
