#' Extract subjects that passed QC for specified neuroimaging modalities
#'
#' @inheritParams filter_timepoint
#' @inheritParams filter_subjects
#' @param mri_y_qc_incl Dataframe with QC inclusion info.
#' @param type Which type of QC to extract data for. Valid options are:
#' - t1w
#' - t2w
#' - dmri
#' - rsfmri
#' - mid
#' - nback
#' - sst
#' @return Vector of observations that (1) passed QC, (2) failed QC, or (3) had
#' missing QC data.
#' @export
get_qc_data <- function(mri_y_qc_incl,
                        t = NULL,
                        subjects = NULL,
                        type) {
    mri_y_qc_incl <- mri_y_qc_incl |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects) |>
        dplyr::select(
            subjectkey,
            paste0("imgincl_", type, "_include")
        )
    qc_pass <- na.omit(mri_y_qc_incl[mri_y_qc_incl[, 2] == 1, ])$"subjectkey"
    qc_fail <- na.omit(mri_y_qc_incl[mri_y_qc_incl[, 2] != 1, ])$"subjectkey"
    qc_missing <- na.omit(mri_y_qc_incl[is.na(mri_y_qc_incl[, 2]), ])$"subjectkey"
    return(
        list(
            "pass" = qc_pass,
            "fail" = qc_fail,
            "missing" = qc_missing
        )
    )
}
