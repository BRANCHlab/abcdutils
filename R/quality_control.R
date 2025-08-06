#' Extract quality control and protocol compliance information for sMRI
#'
#' @param mri_y_qc_raw_smr Dataframe with QC info for T1w or T2w sMRI.
#'
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @param subjects Vector of subjects to filter to.
#'
#' @param metric Either "qc" (default) for quality control or "pc" for protocol
#' compliance.
#'
#' @return qc_results, a list of:
#' 1. Subjects that passed QC (failed by none)
#' 2. Subjects that failed according to at least 1 rater
#' 3. Subjects that were missing all QC data
#'
#' @export
qc_smri <- function(mri_y_qc_raw_smr,
                    t,
                    subjects = NULL,
                    metric = "qc") {
    # Ensure every column is numeric
    mri_qc <- numcol_to_numeric(mri_y_qc_raw_smr)
    ###########################################################################
    # Filter to specified time / subjects, then select only QC columns
    ###########################################################################
    mri_qc <- mri_y_qc_raw_smr |>
        filter_timepoint(t = t) |> 
        filter_subjects(subjects = subjects)
    if (metric == "qc") {
        mri_qc <- mri_qc |>
            dplyr::select(
                "subjectkey",
                dplyr::contains("qc_score"),
                -dplyr::contains("fm_qc") # field map columns
            )
    } else if (metric == "pc") {
        mri_qc <- mri_qc |>
            dplyr::select(
                "subjectkey",
                dplyr::contains("pc_score"),
                -dplyr::contains("fm_pc") # field map columns
            )
    }
    ###########################################################################
    # Missing QC values
    ###########################################################################
    # Number of missing values across all QC columns
    missing_qc <- rowSums(is.na(mri_qc[, -1]))
    # Subjects who fail due to missing QC data
    missing_qc_subs <- mri_qc$"subjectkey"[missing_qc == 3]
    # Remaining subjects
    mri_qc <- mri_qc[missing_qc < 3, ]
    ###########################################################################
    # Any QC fail
    ###########################################################################
    mri_qc[is.na(mri_qc)] <- 1
    min_qc <- do.call(pmin, mri_qc[, -1])
    fail_qc_subs <- mri_qc$"subjectkey"[min_qc == 0]
    pass_qc_subs <- mri_qc$"subjectkey"[min_qc == 1]
    ###########################################################################
    # Formatting results
    ###########################################################################
    qc_results <- list(
        "pass" = pass_qc_subs,
        "fail" = fail_qc_subs,
        "missing" = missing_qc_subs
    )
    return(qc_results)
}

qc_smri2 <- function(mri_y_qc_raw_smr,
                    t,
                    subjects = NULL,
                    metric = "qc") {
    # Ensure every column is numeric
    mri_qc <- numcol_to_numeric(mri_y_qc_raw_smr)
    ###########################################################################
    # Filter to specified time / subjects, then select only QC columns
    ###########################################################################
    mri_qc <- mri_y_qc_raw_smr |>
        filter_timepoint(t = t) |> 
        filter_subjects(subjects = subjects)
    if (metric == "qc") {
        mri_qc <- mri_qc |>
            dplyr::select(
                "subjectkey",
                dplyr::contains("qc_score"),
                -dplyr::contains("fm_qc") # field map columns
            )
    } else if (metric == "pc") {
        mri_qc <- mri_qc |>
            dplyr::select(
                "subjectkey",
                dplyr::contains("pc_score"),
                -dplyr::contains("fm_pc") # field map columns
            )
    }
    ###########################################################################
    # Missing QC values
    ###########################################################################
    # Number of missing values across all QC columns
    missing_qc <- rowSums(is.na(mri_qc[, -1]))
    # Subjects who fail due to missing QC data
    missing_qc_subs <- mri_qc$"subjectkey"[missing_qc == 3]
    # Remaining subjects
    mri_qc <- mri_qc[missing_qc < 3, ]
    ###########################################################################
    # Any QC fail
    ###########################################################################
    mri_qc[is.na(mri_qc)] <- 0
    max_qc <- do.call(pmax, mri_qc[, -1])
    fail_qc_subs <- mri_qc$"subjectkey"[max_qc == 0]
    pass_qc_subs <- mri_qc$"subjectkey"[max_qc == 1]
    ###########################################################################
    # Formatting results
    ###########################################################################
    qc_results <- list(
        "pass" = pass_qc_subs,
        "fail" = fail_qc_subs,
        "missing" = missing_qc_subs
    )
    return(qc_results)
}


#' Extract quality control and protocol compliance information for dMRI
#'
#' @param mri_y_qc_raw_dmr Dataframe with QC info for dMRI.
#'
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @param subjects Vector of subjects to filter to.
#'
#' @param metric Either "qc" (default) for quality control or "pc" for protocol
#' compliance.
#'
#' @return qc_results, a list of:
#' 1. Subjects that passed QC (failed by none)
#' 2. Subjects that failed according to at least 1 rater
#' 3. Subjects that were missing all QC data
#'
#' @export
qc_dmri <- function(mri_y_qc_raw_dmr,
                    t = 0,
                    subjects = NULL,
                    metric = "qc") {
    # Ensure every column is numeric
    mri_qc <- numcol_to_numeric(mri_y_qc_raw_dmr)
    ###########################################################################
    # Filter to specified time / subjects, then select only QC columns
    ###########################################################################
    mri_qc <- mri_y_qc_raw_dmr |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    if (metric == "qc") {
        mri_qc <- mri_qc |>
            dplyr::select(
                "subjectkey",
                dplyr::contains("qc_score"),
                -dplyr::contains("fm_qc") # field map columns
            )
    } else if (metric == "pc") {
        mri_qc <- mri_qc |>
            dplyr::select(
                "subjectkey",
                dplyr::contains("pc_score"),
                -dplyr::contains("fm_pc") # field map columns
            )
    }
    ###########################################################################
    # Missing QC values
    ###########################################################################
    # Number of missing values across all QC columns
    missing_qc <- rowSums(is.na(mri_qc[, -1]))
    # Subjects who fail due to missing QC data
    missing_qc_subs <- mri_qc$"subjectkey"[missing_qc == 6]
    # Remaining subjects
    mri_qc <- mri_qc[missing_qc < 6, ]
    ###########################################################################
    # Any QC fail
    ###########################################################################
    mri_qc[is.na(mri_qc)] <- 1
    min_qc <- do.call(pmin, mri_qc[, -1])
    fail_qc_subs <- mri_qc$"subjectkey"[min_qc == 0]
    pass_qc_subs <- mri_qc$"subjectkey"[min_qc == 1]
    ###########################################################################
    # Formatting results
    ###########################################################################
    qc_results <- list(
        "pass" = pass_qc_subs,
        "fail" = fail_qc_subs,
        "missing" = missing_qc_subs
    )
    return(qc_results)
}

#' Extract quality control and protocol compliance information for dMRI
#'
#' @param mri_y_qc_raw_dmr Dataframe with QC info for dMRI.
#'
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @param subjects Vector of subjects to filter to.
#'
#' @param metric Either "qc" (default) for quality control or "pc" for protocol
#' compliance.
#'
#' @return qc_results, a list of:
#' 1. Subjects that passed QC (failed by none)
#' 2. Subjects that failed according to at least 1 rater
#' 3. Subjects that were missing all QC data
#'
#' @export
qc_dmri2 <- function(mri_y_qc_raw_dmr,
                    t = 0,
                    subjects = NULL,
                    metric = "qc") {
    # Ensure every column is numeric
    mri_qc <- numcol_to_numeric(mri_y_qc_raw_dmr)
    ###########################################################################
    # Filter to specified time / subjects, then select only QC columns
    ###########################################################################
    mri_qc <- mri_y_qc_raw_dmr |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    if (metric == "qc") {
        mri_qc <- mri_qc |>
            dplyr::select(
                "subjectkey",
                dplyr::contains("qc_score"),
                -dplyr::contains("fm_qc") # field map columns
            )
    } else if (metric == "pc") {
        mri_qc <- mri_qc |>
            dplyr::select(
                "subjectkey",
                dplyr::contains("pc_score"),
                -dplyr::contains("fm_pc") # field map columns
            )
    }
    ###########################################################################
    # Missing QC values
    ###########################################################################
    # Number of missing values across all QC columns
    missing_qc <- rowSums(is.na(mri_qc[, -1]))
    # Subjects who fail due to missing QC data
    missing_qc_subs <- mri_qc$"subjectkey"[missing_qc == 6]
    # Remaining subjects
    mri_qc <- mri_qc[missing_qc < 6, ]
    ###########################################################################
    # Any QC fail
    ###########################################################################
    mri_qc[is.na(mri_qc)] <- 0
    max_qc <- do.call(pmax, mri_qc[, -1])
    fail_qc_subs <- mri_qc$"subjectkey"[max_qc == 0]
    pass_qc_subs <- mri_qc$"subjectkey"[max_qc == 1]
    ###########################################################################
    # Formatting results
    ###########################################################################
    qc_results <- list(
        "pass" = pass_qc_subs,
        "fail" = fail_qc_subs,
        "missing" = missing_qc_subs
    )
    return(qc_results)
}

#' Extract quality control and protocol compliance information for rs-fMRI
#'
#' @param mri_y_qc_raw_rsfmr Dataframe with QC info for rs-fMRI.
#'
#' @param t timepoint of data collection (0: baseline, 1: 1yfu, ...)
#'
#' @param subjects Vector of subjects to filter to.
#'
#' @param metric Either "qc" (default) for quality control or "pc" for protocol
#' compliance.
#'
#' @return qc_results, a list of:
#' 1. Subjects that passed QC (failed by none)
#' 2. Subjects that failed according to at least 1 rater
#' 3. Subjects that were missing all QC data
#'
#' @export
qc_rsfmri <- function(mri_y_qc_raw_rsfmr,
                      t = 0,
                      subjects = NULL,
                      metric = "qc") {
    # Ensure every column is numeric
    mri_qc <- numcol_to_numeric(mri_y_qc_raw_rsfmr)
    ###########################################################################
    # Filter to specified time / subjects, then select only QC columns
    ###########################################################################
    mri_qc <- mri_y_qc_raw_rsfmr |>
        filter_timepoint(t = t) |>
        filter_subjects(subjects = subjects)
    if (metric == "qc") {
        mri_qc <- mri_qc |>
            dplyr::select(
                "subjectkey",
                dplyr::contains("qc_score"),
                -dplyr::contains("fm_qc") # field map columns
            )
    } else if (metric == "pc") {
        mri_qc <- mri_qc |>
            dplyr::select(
                "subjectkey",
                dplyr::contains("pc_score"),
                -dplyr::contains("fm_pc") # field map columns
            )
    }
    ###########################################################################
    # Missing QC values
    ###########################################################################
    # Number of missing values across all QC columns
    missing_qc <- rowSums(is.na(mri_qc[, -1]))
    # Subjects who fail due to missing QC data
    missing_qc_subs <- mri_qc$"subjectkey"[missing_qc == 12]
    # Remaining subjects
    mri_qc <- mri_qc[missing_qc < 12, ]
    ###########################################################################
    # Any QC fail
    ###########################################################################
    mri_qc[is.na(mri_qc)] <- 1
    min_qc <- do.call(pmin, mri_qc[, -1])
    fail_qc_subs <- mri_qc$"subjectkey"[min_qc == 0]
    pass_qc_subs <- mri_qc$"subjectkey"[min_qc == 1]
    ###########################################################################
    # Formatting results
    ###########################################################################
    qc_results <- list(
        "pass" = pass_qc_subs,
        "fail" = fail_qc_subs,
        "missing" = missing_qc_subs
    )
    return(qc_results)
}
