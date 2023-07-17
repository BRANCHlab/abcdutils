#' rsfMRI QC
#'
#' @description
#' Given a raw MRI QC file and a dataframe containing subjects, removes subjects
#'  that do not pass all rsfMRI quality control and protocol compliance checks
#'
#' @param abcd_df A dataframe containing subjectkeys
#' @param mriqcrp10301 The `mriqcrp10301` data object
#' @param t The collection period of interest (defaults to baseline: 0)
#' @param no_na whether observations containing NAs should be removed
#'
#' @return rsfmri_qc_list a list of:
#' 1. The original dataframe with failing QC participants dropped
#' 2. A list of subjectkeys of dropped QC participants
#'
#' @export
qc_rsfmri <- function(abcd_df, mriqcrp10301, t = 0, no_na = FALSE) {
    mri_qc <- mriqcrp10301 |>
        abcd_import(t = t, subjects = abcd_df[, "subjectkey"]) |>
        dplyr::select("subjectkey",
                      dplyr::contains(c("qc_score", "pc_score")),
                      -dplyr::contains("mid"))
    mri_qc <- Filter(function(x) !all(is.na(x)), mri_qc)
    mri_qc <- col_to_num(mri_qc, 2:length(mri_qc))
    mri_qc[is.na(mri_qc)] <- 1
    rsfmri_qc <- mri_qc |>
        dplyr::select("subjectkey", dplyr::contains("rsfmri"))
    rsfmri_qc$min_qc <- do.call(pmin, rsfmri_qc[, 2:length(rsfmri_qc)])
    failed_qc <- rsfmri_qc$"subjectkey"[rsfmri_qc$"min_qc" == 0]
    print(paste0(length(failed_qc), " subjects failed rsFMRI QC"))
    passing_qc <- which(rsfmri_qc$"min_qc" == 1)
    abcd_df_qc <- abcd_df[passing_qc, ]
    if (no_na) abcd_df_qc <- stats::na.omit(abcd_df_qc)
    return(abcd_df_qc)
}


#' dmri QC
#'
#' @description
#' Given a raw MRI QC file and a dataframe containing subjects, removes subjects
#'  that do not pass all dmri quality control and protocol compliance checks
#'
#' @param abcd_df A dataframe containing subjectkeys
#' @param mriqcrp10301 The `mriqcrp10301` data object
#' @param t The collection period of interest (defaults to baseline: 0)
#' @param no_na whether observations containing NAs should be removed
#'
#' @return dmri_qc_list a list of:
#' 1. The original dataframe with failing QC participants dropped
#' 2. A list of subjectkeys of dropped QC participants
#'
#' @export
qc_dmri <- function(abcd_df, mriqcrp10301, t = 0, no_na = FALSE) {
    mri_qc <- mriqcrp10301 |>
        abcd_import(t = t, subjects = abcd_df[, "subjectkey"]) |>
        dplyr::select("subjectkey",
                      dplyr::contains(c("qc_score", "pc_score")),
                      -dplyr::contains("mid"))
    mri_qc <- Filter(function(x) !all(is.na(x)), mri_qc)
    mri_qc <- col_to_num(mri_qc, 2:length(mri_qc))
    mri_qc[is.na(mri_qc)] <- 1
    dmri_qc <- mri_qc |>
        dplyr::select("subjectkey", dplyr::contains("dmri"))
    dmri_qc$min_qc <- do.call(pmin, dmri_qc[, 2:length(dmri_qc)])
    failed_qc <- dmri_qc$"subjectkey"[dmri_qc$"min_qc" == 0]
    print(paste0(length(failed_qc), " subjects failed dmri QC"))
    passing_qc <- which(dmri_qc$"min_qc" == 1)
    abcd_df_qc <- abcd_df[passing_qc, ]
    if (no_na) abcd_df_qc <- stats::na.omit(abcd_df_qc)
    return(abcd_df_qc)
}

#' smri QC
#'
#' @description
#' Given a raw MRI QC file and a dataframe containing subjects, removes subjects
#'  that do not pass all smri quality control and protocol compliance checks
#'
#' @param abcd_df A dataframe containing subjectkeys
#' @param mri_y_qc_raw_smr_t1 QC info for T1 sMRI
#' @param mri_y_qc_raw_smr_t2 QC info for T2 sMRI
#' @param t The collection period of interest (defaults to baseline: 0)
#' @param no_na whether observations containing NAs should be removed
#'
#' @return smri_qc_list a list of:
#' 1. The original dataframe with failing QC participants dropped
#' 2. A list of subjectkeys of dropped QC participants
#'
#' @export
qc_smri <- function(abcd_df,
                    mri_y_qc_raw_smr_t1,
                    mri_y_qc_raw_smr_t2,
                    t = 0,
                    no_na = FALSE) {
    mri_t1_qc <- mri_y_qc_raw_smr_t1 |>
        abcd_import(t = t, subjects = abcd_df[, "subjectkey"]) |>
        dplyr::select("subjectkey",
                      dplyr::contains(c("qc_score", "pc_score")),
                      -dplyr::contains("mid"))
    mri_t2_qc <- mri_y_qc_raw_smr_t2 |>
        abcd_import(t = t, subjects = abcd_df[, "subjectkey"]) |>
        dplyr::select("subjectkey",
                      dplyr::contains(c("qc_score", "pc_score")),
                      -dplyr::contains("mid"))
    mri_qc <- dplyr::inner_join(mri_t1_qc, mri_t2_qc, by = "subjectkey")
    mri_qc <- Filter(function(x) !all(is.na(x)), mri_qc)
    mri_qc <- col_to_num(mri_qc, 2:length(mri_qc))
    mri_qc[is.na(mri_qc)] <- 1
    smri_qc <- mri_qc |>
        dplyr::select("subjectkey", dplyr::contains(c("t1", "t2")))
    smri_qc$min_qc <- do.call(pmin, smri_qc[, 2:length(smri_qc)])
    failed_qc <- smri_qc$"subjectkey"[smri_qc$"min_qc" == 0]
    print(paste0(length(failed_qc), " subjects failed smri QC"))
    passing_qc <- which(smri_qc$"min_qc" == 1)
    abcd_df_qc <- abcd_df[passing_qc, ]
    if (no_na) abcd_df_qc <- stats::na.omit(abcd_df_qc)
    return(abcd_df_qc)
}
