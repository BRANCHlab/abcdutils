#' Convert raw CBCL scores to thresholded ones
#'
#' @param cbcl_df cbcl dataframe
#' @param depress_threshold_border cbcl threshold score
#' @param depress_threshold_clinical cbcl threshold score
#' @param anxiety_threshold_border cbcl threshold score
#' @param anxiety_threshold_clinical cbcl threshold score
#' @param attention_threshold_border cbcl threshold score
#' @param attention_threshold_clinical cbcl threshold score
#' @param aggressive_threshold_border cbcl threshold score
#' @param aggressive_threshold_clinical cbcl threshold score
#'
#' @return cbcl_df thresholded dataframe
#'
#' @export
cbcl_raw_to_thresh <- function(cbcl_df, depress_threshold_border = 5,
                               depress_threshold_clinical = 7,
                               anxiety_threshold_border = 6,
                               anxiety_threshold_clinical = 8,
                               attention_threshold_border = 9,
                               attention_threshold_clinical = 12,
                               aggressive_threshold_border = 11,
                               aggressive_threshold_clinical = 15) {
    if ("cbcl_depress_r" %in% colnames(cbcl_df)) {
        cbcl_df <- cbcl_df |>
            dplyr::mutate(cbcl_depress = dplyr::case_when(
                cbcl_df$"cbcl_depress_r" < depress_threshold_border ~ 0,
                cbcl_df$"cbcl_depress_r" < depress_threshold_clinical ~ 1,
                cbcl_df$"cbcl_depress_r" >= depress_threshold_clinical ~ 2,
                TRUE ~ NA)) |>
            dplyr::select(-"cbcl_depress_r")
    }
    if ("cbcl_anxiety_r" %in% colnames(cbcl_df)) {
        cbcl_df <- cbcl_df |>
            dplyr::mutate(cbcl_anxiety = dplyr::case_when(
                cbcl_df$"cbcl_anxiety_r" < depress_threshold_border ~ 0,
                cbcl_df$"cbcl_anxiety_r" < depress_threshold_clinical ~ 1,
                cbcl_df$"cbcl_anxiety_r" >= depress_threshold_clinical ~ 2,
                TRUE ~ NA)) |>
            dplyr::select(-"cbcl_anxiety_r")
    }
    if ("cbcl_attention_r" %in% colnames(cbcl_df)) {
        cbcl_df <- cbcl_df |>
            dplyr::mutate(cbcl_attention = dplyr::case_when(
                cbcl_df$"cbcl_attention_r" < depress_threshold_border ~ 0,
                cbcl_df$"cbcl_attention_r" < depress_threshold_clinical ~ 1,
                cbcl_df$"cbcl_attention_r" >= depress_threshold_clinical ~ 2,
                TRUE ~ NA)) |>
            dplyr::select(-"cbcl_attention_r")
    }
    if ("cbcl_aggressive_r" %in% colnames(cbcl_df)) {
        cbcl_df <- cbcl_df |>
            dplyr::mutate(cbcl_aggressive = dplyr::case_when(
                cbcl_df$"cbcl_aggressive_r" < depress_threshold_border ~ 0,
                cbcl_df$"cbcl_aggressive_r" < depress_threshold_clinical ~ 1,
                cbcl_df$"cbcl_aggressive_r" >= depress_threshold_clinical ~ 2,
                TRUE ~ NA)) |>
            dplyr::select(-"cbcl_aggressive_r")
    }
    return(cbcl_df)
}
