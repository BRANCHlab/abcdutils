cbcl_raw_to_thresh <- function(cbcl_df) {
    depress_threshold_borderline <- 5
    depress_threshold_clinical <- 7
    anxiety_threshold_borderline <- 6
    anxiety_threshold_clinical <- 8
    attention_threshold_borderline <- 9
    attention_threshold_clinical <- 12
    aggressive_threshold_borderline <- 11
    aggressive_threshold_clinical <- 15
    if ("cbcl_depress_r" %in% colnames(cbcl_df)) {
        cbcl_df <- cbcl_df |>
            dplyr::mutate(cbcl_depress = dplyr::case_when(
                cbcl_df$"cbcl_depress_r" < depress_threshold_borderline ~ 0,
                cbcl_df$"cbcl_depress_r" < depress_threshold_clinical ~ 1,
                cbcl_df$"cbcl_depress_r" >= depress_threshold_clinical ~ 2,
                TRUE ~ NA)) |>
            dplyr::select(-"cbcl_depress_r")
    }
    if ("cbcl_anxiety_r" %in% colnames(cbcl_df)) {
        cbcl_df <- cbcl_df |>
            dplyr::mutate(cbcl_anxiety = dplyr::case_when(
                cbcl_df$"cbcl_anxiety_r" < depress_threshold_borderline ~ 0,
                cbcl_df$"cbcl_anxiety_r" < depress_threshold_clinical ~ 1,
                cbcl_df$"cbcl_anxiety_r" >= depress_threshold_clinical ~ 2,
                TRUE ~ NA)) |>
            dplyr::select(-"cbcl_anxiety_r")
    }
    if ("cbcl_attention_r" %in% colnames(cbcl_df)) {
        cbcl_df <- cbcl_df |>
            dplyr::mutate(cbcl_attention = dplyr::case_when(
                cbcl_df$"cbcl_attention_r" < depress_threshold_borderline ~ 0,
                cbcl_df$"cbcl_attention_r" < depress_threshold_clinical ~ 1,
                cbcl_df$"cbcl_attention_r" >= depress_threshold_clinical ~ 2,
                TRUE ~ NA)) |>
            dplyr::select(-"cbcl_attention_r")
    }
    if ("cbcl_aggressive_r" %in% colnames(cbcl_df)) {
        cbcl_df <- cbcl_df |>
            dplyr::mutate(cbcl_aggressive = dplyr::case_when(
                cbcl_df$"cbcl_aggressive_r" < depress_threshold_borderline ~ 0,
                cbcl_df$"cbcl_aggressive_r" < depress_threshold_clinical ~ 1,
                cbcl_df$"cbcl_aggressive_r" >= depress_threshold_clinical ~ 2,
                TRUE ~ NA)) |>
            dplyr::select(-"cbcl_aggressive_r")
    }
    return(cbcl_df)
}

