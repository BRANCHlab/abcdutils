dummy <- function(abcd_df, cols) {
    abcd_df <- fastDummies::dummy_cols(
        .data = abcd_df,
        select_columns = cols,
        remove_selected_columns = TRUE)
    return(abcd_df)
}

#' Convert specific columns to numeric
#'
#' @param df The dataframe containing columns to be converted
#' @param col_indices The positions of the columns to be converted
#'
#' @return df The dataframe with numeric columns
#'
#' @export
col_to_num <- function(df, col_indices) {
    key_cols <- colnames(df)[col_indices]
    df[key_cols] <- sapply(df[key_cols], as.numeric)
    return(df)
}

#' Convert char columns to factors
#'
#' @param df The dataframe containing char columns to be converted
#'
#' @return df The dataframe with factor columns
#'
#' @export
char_to_fac <- function(df) {
    df[sapply(df, is.character)] <-
        lapply(df[sapply(df, is.character)], as.factor)
    return(df)
}
