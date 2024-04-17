#' Generate mock ABCD data that is safe to share.
#'
#' Given a real ABCD dataframe, generates a completely randomized counterpart
#'  that can be used for demonstrating or practice purposes. Process shuffles
#'  rows to ensure overall similar structure to the original ABCD dataframe in
#'  terms of NA frequencies and within-column summary statistics.
#'
#' @param df a template ABCD dataframe
#' @param n number of subjects for fake dataframe. If NULL, will match the
#'  template dataframe.
#' @param seed set seed before mock dataframe generation. Note that this will
#'  alter the global environment.
#'
#' @return mock_df A scrambled abcd dataframe.
#'
#' @export
generate_mock_data <- function(df, n = NULL, seed = NULL) {
    if (!is.null(seed)) {
        set.seed(seed)
        print("The global seed has been changed!")
    }
    data_dictionary <- df[1, ]
    df <- df[-1, ]
    # Select a random subset of rows to begin with
    if (is.null(n)) {
        n <- nrow(df)
    }
    random_rows <- sample(nrow(df), size = n)
    df <- df[random_rows, ]
    # Shuffle all the rows
    for (i in seq_along(df)) {
        df[, i] <- df[sample(nrow(df)), i]
    }
    # Generate fake subject keys and assign them
    subjectchars <- c(toupper(letters), as.character(0:9))
    for (i in seq_len(nrow(df))) {
        subjectkey <- paste(
            c("NDAR_INV", sample(subjectchars, size = 8)),
            collapse = ""
        )
        df[i, c("subjectkey", "src_subject_id")] <- subjectkey
    }
    mock_df <- rbind(data_dictionary, df)
    return(mock_df)
}
