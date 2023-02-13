#' Dummify categorical columns
#'
#' Wrapper function for fsatDummies::dummy_cols
#'
#' @param abcd_df Dataframe containing column to be dummied
#' @param cols Columns to be dummied
#'
#' @return abcd_df Dataframe in dummy format
#'
#' @export
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

#' Training and testing split
#'
#' @description
#' Given a vector of subject_id and a threshold, returns a list of which members
#'  should be in the training set and which should be in the testing set. The
#'  function relies on whether or not the absolute value of the Jenkins's
#'  one_at_a_time hash function exceeds the maximum possible value
#'  (2147483647) multiplied by the threshold
#'  
#'
#' @param train_frac The fraction (0 to 1) of subjects for training
#' @param subjects The available subjects for distribution
#'
#' @return split a named list containing the training and testing subject_ids
#'
#' @export
train_test_split <- function(train_frac, subjects) {
    train_thresh <- 2147483647 * train_frac
    train <-
        subjects[abs(digest::digest2int(subjects, seed = 42)) < train_thresh]
    test <-
        subjects[abs(digest::digest2int(subjects, seed = 42)) >= train_thresh]
    train_df <- data.frame(subjectkey = train, split = "train")
    test_df <- data.frame(subjectkey = test, split = "test")
    assigned_df <- rbind(train_df, test_df)
    return(assigned_df)
}

#' Filter data to training or testing subjects only
#'
#' @description
#' Given a dataframe, assigned_df object (from train_test_split()), and split,
#'  return just the data for subjects that were assigned the specified split
#'
#' @param assigned_df Dataframe containing "subjectkey" and "split" cols
#' @param df Dataframe to be subsetted into training or testing split
#' @param split Split to keep ("train" or "test")
#'
#' @return split_df
#'
#' @export
keep_split <- function(df, assigned_df, split) {
    train_or_test <- split
    split_df <- assigned_df |>
        dplyr::filter(split == train_or_test) |>
        dplyr::inner_join(df, by = "subjectkey") |>
        dplyr::select(-split)
    return(split_df)
}
