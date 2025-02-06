#' Convert specific columns to numeric
#'
#' @param df The dataframe containing columns to be converted
#' @param col_indices The positions of the columns to be converted
#' @return df The dataframe with numeric columns
#' @export
col_to_num <- function(df, col_indices) {
    key_cols <- colnames(df)[col_indices]
    df[key_cols] <- sapply(df[key_cols], as.numeric)
    return(df)
}

#' Convert dataframe columns to numeric type
#'
#' @keywords internal
#' Converts all columns in a dataframe that can be converted to numeric type to
#'  numeric type.
#' @param df A dataframe
#' @return df The dataframe with all possible columns converted to type numeric
numcol_to_numeric <- function(df) {
    df[] <- lapply(df,
        function(x) {
            tryCatch(
                {
                    return(as.numeric(x))
                }, warning = function(cond) {
                    if (cond$"message" == "NAs introduced by coercion") {
                        return(x)
                    }
                }
            )
        }
    )
    return(df)
}

#' Convert char columns to factors
#'
#' @param df The dataframe containing char columns to be converted
#' @return df The dataframe with factor columns
#' @export
char_to_fac <- function(df) {
    df[sapply(df, is.character)] <-
        lapply(df[sapply(df, is.character)], as.factor)
    return(df)
}

#' Training and testing split
#'
#' @description
#' Given a vector of subjects and a threshold, returns a list of which members
#'  should be in the training set and which should be in the testing set. The
#'  function relies on whether or not the absolute value of the Jenkins's
#'  one_at_a_time hash function exceeds the maximum possible value
#'  (2147483647) multiplied by the threshold
#'
#' @param train_frac The fraction (0 to 1) of subjects for training
#' @param subjects Vector of subjects to assign.
#' @return split a named list containing the training and testing subject_ids
#' @export
train_test_assign <- function(train_frac, subjects) {
    train_thresh <- 2147483647 * train_frac
    hash <- abs(digest::digest2int(subjects, seed = 42))
    train <- subjects[hash < train_thresh]
    test <- subjects[hash >= train_thresh]
    assigned_subs <- list(train = train, test = test)
    return(assigned_subs)
}

#' Merge list of dataframes
#'
#' @param df_list list of dataframes
#' @param join String indicating if join should be "inner" or "full"
#' @return merged_df inner join of all dataframes in list
#' @export
merge_df_list <- function(df_list, join = "inner") {
    if (join == "inner") {
        merged_df <- df_list |>
            purrr::reduce(dplyr::inner_join, by = "subjectkey")
    } else if (join == "full") {
        merged_df <- df_list |>
            purrr::reduce(dplyr::full_join, by = "subjectkey")
    } else {
        print("Invalid join type specified. Options are 'inner' and 'full'.")
        return(NULL)
    }
    return(merged_df)
}

#' Flexible conditional statement evaluation
#'
#' If a conditional statement would have returns an error or 0-length vector,
#'  this function returns FALSE instead. Otherwise, evaluates the expression
#'  normally.
#'
#' @keywords internal
#' @param x a conditional expression
is_true <- function(x) {
    tryCatch({
        if (length(x) > 0) {
            return(x)
        } else {
            return(FALSE)
        }
    },
    warning = function(cond) {
        return(FALSE)
    },
    error = function(cond) {
        return(FALSE)
    })
}

#' Collapse two complementary columns
#'
#' When two columns contain complementary information (i.e., when one column has
#'  a value, the other must always be NA), replace both columns with a single
#'  one.
#'
#' @param df dataframe containing columns
#' @param c1 string name of first column
#' @param c2 string name of second column
#' @param new_col string name of collapsed column
#' @export
col_collapse <- function(df, c1, c2, new_col) {
    c1_col <- df[, c1]
    c2_col <- df[, c2]
    df <- df |> dplyr::mutate(
        collapsed_col = ifelse(
            is.na(c1_col) | is.na(c2_col),
            dplyr::coalesce(c1_col, c2_col),
            NA
        )
    )
    df <- df |> dplyr::select(-c(!!c1, !!c2))
    new_col_pos <- which(colnames(df) == "collapsed_col")
    colnames(df)[new_col_pos] <- new_col
    return(df)
}

#' Sort a dataframe by subjectkey
#'
#' @param abcd_df A dataframe with a subjectkey column.
#' @export
sort_subjects <- function(abcd_df) {
    sorted_df <- abcd_df |>
        dplyr::arrange(abcd_df$"subjectkey")
    return(sorted_df)
}
