#' Import an abcd csv file
#'
#' Wraps around fread and renames src_subject_id to subjectkey
#'
#' @param csv path to csv to be imported
#' @param rename_src if true, converts "src_subject_id" to "subjectkey"
#'
#' @export
abcd_load <- function(csv, rename_src = TRUE) {
    data <- data.table::fread(csv, data.table = FALSE)
    if (rename_src && "src_subject_id" %in% colnames(data)) {
        data <- data |>
            dplyr::rename("subjectkey" = "src_subject_id")
    }
    return(data)
}

#' Initialize a path maker
#'
#' @description
#' Returns a function that appends a fixed file path prefix to a given file
#'  name.
#'
#' @param path The fixed file path prefix
#'
#' @return path_fn A function that appends the fixed file path to a given name.
#'  `path_fn` is also able to evaluate a `date` parameter. By default, `date`
#'  is `NULL`. If a date is provided, that date will be appended as a prefix to
#'  the provided filename. If set to `TRUE`, today's date (`Sys.Date()`) will be
#'  used.
#'
#' @examples
#' fig_path <- path_maker("/home/prashanth/figures/")
#' fig_path("my_plot.png")
#' fig_path("my_plot.png", date = "2023_03_01")
#' fig_path("my_plot.png", date = TRUE)
#'
#' @export
path_maker <- function(path) {
    # Check if the path trails with a /. If not, add it.
    last_char <- substr(path, nchar(path), nchar(path))
    if (last_char != "/") {
        path <- paste0(path, "/")
    }
    # Function that produces a complete filepath
    path_fn <- function(filename, date = NULL) {
        if (is.null(date)) {
            date <- ""
        } else if (date == TRUE) {
            date <- gsub("-", "_", Sys.Date())
            date <- paste0(date, "_")
        } else {
            date <- paste0(date, "_")
        }
        paste0(path, date, filename)
    }
    return(path_fn)
}

#' Import subjectkey csv as a vector
#'
#' @param path Path to .csv file containing subjectkey column
#'
#' @export
read_subjects <- function(path) {
    subject_df <- readr::read_csv(path)
    subjects <- subject_df$"subjectkey"
    return(subjects)
}

#' Export subjectkey vector as csv
#'
#' @param subjects Vector of subjectkeys.
#' @param path Path to .csv file containing subjectkey column
#'
#' @export
write_subjects <- function(subjects, path) {
    subject_df <- data.frame("subjectkey" = subjects)
    readr::write_csv(subject_df, path)
}

#' Replace "src_subject_id" with "subjectkey"
#'
#' @param df A data frame that may have "src_subject_id" as a UID column.
#' @return df with "subjectkey" as a UID column.
#' @export
swap_src_subjectkey <- function(df) {
    if ("subjectkey" %in% colnames(df)) {
        return(df)
    } else if ("src_subject_id" %in% colnames(df)) {
        df <- dplyr::rename(df, "subjectkey" = "src_subject_id")
        return(df)
    } else {
        abcdutils_error(
            "Provided data frame must have either \"src_subject_id\" or",
            " \"subjectkey\" column."
        )
    }
}

