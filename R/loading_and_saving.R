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
