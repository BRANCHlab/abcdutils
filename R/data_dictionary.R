#' Open data dictionary in browser
#'
#' This function will launch the ABCD interactive data dictionary in a browser.
#' @export
open_dd <- function() {
    url <- paste0("https://data-dict.abcdstudy.org/?")
    utils::browseURL(url)
}

#' Search the ABCD data dictionary (5.0 version)
#'
#' This function enables grep-searching the complete ABCD data dictionary.
#' Users can restrict the search to a limited number of fields using the
#' `fields` parameter.
#'
#' @param search_string String to search data dictionary for.
#' @param fields A string or vector of strings indicating column names to use
#'  for the search. Options are
#'  - "table_name" The name of the 5.0 release file containing the data.
#'  - "var_name" The name of the variable as it appears in the raw data.
#'  - "var_label" What the variable is.
#'  - "notes" Typically this contains more details on what certain values mean.
#'  - "condition" Some data was only collected conditionally on responses to
#'  other variables. This column indicates what those conditions are.
#'  - "table_name_nda" The file that this info was stored in pre-5.0 releases
#' @export
search_dd <- function(search_string, fields = NULL) {
    abcd_dict <- data.frame(abcdutils::abcd_dict)
    if (is.null(fields)) {
        dict_to_search <- abcd_dict
    } else {
        col_indices <- colnames(abcd_dict) %in% fields
        dict_to_search <- abcd_dict[, col_indices, drop = FALSE]
    }
    matching_rows <- apply(
        dict_to_search,
        1,
        function(row) {
            any(grepl(search_string, row, ignore.case = TRUE))
        }
    )
    clean_dict <- tibble::tibble(abcd_dict[matching_rows, , drop = FALSE])
    return(clean_dict)
}
