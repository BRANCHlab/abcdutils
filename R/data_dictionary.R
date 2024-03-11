#' Remove the data dictionary from a dataset
#'
#' @param abcd_df An ABCD dataframe containing a data dictionary
#'
#' @return abcd_df_no_dd The same dataframe without the data dictionary
#'
#' @export
remove_dd <- function(abcd_df) {
    abcd_df_no_dd <- abcd_df[-1, ]
    return(abcd_df_no_dd)
}

#' Open data dictionary link
#'
#' @export
open_dict <- function() {
    url <- paste0("https://data-dict.abcdstudy.org/?")
    utils::browseURL(url)
}

#' Search the abcd data dictionary
#'
#' @param search_string The string to be searched
#'
#' @export
search_dd_web <- function(search_string) {
    if (class(search_string)[1] != "character") {
        search_string <- deparse(substitute(short_name))
    }
    url <- paste0(
        "https://nda.nih.gov/general-query.html",
        "?q=query=data-structure ~and~ searchTerm=", search_string,
        " ~and~ resultsView=table-view")
    utils::browseURL(url)
}

#' Search through a local data dictionary csv file (5.0)
#'
#' @param search_string Parameter to search for
#' @param fields A string or vector of strings indicating column names to use
#' for the search. Options are
#' - "table_name" The name of the 5.0 release file containing the data.
#' - "var_name" The name of the variable as it appears in the raw data.
#' - "var_label" What the variable is.
#' - "notes" Typically this contains more details on what certain values mean.
#' - "condition" Some data was only collected conditionally on responses to
#' other variables. This column indicates what those conditions are.
#' - "table_name_nda" The file that this info was stored in pre-5.0 releases
#'
#' @export
search_dd <- function(search_string, fields = NULL) {
    abcd_dict <- abcdutils::abcd_dict
    if (is.null(fields)) {
        dict_to_search <- abcd_dict
    } else {
        dict_to_search <- abcd_dict[, colnames(abcd_dict) %in% fields]
    }
    matching_rows <- apply(
        dict_to_search,
        1,
        function(row) {
            any(
                grepl(
                    search_string,
                    row,
                    ignore.case = TRUE
                )
            )
        }
    )
    return(abcd_dict[matching_rows, , drop = FALSE])
}
