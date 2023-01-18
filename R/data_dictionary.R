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
#' @param short_name The short name of the abcd data object
#'
#' @export
abcd_dd <- function(short_name) {
    if (class(short_name)[1] != "character") {
        short_name <- deparse(substitute(short_name))
    }
    url <- paste0(
        "https://nda.nih.gov/data_structure.html?short_name=",
        short_name)
    utils::browseURL(url)
}

#' Search the abcd data dictionary
#'
#' @param search_string The string to be searched
#'
#' @export
search_dd <- function(search_string) {
    if (class(search_string)[1] != "character") {
        search_string <- deparse(substitute(short_name))
    }
    url <- paste0(
        "https://nda.nih.gov/general-query.html",
        "?q=query=data-structure ~and~ searchTerm=", search_string,
        " ~and~ resultsView=table-view")
    utils::browseURL(url)
}
