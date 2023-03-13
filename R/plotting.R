#' Visualize missing data on a per-dataframe basis
#'
#' @param df_list Named list of dataframes
#'
#' @return missing_list list of missingness plot and missingness data
#'
#' @export
vis_missing_by_df <- function(df_list) {
    # Calculate number of NAs
    df_list <- lapply(df_list,
        function(x) {
            x$na_summary <- rowSums(is.na(x))
            x$na_summary[x$na_summary > 0] <- NA
            return(x)
        }
    )
    # Assign proper NA column names and just select na columns
    for (i in seq_along(df_list)) {
        sum_name <- paste0(names(df_list)[i], "_na")
        df_list[[i]][, sum_name] <- df_list[[i]]$na_summary
        df_list[[i]] <- df_list[[i]] |>
            dplyr:: select("subjectkey", dplyr::contains(sum_name))
    }
    merged_df <- merge_df_list(df_list, join = "full")
    visdat::vis_miss(merged_df)
    output_list <- list(visdat::vis_miss(merged_df), merged_df)
    return(output_list)
}

#' Clean a plot
#'
#' Given a ggplot object and a list of elements to be removed, return a cleaned
#'  version of the plot.
#'
#' @param plot A ggplot object
#' @param removables A character vector of items to be removed. Can contain "x"
#'  for x-axis label, "y" for y-axis label, and "legend" to remove the legend.
#'
#' @return plot A cleaned ggplot object
#'
#' @export
clean_plot <- function(plot, removables = c()) {
    if ("x" %in% removables) {
        plot <- plot + ggplot2::xlab("")
    }
    if ("y" %in% removables) {
        plot <- plot + ggplot2::ylab("")
    }
    if ("legend" %in% removables) {
        plot <- plot + ggplot2::theme(legend.position = "none")
    }
    return(plot)
}
