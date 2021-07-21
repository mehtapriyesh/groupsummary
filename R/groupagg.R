
#' Grouping and aggregating using multiple variables
#'
#' @param data Data to be used
#' @param grouping The grouping variable/s to be used. In case multiple, you can pass a vector containing name of variables as strings
#' @param agg The variable/s to be aggregated. In case multiple, you can pass a vector containing name of variables as strings
#' @param ... The function you'll be using to create aggregation. Ex: sum, mean, median, etc...
#'
#' @return Returning the aggregation
#' @export
#'
#' @examples
#' grouping = "vs"
#' agg = "wt"
#' mtcars %>% groupagg(grouping, agg, median)
#'   vs    wt
#' <dbl> <dbl>
#'   1     0  3.57
#'   2     1  2.62
#'
#'   grouping = c("cyl", "vs")
#'   agg = c("wt", "qsec")
#'   mtcars %>% groupagg(grouping, agg, mean)
#'

groupagg <- function(data, grouping, agg, ...) {
  data %>%  group_by(across(grouping)) %>%
    summarise_at(.vars = agg, .funs = ...)
}



#' Creating boxplot with some stats
#'
#' @param data
#' @param grouping
#' @param agg
#'
#' @return
#' @export
#'
#' @examples mtcars %>% plot_with_stat("cyl", "mpg")

plot_with_stat <- function(data, grouping, agg) {
  stat = mean(data[[agg]])

  data %>% ggplot(aes_string(x = grouping, y = agg, group = grouping)) +
    geom_boxplot() +
    labs(title = glue("Mean of {agg} is {stat}"))
}











