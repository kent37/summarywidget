#' Widget that shows a single summary statistic
#'
#' `summarywidget` displays a single statistic derived from a linked table.
#' It's primary use is with the `crosstalk` package. Used with `crosstalk`,
#' a `summarywidget` displays a value which updates as the data selection
#' changes.
#'
#' @param data Data to summarize, normally an instance of [crosstalk::SharedData].
#' @param statistic The statistic to compute.
#' @param digits Number of decimal places to display, or NULL to display full precision.
#' @param selection Expression to select a fixed subset of `data`. May be
#' a logical vector or a one-sided formula that evaluates to a logical vector.
#' If used, the `key` given to [crosstalk::SharedData] must be a fixed column (not row numbers).
#' @param column For `sum` and `mean` statistics, the column of `data` to summarize.
#' Not used for `count` statistic.
#'
#' @import crosstalk
#' @import htmlwidgets
#'
#' @export
summarywidget <- function(data,
                          statistic=c("count", "sum", "mean"), digits=0,
                          selection=NULL, column = NULL,
                          width=NULL, height=NULL, elementId = NULL) {

  if (crosstalk::is.SharedData(data)) {
    # Using Crosstalk
    key <- data$key()
    group <- data$groupName()
    data <- data$origData()
  } else {
    # Not using Crosstalk
    warning("summarywidget works best when data is an instance of crosstalk::SharedData.")
    key <- NULL
    group <- NULL
  }

  statistic <- match.arg(statistic)

  # If selection is given, apply it
  if (!is.null(selection)) {
    # Evaluate any formula
    if (inherits(selection, 'formula')) {
      if (length(selection) != 2L)
        stop("Unexpected two-sided formula: ", deparse(selection))
      selection = eval(selection[[2]], data, environment(selection))
    }

    if (!is.logical(selection))
      stop("Selection must contain TRUE/FALSE values.")
    data = data[selection,]
    key = key[selection]
  }

  # We just need one column, either the row.names or the specified column.
  if (is.null(column)) {
    if (statistic != 'count')
      stop("Column must be provided with ", statistic, " statistic.")
    data = row.names(data)
  } else {
    if (!(column %in% colnames(data)))
      stop("No ", column, " column in data.")
    data = data[[column]]
  }

  # forward options using x
  x = list(
    data = data,
    settings = list(
      statistic = statistic,
      digits = digits,
      crosstalk_key = key,
      crosstalk_group = group
    )
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'summarywidget',
    x,
    width = width,
    height = height,
    package = 'summarywidget',
    elementId = elementId,
    dependencies = crosstalk::crosstalkLibs()
  )
}

#' Shiny bindings for summarywidget
#'
#' Output and render functions for using summarywidget within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a summarywidget
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name summarywidget-shiny
#'
#' @export
summarywidgetOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'summarywidget', width, height, package = 'summarywidget')
}

#' @rdname summarywidget-shiny
#' @export
renderSummarywidget <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, summarywidgetOutput, env, quoted = TRUE)
}

# Use a <span> container rather than the default <div>
summarywidget_html <- function(id, style, class, ...){
  htmltools::tags$span(id = id, class = class)
}
