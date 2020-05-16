#' @import checkmate
#' @import crosstalk
#' @import htmlwidgets
NULL

#' Show a single summary statistic in a widget
#'
#' A `summarywidget` displays a single statistic derived from a linked table.
#' Its primary use is with the `crosstalk` package. Used with `crosstalk`,
#' a `summarywidget` displays a value which updates as the data selection
#' changes.
#'
#' @param data Data to summarize, normally an instance of [crosstalk::SharedData].
#' @param statistic The statistic to compute.
#' @param column For `sum` and `mean` statistics, the column of `data` to summarize.
#' Not used for `count` statistic.
#' @param selection Expression to select a fixed subset of `data`. May be
#' a logical vector or a one-sided formula that evaluates to a logical vector.
#' If used, the `key` given to [crosstalk::SharedData] must be a fixed column (not row numbers).
#' @param digits Shortcut for number of decimal places to display
#'  (`minimumFractionDigits <- digits` and `maximumFractionDigits <- digits` are
#'  either added to `localesOptions` or are overwritten).
#' @param locales A locales string accepted by
#'  \url{https://www.w3schools.com/jsref/jsref_tolocalestring_number.asp}. If
#'  `NULL` the preferred language of the user's browser is taken.
#' @param localesOptions A list of options accepted by
#'  \url{https://www.w3schools.com/jsref/jsref_tolocalestring_number.asp}.
#' @param width Currently not used.
#' @param height Currently not used.
#' @param elementId Currently not used.
#'
#' @export
summarywidget <- function(
  data,
  statistic = c("count", "sum", "mean"),
  column = NULL,
  selection = NULL,
  digits = 0,
  locales = NULL,
  localesOptions = NULL,
  width = NULL,
  height = NULL,
  elementId = NULL
) {

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

  if (!is.null(digits)) {
    qassert(digits, "N1")

    localesOptions <- as.list(localesOptions)
    localesOptions$minimumFractionDigits <- digits
    localesOptions$maximumFractionDigits <- digits
  }
  if (!is.null(locales)) {
    qassert(locales, "S1")
  }
  if (!is.null(localesOptions)) {
    qassert(localesOptions, "L+")
  }

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
      locales = locales,
      localesOptions = localesOptions,
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
