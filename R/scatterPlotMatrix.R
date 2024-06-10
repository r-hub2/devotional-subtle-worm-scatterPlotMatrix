#' `htmlwidget` for `d3.js` scatter plot matrix
#'
#' @param data
#'   data.frame with data to use in the chart.
#' @param categorical
#'   List of list (one for each data column) containing the name of available categories,
#'   or `NULL` if column corresponds to continuous data;
#'   `NULL` is allowed, meaning all columns are continuous.
#'   A named list can also be provided to only indicate which columns are categorical, associating a column name to available categories.
#' @param inputColumns
#'   List of boolean (one for each data column), `TRUE` for an input column, `FALSE` for an output column;
#'   `NULL` is allowed, meaning all columns are inputs.
#'   A list of column names can also be provided to only indicate which columns are inputs.
#' @param keptColumns
#'   List of boolean (one for each data column), `FALSE` if column has to be ignored;
#'   `NULL` is allowed, meaning all columns are available.
#'   A list of column names can also be provided to only indicate which columns are to be kept.
#' @param cutoffs
#'   List of `SpCutoff`; a `SpCutoff` is a list defining a `xDim`, `yDim` and a list of `xyCutoff`;
#'   a `xyCutoff` is a pair of `cutoff` (one for x axis, one for y axis);
#'   a `cutoff` is a list containing two values (min and max values)
#'   or `NULL` if there is no cutoff to apply for this axis;
#'   `NULL` is allowed, meaning there is no cutoff to apply.
#' @param zAxisDim
#'   Name of the column represented by z axis (used to determine the color to attribute to a point);
#'   `NULL` is allowed, meaning there is no coloring to apply.
#' @param distribType
#'   Binary code indicating the type of distribution plot (bit 1: density plot, bit 2: histogram).
#' @param regressionType
#'   Binary code indicating the type of regression plot (bit 1: linear, bit 2: loess).
#' @param corrPlotType
#'   String indicating the type of correlation plots to use.
#'   Supported values: `Circles` to use a circle tree map;
#'   `Text` to display values of correlation as colored text labels (color scale domain is `[-1; 1]`);
#'   `AbsText` to display values of correlation as colored text labels (color scale domain is `[0; 1]`,
#'   absolute value of correlations is used);
#'   `Empty` to not display values of correlation;
#'   default value is `Circles`.
#' @param corrPlotCS
#'   Name of the color Scale to use for correlation plot when plot type is `Text` or `AbsText`;
#'   supported names: `Viridis`, `Inferno`, `Magma`, `Plasma`, `Warm`, `Cool`, `Rainbow`, `CubehelixDefault`,
#'   `Blues`,`Greens`, `Greys`, `Oranges`, `Purples`, `Reds`, `BuGn`, `BuPu`, `GnBu`, `OrRd`, `PuBuGn`, `PuBu`,
#'   `PuRd`, `RdBu`, `RdPu`, `YlGnBu`, `YlGn`, `YlOrBr`, `YlOrRd`;
#'   default value is `NULL`,
#'   which corresponds to `RdBu` if `corrPlotType` is `Text`,
#'   or `Blues` if `corrPlotType` is `AbsText`.
#' @param rotateTitle
#'   `TRUE` if column title must be rotated.
#' @param columnLabels
#'   List of string (one for each data column) to display in place of column name found in data,
#'   or `NULL` if there is no alternative name;
#'   `NULL` is allowed, meaning all columns are without alternative name;
#'   `<br>` can be used to insert line breaks.
#' @param continuousCS
#'   Name of the color Scale to use for continuous data;
#'   supported names: `Viridis`, `Inferno`, `Magma`, `Plasma`, `Warm`, `Cool`, `Rainbow`, `CubehelixDefault`,
#'   `Blues`, `Greens`, `Greys`, `Oranges`, `Purples`, `Reds`, `BuGn`, `BuPu`, `GnBu`, `OrRd`, `PuBuGn`, `PuBu`,
#'   `PuRd`, `RdBu`, `RdPu`, `YlGnBu`, `YlGn`, `YlOrBr`, `YlOrRd`;
#'   default value is `Viridis`.
#' @param categoricalCS
#'   Name of the color Scale to use for categorical data;
#'   supported names: `Category10`, `Accent`, `Dark2`, `Paired`, `Set1`;
#'   default value is `Category10`.
#' @param mouseMode
#'   Type of mouse interaction. Three types are available: `tooltip`, `filter` or `zoom`.
#' @param eventInputId
#'   When plot event occurred, reactive input to write to; `NULL` is allowed, meaning no event is sent.
#'   An event is a list with two named elements 'type' and 'value'.
#'   * If `type` is equal to `zAxisChange`:
#'     * `value` is the new column to use as reference (see `zAxisDim` argument).
#'   * If `type` is equal to `cutoffChange`:
#'     * `value$adjusting` is `TRUE` when pointer is moving, changing a cutoff; 
#'     * `value$cutoffs` gives the new values for the cutoffs.
#'   * If `type` is equal to `pointClicked`:
#'     * `value$pointIndex` is the index of the clicked point.
#' @param controlWidgets
#'   Tells if some widgets must be available to control plot;
#'   `NULL` is allowed, meaning that `!HTMLWidgets.shinyMode` is to use;
#'   default value is `FALSE`.
#' @param cssRules
#'   CSS rules to add.
#'   Must be a named list of the form list(selector = declarations),
#'   where selector is a valid CSS selector and declarations is a string or vector of declarations.
#' @param plotProperties
#'   Adjust some properties which can not be set through CSS (mainly size, color and opacity of points).
#'   Default value is `NULL` which is equivalent to:
#'   ```
#'     list(
#'       noCatColor = "#43665e",
#'       watermarkColor = "#ddd",
#'       point = list(
#'         alpha = 0.5,
#'         radius = 2
#'       ),
#'       regression = list(
#'         strokeWidth = 4
#'       )
#'     )
#'   ```
#' @param slidersPosition
#'   Set initial position of sliders, specifying which columns intervals are visible.
#'   Default value is `NULL` which is equivalent to:
#'   ```
#'     list(
#'       dimCount = 8,
#'       xStartingDimIndex = 1,
#'       yStartingDimIndex = 1
#'     )
#'   ```
#' @param width
#'   Integer in pixels defining the width of the widget.
#' @param height
#'   Integer in pixels defining the height of the widget.
#' @param elementId
#'   Unique `CSS` selector id for the widget.
#'
#' @examples
#'  if(interactive()) {
#'    library(scatterPlotMatrix)
#'
#'    scatterPlotMatrix(iris, zAxisDim = "Species")
#'    # Each point has a color depending of its 'Species' value
#'
#'    categorical <- list(cyl = c(4, 6, 8), vs = c(0, 1), am = c(0, 1), gear = 3:5, carb = 1:8)
#'    scatterPlotMatrix(mtcars, categorical = categorical, zAxisDim = "cyl")
#'    # 'cyl' and four last columns have a box representation for categories
#'    # (use top slider to see the last three columns)
#'
#'    scatterPlotMatrix(iris, zAxisDim = "Species", distribType = 1)
#'    # Distribution plots are of type 'density plot' (instead of histogram)
#'
#'    scatterPlotMatrix(iris, zAxisDim = "Species", regressionType = 1)
#'    # Add linear regression plots
#'
#'    columnLabels <- gsub("\\.", "<br>", colnames(iris))
#'    scatterPlotMatrix(iris, zAxisDim = "Species", columnLabels = columnLabels)
#'    # Given names are displayed in place of dataset column names; <br> is used to insert line breaks
#'
#'    scatterPlotMatrix(iris, cssRules = list(
#'        ".jitterZone" = "fill: pink",
#'        ".tick text" = c("fill: red", "font-size: 1.8em")
#'    ))
#'    # Background of plot is pink and text of axes ticks is red and greater
#'
#'    scatterPlotMatrix(iris, plotProperties = list(
#'      noCatColor = "DarkCyan",
#'      point = list(
#'        alpha = 0.3,
#'        radius = 4
#'      )
#'    ))
#'    # Points of plots are different:
#'    # two times greater, with opacity reduced from 0.5 to 0.3, and a `DarkCyan` color
#'
#'  }
#'
#' @importFrom htmlwidgets createWidget sizingPolicy shinyWidgetOutput shinyRenderWidget
#'
#' @export
scatterPlotMatrix <- function(
  data,
  categorical = NULL,
  inputColumns = NULL,
  cutoffs = NULL,
  keptColumns = NULL,
  zAxisDim = NULL,
  distribType = 2,
  regressionType = 0,
  corrPlotType = "Circles",
  corrPlotCS = NULL,
  rotateTitle = FALSE,
  columnLabels = NULL,
  continuousCS = "Viridis",
  categoricalCS = "Category10",
  mouseMode = "tooltip",
  eventInputId = NULL,
  controlWidgets = FALSE,
  cssRules = NULL,
  plotProperties = NULL,
  slidersPosition = NULL,
  width = NULL,
  height = NULL,
  elementId = NULL
) {

  args <- checkSpmArgs(
    list(
      data = data,
      rowLabels = rownames(data),
      categorical = categorical,
      inputColumns = inputColumns,
      cutoffs = cutoffs,
      keptColumns = keptColumns,
      zAxisDim = zAxisDim,
      distribType = distribType,
      regressionType = regressionType,
      corrPlotType = corrPlotType,
      corrPlotCS = corrPlotCS,
      rotateTitle = rotateTitle,
      columnLabels = columnLabels,
      continuousCS = continuousCS,
      categoricalCS = categoricalCS,
      mouseMode = mouseMode,
      eventInputId = eventInputId,
      controlWidgets = controlWidgets,
      cssRules = cssRules,
      plotProperties = plotProperties,
      slidersPosition = slidersPosition
    )
  )

  # create widget
  htmlwidgets::createWidget(
    name = "scatterPlotMatrix",
    args,
    width = width,
    height = height,
    sizingPolicy = htmlwidgets::sizingPolicy(padding = 0, browser.defaultHeight = 960, browser.defaultWidth = "auto"),
    package = "scatterPlotMatrix",
    elementId = elementId
  )
}

#' Shiny bindings for scatterPlotMatrix
#'
#' Output and render functions for using scatterPlotMatrix within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a scatterPlotMatrix
#' @param env The environment in which to evaluate `expr`.
#' @param quoted Is `expr` a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name scatterPlotMatrix-shiny
#'
#' @export
scatterPlotMatrixOutput <- function(outputId, width = "100%", height = "600px") {
  htmlwidgets::shinyWidgetOutput(outputId, "scatterPlotMatrix", width, height, package = "scatterPlotMatrix")
}

#' @rdname scatterPlotMatrix-shiny
#' @export
renderScatterPlotMatrix <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, scatterPlotMatrixOutput, env, quoted = TRUE)
}

checkSpmArgs <- function(args) {
  return(
    checkEventInputId(
      checkSlidersPosition(
        checkScatterPlotProperties(
          checkMouseMode(
            checkCategoricalCS(
              checkContinuousCS(
                checkColumnLabels(
                  checkColumnLabels(
                    checkRotateTitle(
                      checkRegressionType(
                        checkCorrPlotCS(
                          checkCorrPlotType(
                            checkDistribType(
                              checkZAxisDim(
                                checkInputColumns(
                                  checkCutoffs(
                                    checkKeptColumns(
                                      checkCategorical(
                                        checkData(args)
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

checkData <- function(args) {
  if (!is.data.frame(args$data) && !is.matrix(args$data)) {
    stop("'data' must be a dataframe")
  }
  return(args)
}

checkCategorical <- function(args) {
  colCount <- ncol(args$data)
  if (!is.null(args$categorical) && !is.list(args$categorical)) {
    message("'categorical' must be a list")
    args["categorical"] <- list(NULL)
  }
  if (!is.null(args$categorical)) {
    args$categorical <- as.list(args$categorical)
    if (!is.null(names(args$categorical)) && !is.null(names(args$data))) {
      unknownNames <- setdiff(names(args$categorical), names(args$data))
      if (length(unknownNames) != 0) {
        message(paste("categorical is a list with unknown column names:", toString(unknownNames)))
        args$categorical <- rep(list(NULL), colCount)
      }
      else {
        args$categorical <- lapply(seq_len(colCount), function(icol) {
          colName <- names(args$data)[icol]
          if (is.null(args$categorical[[colName]])) {
            return(NULL)
          }
          else {
            return(args$categorical[[colName]])
          }
        })
      }
    }
    if (colCount != length(args$categorical)) {
      message("Length of 'categorical' must be equal to the number of columns of 'data'")
      args["categorical"] <- list(NULL)
    }
    else {
      for (i in seq_len(length(args$categorical))) {
        if (!is.null(args$categorical[[i]])) {
          if (is.vector(args$categorical[[i]])) {
            args$categorical[[i]] <- as.list(args$categorical[[i]])
          }
          else {
            message(paste("categorical", i, "must be a vector"))
            args[["categorical"]][i] <- list(NULL)
          }
        }
      }
    }
  }
  else {
    # Try some automatic generations
    categorical <- lapply(seq_len(ncol(args$data)), function(icol) {
      if (is.factor(args$data[, icol])) {
        categories <- as.list(levels(args$data[, icol]))
        if (length(categories) < 8) {
          return(categories)
        }
      }
      return(NULL)
    })
    args["categorical"] <- list(categorical)
  }
  return(args)
}

checkKeptColumns <- function(args) {
  colCount <- ncol(args$data)
  if (!is.null(args$keptColumns) && !is.vector(args$keptColumns)) {
    message("'keptColumns' must be a vector")
    args["keptColumns"] <- list(NULL)
  }
  if (!is.null(args$keptColumns)) {
    namesMode <- length(args$keptColumns) != 0 && all(unlist(lapply(args$keptColumns, is.character)))
    if (namesMode && !is.null(names(args$data))) {
      unknownNames <- setdiff(args$keptColumns, names(args$data))
      if (length(unknownNames) != 0) {
        message(paste("keptColumns is a list with unknown column names:", toString(unknownNames)))
        args$keptColumns <- rep(list(TRUE), colCount)
      }
      else {
        args$keptColumns <- lapply(seq_len(colCount), function(icol) {
          colName <- names(args$data)[icol]
          return (colName %in% args$keptColumns)
        })
      }
    }
    if (colCount != length(args$keptColumns)) {
      message("Length of 'keptColumns' must be equal to the number of columns of 'data'")
      args["keptColumns"] <- list(NULL)
    }
    else {
      for (i in seq_len(length(args$keptColumns))) {
        if (!is.logical(args$keptColumns[[i]])) {
          message(paste("keptColumns", i, "must be of logical type"))
          args[["keptColumns"]][i] <- TRUE
        }
      }
    }
  }
  return(args)
}

checkInputColumns <- function(args) {
  colCount <- ncol(args$data)
  if (!is.null(args$inputColumns) && !is.vector(args$inputColumns)) {
    message("'inputColumns' must be a vector")
    args["inputColumns"] <- list(NULL)
  }
  if (!is.null(args$inputColumns)) {
    namesMode <- length(args$inputColumns) != 0 && all(unlist(lapply(args$inputColumns, is.character)))
    if (namesMode && !is.null(names(args$data))) {
      unknownNames <- setdiff(args$inputColumns, names(args$data))
      if (length(unknownNames) != 0) {
        message(paste("inputColumns is a list with unknown column names:", toString(unknownNames)))
        args$inputColumns <- rep(list(TRUE), colCount)
      }
      else {
        args$inputColumns <- lapply(seq_len(colCount), function(icol) {
          colName <- names(args$data)[icol]
          return (colName %in% args$inputColumns)
        })
      }
    }
    if (colCount != length(args$inputColumns)) {
      message("Length of 'inputColumns' must be equal to the number of columns of 'data'")
      args["inputColumns"] <- list(NULL)
    }
    else {
      for (i in seq_len(length(args$inputColumns))) {
        if (!is.logical(args$inputColumns[[i]])) {
          message(paste("inputColumns", i, "must be of logical type"))
          args[["inputColumns"]][i] <- TRUE
        }
      }
    }
  }
  return(args)
}

checkCutoffs <- function(args) {
  if (!is.null(args$cutoffs) && !is.list(args$cutoffs)) {
    message("'cutoffs' must be a list")
    args["cutoffs"] <- list(NULL)
  }
  if (!is.null(args$cutoffs)) {
    expectedNames <- c("xDim", "yDim", "xyCutoffs")
    for (i in seq_len(length(args$cutoffs))) {
      ## if cutoffs are provided for current column
      if (!setequal(intersect(expectedNames, names(args$cutoffs[[i]])), expectedNames)) {
        message(paste("cutoff", i, "doesn't contains expected fields:", toString(expectedNames)))
        args["cutoffs"] <- list(NULL)
        break
      }
    }
  }
  return(args)
}

checkZAxisDim <- function(args) {
  colNames <- colnames(args$data)
  if (!is.null(args$zAxisDim) && is.na(match(args$zAxisDim, colNames))) {
    message(paste(
      "zAxisDim:",
      args$zAxisDim,
      "must be a valid column dimension, it must be one of:",
      toString(colNames)
    ))
    args["zAxisDim"] <- list(NULL)
  }
  return(args)
}
  
checkDistribType <- function(args) {
  if (!is.numeric(args$distribType)) {
    message("'distribType' must be of numeric type")
    args["distribType"] <- 2
  }
  else {
    if (is.na(match(args$distribType, 0:3))) {
      message(paste("distribType:", args$distribType, "must be one of:", toString(0:3)))
      args["distribType"] <- 2
    }
  }
  return(args)
}

checkRegressionType <- function(args) {
  if (!is.numeric(args$regressionType)) {
    message("'regressionType' must be of numeric type")
    args["regressionType"] <- 0
  }
  else {
    if (is.na(match(args$regressionType, 0:3))) {
      message(paste("regressionType:", args$regressionType, "must be one of:", toString(0:3)))
      args["regressionType"] <- 0
    }
  }
  return(args)
}

checkCorrPlotType <- function(args) {
  corrPlotTypes <- c("Empty", "Circles", "Text", "AbsText")
  if (is.null(args$corrPlotType) || is.na(match(args$corrPlotType, corrPlotTypes))) {
    message(paste(
      "corrPlotType:",
      args$corrPlotType,
      "must be a valid correlation plot type, it must be one of:",
      toString(corrPlotTypes)
    ))
    args$corrPlotType <- corrPlotTypes[1]
  }
  return(args)
}

checkCorrPlotCS <- function(args) {
  continuousCSList <- c(
    "Viridis", "Inferno", "Magma", "Plasma", "Warm", "Cool", "Rainbow", "CubehelixDefault",
    "Blues", "Greens", "Greys", "Oranges", "Purples", "Reds", "BuGn", "BuPu", "GnBu", "OrRd",
    "PuBuGn", "PuBu", "PuRd", "RdBu", "RdPu", "YlGnBu", "YlGn", "YlOrBr", "YlOrRd"
  )
  if (!is.null(args$corrPlotCS) && is.na(match(args$corrPlotCS, continuousCSList))) {
    message(paste(
      "corrPlotCS:",
      args$corrPlotCS,
      "must be a valid continuous color scale name, it must be one of:",
      toString(continuousCSList)
    ))
    args$corrPlotCS <- NULL
  }
  return(args)
}

checkRotateTitle <- function(args) {
  if (!is.logical(args$rotateTitle)) {
    message("'rotateTitle' must be of logical type")
    args["rotateTitle"] <- FALSE
  }
  return(args)
}

checkColumnLabels <- function(args) {
  colCount <- ncol(args$data)
  if (!is.null(args$columnLabels) && !is.vector(args$columnLabels)) {
    message("'columnLabels' must be a vector")
    args["columnLabels"] <- list(NULL)
  }
  if (!is.null(args$columnLabels)) {
    if (colCount != length(args$columnLabels)) {
      message("Length of 'columnLabels' must be equal to the number of columns of 'data'")
      args["columnLabels"] <- list(NULL)
    }
    else {
      for (i in seq_len(length(args$columnLabels))) {
        if (!is.character(args$columnLabels[[i]])) {
          message(paste("columnLabels", i, "must be of character type"))
          args[["columnLabels"]][i] <- list(NULL)
        }
      }
    }
  }
  return(args)
}
  
checkContinuousCS <- function(args) {
  continuousCSList <- c(
    "Viridis", "Inferno", "Magma", "Plasma", "Warm", "Cool", "Rainbow", "CubehelixDefault",
    "Blues", "Greens", "Greys", "Oranges", "Purples", "Reds", "BuGn", "BuPu", "GnBu", "OrRd",
    "PuBuGn", "PuBu", "PuRd", "RdBu", "RdPu", "YlGnBu", "YlGn", "YlOrBr", "YlOrRd"
  )
  if (is.null(args$continuousCS) || is.na(match(args$continuousCS, continuousCSList))) {
    message(paste(
      "continuousCS:",
      args$continuousCS,
      "must be a valid continuous color scale name, it must be one of:",
      toString(continuousCSList)
    ))
    args$continuousCS <- continuousCSList[1]
  }
  return(args)
}

checkCategoricalCS <- function(args) {
  categoricalCSList <- c("Category10", "Accent", "Dark2", "Paired", "Set1")
  if (is.null(args$categoricalCS) || is.na(match(args$categoricalCS, categoricalCSList))) {
    message(paste(
      "categoricalCS:",
      args$categoricalCS,
      "must be a valid categorical color scale name, it must be one of:",
      toString(categoricalCSList)
    ))
    args["categoricalCS"] <- categoricalCSList[1]
  }
  return(args)
}

checkMouseMode <- function(args) {
  mouseModeList <- c("tooltip", "filter", "zoom")
  if (is.null(args$mouseMode) || is.na(match(args$mouseMode, mouseModeList))) {
    message(paste(
      "mouseMode:",
      args$mouseMode,
      "must be a valid mouse mode, it must be one of:",
      toString(mouseModeList)
    ))
    args$mouseMode <- "tooltip"
  }
  return(args)
}

checkScatterPlotProperties <- function(args) {
  if (!is.null(args$plotProperties) && !is.list(args$plotProperties)) {
    message("'plotProperties' must be a list")
    args["plotProperties"] <- list(NULL)
  }
  if (!is.null(args$plotProperties)) {
    validKeys <- c("noCatColor", "watermarkColor", "point", "regression")
    unknownKeys <- setdiff(names(args$plotProperties), validKeys)
    if (length(unknownKeys) != 0) {
      message(paste0("plotProperties constains invalid properties: ", toString(unknownKeys),
        ". Valid properties are:", toString(validKeys)))
      args["plotProperties"] <- list(NULL)
    }
  }
  return(args)
}

checkSlidersPosition <- function(args) {
  if (!is.null(args$slidersPosition) && !is.list(args$slidersPosition)) {
    message("'slidersPosition' must be a list")
    args["slidersPosition"] <- list(NULL)
  }
  if (!is.null(args$slidersPosition)) {
    validKeys <- c("dimCount", "xStartingDimIndex", "yStartingDimIndex")
    unknownKeys <- setdiff(names(args$slidersPosition), validKeys)
    if (length(unknownKeys) != 0) {
      message(paste0("slidersPosition constains invalid properties: ", toString(unknownKeys),
        ". Valid properties are:", toString(validKeys)))
      args["slidersPosition"] <- list(NULL)
    }
  }
  return(args)
}

checkEventInputId <- function(args) {
  if (!is.null(args$eventInputId) && !is.character(args$eventInputId)) {
    message("'eventInputId' must be of character type")
    args["eventInputId"] <- list(NULL)
  }
  return(args)
}

#' Distribution plots
#' 
#' Tells which type of representation to use for distribution plots.
#' @param id
#'   Output variable to read from (id which references the requested plot).
#' @param distribType
#'   Binary code indicating the type of distribution plot (bit 1: histogram, bit 2: density plot).
#'
#' @return
#'   No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'      selectInput(
#'        "distribType",
#'        "Distribution Representation:",
#'        choices = list("Histogram" = 2, "Density Plot" = 1),
#'        selected = 2
#'      ),
#'      p("The selector controls type of representation to use for distribution plots"),
#'      scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'      output$spMatrix <- renderScatterPlotMatrix({
#'        scatterPlotMatrix(iris)
#'      })
#'      observeEvent(input$distribType, {
#'        scatterPlotMatrix::setDistribType("spMatrix", input$distribType)
#'      })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setDistribType <- function(id, distribType) {
  method <- "setDistribType" # nolint
  callJS()
}

#' Regression plots
#' 
#' Tells which type of regression to use for regression plots.
#' @param id
#'   Output variable to read from (id which references the requested plot).
#' @param regressionType
#'   Binary code indicating the type of regression plot (bit 1: linear, bit 2: loess).
#'
#' @return
#'   No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'      checkboxInput("linearRegressionCB", "Linear Regression", FALSE),
#'      checkboxInput("loessCB", "Local Polynomial Regression", FALSE),
#'      p("The chech boxes controls type of regression to use for regression plots"),
#'      scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'      output$spMatrix <- renderScatterPlotMatrix({
#'        scatterPlotMatrix(iris)
#'      })
#'      observe({
#'        linearFlag <- ifelse(input$linearRegressionCB, 1, 0)
#'        loessFlag <- ifelse(input$loessCB, 2, 0)
#'        scatterPlotMatrix::setRegressionType("spMatrix", linearFlag + loessFlag)
#'      })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setRegressionType <- function(id, regressionType) {
  method <- "setRegressionType" # nolint
  callJS()
}

#' Correlation plot type
#' 
#' Tells which type of correlation plot to use.
#' @param id
#'   Output variable to read from (id which references the requested plot).
#' @param corrPlotType
#'   One of the available correlation plot types (`Empty`, `Circles`, `Text`, `AbsText`).
#'
#' @return
#'   No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'      selectInput(
#'        "corrPlotTypeSelect",
#'        "Correlation Plot Type:",
#'        choices = list(
#'          "Empty" = "Empty",
#'          "Circles" = "Circles",
#'          "Text" = "Text",
#'          "AbsText" = "AbsText"
#'        ),
#'        selected = "Circles"
#'      ),
#'      p("The selector controls the type of correlation to use"),
#'      scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'      output$spMatrix <- renderScatterPlotMatrix({
#'        scatterPlotMatrix(iris, zAxisDim = "Sepal.Length", continuousCS = "Plasma")
#'      })
#'      observeEvent(input$corrPlotTypeSelect, {
#'        scatterPlotMatrix::setCorrPlotType("spMatrix", input$corrPlotTypeSelect)
#'      })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setCorrPlotType <- function(id, corrPlotType) {
  method <- "setCorrPlotType" # nolint
  callJS()
}

#' Color scale for correlation plots
#'
#' Tells which color scale to use for correlation plots.
#' @param id
#'   Output variable to read from (id which references the requested plot).
#' @param corrPlotCsId
#'   One of the available color scale ids
#'   (`Viridis`, `Inferno`, `Magma`, `Plasma`, `Warm`, `Cool`, `Rainbow`, `CubehelixDefault`,
#'    `Blues`, `Greens`, `Greys`, `Oranges`, `Purples`, `Reds`, `BuGn`, `BuPu`, `GnBu`, `OrRd`,
#'    `PuBuGn`, `PuBu`, `PuRd`, `RdBu`, `RdPu`, `YlGnBu`, `YlGn`, `YlOrBr`, `YlOrRd`).
#'
#' @return No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'      selectInput(
#'        "corrPlotCsSelect",
#'        "Correlation Plot Color Scale:",
#'        choices = list(
#'          "Viridis" = "Viridis", "Inferno" = "Inferno", "Magma" = "Magma",
#'          "Plasma" = "Plasma", "Warm" = "Warm", "Cool" = "Cool", "Rainbow" ="Rainbow",
#'          "CubehelixDefault" = "CubehelixDefault", "Blues" = "Blues",
#'          "Greens" = "Greens", "Greys" = "Greys", "Oranges" = "Oranges",
#'          "Purples" = "Purples", "Reds" = "Reds", "BuGn" = "BuGn", "BuPu" = "BuPu",
#'          "GnBu" = "GnBu", "OrRd" = "OrRd", "PuBuGn" = "PuBuGn", "PuBu" = "PuBu",
#'          "PuRd" = "PuRd", "RdBu" = "RdBu", "RdPu" = "RdPu", "YlGnBu" = "YlGnBu",
#'          "YlGn" = "YlGn", "YlOrBr" = "YlOrBr", "YlOrRd" = "YlOrRd"
#'        ),
#'        selected = "Plasma"
#'      ),
#'      p("The selector controls the color scale to use for correlation plot
#'         when plot type is 'Text' or 'AbsText'"),
#'      scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'      output$spMatrix <- renderScatterPlotMatrix({
#'        scatterPlotMatrix(iris, corrPlotType = "Text")
#'      })
#'      observeEvent(input$corrPlotCsSelect, {
#'        scatterPlotMatrix::setCorrPlotCS("spMatrix", input$corrPlotCsSelect)
#'      })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setCorrPlotCS <- function(id, corrPlotCsId) {
  method <- "setCorrPlotCS" # nolint
  callJS()
}

#' Continuous color scale
#' 
#' Tells which color scale to use when the Z axis is set to a continuous column.
#' @param id
#'   Output variable to read from (id which references the requested plot).
#' @param continuousCsId
#'   One of the available color scale ids
#'   (`Viridis`, `Inferno`, `Magma`, `Plasma`, `Warm`, `Cool`, `Rainbow`, `CubehelixDefault`,
#'    `Blues`, `Greens`, `Greys`, `Oranges`, `Purples`, `Reds`, `BuGn`, `BuPu`, `GnBu`, `OrRd`,
#'    `PuBuGn`, `PuBu`, `PuRd`, `RdBu`, `RdPu`, `YlGnBu`, `YlGn`, `YlOrBr`, `YlOrRd`).
#'
#' @return No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'      selectInput(
#'        "continuousCsSelect",
#'        "Continuous Color Scale:",
#'        choices = list(
#'          "Viridis" = "Viridis", "Inferno" = "Inferno", "Magma" = "Magma",
#'          "Plasma" = "Plasma", "Warm" = "Warm", "Cool" = "Cool", "Rainbow" ="Rainbow",
#'          "CubehelixDefault" = "CubehelixDefault", "Blues" = "Blues",
#'          "Greens" = "Greens", "Greys" = "Greys", "Oranges" = "Oranges",
#'          "Purples" = "Purples", "Reds" = "Reds", "BuGn" = "BuGn", "BuPu" = "BuPu",
#'          "GnBu" = "GnBu", "OrRd" = "OrRd", "PuBuGn" = "PuBuGn", "PuBu" = "PuBu",
#'          "PuRd" = "PuRd", "RdBu" = "RdBu", "RdPu" = "RdPu", "YlGnBu" = "YlGnBu",
#'          "YlGn" = "YlGn", "YlOrBr" = "YlOrBr", "YlOrRd" = "YlOrRd"
#'        ),
#'        selected = "Viridis"
#'      ),
#'      p("The selector controls the colors used when reference column is of type continuous"),
#'      scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'      output$spMatrix <- renderScatterPlotMatrix({
#'        scatterPlotMatrix(iris, zAxisDim = "Sepal.Length")
#'      })
#'      observeEvent(input$continuousCsSelect, {
#'        scatterPlotMatrix::setContinuousColorScale("spMatrix", input$continuousCsSelect)
#'      })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setContinuousColorScale <- function(id, continuousCsId) {
  method <- "setContinuousColorScale" # nolint
  callJS()
}

#' Categorical color scale
#' 
#' Tells which color scale to use when the Z axis is set to a categorical column.
#' @param id
#'   Output variable to read from (id which references the requested plot).
#' @param categoricalCsId
#'   One of the available color scale ids (Category10, Accent, Dark2, Paired, Set1).
#'
#' @return
#'   No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'      selectInput(
#'        "categoricalCsSelect",
#'        "Categorical Color Scale:",
#'        choices = list(
#'          "Category10" = "Category10", "Accent" = "Accent", "Dark2" = "Dark2",
#'          "Paired" = "Paired", "Set1" = "Set1"
#'        ),
#'        selected = "Category10"
#'      ),
#'      p("The selector controls the colors used when reference column is of type categorical"),
#'      scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'      output$spMatrix <- renderScatterPlotMatrix({
#'        scatterPlotMatrix(iris, zAxisDim = "Species")
#'      })
#'      observeEvent(input$categoricalCsSelect, {
#'        scatterPlotMatrix::setCategoricalColorScale("spMatrix", input$categoricalCsSelect)
#'      })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setCategoricalColorScale <- function(id, categoricalCsId) {
  method <- "setCategoricalColorScale" # nolint
  callJS()
}

#' Cutoffs values
#'
#' Tells which cutoffs to use for each pair of columns.
#'
#' It's possible to filter some points by defining cutoffs to apply to columns.
#'
#' @param id
#'   output variable to read from (id which references the requested plot)
#' @param cutoffs
#'   List of `SpCutoff`; a `SpCutoff` is a list defining a `xDim`, `yDim` and a list of `xyCutoff`;
#'   a `xyCutoff` is a pair of `cutoff` (one for x axis, one for y axis);
#'   a `cutoff` is a list containing two values (min and max values)
#'   or `NULL` if there is no cutoff to apply for this axis;
#'   `NULL` is allowed, meaning there is no cutoff to apply.
#'
#' @return
#'   No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'      checkboxInput("setosaCB", "Setosa", TRUE),
#'      checkboxInput("versicolorCB", "Versicolor", TRUE),
#'      checkboxInput("viginicaCB", "Viginica", TRUE),
#'      scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'      output$spMatrix <- renderScatterPlotMatrix({
#'        scatterPlotMatrix(
#'          data = iris,
#'          zAxisDim = "Species"
#'        )
#'      })
#'
#'      observe({
#'        speciesCBs = c(input$setosaCB, input$versicolorCB, input$viginicaCB)
#'        toKeepIndexes <- Filter(function(i) speciesCBs[i], 1:length(speciesCBs))
#'        xyCutoffs <- sapply(toKeepIndexes, function(i) {
#'          list(list(NULL, c(i - 1.1, i - 0.9)))
#'        })
#'        scatterPlotMatrix::setCutoffs("spMatrix", list(
#'          list(xDim="Sepal.Length", yDim="Species", xyCutoffs = xyCutoffs)
#'        ))
#'      })
#'    }
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setCutoffs <- function(id, cutoffs) {
  method <- "setCutoffs" # nolint
  callJS()
}

#' Row highlight
#'
#' Asks to change the highlighted row.
#'
#' @param id
#'   output variable to read from (id which references the requested plot)
#' @param pointIndex
#'   index of the point to highlight; `NULL` means no point is to highlight.
#'
#' @return
#'   No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive() && require(shiny)) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'        actionButton("highlightPointAction", "Highlight Last Point"),
#'        actionButton("clearHlPointAction", "Remove Highlighting"),
#'        p("These buttons sets/unsets a selected line"),
#'        scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'        output$spMatrix <- renderScatterPlotMatrix({
#'            scatterPlotMatrix(iris)
#'        })
#'        observeEvent(input$highlightPointAction, {
#'            lastRowIndex <- nrow(iris)
#'            scatterPlotMatrix::highlightPoint("spMatrix", lastRowIndex)
#'        })
#'      
#'        observeEvent(input$clearHlPointAction, {
#'            scatterPlotMatrix::highlightPoint("spMatrix", NULL)
#'        })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
highlightPoint <- function(id, pointIndex) {
  method <- "highlightPoint" # nolint
  callJS()
}

#' Column visibility
#'
#' Tells which columns have to be visible.
#'
#' @param id
#'   Output variable to read from (id which references the requested plot).
#' @param keptColumns
#'   Vector of boolean (one for each data column), `FALSE` if column has to be hidden.
#'   A named list can also be provided to only indicate which columns must be assigned to a new visibility.
#'
#' @return
#'   No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'      checkboxInput("hideColumnsCB", "Hide last columns", FALSE),
#'      p("The check box controls the visibility of the two last columns"),
#'      scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'      output$spMatrix <- renderScatterPlotMatrix({
#'        scatterPlotMatrix(iris)
#'      })
#'      observeEvent(input$hideColumnsCB, {
#'        keptColumns <- vapply(
#'          1:ncol(iris),
#'          function(i) {
#'            return(ifelse(input$hideColumnsCB, ncol(iris) - i >= 2, TRUE))
#'          },
#'          logical(1)
#'        )
#'        scatterPlotMatrix::setKeptColumns("spMatrix", keptColumns)
#'      })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setKeptColumns <- function(id, keptColumns) {
  method <- "setKeptColumns" # nolint
  callJS()
}

#' Set mouse interaction type
#' 
#' Three types of mouse interactions are available (`tooltip`, `filter` or `zoom`).
#' @param id
#'   Output variable to read from (id which references the requested plot).
#' @param interactionType
#'   Type of mouse interaction.
#'
#' @return
#'   No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'      selectInput(
#'        "mouseMode",
#'        "Mouse Interactions:",
#'        c("Tooltip" = "tooltip", "Filter" = "filter", "Zoom" = "zoom")
#'      ),
#'      p("The selector controls the type of mouse interactions with the scatterPlotMatrix"),
#'      scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'      output$spMatrix <- renderScatterPlotMatrix({
#'        scatterPlotMatrix(iris)
#'      })
#'      observe({
#'        scatterPlotMatrix::changeMouseMode("spMatrix", input$mouseMode)
#'      })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
changeMouseMode <- function(id, interactionType) {
  method <- "changeMouseMode" # nolint
  callJS()
}

#' Z axis
#' 
#' Tells which column to use as reference to determine color of each points.
#' 
#' @param id
#'   Output variable to read from (id which references the requested plot).
#' @param dim
#'   name of the column to use as reference.
#'
#' @return
#'   No return value, called from shiny applications for side effects.
#'
#' @examples
#'  if(interactive()) {
#'    library(shiny)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'      fluidRow(
#'        column(
#'          2,
#'          selectInput("zAxisSelect", "Z Axis:", colnames(iris))
#'        ),
#'        column(
#'          2,
#'          checkboxInput("zAxisUsedCB", "Use Z Axis", FALSE)
#'        )
#'      ),
#'      scatterPlotMatrixOutput("spMatrix")
#'    )
#'
#'    server <- function(input, output, session) {
#'      output$spMatrix <- renderScatterPlotMatrix({
#'        scatterPlotMatrix(iris)
#'      })
#'
#'      observe({
#'        scatterPlotMatrix::setZAxis("spMatrix", if (input$zAxisUsedCB) input$zAxisSelect else NULL)
#'      })
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
setZAxis <- function(id, dim) {
  method <- "setZAxis" # nolint
  callJS()
}

#' Retrieve plot configuration
#' 
#' Result will be sent through a reactive input (see example below).
#' @param id
#'   Output variable to read from (id which references the requested plot).
#' @param configInputId
#'   Reactive input to write to.
#'
#' @return
#'   No return value, called from shiny applications for side effects.
#'
#' @examples
#' \dontrun{
#'    library(shiny)
#'    library(shinyjs)
#'    library(scatterPlotMatrix)
#'
#'    ui <- fluidPage(
#'        useShinyjs(),
#'        p("The button allows to save the widget as an html file, reproducing its configuration"),
#'        actionButton("downloadButton", "Download Widget"),
#'        downloadButton("associatedDownloadButton", "Download Widget",
#'            style = "visibility: hidden;"
#'        ),
#'        scatterPlotMatrixOutput("spMatrix", height = 960)
#'    )
#'
#'    server <- function(input, output, session) {
#'        output$spMatrix <- renderScatterPlotMatrix({
#'            scatterPlotMatrix(iris)
#'        })
#'        observeEvent(input$downloadButton, {
#'            scatterPlotMatrix::getPlotConfig("spMatrix", "ConfigForDownload")
#'        })
#'        observeEvent(input$ConfigForDownload, {
#'          spmForDownload <<- scatterPlotMatrix(
#'            data = iris,
#'            categorical = input$ConfigForDownload$categorical,
#'            inputColumns = input$ConfigForDownload$inputColumns,
#'            cutoffs = input$ConfigForDownload$cutoffs,
#'            keptColumns = input$ConfigForDownload$keptColumns,
#'            zAxisDim = input$ConfigForDownload$zAxisDim,
#'            distribType = as.numeric(input$ConfigForDownload$distribType),
#'            regressionType = as.numeric(input$ConfigForDownload$regressionType),
#'            corrPlotType = input$ConfigForDownload$corrPlotType,
#'            corrPlotCS = input$ConfigForDownload$corrPlotCS,
#'            rotateTitle = input$ConfigForDownload$rotateTitle,
#'            columnLabels = input$ConfigForDownload$columnLabels,
#'            continuousCS = input$ConfigForDownload$continuousCS,
#'            categoricalCS = input$ConfigForDownload$categoricalCS,
#'            mouseMode = input$ConfigForDownload$mouseMode,
#'            controlWidgets = NULL,
#'            cssRules = input$ConfigForDownload$cssRules,
#'            plotProperties = input$ConfigForDownload$plotProperties,
#'            slidersPosition = input$ConfigForDownload$slidersPosition
#'          )
#'          shinyjs::runjs("document.getElementById('associatedDownloadButton').click();")
#'        })
#'        output$associatedDownloadButton <- downloadHandler(
#'          filename = function() {
#'            paste("scatterPlotMatrix-", Sys.Date(), ".html", sep = "")
#'          },
#'          content = function(tmpContentFile) {
#'            htmlwidgets::saveWidget(spmForDownload, tmpContentFile)
#'          }
#'        )
#'    }
#'
#'    shinyApp(ui, server)
#'  }
#'
#' @export
getPlotConfig <- function(id, configInputId) {
  method <- "getPlotConfig" # nolint
  callJS()
}

callJS <- function() {
  message <- Filter(function(x) !is.symbol(x), as.list(parent.frame(1)))
  session <- shiny::getDefaultReactiveDomain()
  method <- paste0("scatterPlotMatrix:", message$method)
  session$sendCustomMessage(method, message)
}
