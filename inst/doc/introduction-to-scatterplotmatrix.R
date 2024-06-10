## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 7
)

## ----setup--------------------------------------------------------------------
library(scatterPlotMatrix)

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris)

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, slidersPosition = list(
  dimCount = 3, # Number of columns to draw
  xStartingDimIndex = 2, # Index of first drawn column horizontally
  yStartingDimIndex = 2 # Index of first drawn column vertically
))

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, zAxisDim = "Species")

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, zAxisDim = "Species", categoricalCS = "Set1")

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, zAxisDim = "Sepal.Length")

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, zAxisDim = "Sepal.Length", continuousCS = "YlOrRd")

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, corrPlotType = "Text")

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, corrPlotType = "AbsText")

## -----------------------------------------------------------------------------
scatterPlotMatrix(mtcars)

## -----------------------------------------------------------------------------
categorical <- list(cyl = c(4, 6, 8), vs = c(0, 1), am = c(0, 1), gear = 3:5, carb = 1:8)
scatterPlotMatrix(mtcars, categorical = categorical, zAxisDim = "cyl")

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, zAxisDim = "Species", distribType = 1)

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, zAxisDim = "Species", regressionType = 1)

## -----------------------------------------------------------------------------
cutoffs <- list(
  list(
    xDim = "Sepal.Length",
    yDim = "Species",
    xyCutoffs = list(
      list(c(4, 8), c(-0.1, 0.1)),
      list(c(4, 8), c(1.9, 2.1)))
    )
)
scatterPlotMatrix(iris, zAxisDim = "Species", cutoffs = cutoffs)

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, zAxisDim = "Species", rotateTitle = TRUE)

## -----------------------------------------------------------------------------
columnLabels <- gsub("\\.", "<br>", colnames(iris))
scatterPlotMatrix(iris, zAxisDim = "Species", columnLabels = columnLabels)

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, cssRules = list(
    ".jitterZone" = "fill: pink", # Set background of plot to pink
    ".tick text" = c("fill: red", "font-size: 1.8em") # Set text of axes ticks red and greater
))

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, plotProperties = list(
  noCatColor = "DarkCyan", # Color used when categories coloring is not applied
  point = list(
    alpha = 0.3, # Opacity value used for points
    radius = 4 # Radius used to draw points as circles
  )
))

## -----------------------------------------------------------------------------
scatterPlotMatrix(iris, controlWidgets = TRUE)

