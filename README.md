
# scatterPlotMatrix

'scatterPlotMatrix' is: 
- an interactive scatter plot matrix (thanks to the [d3.js](https://d3js.org/));
- an HTML widget for R that render in various contexts including the R console, 'R Markdown' documents, and 'Shiny' web applications (thanks to the [htmlwidgets](https://www.htmlwidgets.org/) package);
- a Shiny widget that can be updated programmatically and whose some actions can trigger reactions.

As an example, a Shiny application is available on [shinyapps.io](https://detocs.shinyapps.io/ScatterPlotMatrix-ParallelPlot-Link-DataFromFile/).

## Installation

You can install this package from CRAN, or the development version from GitLab:

``` r
# CRAN version
install.packages('scatterPlotMatrix')

# Or GitLab version
if (!require('devtools')) install.packages('devtools')
devtools::install_gitlab(host = 'ifpen-gitlab.appcollaboratif.fr', repo = 'detocs/scatterplotmatrix', subdir = 'htmlwidget/spm')
```

## Examples

``` r
library(scatterPlotMatrix)
categorical <- list(cyl = c(4, 6, 8), vs = c(0, 1), am = c(0, 1), gear = 3:5, carb = 1:8)
scatterPlotMatrix(mtcars, categorical = categorical, zAxisDim = "cyl")
```

See also Shiny testing app:
[https://detocs.shinyapps.io/scatterPlotMatrix-testing-app/](https://detocs.shinyapps.io/ScatterPlotMatrix-ParallelPlot-Link-DataFromFile/)
whose source code is available in [test/testingApp/Shiny-ScatterPlotMatrix-ParallelPlot-CsvFile.R](https://ifpen-gitlab.appcollaboratif.fr/detocs/scatterplotmatrix/-/blob/master/test/testingApp/Shiny-ScatterPlotMatrix-ParallelPlot-CsvFile.R)
