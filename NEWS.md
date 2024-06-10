# scatterPlotMatrix 0.3.0

* Added:
  * fire an event when `cutoffs` are changed
  * fire an event when a row is clicked
  * new API for `highlightPoint` to specify the highlighted row
  * fire an event when a row is highlighted
  * new argument `mouseMode` to specify the type of mouse interaction
  * a named list can also be provided for `categorical` and `cutoffs` arguments
  * a list of column names can also be provided for `inputColumns` and `keptColumns` arguments

* Changed:
  * *breaking change*, if `eventInputId` argument is `NULL`, it means no event is sent (before, it meant a default value was used)
  * javascript versions of the graphs are now available in three formats: [iife](https://esbuild.github.io/api/#format-iife), [CommonJS](https://esbuild.github.io/api/#format-commonjs) and [ECMAScript module](https://esbuild.github.io/api/#format-esm)
  * sliders are more attractive (use a frame rather than a central axis; use rounded edges)

* Fixed:
  * when `renderScatterPlotMatrix` function is called again but without specifying the `plotProperties` attribute, previous value is used (default is not applied)

# scatterPlotMatrix 0.2.0

* Added:
  * new argument `cssRules` to apply CSS rules
  * new argument `scatterPlotProperties` to adjust some properties (size and color of points, etc.)
  * new argument and API `cutoffs` to allow points filtering
  * `multibrush` functionality
  * new `corrPlotType` values (`AbsText` and `Empty`)
  * send a `zAxisChange` event when coloration is changed by clicking a column header

* Changed:
  * smooth density curves/reduce bands number
  * keep correlation circle positions unchanged from one tile to the next (and draw reference circles corresponding to a correlation of 1)
  * when `corrPlotType` is 'Text' and if a categorical variable is selected for coloration, don't use a gradient, use the color associated to each category
  * set position of tooltips to the right side
  * when mouse hovers a point, highlighted point just has to be drawn greater, keeping its color (not black)
  * when mouse hovers a point, print values for all visible columns

* Fixed:
  * generating two times a plot with a categorical `zAxis`, some correlation and distribution plots are wrong
  * wrong display for `CorrelationPlot` values if filtering keeps no points
  * Y axis of distribution plots are not updated when filtered points are changed
  * `setZAxis` should not log a warning when `controlWidgets` is not active
  * don't send a `ZAXIS_EVENT` when the `zAxis` is not set to a new value

# scatterPlotMatrix 0.1.0

* Added a `NEWS.md` file to track changes to the package.
