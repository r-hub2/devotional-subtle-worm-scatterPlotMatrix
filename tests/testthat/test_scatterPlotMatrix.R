describe("options", {
    # use mtcars dataset
    data(mtcars, iris)

    # ***************
    # 'data' argument
    # ***************
    describe("data", {
        it("should accept a dataframe", {
            data <- data.frame()
            expect_equal(
                scatterPlotMatrix(data)$x$data,
                data
            )
        })

        it("should accept a matrix", {
            data <- matrix()
            expect_equal(
                scatterPlotMatrix(data)$x$data,
                data
            )
        })

        it("should refuse invalid value and throw an error", {
            expect_error(
                scatterPlotMatrix(data = list()),
                "'data' must be a dataframe"
            )
        })
    })

    # **********************
    # 'categorical' argument
    # **********************
    describe("categorical", {
        it("should keep unchanged a valid value", {
            categorical <- list(
                NULL,
                list(4, 6, 8),
                NULL, NULL, NULL, NULL, NULL,
                list(0, 1),
                list(0, 1),
                list(3:5),
                list(1:8)
            )
            expect_equal(
                scatterPlotMatrix(mtcars, categorical = categorical)$x$categorical,
                categorical
            )
        })
        it("should accept equivalent named list value", {
            categorical <- list(
                NULL,
                list(4, 6, 8),
                NULL, NULL, NULL, NULL, NULL,
                list(0, 1),
                list(0, 1),
                list(3:5),
                list(1:8)
            )
            categoricalAsNamedList <- list(
                cyl = list(4, 6, 8),
                vs = list(0, 1),
                am = list(0, 1),
                gear = list(3:5),
                carb = list(1:8)
            )
            expect_equal(
                scatterPlotMatrix(mtcars, categorical = categoricalAsNamedList)$x$categorical,
                categorical
            )
        })
        it("should fix invalid type and print a message", {
            categorical <- 1
            expect_message(
                out <- scatterPlotMatrix(mtcars, categorical = categorical),
                "'categorical' must be a list"
            )
            expect_equal(
                out$x$categorical,
                rep(list(NULL), ncol(mtcars))
            )
        })
        it("should fix invalid elements and print a message", {
            categorical <- list(
                NULL,
                data.frame(),
                NULL, NULL, NULL, NULL, NULL,
                list(0, 1),
                list(0, 1),
                list(3:5),
                list(1:8)
            )
            fixed <- categorical
            fixed[2] <- list(NULL)
            expect_message(
                out <- scatterPlotMatrix(mtcars, categorical = categorical),
                "categorical 2 must be a vector"
            )
            expect_equal(
                out$x$categorical,
                fixed
            )
        })
        it("should accept vector type for elements (by coercing to list)", {
            categorical <- list(NULL, c(4, 6, 8), NULL, NULL, NULL, NULL, NULL, c(0, 1), c(0, 1), 3:5, 1:8)
            fixed <- vapply(
                categorical,
                function(cat) ifelse(is.null(cat), list(NULL), list(as.list(cat))),
                list(1)
            )
            expect_equal(
                scatterPlotMatrix(mtcars, categorical = categorical)$x$categorical,
                fixed
            )
        })
        it("should fix invalid length and print a message", {
            categorical <- list()
            expect_message(
                out <- scatterPlotMatrix(mtcars, categorical = categorical),
                "Length of 'categorical' must be equal to the number of columns of 'data'"
            )
            expect_equal(
                out$x$categorical,
                NULL
            )
        })
        it("should initialize using factor elements", {
            expect_equal(
                scatterPlotMatrix(iris)$x$categorical,
                list(NULL, NULL, NULL, NULL, list("setosa", "versicolor", "virginica"))
            )
        })
    })

    # **********************
    # 'keptColumns' argument
    # **********************
    describe("keptColumns", {
        it("should keep unchanged a valid value", {
            keptColumns <- list(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
            expect_equal(
                scatterPlotMatrix(mtcars, keptColumns = keptColumns)$x$keptColumns,
                keptColumns
            )
        })
        it("should accept equivalent list of column names", {
            keptColumns <- list(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
            keptColumnsAsColumnNames <- list("mpg", "disp", "drat", "qsec", "am", "gear", "carb")
            expect_equal(
                scatterPlotMatrix(mtcars, keptColumns = keptColumnsAsColumnNames)$x$keptColumns,
                keptColumns
            )
        })
        it("should fix invalid type and print a message", {
            keptColumns <- data.frame()
            expect_message(
                out <- scatterPlotMatrix(mtcars, keptColumns = keptColumns),
                "'keptColumns' must be a vector"
            )
            expect_equal(
                out$x$keptColumns,
                NULL
            )
        })
        it("should fix invalid elements and print a message", {
            keptColumns <- list(TRUE, "false", TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
            fixed <- keptColumns
            fixed[2] <- TRUE
            expect_message(
                out <- scatterPlotMatrix(mtcars, keptColumns = keptColumns),
                "keptColumns 2 must be of logical type"
            )
            expect_equal(
                out$x$keptColumns,
                fixed
            )
        })
        it("should fix invalid length and print a message", {
            keptColumns <- list()
            expect_message(
                out <- scatterPlotMatrix(mtcars, keptColumns = keptColumns),
                "Length of 'keptColumns' must be equal to the number of columns of 'data'"
            )
            expect_equal(
                out$x$keptColumns,
                NULL
            )
        })
    })

    # **************************
    # 'cutoffs' argument
    # **************************
    describe("cutoffs", {
        it("should keep unchanged a valid value", {
            cutoffs <- list(list(
                xDim = "Sepal.Length",
                yDim = "Species",
                xyCutoffs = list(
                    list(c(4, 8), c(-0.1, 0.1)),
                    list(c(4, 8), c(1.9, 2.1))
                )
            ))
            expect_equal(
                scatterPlotMatrix(iris, cutoffs = cutoffs)$x$cutoffs,
                cutoffs
            )
        })

        it("should fix invalid type and print a message", {
            cutoffs <- 1
            expect_message(
                out <- scatterPlotMatrix(iris, cutoffs = cutoffs),
                "'cutoffs' must be a list"
            )
            expect_equal(
                out$x$cutoffs,
                NULL
            )
        })

        it("should fix invalid spCutoffs and print a message", {
            cutoffs <- list(list(wrong = ""))
            expect_message(
                out <- scatterPlotMatrix(iris, cutoffs = cutoffs),
                "cutoff 1 doesn't contains expected fields"
            )
            expect_equal(
                out$x$cutoffs,
                NULL
            )
        })

    })

    # ***********************
    # 'inputColumns' argument
    # ***********************
    describe("inputColumns", {
        it("should keep unchanged a valid value", {
            inputColumns <- list(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
            expect_equal(
                scatterPlotMatrix(mtcars, inputColumns = inputColumns)$x$inputColumns,
                inputColumns
            )
        })
        it("should accept equivalent list of column names", {
            inputColumns <- list(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
            inputColumnsAsColumnNames <- list("mpg", "disp", "drat", "qsec", "am", "gear", "carb")
            expect_equal(
                scatterPlotMatrix(mtcars, inputColumns = inputColumnsAsColumnNames)$x$inputColumns,
                inputColumns
            )
        })
        it("should fix invalid type and print a message", {
            inputColumns <- data.frame()
            expect_message(
                out <- scatterPlotMatrix(mtcars, inputColumns = inputColumns),
                "'inputColumns' must be a vector"
            )
            expect_equal(
                out$x$inputColumns,
                NULL
            )
        })
        it("should fix invalid elements and print a message", {
            inputColumns <- list(TRUE, "false", TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
            fixed <- inputColumns
            fixed[2] <- TRUE
            expect_message(
                out <- scatterPlotMatrix(mtcars, inputColumns = inputColumns),
                "inputColumns 2 must be of logical type"
            )
            expect_equal(
                out$x$inputColumns,
                fixed
            )
        })
        it("should fix invalid length and print a message", {
            inputColumns <- list()
            expect_message(
                out <- scatterPlotMatrix(mtcars, inputColumns = inputColumns),
                "Length of 'inputColumns' must be equal to the number of columns of 'data'"
            )
            expect_equal(
                out$x$inputColumns,
                NULL
            )
        })
    })

    # **********************
    # 'zAxisDim' argument
    # **********************
    describe("zAxisDim", {
        it("should keep unchanged a valid value", {
            zAxisDim <- "cyl"
            expect_equal(
                scatterPlotMatrix(mtcars, zAxisDim = zAxisDim)$x$zAxisDim,
                zAxisDim
            )
        })
        it("should fix invalid value and print a message", {
            expect_message(
                out <- scatterPlotMatrix(mtcars, zAxisDim = 1),
                paste("must be a valid column dimension, it must be one of:", toString(colnames(mtcars)))
            )
            expect_equal(
                out$x$zAxisDim,
                NULL
            )
        })
    })

    # **********************
    # 'distribType' argument
    # **********************
    describe("distribType", {
        it("should fix invalid type and print a message", {
            distribType <- data.frame()
            expect_message(
                out <- scatterPlotMatrix(mtcars, distribType = distribType),
                "'distribType' must be of numeric type"
            )
            expect_equal(
                out$x$distribType,
                2
            )
        })

        it("should keep unchanged a valid value 1", {
            expect_equal(
                scatterPlotMatrix(data.frame(), distribType = 1)$x$distribType,
                1
            )
        })

        it("should keep unchanged a valid value 2", {
            expect_equal(
                scatterPlotMatrix(data.frame(), distribType = 2)$x$distribType,
                2
            )
        })

        it("should fix invalid value and print a message", {
            expect_message(
                out <- scatterPlotMatrix(data.frame(), distribType = 5),
                paste("must be one of:", toString(0:3))
            )
            expect_equal(
                out$x$distribType,
                2
            )
        })
    })

    # *************************
    # 'regressionType' argument
    # *************************
    describe("regressionType", {
        it("should fix invalid type and print a message", {
            regressionType <- data.frame()
            expect_message(
                out <- scatterPlotMatrix(mtcars, regressionType = regressionType),
                "'regressionType' must be of numeric type"
            )
            expect_equal(
                out$x$regressionType,
                0
            )
        })

        it("should keep unchanged a valid value 1", {
            expect_equal(
                scatterPlotMatrix(data.frame(), regressionType = 1)$x$regressionType,
                1
            )
        })

        it("should keep unchanged a valid value 2", {
            expect_equal(
                scatterPlotMatrix(data.frame(), regressionType = 2)$x$regressionType,
                2
            )
        })

        it("should fix invalid value and print a message", {
            expect_message(
                out <- scatterPlotMatrix(data.frame(), regressionType = 5),
                paste("must be one of:", toString(0:3))
            )
            expect_equal(
                out$x$regressionType,
                0
            )
        })
    })

    # ***********************
    # 'corrPlotType' argument
    # ***********************
    describe("corrPlotType", {
        corrPlotTypes <- c("Empty", "Circles", "Text", "AbsText")
        for (type in corrPlotTypes) {
            it(paste("should keep unchanged a valid value", type), {
                expect_equal(
                    scatterPlotMatrix(mtcars, corrPlotType = type)$x$corrPlotType,
                    type
                )
            })
        }
        it("should fix invalid value and print a message", {
            expect_message(
                out <- scatterPlotMatrix(mtcars, corrPlotType = 1),
                paste("must be a valid correlation plot type, it must be one of:", toString(corrPlotTypes))
            )
            expect_equal(
                out$x$corrPlotType,
                corrPlotTypes[1]
            )
        })
    })

    # *********************
    # 'corrPlotCS' argument
    # *********************
    describe("corrPlotCS", {
        continuousCSList <- c(
            "Viridis", "Inferno", "Magma", "Plasma", "Warm", "Cool", "Rainbow",
            "CubehelixDefault", "Blues", "Greens", "Greys", "Oranges", "Purples",
            "Reds", "BuGn", "BuPu", "GnBu", "OrRd", "PuBuGn", "PuBu", "PuRd", "RdBu",
            "RdPu", "YlGnBu", "YlGn", "YlOrBr", "YlOrRd"
        )
        for (cs in continuousCSList) {
            it(paste("should keep unchanged a valid value", cs), {
                expect_equal(
                    scatterPlotMatrix(mtcars, corrPlotCS = cs)$x$corrPlotCS,
                    cs
                )
            })
        }
        it("should fix invalid value and print a message", {
            expect_message(
                out <- scatterPlotMatrix(mtcars, corrPlotCS = 1),
                paste("must be a valid continuous color scale name, it must be one of:", toString(continuousCSList))
            )
            expect_equal(
                out$x$corrPlotCS,
                NULL
            )
        })
    })

    # **********************
    # 'rotateTitle' argument
    # **********************
    describe("rotateTitle", {
        it("should keep unchanged a valid value TRUE", {
            expect_equal(
                scatterPlotMatrix(data.frame(), rotateTitle = TRUE)$x$rotateTitle,
                TRUE
            )
        })

        it("should keep unchanged a valid value FALSE", {
            expect_equal(
                scatterPlotMatrix(data.frame(), rotateTitle = FALSE)$x$rotateTitle,
                FALSE
            )
        })

        it("should fix invalid value and print a message", {
            expect_message(
                out <- scatterPlotMatrix(data.frame(), rotateTitle = 1),
                "'rotateTitle' must be of logical type"
            )
            expect_equal(
                out$x$rotateTitle,
                FALSE
            )
        })
    })

    # ***********************
    # 'columnLabels' argument
    # ***********************
    describe("columnLabels", {
        it("should keep unchanged a valid value", {
            columnLabels <- gsub("\\.", "<br>", colnames(iris))
            expect_equal(
                scatterPlotMatrix(iris, columnLabels = columnLabels)$x$columnLabels,
                columnLabels
            )
        })
        it("should fix invalid type and print a message", {
            columnLabels <- data.frame()
            expect_message(
                out <- scatterPlotMatrix(mtcars, columnLabels = columnLabels),
                "'columnLabels' must be a vector"
            )
            expect_equal(
                out$x$columnLabels,
                NULL
            )
        })
        it("should fix invalid elements and print a message", {
            columnLabels <- list(
                "Col 1", FALSE, "Col 3", "Col 4", "Col 5", "Col 6",
                "Col 7", "Col 8", "Col 9", "Col 10", "Col 11"
            )
            fixed <- columnLabels
            fixed[2] <- list(NULL)
            expect_message(
                out <- scatterPlotMatrix(mtcars, columnLabels = columnLabels),
                "columnLabels 2 must be of character type"
            )
            expect_equal(
                out$x$columnLabels,
                fixed
            )
        })
        it("should fix invalid length and print a message", {
            columnLabels <- list()
            expect_message(
                out <- scatterPlotMatrix(mtcars, columnLabels = columnLabels),
                "Length of 'columnLabels' must be equal to the number of columns of 'data'"
            )
            expect_equal(
                out$x$columnLabels,
                NULL
            )
        })
    })

    # ***********************
    # 'continuousCS' argument
    # ***********************
    describe("continuousCS", {
        continuousCSList <- c(
            "Viridis", "Inferno", "Magma", "Plasma", "Warm", "Cool", "Rainbow",
            "CubehelixDefault", "Blues", "Greens", "Greys", "Oranges", "Purples",
            "Reds", "BuGn", "BuPu", "GnBu", "OrRd", "PuBuGn", "PuBu", "PuRd", "RdBu",
            "RdPu", "YlGnBu", "YlGn", "YlOrBr", "YlOrRd"
        )
        for (cs in continuousCSList) {
            it(paste("should keep unchanged a valid value", cs), {
                expect_equal(
                    scatterPlotMatrix(mtcars, continuousCS = cs)$x$continuousCS,
                    cs
                )
            })
        }
        it("should fix invalid value and print a message", {
            expect_message(
                out <- scatterPlotMatrix(mtcars, continuousCS = 1),
                paste("must be a valid continuous color scale name, it must be one of:", toString(continuousCSList))
            )
            expect_equal(
                out$x$continuousCS,
                continuousCSList[1]
            )
        })
    })

    # ************************
    # 'categoricalCS' argument
    # ************************
    describe("categoricalCS", {
        categoricalCSList <- c("Category10", "Accent", "Dark2", "Paired", "Set1")
        for (cs in categoricalCSList) {
            it(paste("should keep unchanged a valid value", cs), {
                expect_equal(
                    scatterPlotMatrix(mtcars, categoricalCS = cs)$x$categoricalCS,
                    cs
                )
            })
        }
        it("should fix invalid value and print a message", {
            expect_message(
                out <- scatterPlotMatrix(mtcars, categoricalCS = 1),
                paste("must be a valid categorical color scale name, it must be one of:", toString(categoricalCSList))
            )
            expect_equal(
                out$x$categoricalCS,
                categoricalCSList[1]
            )
        })
    })

    # ********************
    # 'mouseMode' argument
    # ********************
    describe("mouseMode", {
        mouseModeList <- c("tooltip", "filter", "zoom")
        for (mouseMode in mouseModeList) {
            it(paste("should keep unchanged a valid value", mouseMode), {
                expect_equal(
                    scatterPlotMatrix(mtcars, mouseMode = mouseMode)$x$mouseMode,
                    mouseMode
                )
            })
        }
        it("should fix invalid value and print a message", {
            expect_message(
                out <- scatterPlotMatrix(mtcars, mouseMode = 1),
                paste("must be a valid mouse mode, it must be one of:", toString(mouseModeList))
            )
            expect_equal(
                out$x$mouseMode,
                mouseModeList[1]
            )
        })
    })

    # ************************
    # 'eventInputId' argument
    # ************************
    describe("eventInputId", {
        it("should keep unchanged a valid value", {
            eventInputId <- "testingInputId"
            expect_equal(
                scatterPlotMatrix(mtcars, eventInputId = eventInputId)$x$eventInputId,
                eventInputId
            )
        })
        it("should fix invalid value and print a message", {
            expect_message(
                out <- scatterPlotMatrix(mtcars, eventInputId = 1),
                "'eventInputId' must be of character type"
            )
            expect_equal(
                out$x$eventInputId,
                NULL
            )
        })
    })
})