TargetDecoyQC <- function(object = NULL,
                          decoy = NULL, score = NULL,
                          log = TRUE, nBins = 50) {

    out <- list(selDecoy = decoy, selScore = score, log = log, nBins = nBins)
    on.exit(return(out))


    # Define UI ---------------------------------------------------------------

    ui <- fluidPage(
        title = "Evaluate Decoys",
        sidebarLayout(
            sidebarPanel(
                actionButton("stop", "Stop app"),
                uiOutput("select_file"),
                hr(),
                uiOutput("select_vars")
            ),
            mainPanel(
                tabsetPanel(type = "tabs",
                    tabPanel("Info",
                        .info_msg(),
                        hr(),
                        h4("Data table with selected variables"),
                        dataTableOutput("fd")
                    ),
                    tabPanel("Histogram",
                        plotOutput("histPlot",
                            dblclick = "hist_dblclick",
                            brush = brushOpts(id = "hist_brush", resetOnNew = TRUE)
                        ),
                        p("Brush and double-click on a selected area to zoom in on x-coordinate.
                               Double click outside a selected area to zoom out"),
                        downloadButton("histPlot_out", label = "Download the plot")
                    ),
                    tabPanel("PP-Plot",
                        plotOutput("PPplot",
                            dblclick = "PPplot_dblclick",
                            brush = brushOpts(id = "PPplot_brush", resetOnNew = TRUE)
                        ),
                        p("Brush and double-click on a selected area to zoom in on y-coordinate.
                               Double click outside a selected area to zoom out"),
                        downloadButton("PPplot_out", label = "Download the plot")
                    )
                )
            )
        )
    ) # END UI definition


    # Define server -----------------------------------------------------------

    server <- function(input, output) {

        # Terminate app
        observeEvent(input$stop, {
            stopApp(returnValue = out)
        })

        ## Make dynamic UI to upload file, but only if no object provided
        output$select_file <- renderUI({
            if (is.null(object)) {
                tagList(
                    hr(),
                    fileInput("infile", "Select a file", accept = ".mzid")
                )
            } else {
                NULL
            }
        })

        in_object <- reactive({
            if (is.null(object)) {
                req(input$infile)
                object <- mzID::mzID(input$infile$datapath)
            }
            TargetDecoy:::.getDF(object)
        })

        ## Make dynamic UI to select variables, depending on object
        output$select_vars <- renderUI({
            decoy_choices <- .find_vars(in_object(), filter = is.logical)
            score_choices <- .find_vars(in_object(), filter = is.numeric)
            tagList(
                checkboxInput("log", "-log10 transform variable?",
                    value = out$log
                ),
                selectInput("decoyVar", "Select Decoy",
                    choices = decoy_choices,
                    selected = out$selDecoy
                ),
                selectInput("scoreVar", "Select Score",
                    choices = score_choices,
                    selected = out$selScore
                ),
                numericInput("nBins", "Number of bins in histogram",
                    value = out$nBins, min = 2, max = 1000
                )
            )
        })

        # Select Data
        data <- reactive({
            req(input$decoyVar, input$scoreVar)

            ## Assign selected variables to output list
            out$selDecoy <<- input$decoyVar
            out$selScore <<- input$scoreVar
            out$log <<- input$log
            out$nBins <<- input$nBins

            TargetDecoy:::decoyScoreTable(
                in_object(),
                decoy = input$decoyVar,
                score = input$scoreVar,
                log10 = input$log
            )
        })

        # Make Data Table
        output$fd <- renderDataTable({ data() })


        ############
        # Histogram#
        ############

        histPlot <- reactive({
            x_lim <- histRanges$x
            y_lim <- histRanges$y
            evalTargetDecoysHist(data(),
                ## Hard-code all variables as they are handled by data()
                decoy = "decoy", score = "score",
                log10 = FALSE,
                nBins = input$nBins,
                zoom = FALSE
            ) +
                coord_cartesian(xlim = x_lim, ylim = y_lim, expand = TRUE)
        })

        output$histPlot <- renderPlot({ histPlot() })

        histRanges <- reactiveValues(x = NULL, y = NULL)
        observeEvent(input$hist_dblclick, {
            brush <- input$hist_brush
            if (!is.null(brush)) {
                histRanges$x <- c(brush$xmin, brush$xmax)
            } else {
                histRanges$x <- NULL
            }
        })

        output$histPlot_out <- downloadHandler(
            filename = "TargetDecoyQC-histogram.png",
            content = function(file) {
                ggsave(file, plot = histPlot())
            }
        )


        #########
        # PP-plot#
        #########
        PPplot <- reactive({
            validate(need(any(!data()[["decoy"]]),
                "At least some non-decoy targets should be present."))

            evalTargetDecoysPPPlot(data(),
                ## Hard-code all variables as they are handled by data()
                decoy = "decoy", score = "score",
                log10 = FALSE,
                zoom = FALSE
            ) +
                coord_cartesian(ylim = PPplotRanges$y)
        })

        output$PPplot <- renderPlot({ PPplot() })

        PPplotRanges <- reactiveValues(x = c(0, 1), y = c(0, 1))
        observeEvent(input$PPplot_dblclick, {
            brush <- input$PPplot_brush
            if (!is.null(brush)) {
                PPplotRanges$y <- c(0, brush$ymax)
            } else {
                PPplotRanges$y <- c(0, 1)
            }
        })

        output$PPplot_out <- downloadHandler(
            filename = "TargetDecoyQC-PPplot.png",
            content = function(file) {
                ggsave(filename = file, plot = PPplot())
            }
        )

    } # END server definition

    app <- list(ui = ui, server = server)
    runApp(app)
}
