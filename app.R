library(shiny)

# Data ----
us_gas <- readRDS(file = "./data/us_gas.RDS")

ts_obj <- us_gas |>
    dplyr::select(date, area_name, process, value) |>
    dplyr::mutate(index = tsibble::yearmonth(date)) |>
    tsibble::as_tsibble(index = index, key = c(area_name, process))

process_table <- us_gas |>
    dplyr::select(process, process_name) |>
    dplyr::distinct()


keys <- attributes(ts_obj)$key

# Create features table ----
features_df <- NULL
features_df <- lapply(1:nrow(keys), function(i) {
    d <- NULL
    print(i)
    d <- ts_obj |>
        dplyr::filter(
            area_name == keys$area_name[i],
            process == keys$process[i]
        )

    s <- TRUE
    # Check for missing values and zeros
    z <- which(d$value == 0)

    m <- which(is.na(d$value))
    if (length(m) > 0) {
        if (length(m) < nrow(d) * 0.1 && length(z) == 0) {
            if (any(diff(m) == 1)) {
                x <- m[which(diff(m) == 1)]
                for (n in x) {
                    d$value[n] <- (d$value[n - 12] + d$value[n - 24] + d$value[n - 36]) / 3
                }

                y <- which(is.na(d$value))
                if (length(y) > 0) {
                    for (n in y) {
                        if (n < nrow(d)) {
                            d$value[n] <- (d$value[n - 1] + d$value[n + 1]) / 2
                        } else {
                            d$value[n] <- (d$value[n - 12] + d$value[n - 24]) / 2
                        }
                    }
                }
            } else {
                for (n in m) {
                    if (n < nrow(d)) {
                        d$value[n] <- (d$value[n - 1] + d$value[n + 1]) / 2
                    } else {
                        d$value[n] <- (d$value[n - 12] + d$value[n - 24]) / 2
                    }
                }
            }
        } else {
            s <- FALSE
        }
    }

    if (s) {
        f <- tsfeatures::tsfeatures(d$value)
        f$arch_stat <- tsfeatures::arch_stat(d$value)
        f <- cbind(f, t(as.data.frame(tsfeatures::autocorr_features(d$value))))
        f$nonlinearity <- tsfeatures::nonlinearity(d$value)
        f <- cbind(f, t(as.data.frame(tsfeatures::pacf_features(d$value))))

        row.names(f) <- NULL
        f$area_name <- keys$area_name[i]
        f$process <- keys$process[i]
        f$nperiods <- NULL
        f$frequency <- NULL
        f$seasonal_period <- NULL
        f$success <- TRUE
    } else {
        f <- data.frame(success = FALSE)
    }

    return(f)
}) |>
    dplyr::bind_rows()


features_clean <- na.omit(features_df)


features_clean <- features_clean |>
    dplyr::filter(success) |>
    dplyr::select(-success)


pca <- features_clean |>
    dplyr::select(-area_name, -process) |>
    prcomp(scale = TRUE)

features <- cbind(features_clean, as.data.frame(pca$x[, 1:3])) |>
    dplyr::left_join(process_table, by = "process")

features_scale <- cbind(scale(features[, 1:25]), features[, c("area_name", "process")])

head(features_scale)

km2 <- kmeans(features_scale[, 1:25], centers = 2, nstart = 25)
km3 <- kmeans(features_scale[, 1:25], centers = 3, nstart = 25)
km4 <- kmeans(features_scale[, 1:25], centers = 4, nstart = 25)
km5 <- kmeans(features_scale[, 1:25], centers = 5, nstart = 25)


features_list <- names(features)
c("process", "PC1", "PC2", "PC3", "process_name")
features_list <- features_list[-which(features_list %in% c("process", "PC1", "PC2", "PC3", "process_name", "area_name"))]



features$cluster2 <- km2[1]$cluster
features$cluster3 <- km3[1]$cluster
features$cluster4 <- km4[1]$cluster
features$cluster5 <- km5[1]$cluster


# UI ----
ui <- fluidPage(
    # App title
    titlePanel("Time Series Cluster Analysis"),
    sidebarLayout(
        sidebarPanel(
            width = 2,
            selectInput(
                inputId = "view",
                label = "View By:",
                choices = c("Variable", "Cluster"),
                selected = "Variable",
                multiple = FALSE
            ),
            conditionalPanel(
                condition = "input.view == 'Variable'",
                selectInput(
                    inputId = "color",
                    label = "Select Variable",
                    choices = c(features_list, "area_name", "process_name"),
                    selected = "area_name",
                    multiple = FALSE
                )
            ),
            conditionalPanel(
                condition = "input.view == 'Cluster'",
                selectInput(
                    inputId = "c_num",
                    label = "Number of Clusters:",
                    choices = c(2:5),
                    selected = 2,
                    multiple = FALSE
                )
            ),
            selectInput(
                inputId = "pc",
                label = "Number of PCs",
                choices = c("Two", "Three"),
                selected = "Two",
                multiple = FALSE
            )
        ),
        mainPanel(
            width = 10,
            plotly::plotlyOutput("pca_plot", height = "500px", width = "100%"),
            conditionalPanel(
                condition = "input.view == 'Cluster'",
                plotly::plotlyOutput("cluster_plot", height = "400px")
            ),
            plotly::plotlyOutput("series", height = "400px")
        )
    )
)

# Server ----
server <- function(input, output) {
    output$pca_plot <- plotly::renderPlotly({
        if (input$view == "Variable") {
            if (input$pc == "Two") {
                p <- plotly::plot_ly(features,
                    x = ~PC1,
                    y = ~PC2,
                    color = ~ get(input$color),
                    type = "scatter",
                    mode = "markers"
                ) #|>
                # plotly::layout(dragmode = "select") |>
                # plotly::event_register("m_s")
            } else {
                p <- plotly::plot_ly(features,
                    x = ~PC1,
                    y = ~PC2,
                    z = ~PC3,
                    color = ~ get(input$color)
                ) #|>
                # plotly::layout(dragmode = "select") |>
                # plotly::event_register("m_s")
            }
        } else if (input$view == "Cluster") {
            if (input$pc == "Two") {
                p <- plotly::plot_ly(features,
                    x = ~PC1,
                    y = ~PC2,
                    color = ~ as.factor(get(paste("cluster", input$c_num, sep = ""))),
                    type = "scatter",
                    mode = "markers"
                ) #|>
                # plotly::layout(dragmode = "select") |>
                # plotly::event_register("m_s")
            } else {
                p <- plotly::plot_ly(features,
                    x = ~PC1,
                    y = ~PC2,
                    z = ~PC3,
                    color = ~ as.factor(get(paste("cluster", input$c_num, sep = "")))
                ) #|>
                # plotly::layout(dragmode = "select") |>
                # plotly::event_register("m_s")
            }
        }

        return(p)
    })


    output$series <- plotly::renderPlotly({
        d <- plotly::event_data("plotly_click")
        if (!is.null(d)) {
            print(d)
            print(d[1, "x"])
            x <- which(features$PC1 == d[1, "x"])
            y <- which(features$PC2 == d[1, "y"])

            if (length(x) == 1 && x == y) {
                d <- us_gas |>
                    dplyr::filter(
                        area_name == features$area_name[x],
                        process == features$process[x]
                    ) |>
                    dplyr::arrange(date)
            }
            p <- plotly::plot_ly(d,
                x = ~date,
                y = ~value,
                type = "scatter",
                mode = "line",
                name = features$area_name[x]
            ) |>
                plotly::layout(
                    title = paste("Natural Gas - ", features$process_name[x], ", ", features$area_name[x]),
                    yaxis = list(title = "MMcf", xaxis = list(title = "Source: EIA API"))
                )
        } else {
            p <- NULL
        }

        return(p)
    })

    output$cluster_plot <- plotly::renderPlotly({
        if (input$view == "Cluster") {
            p1 <- plotly::plot_ly(features,
                y = ~trend,
                color = ~ as.factor(get(paste("cluster", input$c_num, sep = ""))),
                type = "box",
                legendgroup = "c",
                showlegend = TRUE
            ) |>
                plotly::layout(
                    legend = list(title = list(text = "<b> Cluster </b>")),
                    yaxis = list(title = "Trend")
                )


            p2 <- plotly::plot_ly(features,
                y = ~linearity,
                color = ~ as.factor(get(paste("cluster", input$c_num, sep = ""))),
                type = "box",
                legendgroup = "c",
                showlegend = FALSE
            ) |>
                plotly::layout(yaxis = list(title = "Linearity"))


            p3 <- plotly::plot_ly(features,
                y = ~entropy,
                color = ~ as.factor(get(paste("cluster", input$c_num, sep = ""))),
                type = "box",
                legendgroup = "c",
                showlegend = FALSE
            ) |>
                plotly::layout(yaxis = list(title = "Entropy"))


            p4 <- plotly::plot_ly(features,
                y = ~x_acf1,
                color = ~ as.factor(get(paste("cluster", input$c_num, sep = ""))),
                type = "box",
                legendgroup = "c",
                showlegend = FALSE
            ) |>
                plotly::layout(yaxis = list(title = "ACF 1"))

            p <- plotly::subplot(p1, p2, p3, p4, nrows = 2, titleY = TRUE)
        } else {
            p <- NULL
        }

        return(p)
    })
}


# Run the app ----
shinyApp(ui = ui, server = server)