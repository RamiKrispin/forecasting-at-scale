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

head(features)

features_list <- names(features)
c("process", "PC1", "PC2", "PC3", "process_name")
features_list <- features_list[-which(features_list %in% c("process", "PC1", "PC2", "PC3", "process_name", "area_name"))]





# UI ----
ui <- fluidPage(
    # App title
    titlePanel("Time Series Cluster Analysis"),
    sidebarLayout(
        sidebarPanel(
            width = 2,
            selectInput(
                inputId = "color",
                label = "Select Variable",
                choices = c(features_list, "area_name", "process_name"),
                selected = "area_name",
                multiple = FALSE
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
            plotly::plotlyOutput("series", height = "400px")
        )
    )
)

# Server ----
server <- function(input, output) {
    output$pca_plot <- plotly::renderPlotly({
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
            )
        } else {
            p <- NULL
        }

        return(p)
    })
}


# Run the app ----
shinyApp(ui = ui, server = server)