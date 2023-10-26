# Pulling US gas monthly data
# Source EIA API
# https://www.eia.gov/opendata/browser/natural-gas/cons/sum

library(EIAapi)

# Settings 
api_key <- Sys.getenv("EIA_API_KEY")
api_path_gas <- "natural-gas/cons/sum/data/"



# Extract metadata
meta_gas_raw <- eia_metadata(api_path = "natural-gas/cons/sum/data/facet/parent",
                               api_key = api_key)

meta_gas <- meta_gas_raw$data
head(meta_gas)

duoarea <- (meta_gas |> dplyr::select(duoarea) |> dplyr::distinct())$duoarea

# Pull the data
d <- lapply(duoarea, function(i){
    print(i)
    df <- NULL
    df <- eia_get(api_key = api_key,
                  api_path = api_path_gas,
                  frequency = "monthly",
                  data = "value",
                  facets = list(duoarea = i))

    
}) |>
dplyr::bind_rows() |>
dplyr::mutate(date = as.Date(paste(period, "-01", sep = ""))) |>
dplyr::select(area_name = `area-name`, process, process_name = `process-name`, series, date, description = `series-description`, value, units)



head(d)
unique(d$area_name)
unique(d$process)
unique(d$process_name)


d |>
dplyr::filter(process_name == "Vehicle Fuel Consumption") |>
dplyr::arrange(area_name, process_name, date) |>
plotly::plot_ly(x = ~ date, y = ~ value, 
                color = ~ area_name,
                name = ~area_name,
                type = "scatter",
                mode = "line")


# Save the data
us_gas <- d
saveRDS(us_gas, file = "./data/us_gas.RDS")

us_gas_csv <- d |> dplyr::select(area_name, process_name, date,description, value)
write.csv(us_gas_csv, "./data/us_gas.csv", row.names = FALSE)
