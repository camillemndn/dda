#' Vietnam daily temperature records
#'
#' Daily maximum and minimum temperatures recorded across Vietnamese provinces.
#'
#' @format A data frame with 1890 rows and 5 variables:
#' \describe{
#'   \item{year}{Observation year.}
#'   \item{region}{Administrative region code.}
#'   \item{province}{Province name.}
#'   \item{t_max}{List column of `tf` daily-maximum temperature traces.}
#'   \item{t_min}{List column of `tf` daily-minimum temperature traces.}
#' }
"vietnam_temperature"

#' Distributional version of `vietnam_temperature`
#'
#' Same observation grid as `vietnam_temperature`, with the temperature
#' traces converted into `dd` (distributional data) objects via density
#' estimation per year.
#'
#' @format A `rowwise_df` with 5 columns: `year`, `region`, `province`,
#'   `t_max`, `t_min` — the latter two being list columns of `dd` objects.
"vietnam_temperature_dd"

#' Vietnamese administrative regions
#'
#' Lookup table from region code to region name.
#'
#' @format A data frame with 6 rows and 2 variables:
#' \describe{
#'   \item{code}{Region code.}
#'   \item{name}{Region name.}
#' }
"vietnam_regions"

#' Vietnamese provinces (geographic boundaries)
#'
#' Administrative-province polygons with climate-region tags.
#'
#' @format A `sf` data frame with 63 rows and 4 variables:
#' \describe{
#'   \item{region}{Administrative region code.}
#'   \item{province}{Province name.}
#'   \item{geometry}{`sfc` polygon geometry of the province boundary.}
#'   \item{climate_region}{Climate-region classification.}
#' }
"vietnam_provinces"
