#' A character string vector of 1240 NZ river names
#' @source \url{https://data.linz.govt.nz/layer/103631-nz-river-name-polygons-pilot/}
"rivers"

#' A vector of length 16 specifying the names of NZ provinces
"regions"

#' A dataset specifying the titles
#' and layer id numbers of available data layers from
#' \href{MfE}{https://data.mfe.govt.nz/layers/}
#' @format A data frame with 613 rows and 2 variables:
#' \describe{
#' \item{title}{Title of the dataset}
#' \item{id}{Layer identifier}
#' }
#' @source \url{https://data.mfe.govt.nz/layers/}
"mfe_names"

#' A dataset specifying the titles, url tags,
#' and layer id numbers of available data layers from
#' {NIWA}{https://data-niwa.opendata.arcgis.com/datasets/}
#' @format A data frame with 44 rows and 3 variables:
#' \describe{
#' \item{title}{Title of the dataset}
#' \item{tag}{url tag of dataset}
#' \item{id}{Layer identifier}
#' }
#' @source \url{https://data-niwa.opendata.arcgis.com/datasets/}
"niwa_names"
