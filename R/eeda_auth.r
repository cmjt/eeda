#' Store the provided API key into an Environment Variable for use by 'eeda' functions
#'
#' This function stores the provided API key as argument in to an environment
#' variable `SHERPAROMEO_KEY` for further use by other `rromeo` functions.
#'
#' For more information regarding API keys, please refer to dedicated vignette
#' with the following command
#' `vignette("setting_up_api_key", package = "rromeo")`
#'
#' @param key A character string giving the API key to save
#' @param platform A character specifying the data service
#' for which to rgister the key. Options are \code{"mfe"} (default) for \href{MfE}{https://data.mfe.govt.nz/} or
#' \code{"niwa"} for \href{NIWA}{https://niwa.co.nz/}.
#'
#' @export
eeda_auth <- function(key, platform = "mfe") {
    if(!platform %in% c("mfe","niwa")) stop("platform must be either 'mfe' or 'niwa'")
    if(class(key) != "character") stop("key should be a character strng")
    if(platform == "mfe"){
        Sys.setenv("MfE_KEY" = key)
    }else{
        Sys.setenv("NIWA_KEY" = key)
        }
}

#' What API keys are registered
#'
#' Funtion that prints out API keys available for use by `eeda` functions
#'
#' @return A names list of character strings of the available keys
#' if found, else nothing.
#' @export
#' @examples \dontrun{
#' eeda_keys()
#' }
eeda_keys <- function(){
    MfE_KEY <- Sys.getenv("MfE_KEY")
    NIWA_KEY <- Sys.getenv("NIWA_KEY")
    keys <- list(MfE_KEY = MfE_KEY, NIWA_KEY = NIWA_KEY)
    if(length(which(keys == "") > 0)) keys[which(keys == "")] <- NULL
    return(keys)
}
