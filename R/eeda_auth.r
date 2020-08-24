#' Store the provided API key into an Environment Variable for use by 'eeda' functions
#'
#' @param key A character string giving the API key to save
#'
#' @export
eeda_auth <- function(key) {
    if(class(key) != "character") stop("key should be a character strng")
        Sys.setenv("MfE_KEY" = key)
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
    keys <- list(MfE_KEY = MfE_KEY)
    if(length(which(keys == "") > 0)) keys[which(keys == "")] <- NULL
    return(keys)
}
