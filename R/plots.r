#' internal funtion to plot downloaded spatial object
#' @importFrom sf st_as_sf
#' @importFrom ggplot2 ggplot geom_sf
show_basic_eeda <- function(x){
    if("sf" %in% class(x)){
        ggplot(x) + geom_sf()
    }else{
        x <- st_as_sf(x)
        show_basic_eeda(x)
    }
}

