#' Function to plot NZ river in relation to NZ
#' @param x a \code{SpatialLinesDataFrame} of NZ rivers
#' @inheritParams get_nz_river
#' @export
show_nz_river <- function(x, ...) {
    if(class(x)[1] == "SpatialLinesDataFrame" |
       class(x)[1] == "SpatialPolygonsDataFrame") {
        plot(x, add = TRUE,...)
    }else
        if(class(x)[1] == "sf") {
            p <- ggplot(x) + geom_sf() +
                geom_sf() +
                theme_void()
            print(p)
        }
    
}
