#' Function to test if values in a vector are unique
is_unique <- function(x) {
    length(table(x)) == 1
}
#' Funtion to "average" (ish) across rgb values of a vector of colours
av_rgb <- function(x) {
    rgb <- col2rgb(x)
    av <- apply(rgb, 1, mean)
    col <- rgb(red = av[1], green = av[2], blue = av[3], maxColorValue = 255)
    return(col)
}
#' Function to "get" triangle vertices from vectors of hex coords
get_tri <- function(x, y, cenx, ceny) {
    tx <- c(rep(x, times = c(1, 2, 2, 2, 2, 2)), x[1])
    tx <- matrix(tx, nrow = 2)
    tx <- rbind(tx, cenx)
    ty <- c(rep(y, times = c(1, 2, 2, 2, 2, 2)), y[1])
    ty <- matrix(ty, nrow = 2)
    ty <- rbind(ty, ceny)
    return(data.frame(x = c(tx), y = c(ty)))
}
#' Function to prep sf data for geom_hex
#' @param x object of class \code{sf}
#' @export
prep_hex <- function(x){
    cent <- sf::st_centroid(x)
    coords <- sf::st_coordinates(cent)
    x$X <- coords[,1]; x$Y <- coords[,2]
    return(x)
}
