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
#' function to prep sf data for geom_hex
#' @param x object of class \code{sf}
#' @export
prep_hex <- function(x){
    cent <- sf::st_centroid(x)
    coords <- sf::st_coordinates(cent)
    x$X <- coords[,1]; x$Y <- coords[,2]
    return(x)
}
#' function to download a \code{SpatialLinesDataFrame} of NZ rivers
#' @param rivers logical, if \code{TRUE} (default) then a \code{SpatialLinesDataFrame}
#' of NZ rivers is returned
#' @param plot logical, if \code{TRUE} (default) then a plot is drawn
#' @export
hexrec <- function(rivers = TRUE, plot = TRUE){
    if(rivers == FALSE & plot == FALSE) stop("What do you want me to do?")
    url <- "https://github.com/cmjt/hexrec/raw/master/gh-data/rivers_nz.rda?raw=True"
    repmis::source_data(url)
    if(plot){
        cols <- c(RColorBrewer::brewer.pal(4, "Dark2"),
                  rep(RColorBrewer::brewer.pal(8, "Dark2"),75,))
        sf <- sf::st_as_sf(rivers_nz)
        coo <- rep(cols,times = table(sf$NETWORK))
        p <- ggplot2::ggplot(sf) + ggplot2::geom_sf(color = coo) +
            ggplot2::theme_void() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "black"),
                                                   plot.margin = grid::unit(c(0,0,0,0), "mm"),
                                                   axis.text = ggplot2::element_blank(), axis.ticks.length = grid::unit(0, "mm"))
        print(p)
    }
    if(rivers){
        return(rivers_nz)
    }
}
