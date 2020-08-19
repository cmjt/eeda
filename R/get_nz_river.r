#' Function to find a NZ river by name. Rivers, by default, are matched to river segment names
#' specified by \url{https://data.linz.govt.nz/layer/103631-nz-river-name-polygons-pilot/}
#' and on the same network.
#' @param x A character string to search NZ river, can be partial. If specified then
#' \code{begin} is ignored. Any other regular expression can also be used.
#' @param begin A character string of the prefix to river name, if specified then \code{x}
#' is ignored.
#' @param ignore.case Logical. By default \code{TRUE}, speecifies \code{grep} to ignore letter case.
#' @return A character vector of matching river names
#' @examples \dontrun{
#' get_nz_river_name(x = "waika")
#' }
#' @export
get_nz_river_name <- function(x = NULL, begin = NULL, ignore.case = TRUE) {
    if(!is.null(x)){
        if(!is.character(x)) stop("Please provide a character string")
        idx <- grep(x, rivers,ignore.case = ignore.case)
        if(!length(idx) > 0) stop(paste("No river found matching", x))
        names <- factor(rivers[idx])   
    }else{
        if(!is.null(begin)){
            if(!is.character(begin)) stop("Please provide a character string")
            idx <- grep(paste("^", begin, sep = ""), rivers, ignore.case = ignore.case)
            if(!length(idx) > 0) stop(paste("No river found starting with", begin))
            names <- factor(rivers[idx])
        }
    }
    nms <- unique(names)
    num <- length(unique(names))
    if(!is.null(x)) {
        cat(paste(num, " rivers matching ", x, ":", sep = ""), "\n")
        cat(paste(nms, "\n"))
    }else{
        if(!is.null(begin)) {
             cat(paste(num, " rivers begining with ", begin, " found:", sep = ""), "\n")
             cat(paste(nms, "\n"))
        }
    }
    return(nms)
}
#' Download and vizualise all NZ rivers of order 4+
#' @param rivers logical, if \code{TRUE} (default) then a \code{SpatialLinesDataFrame}
#' of NZ rivers is returned
#' @param plot logical, if \code{TRUE} (default) then a plot is drawn
#' @return a \code{SpatialLinesDataFrame} of NZ river networks
#' @examples \dontrun{
#' rivers <- get_nz_rivers()
#' }
#' @export
#' @importFrom grid unit
#' @importFrom sf st_as_sf
#' @importFrom ggplot2 ggplot geom_sf theme_void theme element_rect element_blank
get_nz_rivers <- function(rivers = TRUE, plot = TRUE){
    if(rivers == FALSE & plot == FALSE) stop("What do you want me to do?")
    url <- "https://github.com/cmjt/hexrec/raw/master/gh-data/rivers_nz.rda?raw=True"
    repmis::source_data(url)
    if(plot){
        cols <- c(RColorBrewer::brewer.pal(4, "Dark2"),
                  rep(RColorBrewer::brewer.pal(8, "Dark2"),75,))
        sf <- st_as_sf(rivers_nz)
        coo <- rep(cols,times = table(sf$NETWORK))
        p <- ggplot(sf) + geom_sf(color = coo) +
            theme_void() + theme(plot.background = element_rect(fill = "black"),
                                 plot.margin = unit(c(0,0,0,0), "mm"),
                                 axis.text = element_blank(), axis.ticks.length = unit(0, "mm"))
        print(p)
    }
    if(rivers){
        return(rivers_nz)
    }
}

#' Function to return \code{SpatialLinesDataFrame} of user specified NZ river(s) 
#' @param x A character string of length one specifying either 1. the complete river NZ river name,
#' 2. the NZ province (e.g., \code{x = "Canterbury"}) of rivers to return, or
#' 3. "NZ" the default, the whole of NZ.
#' @param network Logical. If TRUE (default) then all connected rivers & streams are returned to the
#' segmentes named as \code{x}.
#' @param plot Logical If TRUE then a plot of the \code{SpatialLinesDataFrame}
#' in relation to NZ is plotted
#' @param order Optional; a single numeric value or a vector in 4:8 specifying
#' order of river segments
#' @param ... other arguments to pass into \code{show_nz_river()}
#' @return A \code{SpatialLinesDataFrame} or a \code{sf} object
#' @examples \dontrun{
#' waikato <- get_nz_river(x = "Waikato River")
#' auckland <- get_nz_river(x = "Auckland")
#' }
#' @export
get_nz_river <- function(x = "NZ", network = TRUE, plot = FALSE, order = 4:8, ...) {
    if(!is.character(x)) stop("Please provide a character string")
    if(length(x) > 1) stop("Please only provide a single river name or region")
    riv <- get_nz_rivers(plot = FALSE)
    if(x == "NZ") {
        res <- riv
    }else{
        if(x %in% regions) {
            idx <- grep(x, riv$REGION)
        }else{
            idx <- grep(x, riv$NAME)
            if(!length(idx) > 0) stop(paste("No river found matching", x))
        }
        if(!network) {
            res <- riv[idx, ]
        }else{
            net <- unique(riv@data[idx, "NETWORK"])
            res <- riv[riv$NETWORK %in% net, ]
        }
    }
    if(!is.null(order)) res <- res[res@data$ORDER %in% order, ]
    if(plot){
        p <- show_basic_eeda(res)
        print(p)
    }
    return(res)
}

