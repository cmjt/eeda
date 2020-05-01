#' Function to find a NZ river by name. Rivers, by default, are matched to river segment names
#' specified by \url{https://data.linz.govt.nz/layer/103631-nz-river-name-polygons-pilot/}
#' and on the same network.
#' @param x A character string to search NZ river, can be partial. If specified then
#' \code{begin} is ignored. Any other regular expression can also be used.
#' @param begin A character string of the prefix to river name, if specified then \code{x}
#' is ignored.
#' @param ignore.case Logical. By default \code{TRUE}, speecifies \code{grep} to ignore letter case.
#' @return a vector of matching river names
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
#' Function to return \code{SpatialLinesDataFrame} of user specified NZ river(s) 
#' @param x A character string of length one spefifying either 1. the complete river NZ river name,
#' 2. the NZ province (e.g., \code{x = "Canterbury"}) of rivers to return, or
#' 3. "NZ" the default, the whole of NZ.
#' @param network Logical. If TRUE (default) then all connected rivers & streams are returned to the
#' segmentes named as \code{x}.
#' @param plot Logical If TRUE then a plot of the \code{SpatialLinesDataFrame}
#' in relation to NZ is plotted
#' @param order Optional; a single numeric value or a vector \code{%in%} 4:8 specifying
#' order of river segments
#' @param ... other arguments to pass into \code{show_nz_river()}
#' @return A \code{SpatialLinesDataFrame} or a \code{sf} object 
get_nz_river <- function(x = "NZ", network = TRUE, plot = FALSE, order = 4:8, ...) {
    if(!is.character(x)) stop("Please provide a character string")
    if(length(x) > 1) stop("Please only provide a single river name or region")
    riv <- nz_rivers
    if(x == "NZ") {
        res <- riv
    }else{
        if(x %in% regions) {
            idx <- grep(x, riv$REGION)
            res <- riv[idx, ]
        }else{
            idx <- grep(x, riv$NAME)
            if(!length(idx) > 0) stop(paste("No river found matching", x))
            if(!network) {
                res <- riv[idx, ]
            }else{
                net <- unique(riv@data[idx, "NETWORK"])
                res <- riv[riv$NETWORK %in% net, ]
            }
        }
    }
    if(!is.null(order)) res <- res[res@data$ORDER %in% order, ]
    if(plot) show_nz_river(res, ...)
    return(res)
}
#' Function to download example data
#' @param example character string of data queried \code{%in%}
#' \code{"NZ"}: \code{SpatialPolygonsDataFrame} of NZ main islands,
#' \code{"Waikato"}: \code{SpatialLinesDataFrame} of all connected
#' rivers & strams to thw Waikato river, or \code{"nz_rivers"}:
#' \code{SpatialLinesDataFrame} of all NZ rivers of order 4 above
get_hexr_data <- function(example = "Waikato") {
    if(!example %in% c("NZ", "Waikato", "nz_rivers"))
        stop("argument should be one of NZ, Waikato, or nz_rivers")
    if(example == "NZ") {
        url <- "https://github.com/cmjt/hexr/blob/master/gh-data/nz.rda?raw=true"
    }else{
        if(example == "Waikato") {
            url <- "https://github.com/cmjt/hexr/blob/master/gh-data/waikato.rda?raw=true"
        }else{
            if(example == "nz_rivers") {
                url <- "https://github.com/cmjt/hexr/blob/master/gh-data/rivers_nz.rda?raw=true"
            }
        }
    }
    tmp <- tempfile(fileext = ".rda")
    download.file(url, destfile = tmp)
    env <- new.env()
    nm <- load(tmp, env)[1]
    unlink(tmp)
    env[[nm]]
}
