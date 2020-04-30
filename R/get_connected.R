#' Getting all self connected networks from a SpatialLines
#' or a SpatialLinesDataFrame object.
#' \code{get_connected} is a modified version of
#' \code{\link[sp2graph]{nt.connect}} that returns a numeric
#' vector specifying the network number elements in
#' \code{x} belong to.
#' @param x A SpatialLines or a SpatialLinesDataFrame object
#' @return A numeric vector the same length as \code{x}
#' specifying the network number each element belongs to
get_connected <- function(x) {
    graph <- shp2graph::readshpnw(x)
    edgelist <- graph[[3]]
    n_edges <- nrow(edgelist)
    ## intialize network vector (nodes)
    network <- rep(0, length = n_edges)
    ect <- 1
    visiting <- matrix(edgelist[1, ], ncol = 3)
    network[1] <- 1
    unvisited <- n_edges - 1
    unvisited_edge <- edgelist[-1, ]
    fromn <- unvisited_edge[, 2]
    ton <- unvisited_edge[, 3]
    while (unvisited > 0) {
        n <- dim(visiting)[1]
        visiting_edge_idxs <- c()
        for (i in 1:n) {
            idxs1 <- c(which(fromn == visiting[i, 2]),
                       which(ton == visiting[i, 2]),
                       which(fromn ==  visiting[i, 3]),
                       which(ton == visiting[i, 3]))
            visiting_edge_idxs <- c(visiting_edge_idxs, idxs1)
        }
        if (length(visiting_edge_idxs) > 0) {
            eidxs <- shp2graph:::norep(visiting_edge_idxs)
            visiting_edge <- unvisited_edge[eidxs, 1]
            network[visiting_edge] <- ect
            visiting <- matrix(unvisited_edge[eidxs, ], ncol = 3)
            unvisited_edge <- matrix(unvisited_edge[-eidxs, ], ncol = 3)
            unvisited <- unvisited - length(eidxs)
            fromn <- unvisited_edge[, 2]
            ton <- unvisited_edge[, 3]
        } else {
            visiting <- matrix(unvisited_edge[1, ], ncol = 3)
            ect <- ect + 1
        }
    }
    network
}
