#' @export
geom_rivers <- function(mapping = NULL, data = NULL,
                        stat =  "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
    do <- list(
        layer(
            data = data,
            mapping = mapping,
            stat = stat,
            geom = GeomRivers,
            position = PositionIdentity,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                na.rm = na.rm,
                ...
            )
        ),
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid = element_blank())
    )
    do
}

GeomRivers <- ggplot2::ggproto("GeomRivers", ggplot2::GeomPolygon,
                      default_aes = ggplot2::aes(colour = "NA", fill = "grey98",
                                        alpha = 1, size = 1, xbins = 30),
                      draw_panel = function(data, panel_params, coord) {
                          coords <- coord$transform(data, panel_params)
                          if("order" %in% names(coords)) {
                              ord <- coords$order
                          }else{
                              ord <- NULL
                          }
                          rivers_summary(x = coords$x, y = coords$y,
                                         bins = coords$xbins[1],
                                         river_id = coords$river_id,
                                         order = ord,
                                         gp =  grid::gpar(size = data$size,
                                                         col = data$colour,
                                                         fill = alpha(data$fill, data$alpha),
                                                         lwd = data$size * .pt)
                                                     )
                          },
                      required_aes = c("x", "y", "river_id")
                      )


rivers_summary <- function(x, y, bins, river_id, order,  gp = grid::gpar()) {
    hb <- hexbin::hexbin(x, y, xbins = bins, IDs = TRUE)
    ids <- hb@cID
    out <- hexbin::hcell2xy(hb)
    nx <- out$x
    ny <- out$y
    dx <- ggplot2::resolution(nx, FALSE)
    dy <- ggplot2::resolution(ny, FALSE) / sqrt(3) / 2 * 1.15
    n <- length(nx)
    hex_c <- hexbin::hexcoords(dx, dy, n = 1)
    hex_x <- hex_c$x
    hex_y <- hex_c$y
    ngp <- gp
    ## graphical parameters need to match length of hex data
    ngp$size <- aggregate(gp$size, list(ids), mean)[, 2]
    ngp$lwd <- aggregate(gp$lwd, list(ids), mean)[, 2]
    if(!hexr:::is.unique(ngp$fill)) {
        ngp$fill <- aggregate(gp$fill, list(ids), av_rgb)[, 2]
    }
    if(!hexr:::is.unique(ngp$col)) {
        ngp$col <- aggregate(gp$col, list(ids), av_rgb)[, 2]
    }
    if(!is.null(order)) {
        ord <- aggregate(order, list(ids), mean)[, 2]
        ord <- ord / mean(ord) ## scale size
    }else{
        ord <- 1
    }
    grid::polygonGrob(
              x = rep.int(hex_x, n) * rep(ord, each = 6) +
                  rep(nx, each = 6),
              y = rep.int(hex_y, n) * rep(ord, each = 6)  +
                  rep(ny, each = 6),
              default.units = "native",
              id.lengths = rep(6, n),
              gp = ngp)
}
