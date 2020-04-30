geom_river_fill <- function(mapping = NULL, data = NULL,
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
            geom = GeomRiverFill,
            position = PositionIdentity,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                na.rm = na.rm,
                ...
            )
        ),
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.grid=element_blank())
    )
    do
}

GeomRiverFill <- ggproto("GeomRiverFill", GeomPolygon,
                         default_aes = aes(colour = "NA", fill = "grey98",
                                           alpha = 1, size = 0.8, xbins = 30),
                         draw_panel = function(data, panel_params, coord) {
                             coords <- coord$transform(data, panel_params)
                             rivers_summary_fill(x = coords$x,y = coords$y, bins = coords$xbins[1],
                                                 var = coords$var,
                                                 seg = coords$seg[1],
                                                 river_id = coords$river_id,
                                                 gp =  grid::gpar(size = data$size,
                                                                  col = data$colour,
                                                                  fill = alpha(data$fill, data$alpha),
                                                                  lwd = data$size * .pt)
                                                 )
                             
                         },
                         required_aes = c("x","y","river_id","var","seg")
                         )


rivers_summary_fill <- function(x, y, bins,river_id,var, seg, gp = grid::gpar()) {
    hb <- hexbin::hexbin(x,y,xbins = bins,IDs = TRUE)
    ids <- hb@cID
    out <- hexbin::hcell2xy(hb)
    nx <- out$x
    ny <- out$y
    dx <- resolution(nx, FALSE)
    dy <- resolution(ny, FALSE) / sqrt(3) / 2 * 1.15
    n <- length(nx)
    hexC <- hexbin::hexcoords(dx, dy, n = 1)
    hex_x <- hexC$x
    hex_y <- hexC$y
    ngp <- gp
    ## graphical parameters need to match length of hex data
    ngp$size <- aggregate(var,list(ids),mean)[,2]
    ngp$size <- rep(ngp$size, each = 18)
    ngp$size <- matrix(ngp$size, nrow = 3)
    segs <- c(1,1,1,2,2,2)
    mask <- ifelse(seg == 1, 2, 1)
    ngp$size[,segs%in%mask] <- 0
    ## for(i in 1:length(mask)){
    ##     ngp$size[,seq(1,ncol(ngp$size), 5 +  mask[i] )] <- 0
    ## }
    ngp$lwd <- aggregate(gp$lwd,list(ids),mean)[,2]
    if(!is.unique(ngp$fill)){
        ngp$fill <- aggregate(gp$fill,list(ids),av.rgb)[,2]
    }
    tri <- get_tri(hex_x,hex_y,0,0)
    cx <- rep.int(tri$x, n)*ngp$size + rep(nx, each = 18)
    cy <- rep.int(tri$y, n)*ngp$size + rep(ny, each = 18)
    grid::polygonGrob(
              x = cx ,
              y = cy ,
              default.units = "native",
              id.lengths = rep(18, n),
              gp = ngp)
}

