#' Function to see available mfe spatial layer datasets available to
#' download
#' @param key API key for \url{http://data.mfe.govt.nz}
#' @export
mfe_data <- function(key) {
    url <- paste("https://data.mfe.govt.nz/services;key=", key,
                 "/wfs?service=WFS&request=GetCapabilities", sep = "")
    pg <- xml2::read_html(url)
    titles <- pg %>% rvest::html_nodes("title") %>% rvest::html_text()
    abstracts <- pg %>% rvest::html_nodes("abstract") %>% rvest::html_text()
    layerid <- pg %>% rvest::html_nodes("name") %>% rvest::html_text()
    res <- data.frame(Title = titles[-1], Abstract = abstracts[-1],
                      Layerid = layerid)
    ley <- grep("layer", res[, 3])
    return(res[ley, ])
}
#' Function to get specific mfe spatial layer ID
#' @inheritParams mfe_data
#' @param x a character string to search mfe datasets
#' @return a vector of mfe dataset ids
#' @export
get_layer_id <- function(x, key) {
    abs <- mfe_data(key = key)
    idx <- unique(grep(x, abs[, 2]))
    id <- sub(".*:", "", abs[idx, 3])
    n <- length(idx)
    cat(paste(n, "results found"), "\n")
    res <- data.frame(id = id); rownames(res) <- abs[idx, 1]
    return(res)
}
#' Function to download a specified spatial layer from
#' \url{http://data.mfe.govt.nz}
#' @param id layer ID of mfe dataset
#' @inheritParams mfe_data
#' @param plot Logical. If TRUE will plot data
#' @export
get_mfe_data <- function(id, key, plot = FALSE) {
    if(length(id) > 1) {
        print("id should be of length 1, using first element only")
        id <- id[1]
    }
    urlp1 <- paste("https://data.mfe.govt.nz/services;key=", key, sep = "")
    urlp2 <- paste("/wfs?service=WFS&version=2.0.0&request=GetFeature&typeNames=",
                   id, "&outputFormat=KML", sep = "")
    url <- paste(urlp1, urlp2, sep = "")
    tmp <- tempfile(pattern = id, fileext = ".kml")
    download.file(url, destfile = tmp)
    lay <- rgdal::ogrListLayers(tmp)
    print(lay)
    res <- rgdal::readOGR(tmp, layer = lay)
    unlink(tmp)
    if(plot) show_nz_river(res)
    return(res)
}
