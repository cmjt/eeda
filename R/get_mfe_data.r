#' Function to exploe all available mfe spatial layer datasets available to
#' download
#' @export
mfe_data <- function() {
    url <- "https://data.mfe.govt.nz/layers/?v=rows"
    pg <- read_html(url)
    ## get the total number of items
    items <- as.numeric(
        strsplit(
        (pg %>%
         html_nodes("span.listPagerHeaderTitle") %>%
         html_text())[2],"of ")[[1]][2])
    num.pages <- ceiling(items/30)
    ## all page urls
    urls <- paste("https://data.mfe.govt.nz/layers/?page=",1:num.pages,"&v=rows", sep = "")
    ## read all pages
    pages <- lapply(urls, xml2::read_html)
    titles <- lapply(pages, function(x) (x %>% rvest::html_table())[[2]][,1])
    titles <- unlist(titles)
    layers <- lapply(pages, function(x) x %>%
                                        rvest::html_nodes(".trigger-download-dialog")  %>%
                                        rvest::html_attr("data-item-id"))
    layers <- unlist(layers)
    lay.nums <- sapply(sapply(layers, strsplit, "layer."), function(x) x[2])
    rownames(lay.nums) <- NULL
    return(data.frame(Data = titles,"Layer number" = lay.nums))
}

#' Function to download a specified spatial layer from
#' \url{http://data.mfe.govt.nz}
#' @param id layer ID of mfe dataset
#' @param key API key for \url{https://data.mfe.govt.nz/}
#' @export
get_mfe_data <- function(id, key) {
    if(length(id) > 1) {
        print("id should be of length 1, using first element only")
        id <- id[1]
    }
    id <- paste("layer-",id, sep = "")
    base_url <- "https://data.mfe.govt.nz"
    endpoint <- paste("/services;key=",key, "/wfs",sep = "")
    q <- list(request = "GetFeature", service = "WFS", typeNames = id,outputFormat = "KML")
    file <- tempfile(fileext = ".kml")
    httr::GET(url = base_url, path = endpoint, query = q, httr::write_disk(file))
    lay <- rgdal::ogrListLayers(file)
    res <- rgdal::readOGR(file, layer = lay)
    unlink(file)
    return(res)
}

#' @importFrom magrittr %>%
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
