#' Function to explore all available mfe spatial layer datasets available to
#' download
#' @examples \dontrun{
#' mfe_data()
#' }
#' @export
#' @importFrom rvest html_nodes html_table html_attr
#' @importFrom magrittr %>%
#' @importFrom xml2 read_html
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
    pages <- lapply(urls, read_html)
    titles <- lapply(pages, function(x) (x %>% html_table())[[2]][,1])
    titles <- unlist(titles)
    layers <- lapply(pages, function(x) x %>%
                                        html_nodes(".trigger-download-dialog")  %>%
                                        html_attr("data-item-id"))
    layers <- unlist(layers)
    lay.nums <- sapply(sapply(layers, strsplit, "layer."), function(x) x[2])
    res <- data.frame(Data = titles,"Layer number" = lay.nums)
    rownames(res) <- NULL
    return(res)
}

#' Function to download a specified spatial layer from
#' \url{http://data.mfe.govt.nz}
#' @param id required, layer ID of the mfe dataset. Must be supplied
#' as a character vector, either as a string of numbers (e.g., "53523")
#' or as the MfE layer id (e.g., "layer-53523")
#' @param key optional, manually set API key for \url{https://data.mfe.govt.nz/}
#' @param sf logical, return data as \code{sf}. FALSE by default
#' @param plot logical, if TRUE then object is plotted
#' @param print logical, if TRUE (default) then abstract of layer is plotted
#' otherwise url is opened
#' @return a \code{Spatial} or \code{sf} object of the requested data layer
#' @examples \dontrun{
#' get_mfe_data(id = "layer-53523")
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom httr GET write_disk
#' @importFrom rgdal ogrListLayers readOGR
#' @importFrom sf st_read
get_mfe_data <- function(id, key = NULL, sf = FALSE, plot = FALSE, print = TRUE) {
    if(is.na(charmatch("layer-",id))) id <- paste("layer-",id,sep = "")
    if(length(id) > 1) {
        print("id should be of length 1, using first element only")
        id <- id[1]
    }
    if(is.null(key)) {
        if(!"MfE_KEY" %in% names(eeda_keys()))
            stop("No MfE_KEY registered, use eeda_auth(...) to set yours")
        key <- eeda_keys()$MfE_KEY
    }
    if(!is.null(key)) print("MfE_KEY found")
    abs_url <- paste("https://data.mfe.govt.nz/layer/",id, sep = "")
    if(print){
        cat((read_html(abs_url) %>% html_nodes("div.fullSpan p") %>% html_text())[1])
    }else{
        browseURL(abs_url)
    }
    base_url <- "https://data.mfe.govt.nz"
    endpoint <- paste("/services;key=",key, "/wfs",sep = "")
    q <- list(request = "GetFeature", service = "WFS", typeNames = id,outputFormat = "KML")
    file <- tempfile(fileext = ".kml")
    GET(url = base_url, path = endpoint, query = q, write_disk(file))
    if(! sf){
        lay <- ogrListLayers(file)
        res <- readOGR(file, layer = lay)
    }else{
        res <- st_read(file)
    }
    unlink(file)
    if(plot) show_basic_eeda(res)
    return(res)
}

