#' Function to explore pubically available datasets
#' at \url{https://data-niwa.opendata.arcgis.com/}
#' @param id required, layer ID of the niwa dataset. Must be supplied
#' as a string of numbers (e.g., "c894b53b102f4f9db55278f7572ca4f6_0")
#' possible ids may be found using \code{avail_niwa()}
#' @inheritParams get_mfe_data
#' @importFrom magrittr %>%
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom rgdal ogrListLayers readOGR
#' @importFrom sf st_read
#' @export

get_niwa_data <- function(id, sf = FALSE, plot = FALSE, print = TRUE){
    base <- "https://data-niwa.opendata.arcgis.com/datasets/"
    tag <- niwa_names[which(niwa_names[,3] %in% id),2]
    url <- paste(base, tag,sep = "")
    pg <- read_html(url)
    if(print){
        abs <- (pg  %>% html_nodes("head") %>%
                html_nodes("meta"))[4] %>%
            html_attr("content")
        cat(abs)
    }else{
        browseURL(url)
    }
    file <- tempfile(fileext = ".kml")
    download.file(paste(base, id, ".kml", sep = "" ),destfile = file)
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
