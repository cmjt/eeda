#' Function to explore pubically available datasets
#' at \url{https://data-niwa.opendata.arcgis.com/}
#' @importFrom magrittr %>%
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text

niwa_data <- function(){
    url <- "https://data-niwa.opendata.arcgis.com/datasets/ross-sea-biodiversity-survey-2004-bioross"
    pg <- xml2::read_html(url)
    abs <- (pg  %>% rvest::html_nodes("head") %>% rvest::html_nodes("meta"))[4] %>% rvest::html_attr("content")
    id <- (pg  %>% rvest::html_nodes("head") %>% rvest::html_nodes("meta"))[7] %>% rvest::html_attr("content")
    id <- gsub(".*items/(.+)/info.*", "\\1", id)
    id <- paste(id,"_0",sep = "")
    kml <- paste("https://opendata.arcgis.com/datasets/", id, ".kml",sep = "")
    download.file(kml,"~/Desktop/tst.kml")
    tst <- sf::st_read("~/Desktop/tst.kml")
}
