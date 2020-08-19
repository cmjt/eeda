## ---- libs
library(eeda)

## ---- quiet
key <- read.delim("keys.txt",header = FALSE)$V1
eeda_auth(key = key)

## ---- demo

demo <- get_nz_rivers(rivers = TRUE, plot = TRUE)

## ---- waikato

get_nz_river_name(x = "waika")

waikato <- get_nz_river(x = "Waikato River",plot = TRUE)

## ---- canter

canter <- get_nz_river(x = "Canterbury",network = TRUE)
class(canter)
table(canter@data$REGION)

## ---- sf
library(sf)
library(ggplot2)
canter <- st_as_sf(canter)
ggplot(canter) + geom_sf(aes(color = REGION))
