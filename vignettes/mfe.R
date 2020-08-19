## ---- libs

library(eeda)

## ---- quiet
key <- read.delim("keys.txt",header = FALSE)$V1

## ---- key

eeda_auth(key = key)
    
## ---- data

available <- mfe_data()
str(available)
head(available,10)
## search for a keyword
idx <- grep("invert",available[,1])
available[idx,]

## ---- get

## as spdf
invert <- get_mfe_data(id = "52713")
class(invert)
## as sf
invert_sf <- get_mfe_data(id = "52713", sf = TRUE)
class(invert_sf)

## ---- base

## ---- sfplot
library(ggplot2)
ggplot(invert_sf) + geom_sf(aes(color = as.numeric(SiteMedian)), size = 2) +
    scale_color_gradient("MCI median") +
    theme_void()

## ---- eeda
