## ---- libs

library(eeda)

## ---- quiet
key <- read.delim("keys.txt",header = FALSE)$V1

## ---- key

eeda_auth(key = key)
    
## ---- data

data(mfe_names)
head(mfe_names,10)

## search
idx <- grep("invert",mfe_names[,1])
mfe_names[idx,]

## ---- get
## as spdf
invert <- get_mfe_data(id = "52713")
## as sf
invert_sf <- get_mfe_data(id = "52713", sf = TRUE)

## ---- base

## ---- sfplot
library(ggplot2)
ggplot(invert_sf) + geom_sf(aes(color = as.numeric(SiteMedian)), size = 2) +
    scale_color_gradient("MCI median") +
    theme_void()

## ---- eeda

