## ---- libs

library(eeda)

## ---- data

data(niwa_names)
str(niwa_names)

## search
idx <- grep("Antarctic",niwa_names[,1])
niwa_names[idx,]
id <- niwa_names[22,3]

## ---- get
## as spdf
ross <- get_niwa_data(id = id)
## as sf
ross_sf <- get_niwa_data(id = id, sf = TRUE)


## ---- plot
## base
library(sp)
plot(ross)

## ggplot2 & geom_sf
library(ggplot2)
ggplot(ross_sf) + geom_sf()

## eeda and hex

locs <- as.data.frame(sp::coordinates(ross))
names(locs) <- c("x","y")
locs$river_id <- 1
p <- ggplot(locs) + geom_rivers(aes(x = x, y = y, river_id = river_id), xbins = 100)
library(plotly)
ggplotly(p, layerData= 2)

plot_ly(locs, x = ~x, y = ~y)

p <- ggplot(locs, aes(x, y)) + stat_binhex(bins = nrow(locs))## + theme(legend.position = "none")
ggplotly(p)
