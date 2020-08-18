## ---- libs

library(hexrec)

## ---- demo

t <- hexrec(rivers = TRUE, plot = TRUE)

## ---- waikato

get_nz_river_name(x = "waika")

waikato <- get_nz_river(x = "Waikato River")

## ---- hex

sf <- broom::tidy(waikato)

library(ggplot2)
p <- ggplot(sf,aes(x = long, y = lat, river_id = id,xbins = 100 )) + geom_hex(bins = 100)


    ##geom_rivers() + geom_river_fill(aes(seg = 1, var = 2))
plotly::ggplotly(p)

p <- ggplot(diamonds, aes(x = log(carat), y = log(price))) + 
  geom_hex(bins = 100)
ggplotly(p)
