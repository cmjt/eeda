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
ggplot(sf) + geom_rivers(aes(x = long, y = lat, river_id = id,xbins = 100 ))
