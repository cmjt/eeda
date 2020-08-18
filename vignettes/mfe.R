## ---- library

library(hexrec)


    
## ---- what dara are available

require(magrittr); library(xml2); library(rvest)
available <- mfe_data()
str(available)
available[,1]

## ---- API key

key <- 	"99acb780f2604ed19efed0e4dd0beb25"	


id <-  "53523"

res <- get_mfe_data(key = key, id = id)

