## load packages

library(data.table)
library(tidyr)


## load the data


load("../dat/inventory.RData")

load("../dat/dat_clean.RData")

load("../dat/county.RData")

## prepare county data
dat_df <- as.data.table(table(dat$Town, dat$ListYear))

colnames(dat_df) <- c("Town", "ListYear", "Freq")

setorder(dat_df, Town)

setorder(county, town)

dat_county <- merge(dat_df, county, by.x = "Town", by.y = "town")

dat_county <- dat_county[, sum(Freq), by = .(county, ListYear)
                         ][, Freq := V1][, V1 := NULL]

save(dat_county, file = "../dat/dat_county.RData")

## consider the stock
inventory <- as.data.table(inventory)

inventory[, town := NULL]

keycol <- "ListYear"

valuecol <- "stock"

gathercols <- as.character(2001:2016)

inventory_long <- gather_(inventory, key = keycol,
                          value = valuecol,
                          gathercols)

inventory_long <- as.data.table(inventory_long)

inventory_long <- inventory_long[, sum(stock), by = .(county, ListYear)
                                 ][, stock := V1, ][, V1 := NULL]

dat_combine <- merge(inventory_long, dat_county,
                     by.x = c("county", "ListYear"),
                     by.y = c("county", "ListYear"))


dat_combine <- dat_combine[, rate := Freq/stock]

save(dat_combine, file = "../dat/dat_combine.RData")
