## load packages

library(data.table)
library(tidyr)
library(plotly)
library(processx)


## load the data


load("../dat/county.RData")

load("../dat/dat_clean.RData")

## freq

dat <- dat[PropertyType == "Residential", ]

dat_df <- as.data.table(table(dat$Town, dat$ListYear))

colnames(dat_df) <- c("Town", "ListYear", "Freq")

dat_wise <- spread(dat_df, ListYear, Freq)

rownames(dat_wise) <- dat_wise$Town

dat_wise <- dat_wise[, Town := NULL]

dat_med <- dat[,  median(AssessedValue), by = .(ListYear, Town)]

dat_med <- spread(dat_med, ListYear, V1)

rownames(dat_med) <- dat_med$Town

dat_med <- dat_med[, Town := NULL]

dat_med[dat_med == 0] <- NA

p <- plot_ly(x = colnames(dat_med),
             y = rownames(dat_med),
             z = as.matrix(dat_med),
             type = "heatmap",
             colors = "Purples",
             colorbar = list(
                 title = "Median Assessed Price"
             )) %>%
    layout(xaxis = list(
               title = "Year"
           ),
           yaxis = list(
               title = "Town"
           ))

orca(p, "../report/heatmap_med.png")
