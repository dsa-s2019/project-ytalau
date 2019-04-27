## load packages

library(data.table)
library(tidyr)
library(plotly)
library(processx)


## load the data


load( "../dat/dat_combine.RData")

load("../dat/dat_clean.RData")

load("../dat/dat_county.RData")

## plot freq

dat_wide <- spread(dat_county, ListYear, Freq)

rownames(dat_wide) <- dat_wide$county

dat_wide <- dat_wide[, county := NULL]


p <- plot_ly(x = colnames(dat_wide),
             y = rownames(dat_wide),
             z = as.matrix(dat_wide),
             type = "heatmap",
             colors = "Purples",
             colorbar = list(
                 title = "Amount of Sales"
             )) %>%
    layout(xaxis = list(
               title = "Year"
           ),
           yaxis = list(
               title = "County"
           ))

orca(p, "../report/heatmap_freq.png")

## plot the rate


combine_wide <- spread(dat_combine[, .(county, ListYear, rate)],
                       ListYear, rate)

rownames(combine_wide) <- combine_wide$county

combine_wide <- combine_wide[, county := NULL]


p <- plot_ly(x = colnames(combine_wide),
             y = rownames(combine_wide),
             z = as.matrix(combine_wide),
             type = "heatmap",
             colors = "Purples",
             colorbar = list(
                 title = "Rate of Sales"
             )) %>%
    layout(xaxis = list(
               title = "Year"
           ),
           yaxis = list(
               title = "County"
           ))

orca(p, "../report/heatmap_rate.png")


## plot the median prices
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
