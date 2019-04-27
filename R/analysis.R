## load packages

library(data.table)
library(CARBayesST)
library(dplyr)
library(spdep)
library(maptools)
library(maps)


## load the data set

load("../dat/dat_combine.RData")

## construct the neigborhood matrix

dat.av <- summarise(group_by(dat_combine, county),
                    rate.mean = median(rate))

ct.county <- map("county",
                 "connecticut",
                 fill = TRUE,
                 plot = FALSE)

county.id <- sapply(strsplit(ct.county$names, ","),
                    function(x) x[2])

ct.poly <- map2SpatialPolygons(ct.county, IDs = county.id)

W.nb <- poly2nb(ct.poly, row.names = dat.av$rate.mean)
W <- nb2mat(W.nb, style = "B")


## spatially varying linear time trend

setorder(dat_combine, ListYear)

formula <- Freq ~ offset(log(stock))

model1 <- ST.CARsepspatial(formula = formula,
                           family = "poisson",
                           data = dat_combine,
                           W = W,
                           burnin = 20000,
                           n.sample = 220000,
                           thin = 10)

trend.mean <- array(NA, c(16, 3))

trend.sd <- array(NA, c(16, 3))

for(i in 1:16)

{posterior <- exp(model1$samples$phi[ , ((i-1) * 8 + 1):(i * 8)] +
                  matrix(rep(model1$samples$beta +
                             model1$samples$delta[ , i], 8),
                              ncol = 8, byrow = FALSE))
trend.mean[i, ] <- quantile(apply(posterior, 1, mean),
                            c(0.5, 0.025, 0.975))
trend.sd[i, ] <- quantile(apply(posterior, 1, sd),
                          c(0.5, 0.025, 0.975))}

png('results.png')
par(mfrow = c(2,1))
plot(jitter(as.numeric(dat_combine$ListYear)),
            dat_combine$rate,
            pch = 19,
            cex = 0.2,
            col="blue",
            xlab="Year",
            main="(a)",
            ylab="Average sales rate",
            ylim=c(0, 0.20),
            cex.axis=1.5,
            cex.lab=1.5,
            cex.main=1.5)
lines(2001:2016, trend.mean[ ,1], col="red", type="l")
lines(2001:2016, trend.mean[ ,2])
lines(2001:2016, trend.mean[ ,3])


plot(2001:2016, trend.sd[ ,1],
     col = "red",
     type = "l",
     xlab = "Year",
     main = "(b)",
     ylab = "Spatial standard deviation",
     ylim = c(0, 0.15),
     cex.axis = 1.5,
     cex.lab = 1.5,
     cex.main = 1.5)
lines(2001:2016, trend.sd[ ,2])
lines(2001:2016, trend.sd[ ,3])
dev.off()
