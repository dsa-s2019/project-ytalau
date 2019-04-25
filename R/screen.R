## load packages and data set

library(data.table)
dat <- fread("../dat/Real_Estate_Sales_2001-2016.csv",
             colClasses = "character")


## convert the variable types

dat <- dat[, AssessedValue := as.numeric(AssessedValue)
           ][, SaleAmount := as.numeric(SaleAmount)
             ][, SalesRatio := as.numeric(SalesRatio)
               ][, PropertyType := as.factor(PropertyType)
                 ][, ResidentialType := as.factor(ResidentialType)]

## understand the missing value in terms of rows n columns

(colSums(is.na(dat)))
##              ID    SerialNumber        ListYear    DateRecorded            Town 
##               0               0               0               0               0 
##         Address   AssessedValue      SaleAmount      SalesRatio    PropertyType 
##              48             304           30792             833            3586 
## ResidentialType      NonUseCode         Remarks 
##          213197          308283          705931 

## many columns have missing values
## it is okay for some columns to have missing values
## many observations have nonmissing NonUseCodes: probably AssessedValue
## is better for further analysis than SaleAmount

## looking into AssessedValue, SaleAmount, SalesRatio, and NonUsecode

## try to impute the AssessedValue from SaleAmount and SalesRatio


setDT(dat)[!is.na(NonUseCode), SaleAmount := NA]

dat <- dat[, N := sum(is.na(AssessedValue),
                      is.na(SaleAmount),
                      is.na(SalesRatio)),
    by = 1:nrow(dat)]

setDT(dat)[N == 1 & is.na(AssessedValue),
           AssessedValue := SaleAmount/SalesRatio]

sum(is.na(dat[, .(AssessedValue)]))
## 187


## duplicate observations

## overall duplications

sum(duplicated(dat[, -c("ID")]))
## 5402

## remove 5401 duplicate observations

dat <- dat[!duplicated(dat[, -c("ID")]), ]

## double check

sum(duplicated(dat[, .("Address", "Town", "ListYear")]))
## 0

save(dat, file = "../dat/dat_clean.RData")
