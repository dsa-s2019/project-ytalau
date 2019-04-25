## Load packages

library('rvest')
library('data.table')
library('xml2')
library('zoo')

## grab the table

link <- "https://www1.ctdol.state.ct.us/lmi/misc/counties.asp"

webpage <- read_html(link)

nodes <- html_nodes(webpage, "table")

table <- html_table(nodes[16])

## some rows span the columns, need to clean up

str(table)

## List of 1
##  $ :'data.frame':	47 obs. of  5 variables:
##   ..$ X1: chr [1:47] "Fairfield County" "Bethel" "Easton" "New Fairfield" ...
##   ..$ X2: chr [1:47] "Fairfield County" "Bridgeport" "Fairfield" "Newtown" ...
##   ..$ X3: chr [1:47] "Fairfield County" "Brookfield" "Greenwich" "Norwalk" ...
##   ..$ X4: chr [1:47] "Fairfield County" "Danbury" "Monroe" "Redding" ...
##   ..$ X5: chr [1:47] "Fairfield County" "Darien" "New Canaan" "Ridgefield" ...

county <- data.table(town = unlist(table[[1]], use.names = FALSE))

county <- county[nchar(town) < 100 & town != "",
                 ][, county := town]

setDT(county)[!grepl(x =  town, pattern = "County"), county := NA]

county <- county[, county := na.locf(county)][county != town, ]

county <- county[, county := trimws(gsub("County", replacement = "", county))]

setorder(county, "county")

save(county, file = "../dat/county.RData")
