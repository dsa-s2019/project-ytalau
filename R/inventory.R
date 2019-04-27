## load the package

library(openxlsx)

inventory_1 <- read.xlsx("../dat/ct_housing_inventory_2000-2010_by_town.xlsx",
                         sheet = 1, startRow = 7, colNames = FALSE)

colnames(inventory_1) <- c("town",
                           "county",
                           2000:2010)

str(inventory_1)

inventory_2 <- read.xlsx("../dat/ct_housing_inventory_2010-present_by_town.xlsx",
                         sheet = 1,
                         startRow = 11,
                         colNames = FALSE)

colnames(inventory_2) <- c("town",
                           "county",
                           2010:2017)


## delete the 2017 data and keep the census 2010 data

ind <- which(colnames(inventory_1) %in% c("2000", "2010"))

inventory_1 <- inventory_1[1:169, - ind]
inventory_2 <- inventory_2[1:169, - length(inventory_2)]

inventory <- cbind(inventory_1, inventory_2[, -(1:2)])

save(inventory, file = "../dat/inventory.RData")
