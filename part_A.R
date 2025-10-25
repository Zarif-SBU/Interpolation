library(mice)
setwd("~/AMS 315 Project 1")
PartA_DV <- read.csv('Part A/422817_DV.csv', header = TRUE)
PartA_IV <- read.csv('Part A/422817_IV.csv', header = TRUE)
PartA <- merge(PartA_IV, PartA_DV, by = 'ID')
any(is.na(PartA[,2]) == TRUE)
any(is.nan(PartA[,2]) == TRUE)
any(is.null(PartA[,2]) == TRUE)

any(is.na(PartA[,3]) == TRUE)
any(is.nan(PartA[,3]) == TRUE)
any(is.null(PartA[,3]) == TRUE)

md.pattern(PartA)
PartA_imp <- PartA[!is.na(PartA$IV)==TRUE|!is.na(PartA$DV)==TRUE,]
imp <- mice(PartA_imp, method = "norm.boot", printFlag = FALSE)
PartA_complete <- complete(imp)
md.pattern(PartA_complete)

