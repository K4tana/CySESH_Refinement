##Reanalysis of Borgert et al. (2023)
## This script will re-analyse validation data from Borgert et al. to test suitable items and IRT models with a state-of-the-art approach. 

#Required packages
pkg <- c("mirt", "tidyverse", "psych")
for (i in pkg){
  library(i, character.only = T)
}
set.seed(1337)
#gpcm
#load processed data directly from the github repository of Borgert et al. (2023)
rdata <- read_csv("Re-analysis/data_processed.csv")

#specify items
items <- c(8:19)

#fit unidimensional gpcm with mh algorithm (performs well with large amounts of data)
fit_g <- mirt(rdata[items], model=1, itemtype = "gpcmIRT", method="MHRM", verbose = T)

#analysis
summary(fit_g)
coef(fit_g)
plot(fit_g)
personfit(fit_g)
itemfit(fit_g)
fscores(fit_g) |> hist()
itemplot(fit_g, item = 1)
itemplot(fit_g, item = 2)
itemplot(fit_g, item = 3)
itemplot(fit_g, item = 4)
itemplot(fit_g, item = 5)
itemplot(fit_g, item = 6)
itemplot(fit_g, item = 7)
itemplot(fit_g, item = 8)
itemplot(fit_g, item = 9)
itemplot(fit_g, item = 10)
itemplot(fit_g, item = 11)
itemplot(fit_g, item = 12)

#DIF
#cut down dataframe to fit the command
difdat <- rdata[,c(items,4)]
fit_dif <- multipleGroup(difdat, model = 1, group = "gender", method = "MHRM")